;; -*- lexical-binding: t -*-
(add-to-list 'load-path
             (file-name-directory (or load-file-name
                                      (buffer-file-name (current-buffer)))))
(require 'elnode)

(defun convert-github-time-to-internal-time (gh-time)
  (apply 'encode-time
    (multiple-value-bind (sec min hour day month) (parse-time-string gh-time)
      (let ((current-year (string-to-number (format-time-string "%Y"))))
        (list sec min hour day month current-year)))))

(defun find-repos ()
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "<td class=\"title\">\n.*?\">\\(.*?\\)</a>" nil t)
      collect (match-string 1))))

(defun find-owners ()
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "<td class=\"owner\">\n.*?\">\\(.*?\\)</a>" nil t)
      collect (match-string 1))))

(defun find-dates ()
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "<td class=\"date\">\n *\\(.*?\\)\n" nil t)
      collect (match-string 1))))

(defun find-descs ()
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "<td .*class=\"desc\">\\(.*?\\)</td>" nil t)
      collect (match-string 1))))



(defun github-fetcher-handler (httpcon)
  (elnode-http-start httpcon "200" '("Content-type" . "application/atom+xml")
    `("Server" . ,(concat "GNU Emacs " emacs-version)))
  (let* ((user-mail-address "rolando_pereira@sapo.pt")
          (my-atom-feed (atom-create "New GitHub Emacs Lisp Repos" "http://github-elisp.herokuapp.com"))
          (repo-info (with-current-buffer (url-retrieve-synchronously "https://github.com/languages/Emacs%20Lisp/created")
                      (loop for repo in (find-repos)
                        for owner in (find-owners)
                        for date in (find-dates)
                        for descs in (find-descs)
                        collect (list owner repo date descs)))))
    (dolist (repo repo-info)
      (atom-add-text-entry my-atom-feed
        (concat (first repo) ": " (second repo))
        (concat "https://github.com/" (first repo) "/" (second repo))
        (fourth repo)
        (convert-github-time-to-internal-time (third repo))))
                                        ;(format-time-string  "%Y-%m-%dT%T%z")))
    (elnode-http-return httpcon
      (with-temp-buffer
        (atom-print my-atom-feed)
        (buffer-substring-no-properties (point-min) (point-max))))))




(elnode-start 'github-fetcher-handler (string-to-number (or (getenv "PORT") "8080")) "0.0.0.0")

(while t
  (accept-process-output nil 1))

