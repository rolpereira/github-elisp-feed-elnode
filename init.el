;; -*- lexical-binding: t -*-
(add-to-list 'load-path
             (file-name-directory (or load-file-name
                                      (buffer-file-name (current-buffer)))))

(require 'elnode)
(require 'atom)

(defvar show-information-regarding-forks t
  "If T then add to the title of each feed the repository that was forked to create the new repository.

This functionality isn't on by default because Heroku doesn't
seem to like it very much, although it works on my machine.

Here's an example using a fork of the Melpa repo to show how this
variable affects the title of the feeds:

* Without fork information the title of the feed:
  rolando2424: melpa

* With fork information it would be:
  rolando2424: melpa (forked from milkypostman/melpa)")


;;; Functions used to extract the information found in "https://github.com/languages/Emacs%20Lisp/created"
(defun find-repos ()
  "Return the name of the repositories found in the current buffer"
  (message "Calling `find-repos'")
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "<td class=\"title\">\n.*?\">\\(.*?\\)</a>" nil t)
	  collect (match-string 1))))

(defun find-owners ()
  "Return the owners of the repositories found in the current buffer"
  (message "Calling `find-owners'")
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "<td class=\"owner\">\n.*?\">\\(.*?\\)</a>" nil t)
	  collect (match-string 1))))

(defun find-dates ()
  "Return the creation dates of the repositories found in the current buffer"
  (message "Calling `find-dates'")
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "<td class=\"date\">\n *\\(.*?\\)\n" nil t)
	  collect (match-string 1))))

(defun find-descs ()
  "Return the description of the repositories found in the current buffer"
  (message "Calling `find-descs'")
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "<td .*class=\"desc\">\\(.*?\\)</td>" nil t)
	  collect (match-string 1))))

(defun find-forked-repo (owner repo)
  "Check if the REPO created by OWNER was forked from another repo and return it.

If no repository was forked to create REPO, then return NIL."
  (message (concat "Calling `find-forked-repo' with arguments " owner " " repo))
  (let ((repo-url (concat "http://github.com/" owner "/" repo)))
    (with-current-buffer (url-retrieve-synchronously repo-url)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "<span class=\"text\">forked from <.*?>\\(.*?\\)</a>" nil t)
        (match-string 1)))))



;;; Helper functions
(defun convert-github-time-to-internal-time (gh-time)
  "Convert a string of type \"DD MON HH:YY\" into the internal representation used by Emacs"
  (let ((current-year (string-to-number (format-time-string "%Y"))))
    (apply 'encode-time
	   (multiple-value-bind (sec min hour day month) (parse-time-string gh-time)
				(list sec min hour day month current-year)))))


;;; Elnode handler
(defun github-fetcher-handler (httpcon)
  (elnode-http-start httpcon "200" '("Content-type" . "application/atom+xml")
		     `("Server" . ,(concat "GNU Emacs " emacs-version)))

  (let* ((my-atom-feed (atom-create "New GitHub Emacs Lisp Repos" "http://github-elisp.herokuapp.com"))
	 (repo-info (with-current-buffer (url-retrieve-synchronously "https://github.com/languages/Emacs%20Lisp/created")
		      ;; Merge the information found in the webpage
                      (loop for repo in (find-repos)
			    for owner in (find-owners)
			    for date in (find-dates)
			    for descs in (find-descs)
			    collect (list owner repo date descs
					  (when show-information-regarding-forks
					    (find-forked-repo owner repo)))))))
    (dolist (repo repo-info)
      (atom-add-text-entry my-atom-feed
			   (if (and show-information-regarding-forks (fifth repo))
			       (concat (first repo) ": " (second repo) " (forked from " (fifth repo) ")")
			     (concat (first repo) ": " (second repo)))
			   (concat "http://github.com/" (first repo) "/" (second repo))
			   (fourth repo)
			   (convert-github-time-to-internal-time (third repo))))

    (elnode-http-return httpcon
			(with-temp-buffer
			  (atom-print my-atom-feed)
			  (buffer-substring-no-properties (point-min) (point-max))))))



(elnode-start 'github-fetcher-handler (string-to-number (or (getenv "PORT") "8080")) "0.0.0.0")

(while t
  (accept-process-output nil 1))

