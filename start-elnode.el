;; -*- lexical-binding: t -*-
;;; init.el --- Show Elisp repos recently created in Github

;; Copyright (C) 2012  Rolando Pereira

;; Author: Rolando Pereira <rolando_pereira@sapo.pt>
;; Keywords: elnode, github, atom, feed

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file uses elnode and heroku to fetch the github page showing
;; the newest elisp repos and displays them as a Atom feed.
;;
;; The page used to find theses repos is:
;; "https://github.com/languages/Emacs%20Lisp/created"
;;
;; You can find this code running in Heroku in the following URL:
;; "http://github-elisp.herokuapp.com"
;;
;; If you want to read this feed in Gwene.org, its name is
;; "gwene.com.herokuapp.github-elisp".

;;; Code:


(add-to-list 'load-path
             (file-name-directory (or load-file-name
                                      (buffer-file-name (current-buffer)))))

(require 'elnode)
(require 'atom)
(require 'string-utils)
(require 'sgml-mode)

;;; Functions used to extract the information found in "https://github.com/languages/Emacs%20Lisp/created"
(defun find-html-structures ()
  (goto-char (point-min))
  (loop with html-structures = '()
	while (re-search-forward "<li class=\"public" nil t)
	do (let ((begin-point (point))
		 end-point)
	     (sgml-skip-tag-forward 1)
	     (setq end-point (point))
	     (push (buffer-substring-no-properties begin-point end-point)
		   html-structures))
	finally return (reverse html-structures)))

(defun find-repo (html-structure)
  "Return, for every repository found in HTML-STRUCTURE, its namespaces"
  (with-temp-buffer
    (insert html-structure)
    (goto-char (point-min))
    (re-search-forward "<span class=\"mega-icon.*\n.*<a.*?>\\(.*?\\)</a>" nil t)
    (match-string 1)))
  
(defun find-owner (html-structure)
  "Return, for every repository found in HTML-STRUCTURE, its owner"
  (with-temp-buffer
    (insert html-structure)
    (goto-char (point-min))
    (re-search-forward "<span class=\"mega-icon.*\n.*<a.*?\"/\\(.*?\\)/" nil t)
    (match-string 1)))

(defun find-updated-date (html-structure)
  "Return, for every repository found in HTML-STRUCTURE, the date when it was last updated"
  (with-temp-buffer
    (insert html-structure)
    (goto-char (point-min))
    (re-search-forward "datetime=\"\\(.*?\\)\"" nil t)
    (match-string 1)))

(defun find-description (html-structure)
  "Return, for every repository found in HTML-STRUCTURE, its description"
  (with-temp-buffer
    (insert html-structure)
    (goto-char (point-min))
    (condition-case nil
	(let (begin-region
	      end-region)
	  (re-search-forward "<p class=\"description\">" nil)
	  (setq begin-region (point))
	  (re-search-forward "</p>" nil t)
	  (setq end-region (match-beginning 0))
	  (string-utils-trim-whitespace (buffer-substring begin-region end-region)))
      (search-failed nil))))

(defun find-forked-repo (html-structure)
  "Return, for every repository found in HTML-STRUCTURE, the repository used to fork it or NIL if it wasn't forked"
  (with-temp-buffer
    (insert html-structure)
    (goto-char (point-min))
    (condition-case nil
	(let (begin-region
	      end-region)
	  (re-search-forward "<p class=\"fork-flag\">.*?<a href=\"/\\(.*?\\)\"" nil)
	  (match-string 1))
      (search-failed nil))))


;;; Helper functions

;;; Elnode handler
(defun github-fetcher-handler (httpcon)
  (elnode-http-start httpcon "200" '("Content-type" . "application/atom+xml")
                     `("Server" . ,(concat "GNU Emacs " emacs-version)))

  (let* ((my-atom-feed (atom-create "New GitHub Emacs Lisp Repos" "http://github-elisp.herokuapp.com"))
         (repo-info (with-current-buffer (url-retrieve-synchronously "https://github.com/languages/Emacs%20Lisp/created")
                      ;; Merge the information found in the webpage
                      (prog1
			  (loop for html-structure in (find-html-structures)
				collect (list (find-owner html-structure)
					      (find-repo html-structure)
					      (find-updated-date html-structure)
					      (find-description html-structure)
					      (find-forked-repo html-structure)))
					 
                        ;; (loop for repo in (find-repos)
                        ;;       for owner in (find-owners)
                        ;;       for date in (find-dates)
                        ;;       ;; for descs in (find-descs)
                        ;;       collect (list owner repo date ;; descs
                        ;;                     (when show-information-regarding-forks
                        ;;                       (find-forked-repo owner repo))))
                        (kill-buffer)))))
    (dolist (repo repo-info)
      (atom-add-text-entry my-atom-feed
                           (if (fifth repo)
                               (concat (first repo) ": " (second repo) " (forked from " (fifth repo) ")")
                             (concat (first repo) ": " (second repo)))
                           (concat "http://github.com/" (first repo) "/" (second repo))
                           (fourth repo)
			   (atom-parse-time (third repo))))


    (elnode-http-return httpcon
                        (with-temp-buffer
                          (atom-print my-atom-feed)
                          (buffer-substring-no-properties (point-min) (point-max))))))



(elnode-start 'github-fetcher-handler (string-to-number (or (getenv "PORT") "8080")) "0.0.0.0")

(while t
  (accept-process-output nil 1))

;;; init.el ends here
