;;; atom.el --- Create an Atom feed

;; Copyright (C) 2011  Frédéric Perrin

;; Author: Frédéric Perrin <frederic.perrin@resel.fr>
;; Keywords: www, hypermedia, atom, rss

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

;; This is a library for creating an Atom feed from a Lisp program.
;; The normal usage is to create a feed with `atom-create', giving it
;; a title and a Web address. Once the feed has been created, entries
;; may be added to the feed, by specifying (at the minimum) a title, a
;; permanent link and the content of the entry. Text-only, HTML and
;; XHTML entries are supported.

;; It is possible to produce both Atom and RSS feeds.

;; A typical usage would look like this:

;; (let ((my-atom-feed (atom-create "My feed" "http://example.org")))
;;   ; A simple, text-only entry
;;   (atom-add-text-entry
;;    my-atom-feed
;;    "Hello world"
;;    "http://example.org/hello"
;;    "Hello the world!")
;;
;;   (atom-add-xhtml-entry
;;    my-atom-feed
;;    "An XHTML example"
;;    "http://example.org/html-example"
;;    "<p>One can also use <acronym>XHTML</acronym> in the entries.</p>")
;;
;;   (atom-print my-atom-feed)
;;   ;; If you prefer RSS feeds:
;;   (atom-to-rss-print my-atom-feed))

;; Full documentation is available at <http://tar-jx.bz/code/atom.html>.

;;; Code:

(require 'xml)
(require 'url-parse)
(require 'cl) ; for setf in url-canonalize

(defun atom-create (title link &optional subtitle self id author updated)
  "Create a new atom structure.

TITLE is the title for the feed, a short, text-only, human
readable string.

LINK is the URL of a page responible for the content of this
feed.

SUBTITLE is a subtitle for the feed; it can be a bit longer than
TITLE, maybe a paragraph long.

SELF is the canonical URL to this feed.

ID is a unique identifier for this feed. If not given, it
defaults to SELF.

AUTHOR is the author of the feed. See `atom-massage-author' for
the possible ways to specify it. In particular, `nil' uses
`user-full-name' and `user-mail-address'.

UPDATED is the date the feed was last updated. If not given,
`(current-time)' is used."
  (let ((atom-feed (list (list 'title nil title))))
    (atom-modify-entry atom-feed 'link `(((href . ,link))))
    (atom-modify-entry atom-feed 'author (atom-massage-author author))
    (if subtitle (atom-modify-entry atom-feed 'subtitle subtitle))
    (if self (atom-modify-entry atom-feed 'link
				`(((href . ,self) (rel . "self")
				   (type . "application/atom+xml")))))
    (atom-modify-entry atom-feed 'updated (atom-format-time updated))
    (atom-modify-entry atom-feed 'id (or id self link))
    atom-feed))

(defun atom-push-entry (atom entry)
  "Add the entry ENTRY to the feed ATOM."
  (nconc atom (list `(entry nil ,@entry))))

(defun atom-modify-entry (entry name val)
  "Set the NAME element of ENTRY to VAL."
  (let ((elem (if (stringp val)
		  (list name nil val)
		(cons name val))))
    (nconc entry (list elem))))

(defun atom-add-entry (atom title link content
			    &optional updated id summary)
  "Add an entry to the atom flux ATOM. Return the newly added
entry.

TITLE is a short, text-only, human readable string.

LINK is a permanent link for this entry. For a given entry, LINK
may change between successive generations of the atom feed.

CONTENT is the content of the entry; use `atom-add-html-entry'
or `atom-add-xhtml-entry' when CONTENT is not text-only.

If SUMMARY is not given, the entry will not contain any summary.

UPDATED defaults to `(current-time)' if omitted, which is
probably not a very good default.

ID defaults to LINK, which is not optimal; see `atom-generate-id'
for a way to create good identifiers. For a given entry, it must
not change between successive generations of the atom feed, even
when the content of the entry ."
  (let ((entry (list (list 'title nil title))))
    (atom-modify-entry entry 'link  (list (list (cons 'href link))))
    (atom-modify-entry entry 'id (or id link))
    (atom-modify-entry entry 'updated (atom-format-time updated))
    (if summary (atom-modify-entry entry 'summary summary))
    (atom-modify-entry entry 'content content)
    (atom-push-entry atom entry)
    entry))

(defalias 'atom-add-text-entry 'atom-add-entry
  "Add an entry to ATOM, with a textual content. See
`atom-add-entry' for details.")

(defun atom-add-html-entry (atom title link content
				  &optional updated id summary)
  "Add an entry to ATOM, with some HTML content. CONTENT should
be a string enconding a valid HTML fragment. See `atom-add-entry'
for additional details."
  (atom-add-entry atom
   title link (atom-massage-html content)
   updated id (and summary (atom-massage-html summary))))

(defun atom-add-xhtml-entry (atom title link content
				  &optional updated id summary noconvert)
  "Add an entry to ATOM, with some XHTML content. CONTENT may be
given either as a string, or as an XML tree, of a valid XHTML
fragment. See `atom-add-entry' for additional details.

If NOCONVERT is nil, translate all links in CONTENT so that they
are no longer relative to LINK."
  (let ((xhtml-content (atom-massage-xhtml content)))
    (unless noconvert
      (atom-xhtml-convert-links (cadr xhtml-content) link))
    (atom-add-entry atom
		    title link xhtml-content
		    updated id (and summary (atom-massage-xhtml summary)))))

(defun atom-print (atom)
  "Print the Atom feed ATOM in the current buffer."
  (insert atom-xml-declaration)
  (insert "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n")
  (xml-print atom)
  (insert "\n</feed>"))

(defun atom-write-file (atom filename)
  "Writes the feed ATOM to FILENAME."
  (with-temp-buffer
    (atom-print atom)
    (write-file filename)))


(defun atom-to-rss (atom)
  "Translate an Atom feed into an RSS one, returning the translation.

Some information may be lost or approximated."
  (let ((rss (list (assoc 'title atom))))
    (atom-to-rss-translator atom rss '((subtitle . description)
				       (updated . pubDate)
				       (link . link)))
    (atom-to-rss-modify-time rss)
    (atom-to-rss-modify-link rss)
    (dolist (entry (xml-get-children atom 'entry))
      (push (atom-to-rss-item entry) rss))
    (reverse rss)))

(defun atom-to-rss-item (entry)
  "Translates an Atom entry into an RSS item."
  (let ((item (list (assoc 'title entry))))
    (atom-to-rss-translator
     (xml-node-children entry) item
     '((id . guid) (content . description) (updated . pubDate) (link . link)))
    (atom-to-rss-modify-time item)
    (atom-to-rss-modify-link item)
    (let ((guid (assoc 'guid item))
	  (descr (assoc 'description item)))
      (if guid
	  (setcar (cdr guid) (list (cons 'isPermaLink "false"))))
      (if (and descr
	       (equal (xml-get-attribute descr 'type) "xhtml"))
	  (setcar (cddr descr) (xml-node-as-text descr))))
    `(item nil ,@item)))

(defun atom-to-rss-translator (source target translations)
  (dolist (translation translations)
    (let* ((from (car translation))
	   (to (cdr translation))
	   (data (copy-tree (cdr (assoc from source)))))
      (when data
	(atom-modify-entry target to data)))))

(defun atom-to-rss-modify-link (entry)
  (let* ((link (assoc 'link entry))
	 (link-addr (xml-get-attribute-or-nil link 'href)))
    (when link
      (setcar (cdr link) nil)
      (setcdr (cdr link) (cons link-addr nil)))))

(defun atom-print-as-rss (atom)
  (let ((rss (atom-to-rss atom)))
    (insert atom-xml-declaration)
    (insert "<rss version=\"2.0\">\n")
    (insert "  <channel>\n")
    (xml-print rss "    ")
    (insert "\n  </channel>\n")
    (insert "</rss>")))

(defun atom-to-rss-time (time)
  "Translates a string from the format used by Atom into the
format used by RSS."
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %T %z" (atom-parse-time time))))

(defun atom-to-rss-modify-time (entry)
  "Modify ENTRY, changing the format of the `pubDate' in it."
  (let ((pubDate (assoc 'pubDate entry)))
    (setcar (cddr pubDate)
	    (atom-to-rss-time (car (xml-node-children pubDate))))))

(defun atom-to-rss-write-file (atom filename)
  "Saves ATOM as a RSS feed into FILENAME."
  (with-temp-buffer
    (atom-print-as-rss atom)
    (write-file filename)))


(defvar atom-time-format-string "%Y-%m-%dT%T%z"
  "The format for string representation of dates.")

(defvar atom-xhtml-namespace "http://www.w3.org/1999/xhtml")

(defvar atom-xml-declaration "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")

(defun atom-format-time (&optional time)
  "Format a time according to RFC3339."
  ;; The time zone must be specified in numeric form, but with a colon between
  ;; the hour and minute parts.
  (replace-regexp-in-string
   "\\(..\\)$" ":\\1"
   (format-time-string atom-time-format-string time)))

(defun atom-parse-time (&optional time)
  "Parse a time as specified in RFC3339 into Emacs's native format."
  ;; Same remark as in `atom-format-time'
  (date-to-time (replace-regexp-in-string ":\\(..\\)$" "\\1" time)))

(defun atom-massage-html (content)
  "Massage CONTENT so it can be used as an HTML fragment in an
Atom feed. CONTENT must be a string."
  (list '((type . "html")) content))

(defun atom-string-to-xml (string)
  "Convert STRING into a Lisp structure as used by `xml.el'."
  (require 'xml-xhtml-entities)
  (let ((xml-entity-alist xml-xhtml-entities)
	(xml-validating-parser t))
    (with-temp-buffer
      (insert "<div xmlns=\"" atom-xhtml-namespace "\">")
      (insert string)
      (insert "</div>")
      ;; `xml-parse-region' returns a list of elements, even though it
      ;; requires an only root node. We are only interested in the first
      ;; one, the DIV we just inserted.
      (car (xml-parse-region (point-min) (point-max))))))

(defun atom-massage-xhtml (content)
  "Massage CONTENT so it can be used as an XHTML fragment in an
Atom feed."
  (list '((type . "xhtml"))
	(or (and (stringp content)
		 (atom-string-to-xml content))
	    `(div ((xmlns . ,atom-xhtml-namespace)) ,@content))))

(defun atom-massage-author (author)
  "Return an XML node representing the author. AUTHOR can be:
- nil, in which case `user-full-name' and `user-mail-address' are
  used;
- a single string, the full name of the author; no email address
  will be included;
- a list with two elements, the full name and the email address
  of the author;
- something else, assumed to be a complete `atomPersonConstruct'."
  `(nil ,@(cond
	   ((null author) `((name nil ,user-full-name)
			    (email nil ,user-mail-address)))
	   ((stringp author) `((name nil ,author)))
	   ((= 2 (length author)) `((name nil ,(car author))
				    (email nil ,(cadr author))))
	   (t `(author nil ,author)))))

(defun atom-xhtml-convert-links (node base)
  "Make all links in NODE (a fragment of an XHTML document)
absolute, in the context of BASE, an URL."
  (dolist (attr-name (list 'href 'src))
    (let ((attr (assoc attr-name (xml-node-attributes node))))
      (when attr (setcdr attr (url-canonalize (cdr attr) base)))))
  (dolist (child (xml-node-children node))
    (when (listp child) (atom-xhtml-convert-links child base))))

(defun atom-generate-id (link creation-date)
  "Generate a string suitable for use as an atom:id element. This
implements Mark Pilgrom's tag: URI method, using the
CREATION-DATE of the entry, and the domain part of LINK."
    (format "tag:%s,%s:/%s"
	    (url-host (url-generic-parse-url link))
	    (format-time-string "%Y-%m-%d" creation-date)
	    (format-time-string "%Y%m%d%H%M%S" creation-date)))


;;; Functions that should probably not be there

(defun url-canonalize (address base)
  "Make ADRESS an absolute URL, taking it in the BASE context."
  ;; I feel such a function should exist in `url-parse'. Did I miss it?
  (let ((url-base (url-generic-parse-url base))
	(url-address (url-generic-parse-url address)))
    (if (url-host url-address)
	address
      (setf (url-filename url-base)
	    (expand-file-name address
			      (file-name-directory (url-filename url-base))))
      (url-recreate-url url-base))))

(defun xml-node-as-text (node)
  "Return a string representing NODE, an XML structure."
  (with-temp-buffer
    (xml-print (xml-node-children node))
    (buffer-string)))

(defun xml-node-create (name attrlist childlist)
  "Create a new XML node."
  (list name attrlist . childlist))

(provide 'atom)
;;; atom.el ends here
