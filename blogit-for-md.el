;;; blogit-for-md.el ---

;; Copyright (c) 2014 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: html blog org-mode
;; X-URL: http://github.com/coldnew/emacs-blogit-for-md
;; Version: 0.3

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

;;; Dependencies

(eval-when-compile (require 'cl))

(require 'ox-html)
(require 'ox-md)
(require 'ox-publish)
(require 's)


;;; Define Back-End for org-export

(org-export-define-derived-backend 'blogit-md 'md
  :translate-alist
  '(
    ;; Use emacs buildin syntax highlight
    (src-block . org-blogit-src-block)
    ;; Fix for multibyte language
    (paragraph . org-blogit-paragraph)
    ;; Fix link path to suite for ghost
    (link . org-blogit-link)
    ;; Increase headline level
    (headline . org-blogit-headline)
    ;; Fix toc for blogit theme
    (inner-template . org-blogit-inner-template)
    ))

;;;; Paragraph

(defun org-blogit-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  ;; Fix multibyte language like chinese will be automatically add
  ;; some space since org-mode will transpose auto-fill-mode's space
  ;; to newline char.
  (let* ((fix-regexp "[[:multibyte:]]")
         (fix-contents
          (replace-regexp-in-string
           (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" contents)))

    ;; Send modify data to org-md-paragraph
    (org-md-paragraph paragraph fix-contents info)))

;;;; Headline

(defun org-blogit-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (let* ((info (plist-put info :headline-offset 1)))
    (org-md-headline headline contents info)))

;;;; Link

(defun org-blogit-link (link contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((link-1 (org-md-link link contents info)))
    ;; change ![img][contents/image] to ![img][/contents/image]
    (s-replace "![img](" "![img](/" link-1)
    ))

;;;; Src Block

(defun org-blogit-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (org-html-src-block src-block contents info))


;;; Template

(defun org-blogit-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-blogit-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))


;;; Tables of Contents

(defun org-blogit-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let ((toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth))))
    (when toc-entries
      ;; NOTE: Need add a space before <div
      (format " <div class=\"table-of-contents\">\n\n"))))


;;; End-user functions

;;;###autoload
(defun blogit-export-as-md
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'blogit-md "*Blogit Markdown Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (markdown-mode))))

(provide 'blogit-for-md)
;;; blogit-for-md.el ends here.
