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



;;; Define Back-End for org-export

(org-export-define-derived-backend 'blogit-md 'md
  :translate-alist
  '((src-block . org-html-src-block)
    ))


;;; End-user functions

;;;###autoload
(defun blogit-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'blogit-md "*Blogit Markdown Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

(provide 'blogit-for-md)
;;; blogit-for-md.el ends here.
