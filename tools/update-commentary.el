;;; update-commentary.el --- update 'Commentary:' section of `stardict.el' -*- lexical-binding: t -*-

;; Copyright (C) 2016 Sergei Maximov

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This script uses Org-mode's export to plain text to update
;; the 'Commentary:' section of `stardict.el' according to `README.org'.

;;; Code:

(require 'f)
(require 'ox)
(require 'rx)

(defconst package-root (f-parent (f-dirname (or load-file-name buffer-file-name))))
(defconst package-file (f-expand "stardict.el" package-root))
(defconst package-readme (f-expand "README.org" package-root))

(defun custom-ascii-template (contents info)
  "Return complete document string after conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (let ((depth (plist-get info :with-toc)))
     (when depth
       (concat
        (org-ascii--build-toc info (and (wholenump depth) depth))
        "\n\n\n")))
   contents))

(org-export-define-derived-backend 'custom-ascii 'ascii
  :translate-alist '((template . custom-ascii-template)))

(defun section-header-regexp (section)
  "Return regexp to match ';;; SECTION:' library header."
  (rx-to-string `(seq line-start
                      (zero-or-more space)
                      (one-or-more ";")
                      (zero-or-more space)
                      ,section
                      (zero-or-more space)
                      ":"
                      (zero-or-more space)
                      line-end)))

(defconst commentary-header-regexp (section-header-regexp "Commentary"))
(defconst code-header-regexp (section-header-regexp "Code"))

(defconst drawers-keyword-regexp
  (rx line-start
      "#+DRAWERS:"
      (one-or-more space)
      (group (zero-or-more (one-or-more (or word
                                            (char ?- ?_)))
                           (zero-or-more space)))
      line-end))

(defun parse-drawers ()
  "Parse in-buffer #+DRAWERS keyword."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))         ; ingore case
      (when (re-search-forward drawers-keyword-regexp nil t)
        (split-string (match-string 1) "[ \t]+" t "[ \t]+")))))

;; Silence warnings for Org-mode >= v8.3
(defvar org-drawers)

(defun org-drawers ()
  "Return the list of known Org-mode drawers.

This function is provided for compatibility with Org-mode versions < 8.3.
On later versions the return value is always nil."
  (when (version< org-version "8.3")
    (append org-drawers (parse-drawers))))

(defun readme-as-plain-text ()
  "Convert README.org to plain text."
  (with-temp-buffer
    (insert-file-contents package-readme)
    (let ((org-drawers (org-drawers)))  ; set drawers to parse for Org-mode < v8.3
      (org-export-as 'custom-ascii nil nil nil
                     '(:ascii-charset utf-8)))))

(let ((converted-readme (readme-as-plain-text)))
  (find-file package-file)
  (let* ((commentary-section-start (1+ (re-search-forward commentary-header-regexp)))
         (commentary-section-end (progn
                                   (re-search-forward code-header-regexp)
                                   (match-beginning 0)))
         (comment-style 'indent)
         (comment-start ";")
         (comment-end "")
         (comment-padding " "))
    (kill-region commentary-section-start commentary-section-end)
    (goto-char commentary-section-start)

    (insert "\n")
    (comment-region (point)
                    (progn
                      (insert converted-readme)
                      (point)))
    (insert "\n")

    (save-buffer)

    (message "%s" converted-readme)))

(provide 'update-commentary)
;;; update-commentary.el ends here
