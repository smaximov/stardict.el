;;; stardict.el --- An interface to StarDict dictionaries -*- lexical-binding: t -*-

;; Copyright (C) 2016 Sergei Maximov

;; Author: Sergei Maximov <s.b.maximov@gmail.com>
;; Created: 19 Jul 2016
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.4") (dash "2.0") (emacs "24.4") (f "0.18") (ht "2.0") (log4e "0.3.0"))
;; Keywords: dict

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

;; Table of Contents
;; ─────────────────

;; 1 stardict
;; .. 1.1 Usage


;; 1 stardict
;; ══════════

;;   *stardict* provides the Emacs interface to StarDict dictionary files.

;;   *Note*: this package is WIP and is not really usable at the moment.


;; 1.1 Usage
;; ─────────

;;   Start by loading *stardict* and specifying the location to search for
;;   dictionary files:

;;   ┌────
;;   │ (require 'stardict)
;;   │
;;   │ (setf stardict-dictionary-path "~/.local/share/stardict/dic")
;;   └────

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'f)
(require 'ht)
(require 'log4e)
(require 'rx)

(log4e:deflogger "stardict" "%t [%l] %m" "%H:%M:%S")

(defgroup stardict nil
  "Interface to StarDict dictionaries"
  :group 'applications)

(defcustom stardict-dictionary-path nil
  "Path to StarDict dictionaries, which can be a single directory or a list of directories."
  :tag "Dictionary Path"
  :group 'stardict
  :type '(choice directory
                 (repeat directory)))

(cl-defstruct (stardict--info (:constructor stardict--make-info)
                              (:copier stardict--copy-info))
  version book-name word-count syn-word-count idx-file-size idx-offset-bits
  author email website description date same-type-sequence)

(defconst stardict--info-header-regexp "^StarDict's dict ifo file$")
(defconst stardict--info-version-regexp
  (rx line-start
      "version="
      (group (or "2.4.2" "3.0.0"))
      line-end))

(defun stardict--parse-info (file)
  "Parse \"*.ifo\" file FILE."
  (let ((info (stardict--make-info)))
    (with-temp-buffer
      (insert-file-contents file)
      (unless (looking-at-p stardict--info-header-regexp)
        (user-error "Failed to parse %S: invalid header" file))
      (forward-line)
      (unless (looking-at stardict--info-version-regexp)
        (user-error "Failed to parse %S: invalid version" file))
      (setf (stardict--info-version info) (match-string 1)))
    info))

(defun stardict--locate-file (path &optional suffixes base-name)
  "Locate the first file found in PATH which satisfies certain criteria.

SUFFIXES is a list of suffixes appended to search patterns when searching.
This list defaults to (\"\").

BASE-NAME is a base-name of the file to search. If BASE-NAME is nil,
the search uses wilcard patterns."
  (let ((suffixes (or suffixes '(""))))
    (if base-name
        (locate-file base-name (list (f-full path)) suffixes)
      (cl-block loop
        (--each suffixes
          (-when-let (file (cl-first (f-glob (concat "*" it) path)))
            (cl-return-from loop file)))))))

(defun stardict--dictionary-info-file (path)
  "Return the first '*.ifo' file found in PATH, if any."
  (stardict--locate-file path '(".ifo")))

(defun stardict--dictionary-index-file (path &optional dict-name)
  "Return the first '*.idx' ('*.idx.gz') file found in PATH, if any.

If DICT-NAME is not nil, its value is used as the base name of
the '*.idx' file during search.  DICT-NAME should match the base name of the
corresponding '*.ifo' file."
  (stardict--locate-file path '(".idx.gz" ".idx") dict-name))

(defun stardict--dictionary-dict-file (path &optional dict-name)
  "Return the first '*.dict' ('*.dict.dz') file found in PATH, if any.

If DICT-NAME is not nil, its value is used as the base name of
the '*.dict' file during search.  DICT-NAME should match the base name of the
corresponding '*.ifo' file."
  (stardict--locate-file path '(".dict.dz" ".dict") dict-name))

(provide 'stardict)
;;; stardict.el ends here
