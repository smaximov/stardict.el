;;; setup-tests.el --- setup tests and define helpers  -*- lexical-binding: t; -*-

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

;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el" (:exclude "tools/*.el")))

(require 'buttercup)

(require 'stardict)

(defconst stardict--root-dir
  (f-dirname (symbol-file 'stardict)))

(defconst stardict--test-data-dir
  (f-expand "tests/data" stardict--root-dir))

(defconst stardict--sample-dictionary
  "stardict-en-ru-sample")

(defconst stardict--sample-dictionary-path
  (f-join stardict--test-data-dir stardict--sample-dictionary))

(defconst stardict--sample-dictionary-info-file
  (f-join stardict--sample-dictionary-path "en-ru.ifo"))

(defconst stardict--sample-dictionary-index-file
  (f-join stardict--sample-dictionary-path "en-ru.idx"))

(defconst stardict--sample-dictionary-dict-file
  (f-join stardict--sample-dictionary-path "en-ru.dict"))

(provide 'setup-tests)
;;; setup-tests.el ends here
