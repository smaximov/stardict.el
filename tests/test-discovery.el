;;; test-discovery.el --- Tests for dictionaries discovery -*- lexical-binding: t -*-

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

(require 'undercover-init)
(require 'buttercup)
(require 'stardict)

(defconst stardict--root-dir
  (f-dirname (symbol-file 'stardict)))

(defconst stardict--test-data-dir
  (f-expand "tests/data" stardict--root-dir))

(defconst stardict--sample-dict
  "stardict-en-ru-sample")

(defconst stardict--sample-dict-path
  (f-join stardict--test-data-dir stardict--sample-dict))

(describe "Dictionaries discovery"
  (it "should detect info files"
    (expect (stardict--dictionary-info-file stardict--sample-dict-path)
            :to-equal (f-join stardict--sample-dict-path "en-ru.ifo")))

  (it "should find index and dict files using dictionary name"
    (let ((dict-name (f-base (stardict--dictionary-info-file stardict--sample-dict-path))))
      (expect (stardict--dictionary-index-file stardict--sample-dict-path dict-name)
              :to-equal (f-join stardict--sample-dict-path "en-ru.idx"))

      (expect (stardict--dictionary-dict-file stardict--sample-dict-path dict-name)
              :to-equal (f-join stardict--sample-dict-path "en-ru.dict")))))

(provide 'test-discovery)
;;; test-discovery.el ends here
