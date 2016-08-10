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

(require 'setup-tests)

(describe "Dictionaries discovery"
  (it "should detect info files"
    (expect (stardict--dictionary-info-file stardict--sample-dictionary-path)
            :to-equal stardict--sample-dictionary-info-file))

  (it "should find index and dict files using dictionary name"
    (let ((dict-name (f-base (stardict--dictionary-info-file stardict--sample-dictionary-path))))
      (expect (stardict--dictionary-index-file stardict--sample-dictionary-path dict-name)
              :to-equal stardict--sample-dictionary-index-file)

      (expect (stardict--dictionary-dict-file stardict--sample-dictionary-path dict-name)
              :to-equal stardict--sample-dictionary-dict-file))))

(provide 'test-discovery)
;;; test-discovery.el ends here
