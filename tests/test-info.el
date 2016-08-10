;;; test-info.el --- Tests parsing *.ifo files -*- lexical-binding: t -*-

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

(describe "Info files"
  :var (info)
  (it "should parse sample info file"
    (expect (lambda ()
              (setf info (stardict--parse-info stardict--sample-dictionary-info-file)))
            :not :to-throw 'user-error)
    (expect (stardict--info-version info)
            :to-equal "2.4.2")))

(provide 'test-info)
;;; test-info.el ends here
