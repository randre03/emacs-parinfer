;;; prep_file.el --- Prepare existing files prior to first use with emacs-parinfer.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Roger Randall

;; Author: Roger Randall <roger.randall@gmail.com>
;; Keywords: convenience, files, lisp, tools

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
;;   See: http://shaunlebron.github.io/parinfer/"
;;

;;; Code:

(require 'paren_mode)

(def usage
     "prep-file <filenames>
      Corrects indentation of the given files, backing up originals to .bak")

(defun parinfer-prep-file (filename)
  (print "Reading " filename "...")
  (let* ((orig-text (slurp filename))
        (prep-text (:text (parinfer-format-text orig-text)))) 
    (print "Writing " filename "...")
    (spit filename prep-text)
    (spit (concat filename ".bak") orig-text)))

;;; Need to implement -main function. but not immediately necessary.


(provide 'prep_file)
;;; prep_file.el ends here
