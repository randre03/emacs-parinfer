;;; string.el --- Assorted string functions for emacs-parinfer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Roger Randall

;; Author: Roger Randall <roger@rogerrandall.com>
;; Keywords: files, convenience, lisp, tools

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
;;; RER 11/18/15 Each of these functions work the same as their cljs counterparts when
;;; compared using their respective REPLs.

;;

;;; Code:
;;   No outside package requirements.

(defun parinfer-insert-string (orig idx insert)
  (concat (substring orig 0 idx)
          insert
          (substring orig idx)))

(defun parinfer-remove-str-range (orig start end)
  (concat (substring orig 0 start)
          (substring orig end)))

(defun parinfer-get-lines (text)
  ;; Get all lines (even empty ones)
  ;; source: http://stackoverflow.com/a/29614863/142317
  (split-string text "\n"))

(provide 'string)
;;; string.el ends here
