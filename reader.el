;;; reader.el --- a basic elisp reader for tracking parens and token states.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Roger Randall

;; Author: Roger Randall <roger@rogerrandall.com>
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

;;

;;; Code:
(require 'dash)
(require 'defn)
(require 'predd)

(defconst parinfer-matching-delim #s(hash-table data ("{" "}", "}" "{"
                                                      "[" "]", "]" "["
                                                      "(" ")", ")" "(")))

(defconst parinfer-opening-delim? '("{" "[" "(")) ; Elisp idiom is to use lists for sets
(defconst parinfer-closing-delim? '("}" "]" ")")) ; Elisp idiom is to use lists for sets

(defun whitespace? (ch)
  (re-search-forward "\s," ch))

;;; -----------------------------------------------------------------------
;;; Delimiter Stack states
;;;
;;;   State is tracked by checking last pushed character.
;;; -----------------------------------------------------------------------

(defun parinfer-prev-ch (stack)
  (:ch (peek stack))) ;; stack is a vector so this is like (last stack)

(defun parinfer-escaping? (stack)
  "Next character will be escaped."
  (string= "\\" (parinfer-prev-ch stack)))

(defun parinfer-in-str? (stack)
  "Next character is inside a string."
  (dlet [ch (parinfer-prev-ch (cond-> stack (parinfer-escaping? stack) pop))]
    (string= "\"" ch)))

(defun parinfer-in-comment? (stack)
  "Next character is inside a comment."
  (string= ";" (parinfer-prev-ch stack)))

(defun parinfer-in-code? (stack)
  "Next character is inside actual code."
  (and (not (parinfer-in-str? stack))
       (not (parinfer-in-comment? stack))))

(defun parinfer-valid-closer? (stack ch)
  "Determine if given closing delimiter (ch) can be used next, assuming currently parsing inside code."
  (string= (parinfer-prev-ch) (parinfer-matching-delim ch)))

;;; -----------------------------------------------------------------------
;;; Delimiter Stack Operations
;;;
;;;
;;;   Delimiters are tracked using a stack of maps structured as {:x-pos :ch :indent-delta}
;;;   State is tracked by checking the last character.
;;; -----------------------------------------------------------------------

(predd-defmulti parinfer-push-char*
                "Update the delimiter stack with the gievn character."
                (lambda (TODO: DESTRUCTURE(:keys [ch]))
                  (cond
                   ((opening-delim? ch) :open)
                   ((closing-delim? ch) :close)
                   (t ch)))) ;; 't' in elisp appears to be same as 'else' in cljscript

(predd-defmethod parinfer-push-char* "\t"
                 (:keys (stack))
                 (cond
                  ((not (parinfer-in-str? stack)) (:ch "  "));; TODO: Correct data structure being used here
                  (t ch)))

;; Function below likely needs to be re-written to confimr to elisp code requirements
(predd-defmethod parinfer-push-char* :open
                 ((:keys (stack) :as state)) ;; Can this be written this way?
                 (cond
                  ((parinfer-escaping? stack) #s(hash-table data (:stack (pop stack))))
                  ((parinfer-in-code? stack) #s(hash-table data (:stack (conj stack (select-keys state [:x-pos :ch :indent-delta])))))
                 (t nil)))

(predd-defmethod parinfer-push-char* :close
                 ((:keys (stack backup ch)))
                 (cond
                  ((parinfer-escaping? stack) #s(hash-table data (:stack (pop stack))))
                  ((parinfer-in-code? stack) (if (parinfer-valid-closer? stack ch)
                                                 (dlet [opener (peek stack)]
                                                   #s(hash-table data (:stack (pop stack)
                                                                              :backup (conj backup opener))))
                                               #s(hash-table data (:ch "")))) ; erase non-matched delimiter
                   (t nil)))

(predd-defmethod parinfer-push-char* ";"
                 ((:keys (stack) :as state))
                 (cond
                  ((pariinfer-escaping? stack) #s(hash-table data (:stack (pop stack))))
                  ((parinfer-in-code? stack) #s(hash-table (conj stack (select-keys state [:x-pos :ch]))))
                  (t nil)))

(predd-defmethod parinfer-push-char* "\n"
                 ((:keys (stack)))
                 (dlet [TODO Figure out how to do threaded cond...]))

(predd-defmethod parinfer-push-char* "\\"
                 ((:keys (stack) :as state))
                 (cond
                  ((parinfer-escaping? stack) (#s(hash-table data (:stack (pop stack)))))
                  ((parinfer-in-comment? stack) nil)
                  (t (#s(hash-table data (conj stack (select-keys state [:x-pos :ch])))))))

(predd-defmethod parinfer-push-char* "\""
                 ((:keys (stack) :as state))
                 (cond
                  ((parinfer-escaping? stack) (#s(hash-table data (:stack (pop stack)))))
                  ((parinfer-in-str? stack) (#s(hash-table data (:stack (pop stack)))))
                  ((parinfer-in-comment? stack) nil)
                  (t (#s(hash-table data (:stack (conj stack (select-keys state [:x-pos :ch]))))))))

(predd-defmethod parinfer-push-ch* :default
                 ((:keys (stack)))
                 (cond
                  ((parinfer-escaping? stack (#s(hash-table data (:stack (pop stack))))))
                  (t nil)))

(predd-defmethod parinfer-push-char
                 ([state])
                 (dlet [new-data (push-char* state)]
                   (merge-with lambda(or %2 %1) state new-data))) ; need to replace merge-with and
                                                                  ; work on lambda structure.

(provide 'reader)
;;; reader.el ends here
