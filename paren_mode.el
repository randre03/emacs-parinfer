;;; paren_mode.el --- Indentation-inferred parens

;; Copyright (c) 2012-2015 Roger Randall
;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.
;;
;; Author: Roger Randall <roger@rogerrandall.com>
;; Maintainer: Roger Randall <roger@rogerrandall.com>

;; URL: https://www.rogerrandall.com
;; Keywords: convenience, languages, tools
;; Version: 0.01.00
;; Package-Requires: ())

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

;; See http://shaunlebron.github.io/parinfer/#paren-mode"
;;
;;; # Setup
;;
;;
;;; # Documentation
;;

;;; Code:

(require 'cl)
(require 'cl-lib)
(require 'cl-seq)
(require 'dash)
(require 'defn)
(require 'indent_mode)
(require 'reader)
(require 'string)

(defn parinfer-initial-state
  ;; This is the initial state of the running State.
  [tbl!
  :lines []                      ;; Final lines containing the inferred closing delimiters
  :line-no -1                    ;; Current line number being processed
  :track-indent? nil           ;; "True" if looking for the first char on a line to signify indentation
  :delim-trail (tbl! :start nil
                     :end   nil) ;; Track EOL delims since we replace them wholesale with inferred delims
  :insert (tbl! :line-dy nil
                :x-pos   nil)    ;; The place to insert closing delims whenever the appropriate indentation is hit
  :stack []                      ;; The delimiter stack, maps of [:x-pos :ch :indent-delta]
  :backup []                     ;; (Unused, but required by the reader.el because of the infer process)
  :dedent-x nil                  ;; Current x-positions subsequent lines cannot be nested inside
  :indent-delta 0])              ;; How much the current line's indentation was changed

(defn parinfer-append-delim-trail
  [[:: :keys [stack line-no insert] :as state]]
  (dlet [opener (peek stack)
         close-ch (parinfer-matching-delim (:ch opener))
         stack (pop stack)]
    (-> state
        (assoc :stack stack
               :dedent-x (:x-pos opener))
        (update-in [:lines (+ line-no (:line-dy insert))] insert-string (:x-pos insert) close-ch)
        (update-in [:insert :x-pos] inc))))

(defn parinfer-min-indent [x [:: :keys [stack]]]
  (dlet [opener (peek stack)]
    (-if-let* [start-x (:x-pos opener)]
        (max (+ 1 start-x) x)
      x)))

(defn parinfer-min-dedent [x [:: :keys [dedent-x]]]
  (if dedent-x
      (min dedent-x x)
    x))

(defn parinfer-correct-indent [[:: :keys [indent-delta x-pos stack dedent-x line-no] :as state]]
  (dlet [opener (peek stack)
         delta (:indent-delta opener 0)
         new-x (-> (+ x-pos delta)
                   (parinfer-min-indent state)
                   (parinfer-min-dedent state))
         new-delta (+ indent-delta (- new-x x-pos))
         indent-str (apply concat (repeat new-x " "))]
    (-> state
        (assoc-in [:lines line-no] indent-str)
        (assoc :indent-delta new-delta
               :x-pos new-x
               :track-indent? nil
               :dedent-x nil))))

(defn parinfer-handle-cursor-delta [[:: :keys [indent-delta line-no x-pos cursor-line cursor-x cursor-dx] :as state]]
  (dlet [cursor-delta? (and (= cursor-line line-no)
                            (= cursor-x x-pos)
                            cursor-dx)]
    (cond-> state
            cursor-delta? (update :indent-delta + cursor-dx))))


(defn parinfer-process-indent
  "Update the state by handling a possible indentation trigger."
  [[:: :keys [stack track-indent? lines line-no ch x-pos cursor-line cursor-x cursor-dx] :as state]]
  (dlet [check-indent? (and track-indent?
                        (parinfer-in-code? stack)
                        (not (parinfer-whitespace? ch))
                        (not (string= ";" ch)))
        matching? (and check-indent?
                       (parinfer-closing-delim? ch)
                       (parinfer-valid-closer? stack ch))
        skip? (and check-indent? (parinfer-closing-delim? ch))
        at-indent? (and check-indent? (not skip?))
        state (assoc state :process? (not skip?))]
    (cond-> state
      matching? parinfer-append-delim-trail
      t parinfer-handle-cursor-delta
      at-indent? parinfer-correct-indent)))

(defn parinfer-process-char
  "Update the state by processing the given character and its position."
  [[:: :keys [lines line-no] :as state] ch]
  (dlet [x-pos (length (gethash lines line-no))
        state (assoc state :x-pos x-pos :ch (concat ch))
        state (parinfer-process-indent state)]
    (cond-> state
      (:process? state) process-char*)))

(defn parinfer-reinsert-delims
  [[:: :keys [removed-delims] :as state]]
  (reduce
    (fn [state _delim]
      (parinfer-append-delim-trail state))
    state
    removed-delims))

(defn parinfer-process-line
  "Update the state by processing the given line of text."
  ([line] (parinfer-process-line parinfer-initial-state line))
  ([{:keys [stack lines line-no cursor-line] :as state} line]
   (dlet [line-no (+ 1 line-no) ; used to be (inc line-no)
          state (assoc state
                  :backup []
                  :cursor-in-comment? nil
                  :delim-trail {:start nil :end nil}

                  ;; different from parinfer-process-line in parinfer.format.indent-mode
                  ;; (even if the stack is empty, we still have to track indentation)
                  :track-indent? (not (parinfer-in-str? stack))

                  :indent-delta 0

                  :lines (conj lines "")
                  :line-no line-no
                  :removed-delims [])
          state (update-in state [:insert :line-dy] #(when % (dec %)))
          state (reduce parinfer-process-char state (concat line "\n"))
          state (-> state
                   parinfer-remove-delim-trail
                   parinfer-reinsert-delims)]
     state)))

(defn parinfer-finalize-state [[:: :keys [stack] :as state]]
  (dlet [valid? (empty? stack)]
    (assoc state :valid? valid?)))

(defn parinfer-process-text
  "Update the state by processing the given text."
  ([text] (parinfer-process-text text nil))
  ([text options]
   (dlet [state (merge parinfer-initial-state options)
         lines (parinfer-get-lines text)
         state (reduce parinfer-process-line state lines)
         state (parinfer-finalize-state state)]
     state)))

(defn parinfer-format-text
  "Format the given text by repositioning any trailing closing delimiters based on indentation."
  ([text] (parinfer-format-text text nil))
  ([text options]
   (dlet [state (process-text text options)
         out-text (if (:valid? state)
                   (join "\n" (:lines state))
                   text)]
    {:text out-text
     :valid? (:valid? state)
     :state state})))

(provide 'paren_mode)
;;; paren_mode.el ends here
