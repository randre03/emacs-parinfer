;;; indent_mode.el --- Indentation-inferred parens   -*- lexical-binding: t; -*-

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
;; See http://shaunlebron.github.io/parinfer/#indent-mode"
;;

;;; Code:

(require 'dash)
(require 'defn)
(require 'reader)
(require 'string)

(defn parinfer-initial-state [tbl!
  :lines []                       ;; Final lines containing the inferred closing delimiters
  :postline-states []             ;; state cache after each line
  :postindent-states []           ;; state cache after each line's indentation point (nil if none)
  :insert (tbl! :line-dy nil
                :x-pos   nil)     ;; the place to insert closing delimiters whenever we hit appropriate indentation
  :line-no -1                     ;; the current line number being processed
  :track-indent? nil              ;;"t" when we are lookingh for the first char on a line to signify indentation
  :delim-trail (tbl! :start nil
                     :end   nil)  ;; track EOL delims since we replace them wholesale with inferred delims
  :stack []                       ;; the delimiter stack, maps of [:x-pos :ch :indent-delta]
  :backup []])                    ;; trailing delims that are pushed back onto the stack at EOL

(defn parinfer-close-delims
  "Update the state by inferring closing delimiters.
  Do this by using the given indentation level.

  Example:

  (defn foo [a b
     ret           ;; <---  When we process `r`, we detect indentation, then...

  (defn foo [a b]  ;; <---  ... we insert a `]` after `b` since `[` is after `r` on the x-axis.
     ret           ;; <---  A `)` is inserted after `ret` if no further indented lines found.
  "
  ([state] (parinfer-close-delims state 0))
  ([state indent-x]

   (dlet [;; Pop the appropriate delims off the stack, and create the inferred delim string.
         [stack delims]
         (loop [stack (:stack state), delims ""]
           (if-not (seq stack) ;; TODO: Replace if-not
             [stack delims]
             (dlet [[:: :keys [x-pos ch]] (peek stack)]
               (if (>= x-pos indent-x)
                 (recur (pop stack) (concat delims (matching-delim ch)))
                 [stack delims]))))

         ;; Insert the delims in the correct place, then update the state
         {:keys [line-dy x-pos]} (:insert state)
         line-no (+ (:line-no state) line-dy)
         state (assoc (update-in state [:lines line-no] parinfer-insert-string x-pos delims) :stack stack)]
     state)))

(defn parinfer-update-delim-trail
  "Update the state's delim trail as we scan across a line.
  We eventually remove the delim trail since we indented
  content below can cause the delims to move.

  Example:

  (foo (+ 2 3) [(bar)] )    ;; a potential comment
                    ^^^^
                     |
                     +-- trailing delims that we will remove
                          (notice whitespace will also be removed)
  "
  [{:keys [stack delim-trail backup x-pos ch cursor-line line-no cursor-x cursor-in-comment?] :as state}]
  (dlet [

        ;; these characters won't block, unless they're escaped
        pass-char? (or (string= ";" ch)
                     (parinfer-whitespace? ch)
                     (parinfer-closing-delim? ch))

        ;; must be in code (before parinfer-push-char)
        reset? (when (parinfer-in-code? stack)
                 (or (parinfer-escaping? stack)
                     (not pass-char?)))

        cursor-in-comment? (or cursor-in-comment?
                               (and (= cursor-line line-no)
                                    (= x-pos cursor-x)
                                    (parinfer-in-comment? stack)))

        ;; Determine if we have a delimiter we can track.
        update? (and (parinfer-in-code? stack)
                     (not (parinfer-escaping? stack))
                     (parinfer-closing-delim? ch)
                     (parinfer-valid-closer? stack ch))

        ;; Clear the backup delimiters if we reset.
        backup (cond-> backup reset? empty)

        ;; Update the delimiter trail range if needed.
        delim-trail (cond
                     (reset? (tbl! :start nil :end nil))
                     (update? (-> delim-trail
                                  (update-in [:start] #(or % x-pos))
                                  (assoc :end (+ 1 x-pos))))
                     (t delim-trail))]

    (assoc state
      :cursor-in-comment? cursor-in-comment?
      :backup backup
      :delim-trail delim-trail)))

(defn parinfer-block-delim-trail
  "The presence of the cursor can block the removal of some part of the delim trail."
  [{:keys [delim-trail line-no cursor-line cursor-x cursor-in-comment?] :as state}]
  (dlet [{:keys [start end]} delim-trail
        cursor-block? (and (= line-no cursor-line)
                           start
                           (> cursor-x start)
                           (not cursor-in-comment?))
        start (cond-> start (and start cursor-block?) (max cursor-x))
        end (cond-> end (and end cursor-block?) (max cursor-x))
        [start end] (when-not (= start end) [start end])]
    (assoc state
           :delim-trail {:start start :end end})))

(defn parinfer-remove-delim-trail
  "Update the state by removing our marked delim trail.
  We remove the delims from the appropriate line of text,
  while also restoring their matching delims onto the stack.

  Example:

  (foo (+ 2 3) [(bar)] )    ;; a potential comment
  ^            ^^   ^^^^
  |            |     |
  |____________|     +-- Remove these from the text.
         |
         +-- Restore these onto the delim stack.
             (fyi, we originally popped them off to validate
              the closing delims. now we need them back to
              infer closing delims for indented lines.)
  "
  [{:keys [delim-trail insert line-no lines backup stack] :as state}]
  (dlet [{:keys [start end]} delim-trail]
    (if (and start end)
      (dlet [line (gethash lines line-no)
                  delims (->> (substring line start end)
                              (map concat)
                              (filter parinfer-closing-delims?))
            remove-count (length delims)
            ignore-count (- (length backup) remove-count)
            [backup stack] (loop [backup backup, stack stack]
                             (if (= ignore-count (length backup))
                               [backup stack]
                               (recur (pop backup) (conj stack (peek backup)))))
            state (-> state
                       (update-in [:lines line-no] parinfer-remove-str-range start end)
                       (assoc :backup backup
                              :stack stack
                              :removed-delims delims))
            insert-line? (zero? (:line-dy insert))
            state (cond-> state
                     insert-line? (update-in [:insert :x-pos] min start))]
        state)
      state)))

(defn parinfer-update-insertion-pt
  "Update the state's trailing delimiter insertion point as we scan the line.

  Example:

  (defn foo [a b] ret)
  ^^^^^ ^^^ ^^ ^  ^^^
                    |
                    +-- final insertion point candidate

  Special rules allow the user to freely position the trailing
  delimiters while editing a line.

  "
  [{:keys [track-indent? cursor-line lines line-no stack x-pos ch] :as state}]
  (dlet [prev-ch (concat (last (gethash lines line-no)))
        insert-at-char? (and
                          ;; must be in code (after parinfer-push-char)
                          (parinfer-in-code? stack)

                          ;; don't insert at blank (a removed character)
                          (not (string= "" ch))

                          ;; don't insert at whitespace, unless escaped
                          (or (not (parinfer-whitespace? ch))
                              (string= "\\" prev-ch))

                          ;; don't insert at closing delim, unless cursor is on this line
                          (or (not (parinfer-closing-delim? ch))
                              (= line-no cursor-line)))

        ;; Add potential insert point for closing delimiters if required.
        insert (when insert-at-char?
                 {:line-dy 0
                  :x-pos (+ 1 x-pos)})]

    (cond-> state
      insert (assoc :insert insert))))

(defn parinfer-get-cached-state
  [state]
  (select-keys state [:stack :insert]))

(defn parinfer-commit-cached-state
  "Cache a subset of the state after some event.
  This is used by process-text-change."
  [state key-]
  (update state key- conj (parinfer-get-cached-state state)))

(defn parinfer-process-indent
  "Update the state by handling a possible indentation trigger.

  Example:

  (defn foo [a b
     ret           ;; <---  When we process `r`, we detect indentation, then
                   ;;       we start backtracking to insert closing delimiters on a previous line.


  (defn foo [a b]
     )             ;; <---  If a line starts with a closing delimiter, it is not
                   ;;       considered an indentation trigger.  In fact, we skip
                   ;;       the character completely, removing it from the line.
  "
  [{:keys [stack track-indent? lines line-no x-pos ch] :as state}]
  (dlet [check-indent? (and track-indent?
                        (parinfer-in-code? stack)
                        (not (parinfer-whitespace? ch))
                        (not (string= ";" ch)))
        skip? (and check-indent? (parinfer-closing-delim? ch))
        at-indent? (and check-indent? (not skip?))
        state (assoc state :process? (not skip?))]
    (if at-indent?
      (-> state
          (parinfer-close-delims x-pos)
          (parinfer-commit-cached-state :postindent-states)
          (assoc :track-indent? nil
                 :processed-indent? t))
      state)))

(defn parinfer-update-line
  "Update the state by adding processed character to the line."
  [{:keys [ch line-no] :as state}]
  (update-in state [:lines line-no] concat ch))

(defn parinfer-save-preinsert-line
  "Save the text of a line before trailing delims were inserted.
  This allows to restore them when skipping to changed lines in
  process-text-change."
  [{:keys [line-no insert lines] :as state}]
  (cond-> state
    (= 0 (:line-dy insert))
    (assoc-in [:insert :line] (gethash lines line-no))))

(defn parinfer-process-char*
  [state]
  ;; NOTE: the order here is important!
  (-> state
      parinfer-update-delim-trail
      parinfer-push-char
      parinfer-update-insertion-pt
      parinfer-update-line))

(defn parinfer-process-char
  "Update the state by processing the given character and its position."
  [{:keys [lines line-no] :as state} ch]
  (dlet [x-pos (length (gethash lines line-no))
        state (assoc state :x-pos x-pos :ch (concat ch))
        state (parinfer-process-indent state)]
    (cond-> state
      (:process? state) parinfer-process-char*)))

(defn parinfer-process-line
  "Update the state by processing the given line of text."
  ([line] (parinfer-process-line parinfer-initial-state line))
  ([{:keys [stack lines line-no cursor-line] :as state} line]
   (dlet [line-no (+ 1 line-no)
         state (assoc state
                  :backup []
                  :cursor-in-comment? nil
                  :delim-trail {:start nil :end nil}
                  :track-indent? (and (seq stack) (not (parinfer-in-str? stack)))
                  :processed-indent? nil
                  :lines (conj lines "")
                  :line-no line-no
                  :removed-delims [])
         state (update-in state [:insert :line-dy] #(when % (dec %)))
         state (reduce parinfer-process-char state (concat line "\n"))
         state (-> state
                   parinfer-block-delim-trail
                   parinfer-remove-delim-trail
                   parinfer-save-preinsert-line
                   (parinfer-commit-cached-state :postline-states))

         ;; if this did not have an indentation trigger point,
         ;; pad postindent-states with a nil.
         state (cond-> state
                 (not (:processed-indent? state))
                 (update :postindent-states conj nil))]
     state)))

(defn parinfer-finalize-state
  [{:keys [stack] :as state}]
  (dlet [valid? (not (parinfer-in-str? stack)) ;; invalid if unclosed string
        close-delims? (and valid? (seq stack))]
    (cond-> (assoc state :valid? valid?)
      close-delims? parinfer-close-delims)))

(defn parinfer-process-text
  "Fully processes the given text.  Returns new state.
  See `parinfer-format-text` for usage."
  ([text] (parinfer-process-text text nil))
  ([text options]
   (dlet [state (merge parinfer-initial-state options)
         lines (get-lines text)
         state (reduce parinfer-process-line state lines)]
     (parinfer-finalize-state state))))

;;----------------------------------------------------------------------
;; faster processing for incremental changes
;;----------------------------------------------------------------------


(defn parinfer-safe-subvec [v i]
  (if (< i (length v))
    (subvec v i)
    []))

(defn parinfer-restore-insert-line
  "restore the text of a line before trailing delimiters were inserted"
  [{:keys [insert line-no] :as state}]
  (dlet [{:keys [line-dy line]} insert]
    (if (and line-dy line (>= line-no 0))
      (dlet [insert-line-no (+ line-dy line-no)]
        (assoc-in state [:lines insert-line-no] line))
      state)))

(defn parinfer-fill-rest-with-cache
  "fill the rest of the lines with info from cache."
  [prev-state state last-i]
  (dlet [state (-> state
                  (update :lines pop) ;; to restore the version of the line _with_ its trailing delims
                  (update :lines into (parinfer-safe-subvec (:lines prev-state) last-i))
                  (update :postline-states into (parinfer-safe-subvec (:postline-states prev-state) (+ 1 last-i)))
                  (update :postindent-states into (parinfer-safe-subvec (:postindent-states prev-state) (+ 1 last-i)))
                  (merge (last (:postline-states prev-state))))
        state (-> state
                  (assoc :line-no (dec (length (:lines state))))
                  parinfer-restore-insert-line)]
    state))

(defn parinfer-process-unchanged-line*
  "process a line that comes after those that have changed.
  'reduced' will halt further processing."
  [prev-state state [old-i line cache]]
  (dlet [state (parinfer-process-line state line)
        new-cache (last (:postindent-states state))
        more? (< (+ 1 old-i) (length (:lines prev-state)))
        can-skip? (and new-cache (= new-cache cache))]
    (if (and can-skip? more?)
      (reduced (parinfer-fill-rest-with-cache prev-state state old-i))
      state)))

(defn parinfer-process-unchanged-lines
  "process the lines after those that have changed."
  [prev-state state start-i]
  (reduce (partial parinfer-process-unchanged-line* prev-state) state
          (map vector
               (iterate inc start-i) ;; old line numbers
               (parinfer-safe-subvec (:lines prev-state) start-i) ;; old lines
               (parinfer-safe-subvec (:postindent-states prev-state) start-i))))

(defn parinfer-initial-cached-state
  "build an initial state based on our starting line and previous cache."
  [{:keys [lines postline-states postindent-states] :as prev-state} options i]
  (dlet [line-data {:lines (subvec lines 0 i)
                   :postindent-states (subvec postindent-states 0 i)
                   :postline-states (subvec postline-states 0 i)
                   :line-no (dec i)}
        last-cache (gethash postline-states (dec i))]
    (-> parinfer-initial-state
        (merge options line-data last-cache)
        parinfer-restore-insert-line)))

(defn parinfer-process-text-change
  "Processes the given change for the given state.  Returns new state.
  See `parinfer-format-text-change` for usage."
  ([prev-state change]
   (parinfer-process-text-change prev-state change nil))
  ([prev-state {:keys [line-no new-line] :as change} options]
   (dlet [; normalize args (allowing multiple line replacements)
         [start-line-no end-line-no] (if (number? line-no) [line-no (+ 1 line-no)] line-no)
         line-replacements (if (string? new-line) [new-line] new-line)

         ;; 1. create initial state for starting at first changed line
         ;; 2. process changed lines
         ;; 3. process unchanged lines that come after
         state (parinfer-initial-cached-state prev-state options start-line-no)
         state (reduce parinfer-process-line state line-replacements)
         state (parinfer-process-unchanged-lines prev-state state end-line-no)]

     (parinfer-finalize-state state))))

;;----------------------------------------------------------------------
;; public functions
;;----------------------------------------------------------------------

(defn parinfer-format-text
  "Fully process the given text.

  'text' is the full text.

  'options' is an optional map with supported keys:
    :cursor-x     - x position of the cursor
    :cursor-line  - line number of the cursor

  Returns a map:
    :text     - full text output
    :valid?   - indicates if the input was valid
    :state    - cached state to be passed to `parinfer-format-text-change`
  "
  ([text] (parinfer-format-text text nil))
  ([text options]
   (dlet [state (parinfer-process-text text options)
         out-text (if (:valid? state)
                    (join "\n" (:lines state))
                    text)]
     {:text out-text
      :valid? (:valid? state)
      :state state})))

(defn parinfer-format-text-change
  "Process changed lines in a previously processed text.

  'text' is the full text (including the change).

  'prev-state' is the state after processing 'text' before the 'change' occurred.
    - found in the :state key of the result returned by `parinfer-format-text` or this function.

  'change' is a map:

    KEY        |  DESCRIPTION             |  TYPE
    -----------+--------------------------+------------------------------------
    :line-no   |  line range to replace   |  a num or min,max line range
    :new-line  |  new line(s) to insert   |  a string or seq if multiple lines

  'options' is an optional map with supported keys:
    :cursor-x     - x position of the cursor
    :cursor-line  - line number of the cursor

  Returns a map:
    :text     - full text output
    :valid?   - indicates if the input was valid
    :state    - cached state to be passed to `parinfer-format-text-change`
  "
  ([text prev-state change]
   (parinfer-format-text-change text prev-state change nil))
  ([text prev-state change options]
   (dlet [state (parinfer-process-text-change prev-state change options)
         out-text (if (:valid? state)
                    (join "\n" (:lines state))
                    text)]
     {:text out-text
      :valid? (:valid? state)
      :state state})))

(provide 'indent_mode)
;;; indent_mode.el ends here
