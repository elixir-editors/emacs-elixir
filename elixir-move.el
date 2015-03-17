;;; elixir-move.el --- Navigate elixir source

;; Copyright (C) 2015  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages

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

(defvar elixir-max-specpdl-size max-specpdl-size
  "Protect against eternal loop")

(defun elixir--skip-to-comment-or-semicolon ()
  "Returns position if comment or semicolon found. "
  (let ((orig (point)))
    (cond ((and done (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
		(member (char-after) (list ?# ?\,)))
	   (when (eq ?\, (char-after))
	     (skip-chars-forward ";" (line-end-position))))
	  ((and (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
		(member (char-after) (list ?# ?\,)))
	   (when (eq ?\, (char-after))
	     (skip-chars-forward ";" (line-end-position))))
	  ((not done)
	   (end-of-line)))
    (skip-chars-backward " \t" (line-beginning-position))
    (and (< orig (point))(setq done t)
	 done)))

(defun elixir--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of statement after a semicolon.

Returns position reached if point was moved. "
  (let ((orig (point)))
    (and (< 0 (abs
               (skip-chars-backward "^;" (or limit (line-beginning-position)))))
         (skip-chars-forward " \t" (line-end-position))
         (setq done t)
         (and (< (point) orig) (point)))))

(defalias 'elixir-backward-statement 'elixir-beginning-of-statement)
(defalias 'elixir-previous-statement 'elixir-beginning-of-statement)
(defalias 'elixir-statement-backward 'elixir-beginning-of-statement)
(defun elixir-beginning-of-statement (&optional orig done limit)
  "Go to the initial line of a simple statement.

For beginning of compound statement use elixir-beginning-of-block.
For beginning of clause elixir-beginning-of-clause. "
  (interactive)
  (save-restriction
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (this (point))
             (cui (current-indentation))

             (pps (progn (goto-char this)
                         (parse-partial-sexp (or limit (point-min))(point))))
             (done done)
             erg)
        (unless done
          (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))
               (setq pps (parse-partial-sexp (or limit (point-min))(point)))))
        (cond
         ((and (bolp)(eolp))
          (skip-chars-backward " \t\r\n\f")
          (elixir-beginning-of-statement orig done limit))
         ((nth 8 pps)
          ;; inside string
          (and (nth 3 pps) (setq done t))
          (goto-char (nth 8 pps))
          (elixir-beginning-of-statement orig done limit))
         ((nth 1 pps)
          (goto-char (1- (nth 1 pps)))
          (elixir--skip-to-semicolon-backward
           (save-excursion (back-to-indentation)(point)))
          (setq done t)
          (elixir-beginning-of-statement orig done limit))
         ((elixir-preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (elixir-beginning-of-statement orig done limit))
         ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
          (forward-comment -1)
          (while (and (not (bobp))
                      (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
            (forward-comment -1))
          (unless (bobp)
            (elixir-beginning-of-statement orig done limit)))
         ;; at inline comment
         ((looking-at "[ \t]*#")
          (when (elixir--skip-to-semicolon-backward
                 (save-excursion (back-to-indentation)(point)))
            (skip-chars-forward " \t")
            (unless (bobp)
              (elixir-beginning-of-statement orig done limit))))
         ;; at beginning of string
         ((and (not done) (looking-at elixir-string-delim-re))
          (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
            (setq done t))
          (back-to-indentation)
          (elixir-beginning-of-statement orig done limit))
         ;; after end of statement
         ((and (not done) (eq (char-before) ?\;))
          (skip-chars-backward ";")
          (elixir-beginning-of-statement orig done limit))
         ;; travel until indentation or semicolon
         ((and (not done)
               (elixir--skip-to-semicolon-backward
                (save-excursion (back-to-indentation)(point))))
          (elixir-beginning-of-statement orig done limit))
         ;; at current indent
         ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          (elixir-beginning-of-statement orig done limit)))
        ;; return nil when before comment
        (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
          (when (< (point) orig)(setq erg (point))))
        (when (and elixir-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

(defalias 'elixir-beginning-of-statement-lc 'elixir-beginning-of-statement-bol)
(defun elixir-beginning-of-statement-bol (&optional indent)
  "Goto beginning of line where statement starts.
  Returns position reached, if successful, nil otherwise.

See also `elixir-up-statement': up from current definition to
next beginning of statement above. "
  (interactive)
  (let* ((indent (or indent (when (eq 'elixir-end-of-statement-bol (car
  elixir-bol-forms-last-indent))(cdr elixir-bol-forms-last-indent))))
	 (orig (point))
         erg)
    (if indent
        (while (and
                (setq erg (elixir-beginning-of-statement))
                (< indent (current-indentation))
                (not (bobp))))
      (setq erg (elixir-beginning-of-statement)))
    ;; reset
    (setq elixir-bol-forms-last-indent nil)
    (beginning-of-line)
    (and (< (point) orig) (setq erg (point)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'elixir-forward-statement 'elixir-end-of-statement)
(defalias 'elixir-previous-statement 'elixir-end-of-statement)
(defalias 'elixir-statement-forward 'elixir-end-of-statement)
(defun elixir-end-of-statement (&optional orig done repeat)
  "Go to the last char of current statement.

Optional argument REPEAT, the number of loops done already,
is checked for elixir-max-specpdl-size error.
Avoid eternal loops due to missing string delimters etc. "
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
          (orig (or orig (point)))
          erg pos last
          ;; use by scan-lists
          parse-sexp-ignore-comments
          forward-sexp-function
          stringchar stm pps err)
      (unless done (elixir--skip-to-comment-or-semicolon))
      (setq pps (parse-partial-sexp (point-min) (point)))
      ;; (origline (or origline (elixir-count-lines)))
      (cond
       ((< elixir-max-specpdl-size repeat)
        (error "elixir-end-of-statement reached loops max.
If no error, customize `elixir-max-specpdl-size'"))
       ;; list
       ((nth 1 pps)
        (if (<= orig (point))
            (progn
              (setq orig (point))
              ;; do not go back at a possible unclosed list
              (goto-char (nth 1 pps))
              (let ((parse-sexp-ignore-comments t))
                (if
                    (ignore-errors (forward-list))
                    (progn
                      (when (looking-at ":[ \t]*$")
                        (forward-char 1))
                      (setq done t)
                      (skip-chars-forward "^#" (line-end-position))
                      (skip-chars-backward " \t\r\n\f"
                                           (line-beginning-position))
                      (elixir-end-of-statement orig done repeat))
                  (setq err (elixir--record-list-error pps))
                  (goto-char orig))))))
       ;; string
       ((nth 3 pps)
        (when (elixir-end-of-string)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f")
          (setq pps (parse-partial-sexp (point-min) (point)))
          (unless (and done
                       (not (or (nth 1 pps) (nth 8 pps)))
                       (eolp))
            (elixir-end-of-statement orig done repeat))))
       ;; in non-terminated string

       ;; in comment
       ((nth 4 pps)
        (elixir--end-of-comment-intern (point))
        (elixir--skip-to-comment-or-semicolon)
        (while (and (eq (char-before (point)) ?\\ )
                    (elixir-escaped)(setq last (point)))
          (forward-line 1)(end-of-line))
        (and last (goto-char last)
             (forward-line 1)
             (back-to-indentation))
        (elixir-end-of-statement orig done repeat))
       ((elixir-current-line-backslashed-p)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f" (line-beginning-position))
        (while (and (eq (char-before (point)) ?\\ )
                    (elixir-escaped))
          (forward-line 1)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
        (unless (eobp)
          (elixir-end-of-statement orig done repeat)))
       ((eq orig (point))
        (skip-chars-forward " \t\r\n\f#'\"")
        (elixir--skip-to-comment-or-semicolon)
        (elixir-end-of-statement orig done repeat))
       ((eq (current-indentation) (current-column))
        (elixir--skip-to-comment-or-semicolon)
        ;; (setq pps (parse-partial-sexp (point-min) (point)))
        (unless done
          (elixir-end-of-statement orig done repeat)))

       ((and (looking-at "[[:print:]]+$")
             (not done)
             (elixir--skip-to-comment-or-semicolon))
        (elixir-end-of-statement orig done repeat)))
      (unless
          (or
           (eq (point) orig)
           (member (char-before) (list 10 32 9)))
        (setq erg (point)))
      (if (and elixir-verbose-p err)
          (elixir--message-error err)
        (and elixir-verbose-p (interactive-p) (message "%s" erg)))
      erg)))

(defun elixir-end-of-statement-bol ()
  "Go to the beginning-of-line following current statement."
  (interactive)
  (let ((erg (elixir-end-of-statement)))
    (setq erg (elixir--beginning-of-line-form))
    (when (and elixir-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun elixir-goto-statement-below ()
  "Goto beginning of next statement. "
  (interactive)
  (let ((orig (point))
        (erg (elixir-end-of-statement)))
    (elixir-beginning-of-statement)
    (when (< (point) orig)
      (goto-char erg)
      (elixir-end-of-statement)
      (elixir-beginning-of-statement))))

(provide 'elixir-move)
;;; elixir-move.el ends here
