;;; elixir-smie.el --- Structural syntax support for elixir-mode

;; Copyright 2011-2015 secondplanet
;;           2013-2015 Samuel Tonini, Matt DeBoard, Andreas Fuchs

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Structural syntax support for elixir-mode

;;; Code:

(require 'smie)

;; HACK: Patch for Emacs 24.3 smie that fix
;; https://github.com/elixir-lang/emacs-elixir/issues/107.
;;
;; defadvice is used to change the behavior only for elixir-mode.
;; Definition of advice is a definition of corresponding function
;; in Emacs 24.4.
(when (and (= 24 emacs-major-version)
           (= 3  emacs-minor-version))
  (defadvice smie-rule-parent (around elixir-mode-patch activate)
    (if (not (eq major-mode 'elixir-mode))
        (progn ad-do-it)
      (setq ad-return-value
            (save-excursion
              (goto-char (cadr (smie-indent--parent)))
              (cons 'column
                    (+ (or offset 0)
                       (smie-indent-virtual)))))))

  (defadvice smie-indent-comment (around elixir-mode-patch activate)
    (if (not (eq major-mode 'elixir-mode))
        (progn ad-do-it)
      (setq ad-return-value
            (and (smie-indent--bolp)
                 (let ((pos (point)))
                   (save-excursion
                     (beginning-of-line)
                     (and (re-search-forward comment-start-skip (line-end-position) t)
                          (eq pos (or (match-end 1) (match-beginning 0))))))
                 (save-excursion
                   (forward-comment (point-max))
                   (skip-chars-forward " \t\r\n")
                   (unless
                       (save-excursion
                         (let ((next (funcall smie-forward-token-function)))
                           (or (if (zerop (length next))
                                   (or (eobp) (eq (car (syntax-after (point))) 5)))
                               (rassoc next smie-closer-alist))))
                     (smie-indent-calculate))))))))

;; FIXME: This is me being lazy. CL is a compile-time dep only.
;; (But for now, there is no real file-compilation plot, so let's
;; scrape by with the runtime dep.)
(require 'cl)

(defvar elixir-smie-verbose-p nil
  "Emit context information about the current syntax state.")

(defvar elixir-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Note that ?_ might be better as class "_", but either seems to
    ;; work:
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?~ "w" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?' "\"'" table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Elixir mode syntax table.")

(defconst elixir-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (statements (statement)
                   (statement ";" statements))
       (statement ("def" non-block-expr "do" statements "end")
                  (non-block-expr "fn" match-statements "end")
                  (non-block-expr "do" statements "end")
                  ("if" non-block-expr "do" statements "else" statements "end")
                  ("if" non-block-expr "do" statements "end")
                  ("if" non-block-expr "COMMA" "do:" non-block-expr)
                  ("if" non-block-expr "COMMA"
                   "do:" non-block-expr "COMMA"
                   "else:" non-block-expr)
                  ("try" "do" statements "after" statements "end")
                  ("try" "do" statements "catch" match-statements "end")
                  ("try" "do" statements "rescue" match-statements "end")
                  ("try" "do" statements "end")
                  ("case" non-block-expr "do" match-statements "end"))
       (non-block-expr (non-block-expr "OP" non-block-expr)
                       (non-block-expr "COMMA" non-block-expr)
                       ("(" non-block-expr ")")
                       ("{" non-block-expr "}")
                       ("[" non-block-expr "]")
                       ("STRING"))
       (match-statements (match-statement "MATCH-STATEMENT-DELIMITER"
                                          match-statements)
                         (match-statement))
       (match-statement (non-block-expr "->" statements)))
     '((assoc "if" "do:" "else:")
       (assoc "COMMA")
       (left "OP")))

    (smie-precs->prec2
     '((left "||")
       (left "&&")
       (nonassoc "=~" "===" "!==" "==" "!=" "<=" ">=" "<" ">")
       (left "+" "-" "<<<" ">>>" "^^^" "~~~" "&&&" "|||")
       (left "*" "/"))))))

(defvar elixir-smie--operator-regexp
  (rx (or "<<<" ">>>" "^^^" "~~~" "&&&" "|||" "===" "!==" "==" "!=" "<="
          "=" ">=" "<" ">" "&&" "||" "<>" "++" "--" "//" "/>" "=~" "|>")))

(defvar elixir-smie--binary-sequence-regexp
  (rx (or "<<" ">>")))

(defvar elixir-smie--block-operator-regexp
  (rx "->" (0+ nonl)))

(defvar elixir-smie--oneline-def-operator-regexp
  (rx "do:" (0+ nonl)))

(defvar elixir-smie--spaces-til-eol-regexp
  (rx (and (1+ space) eol))
  "Regex representing one or more whitespace characters concluding with eol.")

(defvar elixir-smie--comment-regexp
  (rx (and (0+ space) "#" (0+ not-newline)))
  "Regex matching comments.")

(defvar elixir-smie-indent-basic 2)

(defmacro elixir-smie-debug (message &rest format-args)
  `(progn
     (when elixir-smie-verbose-p
       (message (format ,message ,@format-args)))
     nil))

(defun elixir-smie--implicit-semi-p ()
  (not (or (memq (char-before) '(?\{ ?\[))
           (looking-back elixir-smie--operator-regexp (- (point) 3) t))))

(defun elixir-smie--semi-ends-match ()
  "Return non-nil if the current line concludes a match block."
  (when (not (eobp))
    (save-excursion
      ;; Warning: Recursion.
      ;; This is easy though.

      ;; 1. If we're at a blank line, move forward a character. This takes us to
      ;;    the next line.
      ;; 2. If we're not at the end of the buffer, call this function again.
      ;;    (Otherwise, return nil.)

      ;; The point here is that we want to treat blank lines as a single semi-
      ;; colon when it comes to detecting the end of match statements. This could
      ;; also be handled by a `while' expression or some other looping mechanism.
      (cl-flet ((self-call ()
                           (if (< (point) (point-max))
                               (elixir-smie--semi-ends-match)
                             nil)))
        (cond
         ((and (eolp) (bolp))
          (forward-char)
          (self-call))
         ((looking-at elixir-smie--spaces-til-eol-regexp)
          (forward-char)
          (self-call))
         ;; And if we're NOT on a blank line, move to the end of the line, and see
         ;; if we're looking back at a block operator.
         (t (move-end-of-line 1)
            (looking-back elixir-smie--block-operator-regexp)))))))

(defun elixir-smie--same-line-as-parent (parent-pos child-pos)
  "Return non-nil if `child-pos' is on same line as `parent-pos'."
  (= (line-number-at-pos parent-pos) (line-number-at-pos child-pos)))

(defun elixir-smie-forward-token ()
  (cond
   ;; If there is nothing but whitespace between the last token and eol, emit
   ;; a semicolon.
   ((looking-at elixir-smie--spaces-til-eol-regexp)
    (goto-char (match-end 0))
    ";")
   ((and (or (looking-at elixir-smie--comment-regexp)
             (looking-at "[\n#]"))
         (elixir-smie--implicit-semi-p))
    (when (not (save-excursion
                 (forward-comment 1)
                 (eobp)))
      (if (eolp) (forward-char 1) (forward-comment 1)))
    ;; Note: `elixir-smie--semi-ends-match' will be called when the point is at
    ;; the beginning of a new line. Keep that in mind.
    (if (elixir-smie--semi-ends-match)
        "MATCH-STATEMENT-DELIMITER"
      ";"))
   ((looking-at elixir-smie--block-operator-regexp)
    (goto-char (match-end 0))
    "->")
   ((looking-at elixir-smie--operator-regexp)
    (goto-char (match-end 0))
    "OP")
   (t (smie-default-forward-token))))

(defun elixir-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position))
           (elixir-smie--implicit-semi-p))
      (if (elixir-smie--semi-ends-match)
          "MATCH-STATEMENT-DELIMITER"
        ";"))
     ((looking-back elixir-smie--oneline-def-operator-regexp (- (point) 3) t)
      (goto-char (match-beginning 0))
      ";")
     ((looking-back elixir-smie--block-operator-regexp (- (point) 3) t)
      (goto-char (match-beginning 0))
      "->")
     ((looking-back elixir-smie--binary-sequence-regexp (- (point) 3) t)
      (goto-char (match-beginning 0))
      "OP")
     ((looking-back elixir-smie--operator-regexp (- (point) 3) t)
      (goto-char (match-beginning 0))
      "OP")
     (t (smie-default-backward-token)))))

(defun verbose-elixir-smie-rules (kind token)
  (let ((value (elixir-smie-rules kind token)))
    (elixir-smie-debug "%s '%s'; sibling-p:%s parent:%s prev-is-OP:%s hanging:%s == %s" kind token
                       (ignore-errors (smie-rule-sibling-p))
                       (ignore-errors smie--parent)
                       (ignore-errors (smie-rule-prev-p "OP"))
                       (ignore-errors (smie-rule-hanging-p))
                       value)
    value))

(defun elixir-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:list-intro . ";")
     -4)
    (`(:elem . args)
     -4)
    (`(:before . "OP")
     (when (and (not (smie-rule-hanging-p))
                (smie-rule-prev-p "OP"))
       -2))
    (`(:after . "OP")
     (cond
      ((smie-rule-sibling-p) nil)
      ((smie-rule-hanging-p) (smie-rule-parent elixir-smie-indent-basic))
      ((and (not (smie-rule-sibling-p))
            (not (smie-rule-hanging-p))
            (smie-rule-parent-p "do:"))
       ;; Dedent the line after an OP if it's after a "do:" token, which implies
       ;; a one-line function.
       (smie-rule-parent
        (- (+ elixir-smie-indent-basic elixir-smie-indent-basic))))
      (t (smie-rule-parent))))
    (`(:before . "MATCH-STATEMENT-DELIMITER")
     (cond
      ((smie-rule-parent-p "MATCH-STATEMENT-DELIMITER")
       (smie-rule-parent))
      ((and (not (smie-rule-sibling-p))
            (nth 2 smie--parent)
            (smie-rule-hanging-p))
       (smie-rule-parent elixir-smie-indent-basic))
      ((and (not (smie-rule-sibling-p))
            (not (nth 2 smie--parent))
            (smie-rule-hanging-p))
       (smie-rule-parent))))
    (`(:before . "fn")
     (smie-rule-parent))
    (`(:before . "do:")
     (cond
      ((smie-rule-parent-p "def" "if")
       (smie-rule-parent))))
    (`(:before . "end")
     (smie-rule-parent))
    ;; Closing paren on the other line
    (`(:before . "(")
     (cond
      ((smie-rule-parent-p "fn")
       (smie-rule-parent elixir-smie-indent-basic))
      (t (smie-rule-parent))))
    (`(:before . "[")
     (cond
      ((smie-rule-hanging-p)
       (smie-rule-parent))))
    (`(:after . "{")
     (cond
      ((smie-rule-hanging-p)
       (smie-rule-parent elixir-smie-indent-basic))
      (t elixir-smie-indent-basic)))
    (`(:after . "[")
     (cond
      ((smie-rule-hanging-p)
       (smie-rule-parent elixir-smie-indent-basic))
      (t elixir-smie-indent-basic)))
    (`(:before . "if")
     (cond
      ((smie-rule-parent-p ";")
       (smie-rule-parent))))
    (`(:before . "->")
     (cond
      ((smie-rule-hanging-p)
       (smie-rule-parent elixir-smie-indent-basic))))
    (`(:after . "->")
     (cond
      ;; This first condition is kind of complicated so I'll try to make this
      ;; comment as clear as possible.

      ;; "If `->' is the last thing on the line, and its parent token
      ;; is `fn' ..."
      ((and (smie-rule-hanging-p)
            (smie-rule-parent-p "fn"))
       ;; "... and if:

       ;; 1. `smie--parent' is non-nil
       ;; 2. the `->' token in question is on the same line as its parent (if
       ;;    the logic has gotten this far, its parent will be `fn')

       ;; ... then indent the line after the `->' aligned with the
       ;; parent, offset by `elixir-smie-indent-basic'."
       (if (and smie--parent (elixir-smie--same-line-as-parent
                              (nth 1 smie--parent)
                              (point)))
           (smie-rule-parent elixir-smie-indent-basic)
         elixir-smie-indent-basic))
      ;; Otherwise, if just indent by two.
      ((smie-rule-hanging-p)
       (cond
        ((smie-rule-parent-p "after" "catch" "do" "rescue" "try")
         elixir-smie-indent-basic)
        (t (smie-rule-parent elixir-smie-indent-basic))))))
    (`(:before . ";")
     (cond
      ;; There is a case after an one line definition of functions/macros
      ;; when an `if' keyword token is involved, where the next block `end'
      ;; token will have a `if' as parent and it's hanging.
      ;;
      ;; Example:
      ;;
      ;; defmacro my_if(expr, do: if_block), do: if(expr, do: if_block, else: nil)
      ;; defmacro my_if(expr, do: if_block, else: else_block) do
      ;;   ...
      ;; end <- parent is `if`
      ((and (smie-rule-parent-p "if")
            (smie-rule-hanging-p))
       (smie-rule-parent))
      ((smie-rule-parent-p "after" "catch" "def" "defmodule" "defp" "do" "else"
                           "fn" "if" "rescue" "try" "unless")
       (smie-rule-parent elixir-smie-indent-basic))
     ))
    (`(:after . ";")
     (cond
      ((smie-rule-parent-p "def")
       (smie-rule-parent))
      ((smie-rule-parent-p "if")
       (smie-rule-parent))
      ((and (smie-rule-parent-p "(")
            (boundp 'smie--parent)
            (save-excursion
              (goto-char (cadr smie--parent))
              (smie-rule-hanging-p)))
       (smie-rule-parent elixir-smie-indent-basic))))))

(provide 'elixir-smie)

;;; elixir-smie.el ends here
