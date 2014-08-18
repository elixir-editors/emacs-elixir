;;; elixir-smie.el --- Structural syntax support for elixir-mode

(require 'smie)

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
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?~ "w" table)

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
    (modify-syntax-entry ?\: "'" table)
    (modify-syntax-entry ?\@ "'" table)
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
                  (non-block-expr "fn" match-statement "end")
                  (non-block-expr "do" statements "end")
                  ("if" non-block-expr "do" statements "else" statements "end")
                  ("if" non-block-expr "do" statements "end")
                  ("if" non-block-expr "COMMA" "do:" non-block-expr)
                  ("if" non-block-expr "COMMA"
                   "do:" non-block-expr "COMMA"
                   "else:" non-block-expr)
                  ("try" "do" statements "after" statements "end")
                  ("try" "do" statements "catch" match-statements "end")
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
  (regexp-opt '("<<<" ">>>" "^^^" "~~~" "&&&" "|||" "===" "!==" "==" "!=" "<="
                ">=" "<" ">" "&&" "||" "<>" "++" "--" "//"
                "/>" "=~" "|>" "->")))

(defvar elixir-smie-indent-basic 2)

(defmacro elixir-smie-debug (message &rest format-args)
  `(progn
     (when elixir-smie-verbose-p
       (message (format ,message ,@format-args)))
     nil))

(defun elixir-smie--implicit-semi-p ()
  (not (or (memq (char-before) '(?\{ ?\[))
           (looking-back elixir-smie--operator-regexp (- (point) 3) t))))

(defun elixir-smie-forward-token ()
  (cond
   ((and (looking-at "[\n#]") (elixir-smie--implicit-semi-p))
    (if (eolp) (forward-char 1) (forward-comment 1))
    ";")
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
      ";")
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
    (`(:after . "OP")
     (cond
      ((smie-rule-sibling-p) nil)
      ((smie-rule-hanging-p) (smie-rule-parent elixir-smie-indent-basic))
      (t elixir-smie-indent-basic)))
    (`(:before . ";")
     (cond
      ((smie-rule-parent-p "after" "catch" "def" "defmodule" "defp" "do" "else"
                           "fn" "if" "rescue" "try" "unless")
       (smie-rule-parent elixir-smie-indent-basic))))
    (`(:after . ";")
     (if (smie-rule-parent-p "if")
         (smie-rule-parent 0)))))

(define-minor-mode elixir-smie-mode
  "SMIE-based indentation and syntax for Elixir"
  nil nil nil nil
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (smie-setup elixir-smie-grammar 'elixir-smie-rules
              :forward-token 'elixir-smie-forward-token
              :backward-token 'elixir-smie-backward-token))

(provide 'elixir-smie)
