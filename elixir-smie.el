(require 'smie)

(progn
  (setq elixir-syntax-class-names nil)

  (defmacro elixir-smie-define-regexp-opt (name &rest table)
    `(elixir-smie-define-regexp ,name (regexp-opt (list ,@table))))

  (defmacro elixir-smie-define-regexp (name regexp &optional flag)
    (let ((regex-name (intern (format "elixir-smie-%s" name))))
      `(progn
         (defconst ,regex-name
           ,regexp)
         (pushnew `(,',regex-name . ,(upcase (symbol-name ',name))) elixir-syntax-class-names))))

  (elixir-smie-define-regexp-opt op
                             "<<<" ">>>" "^^^" "~~~" "&&&" "|||"                              ; op3
                             "===" "!=="                                                      ; comp3
                             "==" "!=" "<=" ">="                                              ; comp2
                             "{}" "[]"                                                        ; container2
                             "<" ">"                                                          ; comp1
                             "+" "-" "*" "/" "=" "|" "!" "^" "@"                              ; op1
                             "&&" "||" "<>" "++" "--" "**" "//" "::" "<-" "->" ".." "/>" "=~" ; op2
                             )
  (elixir-smie-define-regexp dot "\\."))

(defun elixir-smie-token-navigator (regex-match match-bound char-position sexp-movement)
  (let ((found-token-class (find-if
                            (lambda (class-def)
                              (funcall regex-match (symbol-value (car class-def))))
                            elixir-syntax-class-names)))
    (cond (found-token-class
           (goto-char (funcall match-bound 0))
           (cdr found-token-class))
          ((when (= ?\" (char-syntax (funcall char-position)))
             (funcall sexp-movement)
             "STRING")))))

(defun elixir-smie-forward-token ()
  (forward-comment (point-max))
  (or (elixir-smie-token-navigator 'looking-at 'match-end 'following-char 'forward-sexp)
      (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "'w_")
              (point)))))

(defun elixir-smie-backward-token ()
  (forward-comment (- (point)))
  (or (elixir-smie-token-navigator '(lambda (regexp) (looking-back regexp nil t)) 'match-beginning 'preceding-char 'backward-sexp)
      (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-backward "'w_")
              (point)))))

(setq elixir-smie-grammar
      (smie-prec2->grammar
       (smie-bnf->prec2
        '((id)
          (statements
           (statement)
           (statement "\n" statement)
           (statement "," statement))
          (statement
           ("if" exp "do" statements "else" statements "end")
           ("if" exp "do" statements "end")
           ("try" "do" statements "after" statements "end")
           ("try" "do" statements "catch" match-statements "end")
           ("try" "do" statements "end")
           ("case" exp "do" match-statements "end")
           ("fn" match-statements "end")
           ("function" "do" match-statements "end")
           )
          (match-statements
           (match-statement "\n" match-statement)
           (match-statement))
          (match-statement
           (statement "->" statements))
          ))))

(defvar elixir-smie-indent-basic 2)

(defun elixir-smie-rules (kind token)
  (message "kind: %s token: %s" kind token)
  (pcase (cons kind token)
    (`(:elem . basic) elixir-smie-indent-basic)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:after . "=") elixir-smie-indent-basic)
    (`(:before . ,(or `"do" `"->" `"after" `"else" `"catch" `"function"))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
          (smie-rule-parent))))))

(define-minor-mode elixir-smie-mode
  "SMIE-based indentation and syntax for Elixir"
  nil nil nil nil
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (smie-setup elixir-smie-grammar 'elixir-smie-rules
              :forward-token 'elixir-smie-forward-token
              :backward-token 'elixir-smie-backward-token))
