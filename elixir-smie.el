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
                                 "&&" "||" "<>" "++" "--" "**" "//" "::" "<-"  ".." "/>" "=~"     ; op2 (minus ->)
                                 )
  (elixir-smie-define-regexp dot "\\.")
  (elixir-smie-define-regexp comma ",")
  (elixir-smie-define-regexp -> "->")
  (elixir-smie-define-regexp << "<<")
  (elixir-smie-define-regexp >> ">>")
  (elixir-smie-define-regexp-opt parens "(" ")" "{" "}" "[" "]" "<<" ">>"))

(defvar elixir-tokenizer-syntax-table (let ((table (copy-syntax-table elixir-mode-syntax-table)))
                                        (modify-syntax-entry ?\n "." table)
                                        table))

(defun elixir-smie-token-navigator (regex-match match-bound char-position sexp-movement)
  (let ((found-token-class (find-if
                            (lambda (class-def)
                              (funcall regex-match (symbol-value (car class-def))))
                            elixir-syntax-class-names)))
    (cond ((eq ?\n (funcall char-position))
           "\n")
          (found-token-class
           (goto-char (funcall match-bound 0))
           (cdr found-token-class))
          ((when (= ?\" (char-syntax (funcall char-position)))
             (funcall sexp-movement)
             "STRING")))))

(setq elixir-smie-block-intro-keywords '(do else catch after rescue -> COMMA))

(defun elixir-smie-next-token-no-lookaround (forwardp nested)
  ;; First, skip comments but determine if newline-as-whitespace is
  ;; significant:
  (with-syntax-table (if (and (not nested)
                              (member
                               (intern
                                (save-excursion
                                  (elixir-smie-next-token-no-lookaround nil t)))
                               elixir-smie-block-intro-keywords))
                         elixir-mode-syntax-table
                       elixir-tokenizer-syntax-table)
    (if forwardp
        (forward-comment (point-max))
      (forward-comment (- (point)))))
  (let* ((found-token-class (find-if
                             (lambda (class-def)
                               (let ((regex (symbol-value (car class-def))))
                                 (if forwardp
                                     (looking-at regex)
                                   (looking-back regex nil t))))
                             elixir-syntax-class-names))
         (maybe-token (cond ((member (if forwardp
                                         (following-char)
                                       (preceding-char))
                                     '(?\n ?\;))
                             (if forwardp
                                 (forward-comment (point-max))
                               (forward-comment (- (point))))
                             ";")
                            (found-token-class
                             (goto-char (if forwardp
                                            (match-end 0)
                                          (match-beginning 0)))
                             (if (string= "PARENS" (cdr found-token-class))
                                 (buffer-substring-no-properties (match-beginning 0) (match-end 0))
                               (cdr found-token-class)))
                            ((when (= ?\" (char-syntax (if forwardp
                                                           (following-char)
                                                         (preceding-char))))
                               (if forwardp
                                   (forward-sexp)
                                 (backward-sexp))
                               "STRING")))))
    (or maybe-token
        (downcase
         (buffer-substring-no-properties
          (point)
          (if forwardp
              (progn (skip-syntax-forward "'w_")
                     (point))
            (progn (skip-syntax-backward "'w_")
                   (point))))))))

(defun elixir-smie-next-token (forwardp)
  ;; When reading match statements (the ones with expr -> statements),
  ;; we need to drop non-; delimiters so the parser knows when a
  ;; match statement ends and another begins, so scan around point to
  ;; see if there are any -> within the current block's scope.

  ;; If the current token is a ";", scan forward to see if the current
  ;; potential statement contains a "->". If so, scan back to find a
  ;; "do". If there is a -> there, emit a match-statement-delimiter
  ;; instead of the ";".
  (let ((current-token (elixir-smie-next-token-no-lookaround forwardp nil)))
    (if (and (string= ";" current-token)
             ;; Scan ahead:
             (let ((level 0)
                   token)
               (save-excursion
                 (block nil
                   (while (and (not (= (point) (point-max))) (not (string= "" token)) (not (string= ";" token)))
                     (setq token (elixir-smie-next-token-no-lookaround t nil))
                     (cond ((and (= level 0) (string= "->" token))
                            (return t))
                           ((find token '("do" "fn") :test 'string=)
                            (incf level))
                           ((string= token "end")
                            (decf level)))))))
             ;; Scan behind:
             (let (token)
               (save-excursion
                 (block nil
                   (while (and (not (= (point) (point-min))) (not (string= "" token)) (not (string= "do" token)) (not (string= "fn" token)))
                     (setq token (elixir-smie-next-token-no-lookaround nil nil))
                     (when (string= "->" token)
                       (return t)))
                   (when (or (string= token "do"))
                     t)))))
        "MATCH-STATEMENT-DELIMITER"
      current-token)))

(defun elixir-smie-forward-token ()
  (elixir-smie-next-token t))

(defun elixir-smie-backward-token ()
  (elixir-smie-next-token nil))

(setq elixir-smie-grammar
      (smie-prec2->grammar
       (smie-bnf->prec2
        '((id)
          (statements
           (statement)
           (statement ";" statements))
          (statement
           ("if" non-block-expr "do" statements "else" statements "end")
           ("if" non-block-expr "do" statements "end")
           ("try" "do" statements "after" statements "end")
           ("try" "do" statements "catch" match-statements "end")
           ("try" "do" statements "end")
           ("case" non-block-expr "do" match-statements "end")
           ("fn" match-statement "end")
           ("function" "do" match-statements "end")
           (non-block-expr "do" statements "end")
           (expr)
           )
          (non-block-expr
           (non-block-expr "OP" non-block-expr)
           (non-block-expr "DOT" non-block-expr)
           (non-block-expr "COMMA" non-block-expr)
           ("(" statements ")")
           ("{" statements "}")
           ("[" statements "]")
           ("STRING"))
          (match-statements
           (match-statement "MATCH-STATEMENT-DELIMITER" match-statements)
           (match-statement))
          (match-statement
           (non-block-expr "->" statements)))
        '((assoc "DOT") (assoc "COMMA") (assoc "OP") (assoc "->" ";")))))

(defvar elixir-smie-indent-basic 2)

(defun verbose-elixir-smie-rules (kind token)
  (let ((value (elixir-smie-rules kind token)))
    (message "%s '%s'; s:%s p:%s == %s" kind token (ignore-errors (smie-rule-sibling-p)) (ignore-errors smie--parent) value)
    value))

(defun elixir-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) elixir-smie-indent-basic)
    (`(:after . "->")
     (when (smie-rule-hanging-p)
       elixir-smie-indent-basic))
    (`(,_ . ,(or `"COMMA")) (smie-rule-separator kind))
    (`(:after . "=") elixir-smie-indent-basic)
    (`(:after . ,(or `"do"))
     elixir-smie-indent-basic)
    (`(:list-intro . ,(or `"do"))
     t)))

(define-minor-mode elixir-smie-mode
  "SMIE-based indentation and syntax for Elixir"
  nil nil nil nil
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (smie-setup elixir-smie-grammar 'elixir-smie-rules ; 'verbose-elixir-smie-rules
              :forward-token 'elixir-smie-forward-token
              :backward-token 'elixir-smie-backward-token))

(define-key elixir-mode-map (kbd "C-M-d") 'smie-down-list)

(provide 'elixir-smie)
