;;; elixir-smie.el --- Structural syntax support for elixir-mode

(require 'smie)

;; FIXME: This is me being lazy. CL is a compile-time dep only.
;; (But for now, there is no real file-compilation plot, so let's
;; scrape by with the runtime dep.)
(require 'cl)

(defvar elixir-smie-verbose-p nil
  "Emit context information about the current syntax state.")

(defvar elixir-mode-syntax-table
  (let ((elixir-mode-syntax-table (make-syntax-table)))

    ;; Note that ?_ might be better as class "_", but either seems to
    ;; work:
    (modify-syntax-entry ?_ "w" elixir-mode-syntax-table)
    (modify-syntax-entry ?? "w" elixir-mode-syntax-table)

    (modify-syntax-entry ?' "\"" elixir-mode-syntax-table)
    (modify-syntax-entry ?# "<" elixir-mode-syntax-table)
    (modify-syntax-entry ?\n ">" elixir-mode-syntax-table)
    (modify-syntax-entry ?\( "()" elixir-mode-syntax-table)
    (modify-syntax-entry ?\) ")(" elixir-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}" elixir-mode-syntax-table)
    (modify-syntax-entry ?\} "){" elixir-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]" elixir-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" elixir-mode-syntax-table)
    (modify-syntax-entry ?\: "'" elixir-mode-syntax-table)
    (modify-syntax-entry ?\@ "'" elixir-mode-syntax-table)
    elixir-mode-syntax-table)
  "Elixir mode syntax table.")

(defun elixir-syntax-propertize (start end)
  (save-excursion
    (goto-char start)
    ;; The ? character on its own is supposed to escape whatever comes
    ;; after it (including any escaped chars. Examples: ?\# and ?".
    (while (search-forward "?" end t)
      (let ((start (1- (point))))
        (unless (or (= (char-syntax (char-before (- (point) 1))) ?w)
                    (= (char-syntax (char-before (- (point) 1))) ?_))
          (put-text-property (1- (point))
                             (point)
                             'syntax-table
                             '(?|))
          (when (= (char-after) ?\\)
            (forward-char)
            (put-text-property (1- (point))
                               (point)
                               'syntax-table
                               '(?\s)))
          (forward-char)
          (put-text-property (1- (point))
                             (point)
                             'syntax-table
                             '(?|))
          (put-text-property start (point) 'font-lock-face 'font-lock-string-face))))))

(defmacro elixir-smie-debug (message &rest format-args)
  `(progn
     (when elixir-smie-verbose-p
       (message (format ,message ,@format-args)))
     nil))

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
                                 "<" ">"                                                          ; comp1
                                 "+" "-" "*" "/" "=" "|" "!" "^" "@"                              ; op1
                                 "&&" "||" "<>" "++" "--" "**" "//" "::" "<-"  ".." "/>" "=~"     ; op2 (minus ->)
                                 "xor" "|>"                                                       ; http://elixir-lang.org/docs/stable/Kernel.html
                                 )
  (elixir-smie-define-regexp dot "\\.")
  (elixir-smie-define-regexp comma ",")
  (elixir-smie-define-regexp -> "->")
  (elixir-smie-define-regexp << "<<")
  (elixir-smie-define-regexp >> ">>")
  (elixir-smie-define-regexp-opt parens "(" ")" "{" "}" "[" "]" "<<" ">>"))

(defconst elixir-smie-block-intro-keywords
  '(do else catch after rescue -> COMMA OP)
  "Keywords in which newlines cause confusion for the parser.")

(defun elixir-skip-comment-backward ()
  "Skip backwards over all whitespace and comments.

Return non-nil if any line breaks were skipped."
  (let ((start-line-no (line-number-at-pos (point))))
    (forward-comment (- (point)))
    (/= start-line-no (line-number-at-pos (point)))))

(defun elixir-skip-comment-forward ()
  "Skip forward over any whitespace and comments.

Return non-nil if any line breaks were skipped."
  (let ((start-line-no (line-number-at-pos (point))))
    (forward-comment (buffer-size))
    (/= start-line-no (line-number-at-pos (point)))))

(defun elixir-smie-next-token-no-lookaround (forwardp nested)
  (block elixir-smie-next-token-no-lookaround
    ;; First, skip comments; but if any comments / newlines were
    ;; skipped, the upper level needs to check if they were significant:
    (when (if forwardp
              (elixir-skip-comment-forward)
            (elixir-skip-comment-backward))
      (return-from elixir-smie-next-token-no-lookaround "\n"))
    (let* ((found-token-class (find-if
                               (lambda (class-def)
                                 (let ((regex (symbol-value (car class-def))))
                                   (if forwardp
                                       (looking-at regex)
                                     (looking-back regex nil t))))
                               elixir-syntax-class-names))
           (maybe-token
            (let ((current-char (if forwardp
                                    (following-char)
                                  (preceding-char))))
              (cond ((member current-char
                             '(?\n ?\;))
                     (if forwardp
                         (forward-comment (point-max))
                       (forward-comment (- (point))))
                     (string current-char))
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
                       "STRING"))))))
      (or maybe-token
          (downcase
           (buffer-substring-no-properties
            (point)
            (if forwardp
                (progn (skip-syntax-forward "'w_")
                       (point))
              (progn (skip-syntax-backward "'w_")
                     (point)))))))))

(defun elixir-smie-next-token (forwardp)
  (block elixir-smie-next-token
    (let ((current-token (elixir-smie-next-token-no-lookaround forwardp nil)))
      (when (string= "\n" current-token)
        ;; This is a newline; if the previous token isn't an OP2, this
        ;; means the line end marks the end of a statement & we get to
        ;; scan forward until there's a non-newline token; otherwise,
        ;; make this line ending something that probably ends the
        ;; statement (but see below).
        (if (save-excursion
              (block nil
                (let ((token (elixir-smie-next-token-no-lookaround nil t)))
                  (while (and (not (= (point) (point-min))) (string= "\n" token))
                    (setq token (elixir-smie-next-token-no-lookaround nil t)))
                  (when (member (intern token) elixir-smie-block-intro-keywords)
                    (return t)))))
            ;; it's a continuation line, return the next token after the newline:
            (return-from elixir-smie-next-token (elixir-smie-next-token forwardp))
          (setq current-token ";")))

      ;; When reading match statements (the ones with expr -> statements),
      ;; we need to drop non-; delimiters so the parser knows when a
      ;; match statement ends and another begins, so scan around point to
      ;; see if there are any -> within the current block's scope.

      ;; If the current token is a ";", scan forward to see if the current
      ;; potential statement contains a "->". If so, scan back to find a
      ;; "do". If there is a -> there, emit a match-statement-delimiter
      ;; instead of the ";".
      (if (and (string= ";" current-token)
               ;; Scan ahead:
               (let ((level 0)
                     token)
                 (save-excursion
                   (block nil
                     (while (and (not (= (point) (point-max))) (not (string= "" token)) (not (or (string= "\n" token) (string= ";" token))))
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
        current-token))))

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
           ("if" non-block-expr "COMMA" "do:" statement)
           ("if" non-block-expr "COMMA" "do:" statement "COMMA" "else:" statement)
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
        '((assoc "DOT") (assoc "if") (assoc "do:") (assoc "else:") (assoc "COMMA") (assoc "OP") (assoc "->" ";")))))

(defvar elixir-smie-indent-basic 2)

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
    (`(:elem . basic)
     (if (smie-rule-hanging-p)
         0
       elixir-smie-indent-basic))
    (`(:after . "OP")
     (unless (smie-rule-sibling-p)
       elixir-smie-indent-basic))
    (`(:before. "OP")
     ;; FIXME: Issue #5: This should prevent comments on lines before
     ;; continuation lines from causing indentation messed-upness, but
     ;; for some reason SMIE doesn't look this far when there's a
     ;; comment terminating the previous line. Ugh.
     nil)
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

(provide 'elixir-smie)
