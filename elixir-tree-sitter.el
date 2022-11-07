;;; elixir-tree-sitter.el --- Tree sitter for elixir-mode

;; Copyright 2022 Wilhelm H Kirschbaum

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

;;  Tree sitter for elixir-mode

;;; Code:

;; Custom faces match highlights.scm as close as possible
;; to help with updates

(require 'treesit)

;; Font lock

(defface elixir-font-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @keyword tag.")

(defface elixir-font-comment-doc-face
  '((t (:inherit font-lock-doc-face)))
  "For use with @comment.doc tag.")

(defface elixir-font-comment-doc-identifier-face
  '((t (:inherit font-lock-doc-face)))
  "For use with @comment.doc tag.")

(defface elixir-font-comment-doc-attribute-face
  '((t (:inherit font-lock-doc-face)))
  "For use with @comment.doc.__attribute__ tag.")

(defface elixir-font-attribute-face
  '((t (:inherit font-lock-preprocessor-face)))
  "For use with @attribute tag.")

(defface elixir-font-operator-face
  '((t (:inherit default)))
  "For use with @operator tag.")

(defface elixir-font-constant-face
  '((t (:inherit font-lock-constant-face)))
  "For use with @constant tag.")

(defface elixir-font-number-face
  '((t (:inherit default)))
  "For use with @number tag.")

(defface elixir-font-module-face
  '((t (:inherit font-lock-type-face)))
  "For use with @module tag.")

(defface elixir-font-punctuation-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @punctuation tag.")

(defface elixir-font-punctuation-delimiter-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @punctuation.delimiter tag.")

(defface elixir-font-punctuation-bracket-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @punctuation.bracket.")

(defface elixir-font-punctuation-special-face
  '((t (:inherit font-lock-variable-name-face)))
  "For use with @punctuation.special tag.")

(defface elixir-font-embedded-face
  '((t (:inherit default)))
  "For use with @embedded tag.")

(defface elixir-font-string-face
  '((t (:inherit font-lock-string-face)))
  "For use with @string tag.")

(defface elixir-font-string-escape-face
  '((t (:inherit font-lock-regexp-grouping-backslash)))
  "For use with Reserved keywords.")

(defface elixir-font-string-regex-face
  '((t (:inherit font-lock-string-face)))
  "For use with @string.regex tag.")

(defface elixir-font-string-special-face
  '((t (:inherit font-lock-string-face)))
  "For use with @string.special tag.")

(defface elixir-font-string-special-symbol-face
  '((t (:inherit font-lock-builtin-face)))
  "For use with @string.special.symbol tag.")

(defface elixir-font-function-face
  '((t (:inherit font-lock-function-name-face)))
  "For use with @function tag.")

(defface elixir-font-sigil-name-face
  '((t (:inherit font-lock-string-face)))
  "For use with @__name__ tag.")

(defface elixir-font-variable-face
  '((t (:inherit default)))
  "For use with @variable tag.")

(defface elixir-font-constant-builtin-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @constant.builtin tag.")

(defface elixir-font-comment-face
  '((t (:inherit font-lock-comment-face)))
  "For use with @comment tag.")

(defface elixir-font-comment-unused-face
  '((t (:inherit font-lock-comment-face)))
  "For use with @comment.unused tag.")

(defface elixir-font-error-face
  '((t (:inherit error)))
  "For use with @comment.unused tag.")

;; Faces end

(defconst elixir--definition-keywords
  '("def" "defdelegate" "defexception" "defguard" "defguardp" "defimpl" "defmacro" "defmacrop" "defmodule" "defn" "defnp" "defoverridable" "defp" "defprotocol" "defstruct"))

(defconst elixir--definition-keywords-re
  (concat "^" (regexp-opt elixir--definition-keywords) "$"))

(defconst elixir--kernel-keywords
  '("alias" "case" "cond" "else" "for" "if" "import" "quote" "raise" "receive" "require" "reraise" "super" "throw" "try" "unless" "unquote" "unquote_splicing" "use" "with"))

(defconst elixir--kernel-keywords-re
  (concat "^" (regexp-opt elixir--kernel-keywords) "$"))

(defconst elixir--builtin-keywords
  '("__MODULE__" "__DIR__" "__ENV__" "__CALLER__" "__STACKTRACE__"))

(defconst elixir--builtin-keywords-re
  (concat "^" (regexp-opt elixir--builtin-keywords) "$"))


(defconst elixir--doc-keywords
  '("moduledoc" "typedoc" "doc"))

(defconst elixir--doc-keywords-re
  (concat "^" (regexp-opt elixir--doc-keywords) "$"))

(defconst elixir--reserved-keywords
  '("when" "and" "or" "not" "in"
    "not in" "fn" "do" "end" "catch" "rescue" "after" "else"))

(defconst elixir--reserved-keywords
  '("when" "and" "or" "not" "in"
    "not in" "fn" "do" "end" "catch" "rescue" "after" "else"))

(defconst elixir--reserved-keywords-re
  (concat "^" (regexp-opt elixir--reserved-keywords) "$"))

(defconst elixir--reserved-keywords-vector
  (apply #'vector elixir--reserved-keywords))

;; reference:
;; https://github.com/elixir-lang/tree-sitter-elixir/blob/main/queries/highlights.scm
(defvar elixir--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'elixir
   :feature 'comment
   '((comment) @elixir-font-comment-face)

   :language 'elixir
   :feature 'string
   :override t
   '([(string) (charlist)] @font-lock-string-face)

   :language 'elixir
   :feature 'string-interpolation
   :override t
   '((string
      [
       quoted_end: _ @elixir-font-string-face
       quoted_start: _ @elixir-font-string-face
       (quoted_content) @elixir-font-string-face
       (interpolation
        "#{" @elixir-font-string-escape-face "}" @elixir-font-string-escape-face
        )
       ])
     (charlist
      [
       quoted_end: _ @elixir-font-string-face
       quoted_start: _ @elixir-font-string-face
       (quoted_content) @elixir-font-string-face
       (interpolation
        "#{" @elixir-font-string-escape-face "}" @elixir-font-string-escape-face
        )
       ])
     )

   :language 'elixir
   :feature 'keyword
   ;; :override `prepend
   `(,elixir--reserved-keywords-vector @elixir-font-keyword-face
     ;; these are operators, should we mark them as keywords?
     (binary_operator
      operator: _ @elixir-font-keyword-face
      (:match ,elixir--reserved-keywords-re @elixir-font-keyword-face)))

   :language 'elixir
   :feature 'unary-operator
   `((unary_operator
      operator: "@" @elixir-font-comment-doc-attribute-face
      operand: (call
                target: (identifier) @elixir-font-comment-doc-identifier-face
                ;; arguments can be optional, but not sure how to specify
                ;; so adding another entry without arguments
                ;; if we don't handle then we don't apply font
                ;; and the non doc fortification query will take specify
                ;; a more specific font which takes precedence
                (arguments
                 [
                  ;; (string) @elixir-font-comment-doc-face
                  (charlist) @elixir-font-comment-doc-face
                  (sigil) @elixir-font-comment-doc-face
                  (boolean) @elixir-font-comment-doc-face
                  ]))
      (:match ,elixir--doc-keywords-re @elixir-font-comment-doc-identifier-face))
     (unary_operator
      operator: "@" @elixir-font-comment-doc-attribute-face
      operand: (call
                target: (identifier) @elixir-font-comment-doc-identifier-face)
      (:match ,elixir--doc-keywords-re @elixir-font-comment-doc-identifier-face))

     (unary_operator operator: "@" @elixir-font-attribute-face
                     operand: [
                               (identifier)  @elixir-font-attribute-face
                               (call target: (identifier)  @elixir-font-attribute-face)
                               (boolean)  @elixir-font-attribute-face
                               (nil)  @elixir-font-attribute-face
                               ])

     (unary_operator operator: "&") @elixir-font-function-face
     (operator_identifier) @elixir-font-operator-face
     )

   :language 'elixir
   :feature 'operator
   '((binary_operator operator: _ @elixir-font-operator-face)
     (dot operator: _ @elixir-font-operator-face)
     (stab_clause operator: _ @elixir-font-operator-face)

     [(boolean) (nil)] @elixir-font-constant-face
     [(integer) (float)] @elixir-font-number-face
     (alias) @elixir-font-module-face
     (call target: (dot left: (atom) @elixir-font-module-face))
     (char) @elixir-font-constant-face
     [(atom) (quoted_atom)] @elixir-font-module-face
     [(keyword) (quoted_keyword)] @elixir-font-string-special-symbol-face)

   :language 'elixir
   :feature 'call
   `((call
      target: (identifier) @elixir-font-keyword-face
      (:match ,elixir--definition-keywords-re @elixir-font-keyword-face))
     (call
      target: (identifier) @elixir-font-keyword-face
      (:match ,elixir--kernel-keywords-re @elixir-font-keyword-face))
     (call
      target: [(identifier) @elixir-font-function-face
               (dot right: (identifier) @elixir-font-function-face)])
     (call
      target: (identifier) @elixir-font-keyword-face
      (arguments
       [
        (identifier) @elixir-font-function-face
        (binary_operator
         left: (identifier) @elixir-font-function-face
         operator: "when")
        ])
      (:match ,elixir--definition-keywords-re @elixir-font-keyword-face))
     (call
      target: (identifier) @elixir-font-keyword-face
      (arguments
       (binary_operator
        operator: "|>"
        right: (identifier) @elixir-font-variable-face))
      (:match ,elixir--definition-keywords-re @elixir-font-keyword-face)))

   :language 'elixir
   :feature 'constant
   `((binary_operator operator: "|>" right: (identifier) @elixir-font-function-face)
     ((identifier) @elixir-font-constant-builtin-face
      (:match ,elixir--builtin-keywords-re @elixir-font-constant-builtin-face))
     ((identifier) @elixir-font-comment-unused-face
      (:match "^_" @elixir-font-comment-unused-face))
     (identifier) @elixir-font-variable-face
     ["%"] @elixir-font-punctuation-face
     ["," ";"] @elixir-font-punctuation-delimiter-face
     ["(" ")" "[" "]" "{" "}" "<<" ">>"] @elixir-font-punctuation-bracket-face)

   :language 'elixir
   :feature 'sigil
   :override t
   `(
     (sigil
      (sigil_name) @elixir-font-sigil-name-face
      quoted_start: _ @elixir-font-string-special-face
      quoted_end: _ @elixir-font-string-special-face) @elixir-font-string-special-face
     (sigil
      (sigil_name) @elixir-font-sigil-name-face
      quoted_start: _ @elixir-font-string-face
      quoted_end: _ @elixir-font-string-face
      (:match "^[sS]$" @elixir-font-sigil-name-face)) @elixir-font-string-face
     (sigil
      (sigil_name) @elixir-font-sigil-name-face
      quoted_start: _ @elixir-font-string-regex-face
      quoted_end: _ @elixir-font-string-regex-face
      (:match "^[rR]$" @elixir-font-sigil-name-face)) @elixir-font-string-regex-face
     )

   :language 'elixir
   :feature 'string-escape
   :override t
   `((escape_sequence) @elixir-font-string-escape-face)
   )
  "Tree-sitter font-lock settings.")


;; Navigation

(defvar elixir--treesit-query-defun
  (let ((query `((call
     target: (identifier) @type
     (arguments
      [
       (alias) @name
       (identifier) @name
       (call target: (identifier)) @name
       (binary_operator
        left: (call target: (identifier)) @name
        operator: "when")
       ])
     (:match ,elixir--definition-keywords-re @type)
     ))))
    (treesit-query-compile 'elixir query)))

(defun elixir--treesit-defun (node)
  "Get the module name from the NODE if exists."
  (treesit-query-capture node elixir--treesit-query-defun))

(defun elixir--treesit-defun-name (&optional node)
  "Get the module name from the NODE if exists."
  (let* ((node (or node (elixir--treesit-largest-node-at-point)))
        (name-node (alist-get 'name (elixir--treesit-defun node))))
    (when name-node (treesit-node-text name-node))))

(defun elixir--treesit-defun-type (&optional node)
  "Get the module name from the NODE if exists."
  (let* ((node (or node (elixir--treesit-largest-node-at-point)))
        (name-node (alist-get 'type (elixir--treesit-defun node))))
    (when name-node (treesit-node-text name-node))))


(defun elixir--treesit-largest-node-at-point ()
  (let* ((node-at-point (treesit-node-at (point)))
         (node-list
          (cl-loop for node = node-at-point
                   then (treesit-node-parent node)
                   while node
                   if (eq (treesit-node-start node)
                          (point))
                   collect node))
         (largest-node (car (last node-list))))
    (if (null largest-node)
        (treesit-node-at (point))
      largest-node)))

(defun elixir--imenu-item-parent-label (_type name)
  "Elixir imenu parent label for NAME."
  (format "%s" name))

(defun elixir--imenu-item-label (type name)
  "Elixir imenu item label for TYPE and NAME."
  (format "%s %s" type name))

(defun elixir--imenu-jump-label (_type _name)
  "Elixir imenu jump label."
  (format "..."))

(defun elixir--imenu-treesit-create-index (&optional node)
  "Return tree Imenu alist for the current Elixir buffer or NODE tree."
  (let* ((node (or node (treesit-buffer-root-node 'elixir)))
         (tree (treesit-induce-sparse-tree
                node
                (rx (seq bol (or "call") eol)))))
    (elixir--imenu-treesit-create-index-from-tree tree)))

(defun elixir--imenu-treesit-create-index-from-tree (node)
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'elixir--imenu-treesit-create-index-from-tree children))
         (type (elixir--treesit-defun-type ts-node))
         (name (when type (elixir--treesit-defun-name ts-node)))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond ((null ts-node) subtrees)
          ((null type) subtrees)
          (subtrees (let ((parent-label (funcall 'elixir--imenu-item-parent-label type name))
                          (jump-label (funcall 'elixir--imenu-jump-label type name)))
                      `((,parent-label ,(cons jump-label marker) ,@subtrees))))
          (t (let ((label (funcall 'elixir--imenu-item-label type name)))
               (list (cons label marker)))))))



(provide 'elixir-tree-sitter)

;;; elixir-tree-sitter.el ends here
