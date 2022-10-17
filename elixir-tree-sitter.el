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

(require 'treesit nil t)

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
(defvar elixir--treesit-settings
  (treesit-font-lock-rules
   :language 'elixir
   :feature 'basic
   `(
     (comment) @elixir-font-comment-face

     ,elixir--reserved-keywords-vector @elixir-font-keyword-face

     (unary_operator
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
                  (string) @elixir-font-comment-doc-face
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

     ;; these are operators, should we mark them as keywords?
     (binary_operator
      operator: _ @elixir-font-keyword-face
      (:match ,elixir--reserved-keywords-re @elixir-font-keyword-face))

     (binary_operator operator: _ @elixir-font-operator-face)
     (dot operator: _ @elixir-font-operator-face)
     (stab_clause operator: _ @elixir-font-operator-face)

     [(boolean) (nil)] @elixir-font-constant-face
     [(integer) (float)] @elixir-font-number-face
     (alias) @elixir-font-module-face
     (call target: (dot left: (atom) @elixir-font-module-face))
     (char) @elixir-font-constant-face
     [(atom) (quoted_atom)] @elixir-font-module-face
     [(keyword) (quoted_keyword)] @elixir-font-string-special-symbol-face

     (call
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
      (:match ,elixir--definition-keywords-re @elixir-font-keyword-face))

     (binary_operator operator: "|>" right: (identifier) @elixir-font-function-face)
     ((identifier) @elixir-font-constant-builtin-face
      (:match ,elixir--builtin-keywords-re @elixir-font-constant-builtin-face))
     ((identifier) @elixir-font-comment-unused-face
      (:match "^_" @elixir-font-comment-unused-face))
     (identifier) @elixir-font-variable-face
     ["%"] @elixir-font-punctuation-face
     ["," ";"] @elixir-font-punctuation-delimiter-face
     ["(" ")" "[" "]" "{" "}" "<<" ">>"] @elixir-font-punctuation-bracket-face

     (charlist
      [
       quoted_end: _ @elixir-font-string-face
       quoted_start: _ @elixir-font-string-face
       (quoted_content) @elixir-font-string-face
       (interpolation
        "#{"
        @elixir-font-string-escape-face
        "}" @elixir-font-string-escape-face)
       ])
     (string
      [
       quoted_end: _ @elixir-font-string-face
       quoted_start: _ @elixir-font-string-face
       (quoted_content) @elixir-font-string-face
       (interpolation
        "#{"
        @elixir-font-string-escape-face
        "}" @elixir-font-string-escape-face)
       ]))
   :language 'elixir
   :feature 'moderate
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
   :feature 'elaborate
   :override t
   `((escape_sequence) @elixir-font-string-escape-face)
   )
  "Tree-sitter font-lock settings.")

(provide 'elixir-tree-sitter)

;;; elixir-tree-sitter.el ends here
