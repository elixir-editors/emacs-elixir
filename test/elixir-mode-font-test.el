;;; elixir-mode-font-test.el --- Font highlighting testsuite

;;; Commentary:
;;
;; `elixir-test-with-temp-buffer' and `elixir-test-face-at' are both slightly
;; modified versions of the original at
;; https://github.com/lunaryorn/puppet-mode/blob/master/test/puppet-mode-test.el

;;; Code:

(require 'test-helper)

(defun elixir-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (elixir-test-with-temp-buffer content
        (get-text-property pos 'face))
    (get-text-property pos 'face)))

(ert-deftest elixir-mode-syntax-table/fontify-regex ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "match = ~r/foo/
match=~r/foo/"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 9) 'font-lock-builtin-face))
   (should (eq (elixir-test-face-at 12) 'font-lock-string-face))
   (should (eq (elixir-test-face-at 18) 'font-lock-variable-name-face))
   ;; no face for regex delimiters
   (should (eq (elixir-test-face-at 15) nil))))

(ert-deftest elixir-mode-syntax-table/sigils ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "asdfg = ~s{Capitalized noncapitalized}"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 9) 'elixir-attribute-face))
   (should (eq (elixir-test-face-at 12) 'font-lock-string-face))
   (should (eq (elixir-test-face-at 26) 'font-lock-string-face))
   ;; no face for regex delimiters
   (should (eq (elixir-test-face-at 38) nil))))

(ert-deftest elixir-mode-syntax-table/fontify-special-macros ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "__MODULE__
__DIR__
__aliases__
%__MODULE__
&__MODULE__
&abc__DIR__"
   (should (eq (elixir-test-face-at 4) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 14) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 24) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 34) 'font-lock-constant-face))
   (should (eq (elixir-test-face-at 44) 'font-lock-constant-face))
   (should-not (eq (elixir-test-face-at 60) 'font-lock-constant-face))))

(ert-deftest elixir-mode-syntax-table/fontify-modules-and-types ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "defmodule Application.Behavior do
  use Application.Behaviour
  Stand.Alone.call
  %RuntimeError{message: msg}
  &Enum"
   (should (eq (elixir-test-face-at 1) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 11) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 22) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 23) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 32) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 37) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 41) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 52) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 53) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 68) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 72) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 114) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 117) 'font-lock-type-face))
   ;; no face for function call
   (should (eq (elixir-test-face-at 79) nil))
   (should (eq (elixir-test-face-at 84) 'font-lock-type-face))
   ;; no face for curly braces
   (should (eq (elixir-test-face-at 97) nil))))

(ert-deftest elixir-mode-syntax-table/fontify-regex-with-quote ()
  "https://github.com/elixir-lang/emacs-elixir/issues/23"
  :tags '(fontification syntax-table)
  :expected-result :failed
  (elixir-test-with-temp-buffer
      "~r/\"/
x = 15"
    (should (eq (elixir-test-face-at 8) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-regex-with-question/1 ()
  "https://github.com/elixir-lang/emacs-elixir/issues/36"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "~r/^matt: (?<ct>\d+)$/mg
x = 15"
    (should (eq (elixir-test-face-at 4) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 25) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-regex-with-question/2 ()
  "https://github.com/elixir-lang/emacs-elixir/issues/29"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "a = \"\" <> \"?\"
x = 15"
    (should (eq (elixir-test-face-at 15) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-function-name/1 ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "def fooBar do
  :foo
end"
    (should (eq (elixir-test-face-at 5) 'font-lock-function-name-face))
    (should (eq (elixir-test-face-at 8) 'font-lock-function-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-function-name/2 ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "def foo? do
  :foo
end"
    (should (eq (elixir-test-face-at 5) 'font-lock-function-name-face))
    (should (eq (elixir-test-face-at 8) 'font-lock-function-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-function-name/3 ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "def foo! do
  :foo
end"
    (should (eq (elixir-test-face-at 5) 'font-lock-function-name-face))
    (should (eq (elixir-test-face-at 8) 'font-lock-function-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-heredoc/1 ()
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-temp-buffer
      "@doc \"\"\""
    (should (eq (elixir-test-face-at 1) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 2) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 6) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/fontify-heredoc/2 ()
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-temp-buffer
      "@moduledoc \"\"\""
    (should (eq (elixir-test-face-at 1) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 2) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 12) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/fontify-heredoc/3 ()
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-temp-buffer
      "~s\"\"\""
    (should (eq (elixir-test-face-at 1) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 2) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 3) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/fontify-atoms ()
  :tags '(fontification atom syntax-table)
  (elixir-test-with-temp-buffer
      ":oriole
:andale
:ms2pid
:CapitalizedAtom
true
false
nil
true_false_nil
"
    (should (eq (elixir-test-face-at 3) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 5) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 10) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 13) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 18) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 23) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 26) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 43) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 48) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 54) 'elixir-atom-face))
    (should-not (eq (elixir-test-face-at 57) 'elixir-atom-face))))

(ert-deftest elixir-mode-syntax-table/fontify-map-keys ()
  :tags '(fontification map syntax-table)
  (elixir-test-with-temp-buffer
      "%{a: 1, b: 2}"
    (should (eq (elixir-test-face-at 3) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 4) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 9) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 10) 'elixir-atom-face))))

(ert-deftest elixir-mode-syntax-table/fontify-interpolation ()
  :tags '(fontification interpolation syntax-table)
  (elixir-test-with-temp-buffer
      "\"#{1 + 2} is 3.\""
    (should (eq (elixir-test-face-at 1) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 11) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/fontify-continuation-lines-assignment ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "some_var =
some_expr"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/dont-fontify-equal-match ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "this == that"
   (should-not (eq (elixir-test-face-at 2) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-triple-quoted-string ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "\"\"\"foo\"bar\"baz #{1 + 2} is 3.\"\"\""
    (should (eq (elixir-test-face-at 1) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 5) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 19) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 31) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/fontify-atom-in-pattern-match ()
  :tags '(fontification atom syntax-table)
  (elixir-test-with-temp-buffer
   ":any = into_to_type(type)
:another=into_to_type(type)"
   (should (eq (elixir-test-face-at 3) 'elixir-atom-face))))

(ert-deftest elixir-mode-syntax-table/fontify-assignment-with-pattern/1 ()
  :expected-result :failed
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "{x, y} = some_expr"
   (should (eq (elixir-test-face-at 2) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 5) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-assignment-with-pattern/2 ()
  :expected-result :failed
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "[h|t] = some_expr"
   (should (eq (elixir-test-face-at 2) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 4) 'font-lock-variable-name-face))))

(ert-deftest elixir-mode-syntax-table/fontify-assignment-with-special-atom ()
  "https://github.com/elixir-lang/emacs-elixir/issues/245"
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "true_false_nil = 1"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 6) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 12) 'font-lock-variable-name-face))))

(provide 'elixir-mode-font-test)

;;; elixir-mode-font-test.el ends here
