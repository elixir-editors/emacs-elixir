;; `elixir-test-with-temp-buffer' and `elixir-test-face-at' are both slightly
;; modified versions of the original at
;; https://github.com/lunaryorn/puppet-mode/blob/master/test/puppet-mode-test.el

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
   "match = ~r/foo/"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 9) 'font-lock-builtin-face))
   (should (eq (elixir-test-face-at 12) 'font-lock-string-face))
   ;; no face for regex delimiters
   (should (eq (elixir-test-face-at 15) nil))))

(ert-deftest elixir-mode-syntax-table/fontify-modules-and-types ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "defmodule Application.Behavior do
  use Application.Behaviour"
   (should (eq (elixir-test-face-at 1) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 11) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 22) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 23) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 32) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 37) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 41) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 52) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 53) 'font-lock-type-face))))

(ert-deftest elixir-mode-syntax-table/fontify-regex-with-quote ()
  "https://github.com/elixir-lang/emacs-elixir/issues/23"
  :tags '(fontification syntax-table)
  :expected-result :failed
  (elixir-test-with-temp-buffer
      "~r/\"/
x = 15"
    (should (eq (elixir-test-face-at 7) 'font-lock-variable-name-face))))

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
:andale"
    (should (eq (elixir-test-face-at 3) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 5) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 10) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 13) 'elixir-atom-face))))

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
    (should (eq (elixir-test-face-at 3) 'font-lock-variable-name-face))
    (should (eq (elixir-test-face-at 11) 'font-lock-string-face))))

(ert-deftest elixir-mode-syntax-table/fontify-continuation-lines-assignment ()
  :expected-result :failed
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "some_var =
some_expr"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))))

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
