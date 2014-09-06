
(elixir-deftest syntax-table/fontify-regex
  :tags '(fontification syntax-table)
  (elixir-test-with-test-buffer ()
      "match = ~r/foo/"
    (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))
    (should (eq (elixir-test-face-at 9) 'font-lock-builtin-face))
    (should (eq (elixir-test-face-at 12) 'font-lock-string-face))
    ;; no face for regex delimiters
    (should (eq (elixir-test-face-at 15) nil))))

(elixir-deftest syntax-table/fontify-modules-and-types
  :tags '(fontification syntax-table)
  (elixir-test-with-test-buffer ()
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

;; http://github.com/elixir-lang/emacs-elixir/issues/23
(elixir-deftest syntax-table/fontify-regex-with-quote
  :tags '(fontification syntax-table)
  :expected-result :failed
  (elixir-test-with-test-buffer ()
      "~r/\"/
x = 15"
    (should (eq (elixir-test-face-at 7) 'font-lock-variable-name-face))))

;; http://github.com/elixir-lang/emacs-elixir/issues/36
(elixir-deftest syntax-table/fontify-regex-with-question/1
  :tags '(fontification syntax-table)
  (elixir-test-with-test-buffer ()
      "~r/^matt: (?<ct>\d+)$/mg
x = 15"
    (should (eq (elixir-test-face-at 4) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 25) 'font-lock-variable-name-face))))

;; http://github.com/elixir-lang/emacs-elixir/issues/29
(elixir-deftest syntax-table/fontify-regex-with-question/2
  :tags '(fontification syntax-table)
  (elixir-test-with-test-buffer ()
      "a = \"\" <> \"?\"
x = 15"
    (should (eq (elixir-test-face-at 15) 'font-lock-variable-name-face))))

(elixir-deftest syntax-table/fontify-function-name/1
  :tags '(fontification syntax-table)
  (elixir-test-with-test-buffer ()
      "def fooBar do
  :foo
end"
    (should (eq (elixir-test-face-at 5) 'font-lock-function-name-face))
    (should (eq (elixir-test-face-at 8) 'font-lock-function-name-face))))

(elixir-deftest syntax-table/fontify-function-name/2
  :tags '(fontification syntax-table)
  (elixir-test-with-test-buffer ()
      "def foo? do
  :foo
end"
    (should (eq (elixir-test-face-at 5) 'font-lock-function-name-face))
    (should (eq (elixir-test-face-at 8) 'font-lock-function-name-face))))

(elixir-deftest syntax-table/fontify-function-name/3
  :tags '(fontification syntax-table)
  (elixir-test-with-test-buffer ()
      "def foo! do
  :foo
end"
    (should (eq (elixir-test-face-at 5) 'font-lock-function-name-face))
    (should (eq (elixir-test-face-at 8) 'font-lock-function-name-face))))

(elixir-deftest syntax-table/fontify-heredoc/1
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-test-buffer ()
      "@doc \"\"\""
    (should (eq (elixir-test-face-at 1) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 2) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 6) 'font-lock-string-face))))

(elixir-deftest syntax-table/fontify-heredoc/2
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-test-buffer ()
      "@moduledoc \"\"\""
    (should (eq (elixir-test-face-at 1) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 2) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 12) 'font-lock-string-face))))

(elixir-deftest syntax-table/fontify-heredoc/3
  :tags '(fontification heredoc syntax-table)
  (elixir-test-with-test-buffer ()
      "~s\"\"\""
    (should (eq (elixir-test-face-at 1) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 2) 'elixir-attribute-face))
    (should (eq (elixir-test-face-at 3) 'font-lock-string-face))))

(elixir-deftest syntax-table/fontify-atoms
  :tags '(fontification atom syntax-table)
  (elixir-test-with-test-buffer ()
      ":oriole
:andale"
    (should (eq (elixir-test-face-at 3) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 5) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 10) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 13) 'elixir-atom-face))))

(elixir-deftest syntax-table/fontify-map-keys
  :tags '(fontification map syntax-table)
  (elixir-test-with-test-buffer ()
      "%{a: 1, b: 2}"
    (should (eq (elixir-test-face-at 3) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 4) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 9) 'elixir-atom-face))
    (should (eq (elixir-test-face-at 10) 'elixir-atom-face))))

(elixir-deftest syntax-table/fontify-interpolation
  :tags '(fontification interpolation syntax-table)
  (elixir-test-with-test-buffer ()
      "\"#{1 + 2} is 3.\""
    (should (eq (elixir-test-face-at 1) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 3) 'font-lock-variable-name-face))
    (should (eq (elixir-test-face-at 11) 'font-lock-string-face))))

