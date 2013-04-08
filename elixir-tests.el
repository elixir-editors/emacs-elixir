(require 'ert)
(require 'ert-x)

(defmacro reindenting-elixir-smie-mode-buffer (initial-contents &rest body)
  `(with-temp-buffer
     (insert ,initial-contents)
     (elixir-mode)
     (indent-region (point-min) (point-max))
     (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
       ,@body)))

(defmacro* elixir-deftest (name args &optional docstr &body body)
  (declare (indent 2)
           (&define :name test name sexp
                    [&optional [":documentation" stringp]]
                    [&optional [":expected-result" sexp]]
                    def-body))
  `(ert-deftest ,(intern (format "elixir-ert-%s" name)) ,args
     ,docstr
     (let ((elixir-smie-verbose-p t))
       ,@body)))

(defun elixir-tests-explain-string-incompatibility (got expected)
  (if (equal got expected)
      nil
    ()))

(defmacro* elixir-def-indentation-test (name initial-contents expected-output)
  (declare (indent 1))
  `(elixir-deftest ,name ()
     (ert-with-test-buffer (:name ,(format "(Expected)" name))
       (elixir-mode)
       (insert ,initial-contents)
       (let ((indented (ert-buffer-string-reindented)))
         (delete-region (point-min) (point-max))
         (insert ,expected-output)
         (ert-with-test-buffer (:name ,(format "(Actual)" name))
           (elixir-mode)
           (insert indented)
           (should (equal indented ,expected-output)))))))

(elixir-def-indentation-test indents-do-blocks
  "
defmodule Foo do
def foobar do
if true, do: IO.puts \"yay\"
20
end
end"
  "
defmodule Foo do
  def foobar do
    if true, do: IO.puts \"yay\"
    20
  end
end")

(elixir-def-indentation-test indents-function-calls-without-parens
  "
test \"foo\" do
assert true, \"should be true\"
assert !false, \"should still be true\"
end
"
  "
test \"foo\" do
  assert true, \"should be true\"
  assert !false, \"should still be true\"
end
")

(elixir-def-indentation-test indents-records-correctly
  "
defrecord Money, [:currency_unit, :amount] do
foo
end
"
  "
defrecord Money, [:currency_unit, :amount] do
  foo
end
")

(elixir-def-indentation-test indents-continuation-lines
  "
has_something(x) &&
has_something(y) ||
has_something(z)
"
  "
has_something(x) &&
  has_something(y) ||
  has_something(z)
")

(elixir-def-indentation-test indents-continuation-lines-with-comments
  "
has_something(x) &&  # foo
has_something(y) ||
has_something(z)
"
  "
has_something(x) &&  # foo
  has_something(y) ||
  has_something(z)
")
