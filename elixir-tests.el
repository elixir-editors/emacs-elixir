(require 'ert)
(require 'ert-x)

(defmacro* elixir-deftest (name args &body body)
  (declare (indent 2)
           (&define :name test name sexp
                    [&optional [":documentation" stringp]]
                    [&optional [":expected-result" sexp]]
                    def-body))
  `(ert-deftest ,(intern (format "elixir-ert-%s" name)) ()
     ""
     ,@args
     (let ((elixir-smie-verbose-p t))
       ,@body)))

(defmacro* elixir-def-indentation-test (name args initial-contents expected-output)
  (declare (indent 2))
  `(elixir-deftest ,name ,args
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

(elixir-def-indentation-test indents-do-blocks ()
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

(elixir-def-indentation-test indents-function-calls-without-parens ()
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

(elixir-def-indentation-test indents-records-correctly ()
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

(elixir-def-indentation-test indents-continuation-lines ()
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

(elixir-def-indentation-test indents-continuation-lines-with-comments/1
  (:expected-result :failed)
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

(elixir-def-indentation-test indents-continuation-lines-with-comments/2 ()
  "
has_something(x) &&
has_something(y) || # foo
has_something(z)
"
  "
has_something(x) &&
  has_something(y) || # foo
  has_something(z)
")

(elixir-def-indentation-test indents-if ()
  "
if condition do
yes
end"
  "
if condition do
  yes
end")

(elixir-def-indentation-test indents-if-else ()
  "
if condition do
yes
else
no
end"
  "
if condition do
  yes
else
  no
end")

(elixir-def-indentation-test indents-try ()
  "
try do
foo
bar
end"
  "
try do
  foo
  bar
end")

(elixir-def-indentation-test indents-try/after ()
  "
try do
foo
bar
after
after_everything()
post_that()
end"
  "
try do
  foo
  bar
after
  after_everything()
  post_that()
end")

(elixir-def-indentation-test indents-try/catch/after ()
  "
try do
foo
bar
catch
baz ->
nope
[yeah] ->
maybe
after
after_everything()
post_that()
end"
  "
try do
  foo
  bar
catch
  baz ->
    nope
  [yeah] ->
    maybe
after
  after_everything()
  post_that()
end")

(elixir-def-indentation-test indents-function ()
  "
function do
a,b,c ->
three_args
a,b ->
two_args
[a|rest] ->
one_arg_list
end
"
  "
function do
  a,b,c ->
    three_args
  a,b ->
    two_args
  [a|rest] ->
    one_arg_list
end
")

(elixir-def-indentation-test indents-fn ()
  "
f = fn x, y ->
x + y
end"
  "
f = fn x, y ->
         x + y
    end")
