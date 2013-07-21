(defmacro* elixir-def-indentation-test (name args initial-contents expected-output)
  (declare (indent 2))
  `(elixir-deftest ,name ,args
     (elixir-ert-with-test-buffer (:name ,(format "(Expected)" name))
         ,initial-contents
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

(elixir-def-indentation-test indents-do-blocks-after-linebreak ()
  "
defmodule FooBar do
def foo do
if true, do: IO.puts \"yay\"
20
end

def bar do
if true, do: IO.puts \"yay\"
20
end
end"
  "
defmodule FooBar do
  def foo do
    if true, do: IO.puts \"yay\"
    20
  end

  def bar do
    if true, do: IO.puts \"yay\"
    20
  end
end")

(elixir-def-indentation-test indents-after-empty-line ()
  "
a = 2

  b = a + 3

    c = a + b"
  "
a = 2

b = a + 3

c = a + b")


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

(elixir-def-indentation-test indents-last-commented-line
    (:expected-result :failed) ; #27
  "
defmodule Bar do
# ohai
end
"
  "
defmodule Bar do
  # ohai
end
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
\[yeah] ->
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
\[a|rest] ->
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

(elixir-def-indentation-test indents-fn-in-assignment
    (:expected-result :failed)
  "
f = fn x, y ->
x + y
end"
  "
f = fn x, y ->
  x + y
end")

(elixir-def-indentation-test indents-fn-as-arguments
    (:expected-result :failed)
  "
Enum.map 1..10, fn x ->
x+1
end"
  "
Enum.map 1..10, fn x ->
  x + 1
end")

(elixir-def-indentation-test indents-list-argument-continuation-lines-nicely ()
  "
to_process = [27, 33, 35, 11, 36, 29, 18, 37, 21, 31, 19, 10, 14, 30,
15, 17, 23, 28, 25, 34, 22, 20, 13, 16, 32, 12, 26, 24]
"
  "
to_process = [27, 33, 35, 11, 36, 29, 18, 37, 21, 31, 19, 10, 14, 30,
              15, 17, 23, 28, 25, 34, 22, 20, 13, 16, 32, 12, 26, 24]
")
