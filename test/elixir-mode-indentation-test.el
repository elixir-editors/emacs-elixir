;;; elixir-mode-indentation-test.el --- Indentation testsuite

;;; Commentary:
;; 

;;; Code:

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

;; Expected test failures indicates that the code tested by that test case is
;; indeed broken. My intention is that while working on a specific problem,
;; the failure expectation will be removed so that we know when the test case
;; passes.
(elixir-def-indentation-test indent-use-dot-module-newline
			     (:tags '(indentation))
  "defmodule Foo do
use GenServer.Behaviour

def foobar do
if true, do: IO.puts \"yay\"
end
end"
  "defmodule Foo do
  use GenServer.Behaviour

  def foobar do
    if true, do: IO.puts \"yay\"
  end
end")

(elixir-def-indentation-test indent-use-dot-module
			     (:tags '(indentation))
  "
defmodule Foo do
use GenServer.Behaviour
def foobar do
if true, do: IO.puts \"yay\"
end
end"
  "
defmodule Foo do
  use GenServer.Behaviour
  def foobar do
    if true, do: IO.puts \"yay\"
  end
end")

(elixir-def-indentation-test indent-do-blocks
			     (:tags '(indentation))
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

(elixir-def-indentation-test indent-do-blocks-after-linebreak-two
			     (:tags '(indentation))
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

(elixir-def-indentation-test indent-do-blocks-after-linebreak-three
			     (:tags '(indentation))
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

def baz do
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

  def baz do
    if true, do: IO.puts \"yay\"
    20
  end
end")

(elixir-def-indentation-test indent-do-blocks-with-space-after-inline
			     (:tags '(indentation))
  "defmodule Foo do
def foobar do
if true, do: IO.puts \"yay\"

20
end
end"
  "defmodule Foo do
  def foobar do
    if true, do: IO.puts \"yay\"

    20
  end
end")

(elixir-def-indentation-test indent-after-empty-line
			     (:tags '(indentation))
  "
def foo do
a = 2

b = a + 3

c = a * b
end"
  "
def foo do
  a = 2

  b = a + 3

  c = a * b
end")

(elixir-def-indentation-test indent-function-calls-without-parens
			     (:tags '(indentation))
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

(elixir-def-indentation-test indent-records-correctly
			     (:tags '(indentation))
  "
defmodule MyModule do
require Record
Record.defrecord :money, [:currency_unit, :amount]

Record.defrecord :animal, [:species, :name]
end
"
  "
defmodule MyModule do
  require Record
  Record.defrecord :money, [:currency_unit, :amount]

  Record.defrecord :animal, [:species, :name]
end
")

(elixir-def-indentation-test indent-continuation-lines
			     (:tags '(indentation))
  "
def foo do
has_something(x) &&
  has_something(y) ||
  has_something(z)
end
"
  "
def foo do
  has_something(x) &&
    has_something(y) ||
    has_something(z)
end
")

(elixir-def-indentation-test indent-continuation-lines-with-comments/1
			     (:tags '(indentation))
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

(elixir-def-indentation-test indent-continuation-lines-with-comments/2
			     (:tags '(indentation))
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

(elixir-def-indentation-test indent-continuation-lines-with-comments/3
			     (:tags '(indentation))
  "
def str(s, sub, start_pos, end_pos) when is_binary(s) and is_binary(sub) do # and start_pos <= end_pos do
                                                                             len = end_pos-start_pos
end
"
  "
def str(s, sub, start_pos, end_pos) when is_binary(s) and is_binary(sub) do # and start_pos <= end_pos do
  len = end_pos-start_pos
end
")

(elixir-def-indentation-test indent-continuation-lines-assignment
			     (:tags '(indentation))
  "
some_var =
some_expr
" "
some_var =
  some_expr
")

(elixir-def-indentation-test indent-continuation-lines-assignment/2
			     (:tags '(indentation))
  "
next_fun =
        case raw do
 true  -> &IO.each_binstream(&1, line_or_bytes)
      false -> &IO.each_stream(&1, line_or_bytes)
       end
" "
next_fun =
  case raw do
    true  -> &IO.each_binstream(&1, line_or_bytes)
    false -> &IO.each_stream(&1, line_or_bytes)
  end
")

(elixir-def-indentation-test indent-last-commented-line
			     (:tags '(indentation))
  "
defmodule Foo do
def bar do
2
end

# last line
end
"
  "
defmodule Foo do
  def bar do
    2
  end

  # last line
end
")

(elixir-def-indentation-test indent-if
			     (:tags '(indentation))
  "
if condition do
yes
end"
  "
if condition do
  yes
end")

(elixir-def-indentation-test indent-if-else
			     (:tags '(indentation))
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

(elixir-def-indentation-test indent-try
			     (:tags '(indentation))
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

(elixir-def-indentation-test indent-try/after
			     (:tags '(indentation))
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

(elixir-def-indentation-test indent-try/catch/after
			     (:tags '(indentation))
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
end
"
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
end
")

(elixir-def-indentation-test indent-try/rescue/1
			     (:tags '(indentation))
  "
try do
raise 'some error'
rescue
RuntimeError -> 'rescued a runtime error'
end
"
  "
try do
  raise 'some error'
rescue
  RuntimeError -> 'rescued a runtime error'
end
")

(elixir-def-indentation-test indent-try/rescue/2
			     (:tags '(indentation))
  "
try do
raise 'some error'
rescue
x in [RuntimeError] ->
x.message
end
"
  "
try do
  raise 'some error'
rescue
  x in [RuntimeError] ->
    x.message
end
")

(elixir-def-indentation-test indent-block-inside-fn-match
			     (:tags '(indentation))
"
defp into(stream, device, raw) do
 fn
   :ok, {:cont, x} ->
  case raw do
true  -> IO.binwrite(device, x)
false -> IO.write(device, x)
end
:ok, _ -> stream
end
end"
"
defp into(stream, device, raw) do
  fn
    :ok, {:cont, x} ->
      case raw do
        true  -> IO.binwrite(device, x)
        false -> IO.write(device, x)
      end
    :ok, _ -> stream
  end
end")

(elixir-def-indentation-test indent-fn-in-assignment
			     (:tags '(indentation))
  "
f = fn x, y ->
x + y
end"
  "
f = fn x, y ->
  x + y
end")

(elixir-def-indentation-test indent-fn-as-arguments
			     (:tags '(indentation))
  "
Enum.map 1..10, fn x ->
x + 1
end"
  "
Enum.map 1..10, fn x ->
  x + 1
end")

(elixir-def-indentation-test indent-list-argument-continuation-lines-nicely
			     (:tags '(indentation))
  "
to_process = [27, 33, 35, 11, 36, 29, 18, 37, 21, 31, 19, 10, 14, 30,
15, 17, 23, 28, 25, 34, 22, 20, 13, 16, 32, 12, 26, 24]
"
  "
to_process = [27, 33, 35, 11, 36, 29, 18, 37, 21, 31, 19, 10, 14, 30,
              15, 17, 23, 28, 25, 34, 22, 20, 13, 16, 32, 12, 26, 24]
")

(elixir-def-indentation-test indent-nested-fn
			     (:tags '(indentation))
  "defmodule FooModule do
def foo do
x = fn(a, b) -> a + b end
end
end"
  "defmodule FooModule do
  def foo do
    x = fn(a, b) -> a + b end
  end
end")

(elixir-def-indentation-test indent-list-of-floats-aligns
			     (:tags '(indentation))
  "
[1.2,
3.4]"
  "
[1.2,
 3.4]")

(elixir-def-indentation-test indent-after-operator
			     (:tags '(indentation))
  "
defmodule Banana do
def start do
a = \"\" <> \"?\"

case bar do
z -> 1
end

case foo do
?x -> x
end

end
end
"
  "
defmodule Banana do
  def start do
    a = \"\" <> \"?\"

    case bar do
      z -> 1
    end

    case foo do
      ?x -> x
    end

  end
end
")

(elixir-def-indentation-test nested-modules
			     (:tags '(indentation))
  "defmodule Mod1 do
  defmodule Mod1a do
    def start do
      foo()
    end
  end
end"
  "defmodule Mod1 do
  defmodule Mod1a do
    def start do
      foo()
    end
  end
end")

(elixir-def-indentation-test cond-comment
			     (:tags '(indentation))
  "
def foo() do
cond do
yadda ->
:ok
badda -> # comment throws this off
:what
end
end
"
  "
def foo() do
  cond do
    yadda ->
      :ok
    badda -> # comment throws this off
      :what
  end
end
")

(elixir-def-indentation-test indent-heredoc
			     (:expected-result :failed :tags '(indentation))
  "
defmodule Foo do
@doc \"\"\"
this is a heredoc string

\"\"\"
def convert do
x = 15
end
end
"
  "
defmodule Foo do
  @doc \"\"\"
  this is a heredoc string

  \"\"\"
  def convert do
    x = 15
  end
end
")

(elixir-def-indentation-test indent-pipes
			     (:tags '(indentation))
    "
def foo(x) do
  a = x
       |> Enum.reverse
end"
    "
def foo(x) do
  a = x
  |> Enum.reverse
end")

(elixir-def-indentation-test indent-inside-parens
			     (:tags '(indentation))
  "
x = do_something(
:foo,
:bar
)"
  "
x = do_something(
  :foo,
  :bar
)")

(elixir-def-indentation-test indent-inside-parens/2
			     (:tags '(indentation))
"
x = do_something(:foo,
                    :bar)"
"
x = do_something(:foo,
                 :bar)")

(elixir-def-indentation-test indent-inside-parens/3
			     (:tags '(indentation))
"
x = do_something(:foo, fn (arg) ->
                         do_another(arg)
                       end)"
"
x = do_something(:foo, fn (arg) ->
  do_another(arg)
end)")

(elixir-def-indentation-test indent-inside-parens/4
			     (:tags '(indentation))
"
defmodule Something do
def something do
x = do_something(:foo, fn (arg) ->
                         do_another(arg)
                       end)
end
end"
"
defmodule Something do
  def something do
    x = do_something(:foo, fn (arg) ->
      do_another(arg)
    end)
  end
end")

(elixir-def-indentation-test indent-inside-parens/5
			     (:tags '(indentation))
"
defmodule IndentPlayground do
def my_func(arr) do
 Enum.map(arr, fn(x) ->
  x * 2
end)
   #back here
end
end"
"
defmodule IndentPlayground do
  def my_func(arr) do
    Enum.map(arr, fn(x) ->
      x * 2
    end)
    #back here
  end
end")

(elixir-def-indentation-test indent-lone-keyword
			     (:tags '(indentation))
  "
def foo do #comment
  :bar
end"
    "
def foo do #comment
  :bar
end")

(elixir-def-indentation-test indent-single-line-match
			     (:tags '(indentation))
   "
case x do
a -> b
c -> d
end
" "
case x do
  a -> b
  c -> d
end
")

(elixir-def-indentation-test indent-multiline-match
			     (:tags '(indentation))
  "
def foo do
  case is_string(x) do
    true ->
      x2 = \" one\"
      x <> x2
    false ->
      x2 = \" two\"
      x <> x2
  end
end"
  "
def foo do
  case is_string(x) do
    true ->
      x2 = \" one\"
      x <> x2
    false ->
      x2 = \" two\"
      x <> x2
  end
end"
  )

(elixir-def-indentation-test indent-multiline-match/2
			     (:tags '(indentation))
  "
def foo do
  case is_string(x) do
    true ->
      x2 = \" one\"
      x <> x2

    false ->
      x2 = \" two\"
      x <> x2
  end
end"
  "
def foo do
  case is_string(x) do
    true ->
      x2 = \" one\"
      x <> x2

    false ->
      x2 = \" two\"
      x <> x2
  end
end"
  )

(elixir-def-indentation-test indent-mixed-match
			     (:tags '(indentation))
   "
case x do
a -> b
c ->
d
e -> f
end
" "
case x do
  a -> b
  c ->
    d
  e -> f
end
")

(elixir-def-indentation-test indent-after-require-Record
			     (:tags '(indentation))
  ;; Mind the significant whitespace after `Record' in each case. There should
  ;; be two spaces after `Record', otherwise this test is meaningless.
  "
defmodule RSS do
require Record

def zip(list1, list2) when length(list1) == length(list2) do
x = 1
end
end"
  "
defmodule RSS do
  require Record

  def zip(list1, list2) when length(list1) == length(list2) do
    x = 1
  end
end")

;; We don't want automatic whitespace cleanup here because of the significant
;; whitespace after `Record' above. By setting `whitespace-action' to nil,
;; `whitespace-mode' won't automatically clean up trailing whitespace (in my
;; config, anyway).

;;; Local Variables:
;;; whitespace-action: nil
;;; End:

(provide 'elixir-mode-indentation-test)

;;; elixir-mode-indentation-test.el ends here
