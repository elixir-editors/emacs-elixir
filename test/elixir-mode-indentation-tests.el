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
(elixir-def-indentation-test indent-use-dot-module-newline ()
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

(elixir-def-indentation-test indent-use-dot-module ()
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

(elixir-def-indentation-test indent-do-blocks ()
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

(elixir-def-indentation-test indent-do-blocks-after-linebreak-two ()
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

(elixir-def-indentation-test indent-do-blocks-after-linebreak-three ()
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

(elixir-def-indentation-test indent-do-blocks-with-space-after-inline ()
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
    ()
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

(elixir-def-indentation-test indent-function-calls-without-parens ()
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

(elixir-def-indentation-test indent-records-correctly ()
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

(elixir-def-indentation-test indent-continuation-lines ()
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
    ()
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

(elixir-def-indentation-test indent-continuation-lines-with-comments/2 ()
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

(elixir-def-indentation-test indent-last-commented-line
    ()
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

(elixir-def-indentation-test indent-if ()
  "
if condition do
yes
end"
  "
if condition do
  yes
end")

(elixir-def-indentation-test indent-if-else ()
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

(elixir-def-indentation-test indent-try ()
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

(elixir-def-indentation-test indent-try/after ()
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

(elixir-def-indentation-test indent-try/catch/after ()
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

(elixir-def-indentation-test indent-fn-in-assignment ()
  "
f = fn x, y ->
x + y
end"
  "
f = fn x, y ->
  x + y
end")

(elixir-def-indentation-test indent-fn-as-arguments ()
  "
Enum.map 1..10, fn x ->
x + 1
end"
  "
Enum.map 1..10, fn x ->
  x + 1
end")

(elixir-def-indentation-test indent-list-argument-continuation-lines-nicely ()
  "
to_process = [27, 33, 35, 11, 36, 29, 18, 37, 21, 31, 19, 10, 14, 30,
15, 17, 23, 28, 25, 34, 22, 20, 13, 16, 32, 12, 26, 24]
"
  "
to_process = [27, 33, 35, 11, 36, 29, 18, 37, 21, 31, 19, 10, 14, 30,
              15, 17, 23, 28, 25, 34, 22, 20, 13, 16, 32, 12, 26, 24]
")

(elixir-def-indentation-test indent-nested-fn
    ()
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
    ()
  "
[1.2,
3.4]"
  "
[1.2,
 3.4]")

(elixir-def-indentation-test indent-after-operator ()
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

(elixir-def-indentation-test nested-modules ()
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

(elixir-def-indentation-test indent-heredoc
    (:expected-result :failed)
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
    (:expected-result :failed)
  "def foo(x) do
  a = x
    |> Enum.reverse
end"
  "def foo(x) do
  a = x
    |> Enum.reverse
end")

(elixir-def-indentation-test indent-inside-parens
    (:expected-result :failed)
  "x = do_something(
  :foo,
  :bar
)"
  "x = do_something(
  :foo,
  :bar
)")

(elixir-def-indentation-test indent-inside-parens/2 ()
  "
x = do_something(:foo,
                 :bar)"
    "
x = do_something(:foo,
                 :bar)")

(elixir-def-indentation-test indent-inside-parens/3
    (:expected-result :failed)
  "
x = do_something(:foo, fn (arg) ->
                         do_another(arg)
                       end)"
"
x = do_something(:foo, fn (arg) ->
                         do_another(arg)
                       end)")

(elixir-def-indentation-test indent-lone-keyword
    (:expected-result :failed)
  "
def foo do #comment
  :bar
end"
    "
def foo do #comment
  :bar
end")

(elixir-def-indentation-test indent-multiline-match ()
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

(elixir-def-indentation-test indent-after-require-Record
    ()
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
