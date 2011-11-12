% Test for elixir-mode

% This is a single line comment.

module Hello                    % module definition
  def hello (person := "world") % method definition, with argument defaults
    "Hello, " + person + "."    % using + as a string method
  end                           % ending indentation
end


% Defining a variable

answer = 42

% Functions

sum = (x, y) -> x + y
difference = (x, y) do
  x - y
end

% Erlang builtins

Erlang.is_atom(true) %=> true

% If elixir-mode-higlight-operators is enabled, these will be colored.

lotsofoperators = ((((4 + 4) div 2) * 3 - 4) rem 5 / 2).to_s % arithmetic and method operators
h = #Hello() % `#' operator
!false %=> true
true && true %=> true
true || false %=> true

% Here are some atoms

true
false
nil
'foo
'bar

% Here are some regular expressions

-r("[ \n\t]")

% Some modules

Date.today
Timer.ms(-> 1 + 2)