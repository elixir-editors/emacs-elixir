## v1.3.1 - 2014/07/05
* [#52](https://github.com/elixir-lang/emacs-elixir/pull/52) - Add CLI for running elixir-mode tests.
* [#48](https://github.com/elixir-lang/emacs-elixir/pull/48) - Add a SMIE rule function for "def". Fixes #38 and #41
* [#50](https://github.com/elixir-lang/emacs-elixir/pull/50) - Remove grammar clause for "fn" terminal. Fixes #7
* [#44](https://github.com/elixir-lang/emacs-elixir/pull/44) - sigil % to ~
* [#46](https://github.com/elixir-lang/emacs-elixir/pull/46) - Added highlight for unless and Task module
* [#45](https://github.com/elixir-lang/emacs-elixir/pull/45) - Highlight defstruct and Actor, Base modules
* [#40](https://github.com/elixir-lang/emacs-elixir/pull/40) - IMenu: Show ExUnit tests under heading "Tests".
* [#37](https://github.com/elixir-lang/emacs-elixir/pull/37) - Remove needless statement
* [#35](https://github.com/elixir-lang/emacs-elixir/pull/35) - Added simple imenu support.
* [#32](https://github.com/elixir-lang/emacs-elixir/pull/32) - Properly make variables local
* [#31](https://github.com/elixir-lang/emacs-elixir/pull/31) - needed for effective code-navigation via syntax-ppss
* [#28](https://github.com/elixir-lang/emacs-elixir/pull/28) - Recognize ? char syntax
* [#24](https://github.com/elixir-lang/emacs-elixir/pull/24) - elixir-mode-eval-on-current-buffer binding comment incorrect
* [#22](https://github.com/elixir-lang/emacs-elixir/pull/22) - Enhance `elixir-mode-iex` to accept additional arguments

## 1.3.0 (June 24, 2013)
- Add `elixir-mode-eval-on-region` to evalute Elixir code on the
  marked region.
- Add `elixir-mode-eval-on-current-buffer` to evalute Elixir code in the current buffer.
- Add `elixir-mode-eval-on-current-line` to evalute Elixir code on the current line.
- Add `elixir-mode-string-to-quoted-on-region` to get the representation of the expression on the marked region.
- Add `elixir-mode-string-to-quoted-on-current-line` to get the
  representation of the expression on the current line.
