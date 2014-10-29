## v2.0.2 - 2014/10/29
* [#136](https://github.com/elixir-lang/emacs-elixir/pull/136) - Expand def of block operator regex to include non-newlines.
* [#137](https://github.com/elixir-lang/emacs-elixir/pull/137) - update rake tasks
* [#135](https://github.com/elixir-lang/emacs-elixir/pull/135) - Update README so the MELPA badge points to stable build.
* [#133](https://github.com/elixir-lang/emacs-elixir/pull/133) - refine readme
* [#134](https://github.com/elixir-lang/emacs-elixir/pull/134) - refine the ability to show the current elixir-mode version
* [#132](https://github.com/elixir-lang/emacs-elixir/pull/132) - Added: test coverage with undercover.el
* [#131](https://github.com/elixir-lang/emacs-elixir/pull/131) - Fix documentation url
* [#127](https://github.com/elixir-lang/emacs-elixir/pull/127) - Add failing test for comment in cond expression (fixed)
* [#128](https://github.com/elixir-lang/emacs-elixir/pull/128) - Added: collection of failing tests
* [#124](https://github.com/elixir-lang/emacs-elixir/pull/124) - Fixed: Build Status badge
* [#123](https://github.com/elixir-lang/emacs-elixir/pull/123) - TravisCI: add-apt-repository and apt-get install fix
* [#121](https://github.com/elixir-lang/emacs-elixir/pull/121) - Add TravisCI support
* [#122](https://github.com/elixir-lang/emacs-elixir/pull/122) - Remove unused code

## v2.0.1 - 2014/09/11
* [#119](https://github.com/elixir-lang/emacs-elixir/pull/119) - Fixed: indent-inside-parens
* [#117](https://github.com/elixir-lang/emacs-elixir/pull/117) - Fixed: emacs 24.3 tests
* [#116](https://github.com/elixir-lang/emacs-elixir/pull/116) - Add "How to run tests" section to CONTRIBUTING.md
* [#118](https://github.com/elixir-lang/emacs-elixir/pull/118) - Added: try/rescue rule
* [#114](https://github.com/elixir-lang/emacs-elixir/pull/114) - Fixed: run tests interectively for terminal

## v2.0.0 - 2014/09/08
* [#113](https://github.com/elixir-lang/emacs-elixir/pull/113) - Cask and ert-runner support
* [#110](https://github.com/elixir-lang/emacs-elixir/pull/110) - Added: ability to run tests via EVM
* [#111](https://github.com/elixir-lang/emacs-elixir/pull/111) - Fixed: elixir-quoted-minor-mode tests for ert-run-tests-interactively
* [#108](https://github.com/elixir-lang/emacs-elixir/pull/108) - Fix various issues caused by code followed by inline comments

## v1.5.0 - 2014/08/27
* [#103](https://github.com/elixir-lang/emacs-elixir/pull/103) - Add elixir-quoted-minor-mode.

## v1.4.10 - 2014/08/26
* [#102](https://github.com/elixir-lang/emacs-elixir/pull/102) - Add support for syntax highlighting for variable interpolation. Fixes #93
* [#101](https://github.com/elixir-lang/emacs-elixir/pull/101) - Fix indentation after inline comment. Fixes #95

## v1.4.9 - 2014/08/25
* [#100](https://github.com/elixir-lang/emacs-elixir/pull/100) - Fix indentation in multi-line match expressions. Fixes #98
* [#99](https://github.com/elixir-lang/emacs-elixir/pull/99) - Tokenize trailing whitespace properly. Fixes #97
* [#96](https://github.com/elixir-lang/emacs-elixir/pull/96) - Remove syntax highlighting for operators.

## v1.4.8 - 2014/08/19
* [#92](https://github.com/elixir-lang/emacs-elixir/pull/92) - Update Rakefile to also run the release.py script. Refs #88.
* [#91](https://github.com/elixir-lang/emacs-elixir/pull/91) - Updates to regexes for various lexemes
* [#90](https://github.com/elixir-lang/emacs-elixir/pull/90) - Fix bug in atom highlighting. 

## v1.4.7 - 2014/08/18
* [#87](https://github.com/elixir-lang/emacs-elixir/pull/87) - Add syntax highlighting for heredocs & failing tests for indentation.
* [#85](https://github.com/elixir-lang/emacs-elixir/pull/85) - Improve regex for defmodule highlighting.
* [#84](https://github.com/elixir-lang/emacs-elixir/pull/84) - Improve fontification for identifiers.

## v1.4.6 - 2014/08/18
* [#82](https://github.com/elixir-lang/emacs-elixir/pull/82) - Remove broken SMIE rule.

## v1.4.5 - 2014/08/18
* [#81](https://github.com/elixir-lang/emacs-elixir/pull/81) - Rewrite token emitting functions

## v1.4.4 - 2014/08/18
* [#79](https://github.com/elixir-lang/emacs-elixir/pull/79) - Remove erroneous defrecord syntax.

## v1.4.3 - 2014/08/16
* [#75](https://github.com/elixir-lang/emacs-elixir/pull/75) - Clean up several minor bugbears in elixir-smie.
* [#74](https://github.com/elixir-lang/emacs-elixir/pull/74) - Remove special indentation rules for operators, except booleans.

## v1.4.2 - 2014/08/15
* [#73](https://github.com/elixir-lang/emacs-elixir/pull/73) - Fix buggy syntax highlighting behavior involving "?"
* [#71](https://github.com/elixir-lang/emacs-elixir/pull/71) - Need two backslashes in regex string.
* [#70](https://github.com/elixir-lang/emacs-elixir/pull/70) - Use define-derived-mode to define elixir-mode
* [#69](https://github.com/elixir-lang/emacs-elixir/pull/69) - Remove unused variable

## v1.4.1 - 2014/08/11
* [#66](https://github.com/elixir-lang/emacs-elixir/pull/66) - Indent correctly after one-liner if/do: statements. Fixes #65
* [#64](https://github.com/elixir-lang/emacs-elixir/pull/64) - wrong indentation if space between if and statement
* [#63](https://github.com/elixir-lang/emacs-elixir/pull/63) - Correctly indent one-line anon fns AND block fns. Fixes #59

## v1.4.0 - 2014/07/09
* [#62](https://github.com/elixir-lang/emacs-elixir/pull/62) - Remove grammar entry causing erroneous alignment to ".". Fixes #49
* [#61](https://github.com/elixir-lang/emacs-elixir/pull/61) - Remove "=" & left-assoc opers from "OP" regex. Fixes #18.
* [#60](https://github.com/elixir-lang/emacs-elixir/pull/60) - Refactor font face defaults.
* [#58](https://github.com/elixir-lang/emacs-elixir/pull/58) - Use string syntax highlighting for regex patterns.
* [#57](https://github.com/elixir-lang/emacs-elixir/pull/57) - Add beginnings of font-face testing.

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
