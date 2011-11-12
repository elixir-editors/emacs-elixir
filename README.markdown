# elixir-mode -- Emacs major mode for [Elixir](https://github.com/josevalim/elixir) ![still maintained](http://stillmaintained.com/secondplanet/elixir-mode.png)

![elixir-mode]("https://img.skitch.com/20111112-tyhx1d5wqus29rx644f46ciu5c.png")

## Installation

Simply add to your load path and require the mode to install.

In your shell...

```shell
% git clone git://github.com/secondplanet/elixir-mode ~/.emacs.d/elixir-mode
% $EDITOR ~/.emacs
```
In the editor...

```lisp
(add-to-list 'load-path "~/.emacs.d/elixir-mode")
(require 'elixir-mode)
```

Save and reload with `M-x load-file` `~/.emacs`.

## Usage

Any file that matches the glob `*.ex[s]` is automatically opened in
Elixir mode, but you can change this functionality easily.

```lisp
;; Highlights *.elixir as well
(add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
```

### Commands

(For the `M-x` prompt.)

#### elixir-mode

Switches to elixir-mode.

#### elixir-cos-mode

Applies compile-on-save minor mode.

#### elixir-mode-iex

Launch `IEX` inside Emacs.

#### elixir-mode-opengithub

Open the GitHub page for Elixir.

#### elixir-mode-compile-file

Compile Elixir files. Works fine on `exs` files, too, if needed.

#### elixir-mode-open-elixir-home

Go to Elixir README in the browser.

#### elixir-mode-show-version

Print version info for elixir-mode.

#### elixir-mode-indent-line

Indent the current line. (Buggy right now.)

### Hooks

Hooks can be used to add functionality to elixir-mode. This example
adds compile on save.

```lisp
(defun elixir-mode-compile-on-save ()
  "Elixir mode compile files on save."
	(and (file-exists (buffer-file-name))
	     (file-exists (elixir-mode-compiled-file-name))
			 (elixir-cos-mode t)))
(add-hook 'elixir-mode-hook 'elixir-mode-compile-on-save)
```

### Configuration

Custom variables for elixir-mode.

#### elixir-compiler-command (string)
##### Default: `"elixirc"`

Command to compile Elixir code.

#### elixir-iex-command (string)
##### Default: `"iex"`

Command to start an interactive REPL in `IEX`.

#### elixir-mode-highlight-operators (boolean)
##### Default: `t`

Should operators be colored? (Currently not working properly.)

#### elixir-mode-cygwin-paths (boolean)
##### Default: `t`

Should Cygwin paths be used on Windows?

#### elixir-mode-cygwin-prefix (string)
##### Default: `"/cygdrive/C"

The prefix for Cygwin-style paths.

### Keymapping

Keymaps can be added to the `elixir-mode-map` variable.
There are no keyboard shortcuts included by default.

## Bugs

This is still very alpha software; there are probably several
bugs. Right now the indentation implementation needs some work, and
the operator and atom font-locking doesn't appear to be working
correctly.

## Notes

This is my first Emacs mode, so please excuse some of the messy bits
in the implementation. To help me out I had a look at and borrowed a
bit of code from io-mode, ruby-mode, and coffee-mode.