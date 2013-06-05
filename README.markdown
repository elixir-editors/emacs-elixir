# elixir-mode -- Emacs major mode for [Elixir](http://elixir-lang.org/)

![elixir-mode](http://cl.ly/image/0I3h1h1v2740/Screen%20Shot%202013-04-06%20at%205.40.05%20PM.png)

## Installation

Simply add to your load path and require the mode to install.

In your shell...

```shell
% git clone git://github.com/elixir-lang/emacs-elixir ~/.emacs.d/emacs-elixir
% $EDITOR ~/.emacs
```
In the editor...

```lisp
(add-to-list 'load-path "~/.emacs.d/emacs-elixir")
(require 'elixir-mode-setup)
(elixir-mode-setup)
```

Save and reload with `M-x load-file` `~/.emacs`.

## Usage

Any file that matches the glob `*.ex[s]` or `*.elixir` is
automatically opened in Elixir mode, but you can change this
functionality easily.

```lisp
;; Highlights *.elixir2 as well
(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
```

###  Interactive Commands

<table>
    <tr>
        <th>Command (For the <code>M-x</code> prompt.)</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>elixir-mode</code></td>
        <td>Switches to elixir-mode.</td>
    </tr>
     <tr>
        <td><code>elixir-cos-mode</code></td>
        <td>Applies compile-on-save minor mode.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-iex</code></td>
        <td>Launch <code>IEX</code> inside Emacs.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-opengithub</code></td>
        <td>Open the GitHub page for Elixir.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-compile-file</code></td>
        <td>Compile Elixir files. Works fine on <code>*.exs</code> files, too, if needed.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-open-elixir-home</code></td>
        <td>Go to Elixir README in the browser.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-show-version</code></td>
        <td>Print version info for elixir-mode.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-indent-line</code></td>
        <td>Indent the current line. (Buggy right now.)</td>
    </tr>
</table>

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

<table>
    <tr>
        <th>Variable</th>
        <th>Default</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>elixir-compiler-command (string)</code></td>
        <td><code>"elixirc"</code></td>
        <td>Command to compile Elixir code.</td>
    </tr>
    <tr>
        <td><code>elixir-iex-command (string)</code></td>
        <td><code>"iex"</code></td>
        <td>Command to start an interactive REPL in <code>IEX</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-highlight-operators (boolean)</code></td>
        <td><code>t</code></td>
        <td>Should operators be colored? (Currently not working properly.)</td>
    </tr>
    <tr>
        <td><code>elixir-mode-cygwin-paths (boolean)</code></td>
        <td><code>t</code></td>
        <td>Should Cygwin paths be used on Windows?</td>
    </tr>
    <tr>
        <td><code>elixir-mode-cygwin-prefix (string)</code></td>
        <td><code>"/cygdrive/C"</code></td>
        <td>The prefix for Cygwin-style paths.</td>
    </tr>
</table>

### Keymapping

Keymaps can be added to the `elixir-mode-map` variable.
There are no keyboard shortcuts included by default.

## Bugs

This is still very alpha software; there are probably several
bugs. Right now the indentation implementation needs some work, and
the IEX mode appears entirely untested.

## Notes

If you want to use `ruby-end-mode` for a more comfortable editing
experience, you can add the following to your `elixir-mode-hook`:

```lisp
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))
```

## History

This mode is based on the
[Emacs mode by secondplanet](https://github.com/secondplanet/elixir-mode).
