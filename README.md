# Elixir Mode

Provides font-locking, indentation and navigation support for the
[Elixir programming language.](http://elixir-lang.org/)

## Installation

### ELPA

elixir-mode is available on the community maintained repository -
[MELPA](http://melpa.milkbox.net/). Just run `M-x package-install
[RET] elixir-mode [RET]` inside your emacs and you're ready to go.

If you're not already using ELPA, check the [emacswiki](http://www.emacswiki.org/emacs/ELPA) page to get
familiar with it.

### Install the most recent version

Clone this repository somewhere

```shell
$ cd ~/.emacs.d/plugins
$ git clone https://github.com/elixir-lang/emacs-elixir
```

Add the following in your .emacs file:

```lisp
(add-to-list 'load-path "~/path/to/emacs-elixir/")
(require 'elixir-mode)
```

## Usage

### Interactive Commands

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
        <td><code>elixir-mode-compile-file</code></td>
        <td>Compile Elixir files. Works fine on <code>*.exs</code> files, too, if needed.</td>
     <tr>
        <td><code>elixir-cos-mode</code></td>
        <td>Applies compile-on-save minor mode.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-iex</code></td>
        <td>
            Launch <code>iex</code> inside Emacs. Use <code>C-u</code>
            <a href="http://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html">univesal-argument</a>
            to run <code>iex</code> with some additional arguments.
        </td>
    </tr>
    <tr>
        <td><code>elixir-mode-eval-on-region</code></td>
        <td>Evaluates the Elixir code on the marked region.
            This is bound to <code>C-c ,r</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-eval-on-current-line</code></td>
        <td>Evaluates the Elixir code on the current line.
            This is bound to <code>C-c ,c</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-eval-on-current-buffer</code></td>
        <td>Evaluates the Elixir code in the current buffer.
            This is bound to <code>C-c ,b</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-string-to-quoted-on-region</code></td>
        <td>Get the representation of the expression on the marked region.
            This is bound to <code>C-c ,a</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-string-to-quoted-on-current-line</code></td>
        <td>Get the representation of the expression on the current line.
            This is bound to <code>C-c ,l</code> while in <code>elixir-mode</code>.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-opengithub</code></td>
        <td>Open the GitHub page for Elixir.</td>
    </tr>
    </tr>
    <tr>
        <td><code>elixir-mode-open-elixir-home</code></td>
        <td>Go to Elixir README in the browser.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-open-docs-master</code></td>
        <td>Open the Elixir documentation for the master.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-open-docs-stable</code></td>
        <td>Open the Elixir documentation for the latest stable release.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-run-tests</code></td>
        <td>Run ERT tests for `elixir-mode`.</td>
    </tr>
    <tr>
        <td><code>elixir-mode-show-version</code></td>
        <td>Print version info for elixir-mode.</td>
    </tr>
</table>

### Configuration

Any file that matches the glob `*.ex[s]` or `*.elixir` is
automatically opened in elixir-mode, but you can change this
functionality easily.

```lisp
;; Highlights *.elixir2 as well
(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
```

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

### Keymapping

Keymaps can be added to the `elixir-mode-map` variable.

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

## Mix

If you looking for integrated emacs [mix](http://elixir-lang.org/getting_started/mix/1.html) support, see here: [elixir-mix.el](https://github.com/tonini/elixir-mix.el)

## History

This mode is based on the
[Emacs mode by secondplanet](https://github.com/secondplanet/elixir-mode).

## Contributions are very welcome!

1. Fork emacs-elixir
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send us a pull-request for your topic branch
6. That's it!
