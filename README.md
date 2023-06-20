[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://github.com/elixir-editors/emacs-elixir/actions/workflows/ci.yml/badge.svg)](https://github.com/elixir-editors/emacs-elixir/actions)
[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/elixir-mode.svg)](https://elpa.nongnu.org/nongnu/elixir-mode.html)
[![MELPA Stable](http://stable.melpa.org/packages/elixir-mode-badge.svg)](http://stable.melpa.org/#/elixir-mode)
[![MELPA](http://melpa.org/packages/elixir-mode-badge.svg)](http://melpa.org/#/elixir-mode)

> **WARNING**
> There is a built-in Elixir mode with tree-sitter support from Emacs 30+ that can also be used with Emacs 29
> This repository is for an older elixir-mode built with SMIE (Simple Minded Indentation Engine) which is not
> as advanced as tree-sitter for a language like Elixir.

# Elixir Mode

Provides font-locking, indentation and navigation support for the
[Elixir programming language.](http://elixir-lang.org/)

## Installation

`elixir-mode` is available on [NON-GNU ELPA](https://elpa.nongnu.org/), 
[MELPA STABLE](https://stable.melpa.org/) and [MELPA](https://melpa.org/).

### Via package.el

`package.el` is the built-in package manager in Emacs.

You can install `elixir-mode` with the following command:

<kbd>M-x package-install [RET] elixir-mode [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is the recommended way of
obtaining `Elixir-Mode`.

MELPA Stable contains packages released from our tags.

### Via use-package

Since Emacs 29, `use-package` is a built-in feature. For versions prior to 29 
one can also install it from MELPA (and MELPA Stable).

To install elixir-mode using `use-package` one can:

``` elisp
(use-package elixir-mode
  :ensure t)
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
        <td><code>elixir-mode-open-github</code></td>
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

### Keymapping

Keymaps can be added to the `elixir-mode-map` variable.

### Pairing

[Smartparens](https://github.com/Fuco1/smartparens) has direct support for Elixir.

Alternatively, if you want to use `ruby-end-mode`, you can add the following to your `elixir-mode-hook`:

```lisp
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))
```

## Notes

This package is tested only with a single version of OTP and 3 versions of Elixir. Please, always report versions
 (Emacs, Elixir and Erlang/OTP) when raising issues.

## Elixir Tooling Integration

You can use [web-mode.el](http://web-mode.org) to edit elixir templates (eex files).

[mix.el](https://github.com/ayrat555/mix.el) provides a minor mode for integration with Mix, a build tool that ships with Elixir.

[exunit.el](https://github.com/ananthakumaran/exunit.el) provides `ExUnit` integration.

## Elixir Format

This mode can call mix for formatting code. When inside an elixir buffer, just type `M-x elixir-format`.

To automate that, you can add this command to the `before-save` hook.

``` elisp
;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
```

To use a `.formatter.exs` you can either set `elixir-format-arguments` globally to a path like this:

``` elisp
(setq elixir-format-arguments (list "--dot-formatter" "/path/to/.formatter.exs"))
```

or you set `elixir-format-arguments` in a hook like this:

``` elisp
(add-hook 'elixir-format-hook (lambda ()
                                 (if (projectile-project-p)
                                      (setq elixir-format-arguments
                                            (list "--dot-formatter"
                                                  (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                   (setq elixir-format-arguments nil))))
```

In this example we use [Projectile](https://github.com/bbatsov/projectile) to determine if we are in a project and then set `elixir-format-arguments` accordingly.

Please note that this code snippet may cause unhappiness if there is no `.formatter.exs` file available.

## Tips & Tricks

### Prettify symbols

Emacs supports [font ligatures](https://en.wikipedia.org/wiki/Ligature_(writing)). For enabling it for Elixir 
you can add it to your configuration:

``` elisp
(add-hook
 'elixir-mode-hook
 (lambda ()
   (push '(">=" . ?\u2265) prettify-symbols-alist)
   (push '("<=" . ?\u2264) prettify-symbols-alist)
   (push '("!=" . ?\u2260) prettify-symbols-alist)
   (push '("==" . ?\u2A75) prettify-symbols-alist)
   (push '("=~" . ?\u2245) prettify-symbols-alist)
   (push '("<-" . ?\u2190) prettify-symbols-alist)
   (push '("->" . ?\u2192) prettify-symbols-alist)
   (push '("<-" . ?\u2190) prettify-symbols-alist)
   (push '("|>" . ?\u25B7) prettify-symbols-alist)))
   
;; Or if you use use-packge

(use-package elixir-mode
 :hook (elixir-mode . (lambda ()
    (push '(">=" . ?\u2265) prettify-symbols-alist)
    (push '("<=" . ?\u2264) prettify-symbols-alist)
    (push '("!=" . ?\u2260) prettify-symbols-alist)
    (push '("==" . ?\u2A75) prettify-symbols-alist)
    (push '("=~" . ?\u2245) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("->" . ?\u2192) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("|>" . ?\u25B7) prettify-symbols-alist))))
```

### Formatting

If you have issues with the formatter provided by this package, you can try
using the format function of language servers. Elixir-ls supports this.

One way to configure this with `eglot` and `use-packge` would be:

``` elisp
(use-package elixir-mode
 :hook (elixir-mode . eglot-ensure)
 (before-save . eglot-format))
```

### Syntax highlighting for LiveView (and similar techniques for other syntaxes)

When you need different major modes in the SAME buffer, Emacs do not provide a standard way. 
There is an external package that can help here by adding more than one major mode called `poly-mode`.

A possible configuration would be:

``` elisp
;; Assumes web-mode and elixir-mode are already set up
;;
(use-package polymode
  :mode ("\.ex$" . poly-elixir-web-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~L" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode))
  )
(setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))
```

## History

This mode is based on the [Emacs mode by secondplanet](https://github.com/secondplanet/elixir-mode).

## Contributing

Please read [CONTRIBUTING.md](https://github.com/elixir-editors/emacs-elixir/blob/master/CONTRIBUTING.md) for guidelines on how to contribute to this project.

## License

Copyright © 2011-2017 Samuel Tonini, Matt DeBoard, Andreas Fuchs, secondplanet and [contributors](https://github.com/elixir-editors/emacs-elixir/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
