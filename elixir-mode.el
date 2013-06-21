;;; elixir-mode.el --- Major mode for editing Elixir files

;; Copyright 2011 secondplanet
;;           2013 Andreas Fuchs, Samuel Tonini
;; Authors: Humza Yaqoob,
;;          Andreas Fuchs <asf@boinkor.net>,
;;          Samuel Tonini <tonini.samuel@gmail.com>
;; URL: https://github.com/elixir-lang/emacs-elixir
;; Created: Mon Nov 7 2011
;; Keywords: languages elixir
;; Version: 1.2.0

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides font-locking, indentation and navigation support
;; for the Elixir programming language.
;;
;;
;;  Manual Installation:
;;
;;   (add-to-list 'load-path "~/path/to/emacs-elixir/")
;;   (require 'elixir-mode)
;;
;;  Interesting variables are:
;;
;;      `elixir-compiler-command`
;;
;;          Path to the executable <elixirc> command
;;
;;      `elixir-iex-command`
;;
;;          Path to the executable <iex> command
;;
;;      `elixir-mode-highlight-operators`
;;
;;          Option for whether or not to highlight operators.
;;
;;      `elixir-mode-cygwin-paths`
;;
;;          Use Cygwin style paths on Windows operating systems.
;;
;;      `elixir-mode-cygwin-prefix`
;;
;;          Cygwin prefix
;;
;;  Major commands are:
;;
;;       M-x elixir-mode
;;
;;           Switches to elixir-mode.
;;
;;       M-x elixir-cos-mode
;;
;;           Applies compile-on-save minor mode.
;;
;;       M-x elixir-mode-compile-file
;;
;;           Compile and save current file.
;;
;;       M-x elixir-mode-iex
;;
;;           Launch `IEX` inside Emacs.
;;
;;       M-x elixir-mode-opengithub
;;
;;           Open the GitHub page of the Elixir repository.
;;
;;       M-x elixir-mode-open-elixir-home
;;
;;           Open the Elixir website.
;;
;;       M-x elixir-mode-open-docs-master
;;
;;           Open the Elixir documentation for the master.
;;
;;       M-x elixir-mode-open-docs-stable
;;
;;           Open the Elixir documentation for the latest stable release.
;;
;;       M-x elixir-mode-run-tests
;;
;;           Run ERT tests for `elixir-mode`.
;;
;;       M-x elixir-mode-show-version
;;
;;           Print `elixir-mode` version.
;;

;;; Code:

(require 'comint)       ; for interactive REPL
(require 'easymenu)     ; for menubar features

(require 'elixir-smie)  ; syntax and indentation support

(defvar elixir-mode--version "1.2.0")

(defvar elixir-mode--website-url
  "http://elixir-lang.org")

(defvar elixir-mode-hook nil)

(defvar elixir-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-d") 'smie-down-list)
    map)
  "Keymap used in elixir-mode.")

(defgroup elixir nil
  "Elixir major mode."
  :group 'languages)

(defcustom elixir-compiler-command "elixirc"
  "Elixir mode command to compile code. Must be in your path."
  :type 'string
  :group 'elixir)

(defcustom elixir-iex-command "iex"
  "Elixir mode command for interactive REPL. Must be in your path."
  :type 'string
  :group 'elixir)

(defcustom elixir-mode-highlight-operators t
  "Elixir mode option for whether or not to highlight operators."
  :type 'boolean
  :group 'elixir)

(defcustom elixir-mode-cygwin-paths t
  "Elixir mode use Cygwin style paths on Windows operating systems."
  :type 'boolean
  :group 'elixir)

(defcustom elixir-mode-cygwin-prefix "/cygdrive/C"
  "Elixir mode Cygwin prefix."
  :type 'string
  :group 'elixir)

(defvar elixir-mode-define-names
  '("def"
    "defdelegate"
    "defmacro"
    "defmacrop"
    "defoverridable"
    "defp"
    "defmacrop")
  "Elixir mode def-like keywords.")
(defvar elixir-mode-keyword-names
  '("->"
    "bc"
    "lc"
    "in"
    "inbits"
    "inlist"
    "quote"
    "unquote"
    "unquote_splicing"
    "var"
    "do"
    "after"
    "for"
    "def"
    "defdelegate"
    "defimpl"
    "defmacro"
    "defmacrop"
    "defmodule"
    "defoverridable"
    "defp"
    "defprotocol"
    "defrecord"
    "destructure"
    "alias"
    "require"
    "import"
    "use"
    "if"
    "true"
    "false"
    "when"
    "case"
    "cond"
    "throw"
    "then"
    "else"
    "elsif"
    "try"
    "catch"
    "rescue"
    "fn"
    "function"
    "receive"
    "end")
  "Elixir mode keywords.")
(defvar elixir-mode-module-names
  '("Behavior"
    "Binary"
    "Bitwise"
    "Builtin"
    "Elixir"
    "Code"
    "EEx"
    "Enum"
    "ExUnit"
    "Exception"
    "File"
    "GenServer"
    "Function"
    "GenServer"
    "GenTCP"
    "HashDict"
    "IO"
    "Keyword"
    "List"
    "Math"
    "Module"
    "Node"
    "OptionParser"
    "OrdDict"
    "Port"
    "Process"
    "Record"
    "Regexp"
    "System"
    "Tuple"
    "URI"
    "UnboundMethod")
  "Elixir mode modules.")
(defvar elixir-mode-builtin-names
  '("Erlang"
    "__MODULE__"
    "__LINE__"
    "__FILE__"
    "__ENV__")
  "Elixir mode builtins.")
(defvar elixir-mode-operator-names
  '("+"
    "++"
    "<>"
    "-"
    "/"
    "*"
    "div"
    "rem"
    "=="
    "!="
    "<="
    "<"
    ">="
    ">"
    "==="
    "!=="
    "and"
    "or"
    "not"
    "&&"
    "||"
    "!"
    "."
    "#"
    "="
    ":="
    "<-")
  "Elixir mode operators.")

(defvar elixir-mode-sigils '("B" "C" "R" "b" "c" "r")
  "%-prefixed sigils that are understood by elixir-mode")

(defvar elixir-basic-offset 2)
(defvar elixir-key-label-offset 0)
(defvar elixir-match-label-offset 2)

(defvar font-lock-operator-face 'font-lock-operator-face)
(defface font-lock-operator-face
  '((((type tty) (class color)) nil)
    (((class color) (background light))
     (:foreground "darkred"))
    (t nil))
  "For use with operators."
  :group 'font-lock-faces)

(defconst elixir-mode-font-lock-defaults
  (list
   ;; comments:
   '("#.*$" . font-lock-comment-face)

   ;; records and modules at point of definition:
   '("^\\s *def\\(module\\|record\\|protocol\\|impl\\)\\s +\\([^( \t\n,]+\\)" 2 font-lock-type-face)

   ;; methods:
   `(,(concat "^\\s *\\<" (regexp-opt elixir-mode-define-names t) "\\>\\s +\\([^( \t\n]+\\)") 2 font-lock-function-name-face)

   ;; keywords:
   `(,(concat "\\<" (regexp-opt elixir-mode-keyword-names t) "\\>") . font-lock-keyword-face)

   ;; % Sigils
   `(,(concat "\\<%" (regexp-opt elixir-mode-sigils t) "\\>") . font-lock-builtin-face)

   ;; builtins:
   `(,(concat "\\<" (regexp-opt elixir-mode-builtin-names t) "\\>") . font-lock-builtin-face)

   ;; core modules:
   `(,(concat "\\<" (regexp-opt elixir-mode-module-names t) "\\>") . font-lock-type-face)

   ;; operators:
   (when elixir-mode-highlight-operators
     `(,(concat "\\<" (regexp-opt elixir-mode-operator-names t) "\\>") . font-lock-operator-face))

   ;; variables:
   '("\\(\\w+\\)\\s-*:?=[^=]" 1 font-lock-variable-name-face)

   ;; regexes:
   '("-[Rr].*[ \n\t]" . font-lock-constant-face)

   ;; atoms, boolean:
   '("\\<\\(true\\|false\\|nil\\)\\>" . font-lock-reference-face)

   ;; atoms, generic
   '("[@:]\\w*\\|\\w*:\\s-" . font-lock-reference-face))
  "Highlighting for Elixir mode.")

(defun elixir-mode-cygwin-path (expanded-file-name)
  "Elixir mode get Cygwin absolute path name."
  (replace-regexp-in-string "^[a-zA-Z]:" elixir-mode-cygwin-prefix expanded-file-name t))

(defun elixir-mode-universal-path (file-name)
  "Elixir mode multi-OS path handler."
  (let ((full-file-name (expand-file-name file-name)))
    (if (and (equal system-type 'windows-nt)
             elixir-mode-cygwin-paths)
        (elixir-mode-cygwin-path full-file-name)
      full-file-name)))

(defun elixir-mode-command-compile (file-name)
  "Elixir mode command to compile a file."
  (let ((full-file-name (elixir-mode-universal-path file-name)))
    (mapconcat 'identity (append (list elixir-compiler-command) (list full-file-name)) " ")))

(defun elixir-mode-compiled-file-name (&optional filename)
  "Elixir mode compiled filename."
  (concat (file-name-sans-extension (or filename (buffer-file-name))) ".beam"))

(defun elixir-mode-compile-file ()
  "Elixir mode compile and save current file."
  (interactive)
  (let ((compiler-output (shell-command-to-string (elixir-mode-command-compile (buffer-file-name)))))
    (when (string= compiler-output "")
      (message "Compiled and saved as %s" (elixir-mode-compiled-file-name)))))

;;;###autoload
(defun elixir-mode-iex ()
  "Elixir mode interactive REPL."
  (interactive)
  (unless (comint-check-proc "*IEX*")
    (set-buffer
     (apply 'make-comint "IEX"
            elixir-iex-command nil '())))
  (pop-to-buffer "*IEX*"))

;;;###autoload
(defun elixir-mode-open-modegithub ()
  "Elixir mode open GitHub page."
  (interactive)
  (browse-url "https://github.com/elixir-lang/emacs-elixir"))

;;;###autoload
(defun elixir-mode-open-elixir-home ()
  "Elixir mode go to language home."
  (interactive)
  (browse-url elixir-mode--website-url))

;;;###autoload
(defun elixir-mode-open-docs-master ()
  "Elixir mode go to master documentation."
  (interactive)
  (browse-url (concat elixir-mode--website-url "/docs/master")))

;;;###autoload
(defun elixir-mode-open-docs-stable ()
  "Elixir mode go to stable documentation."
  (interactive)
  (browse-url (concat elixir-mode--website-url "/docs/stable")))

;;;###autoload
(defun elixir-mode-show-version ()
  "Elixir mode print version."
  (interactive)
  (message (format "elixir-mode v%s" elixir-mode--version)))

(easy-menu-define elixir-mode-menu elixir-mode-map
  "Elixir mode menu."
  '("Elixir"
    ["Indent line" smie-indent-line]
    ["Compile file" elixir-mode-compile-file]
    ["IEX" elixir-mode-iex]
    "---"
    ["elixir-mode on GitHub" elixir-mode-open-modegithub]
    ["Elixir homepage" elixir-mode-open-elixirhome]
    ["About" elixir-mode-show-version]
    ))

;;;###autoload
(defun elixir-mode ()
  "Major mode for editing Elixir files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map elixir-mode-map)
  (set-syntax-table elixir-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(elixir-mode-font-lock-defaults))
  (setq major-mode 'elixir-mode)
  (setq mode-name "Elixir")
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-variable-buffer-local 'tab-width) elixir-basic-offset)
  (set (make-variable-buffer-local 'default-tab-width) elixir-basic-offset)
  (smie-setup elixir-smie-grammar 'verbose-elixir-smie-rules ; 'elixir-smie-rules
              :forward-token 'elixir-smie-forward-token
              :backward-token 'elixir-smie-backward-token)
  (run-hooks 'elixir-mode-hook)
  (run-hooks 'prog-mode-hook))

(define-minor-mode elixir-cos-mode
  "Elixir mode toggle compile on save."
  :group 'elixir-cos :lighter " CoS"
  (cond
   (elixir-cos-mode
    (add-hook 'after-save-hook 'elixir-mode-compile-file nil t))
   (t
    (remove-hook 'after-save-hook 'elixir-mode-compile-file t))))

;;;###autoload
(defun elixir-mode-run-tests ()
  "Run ERT tests for `elixir-mode'."
  (interactive)
  (load "elixir-mode-tests")
  (ert-run-tests-interactively "^elixir-ert-.*$"))

;; Invoke elixir-mode when appropriate

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir$" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex$" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs$" . elixir-mode)))

(provide 'elixir-mode)
;;; elixir-mode.el ends here
