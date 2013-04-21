;;; elixir-mode.el --- Major mode for editing Elixir files

;; Copyright (c) 2011 secondplanet
;; Author: Humza Yaqoob, Andreas Fuchs <asf@boinkor.net>
;; URL: https://github.com/elixir-lang/emacs-elixir
;; Created: Mon Nov 7 2011
;; Keywords: languages elixir
;; Version: 1.1.0

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

;; Provides font-locking, indentation support, and navigation for Elixir programs.
;;
;; To install, add the put this directory on the `load-path', and load
;; `elixir-mode-setup.el'. Then, run:
;;
;;   (elixir-mode-setup)
;;
;; To auto-load the mode and set up file associations.

;;; Code:

(require 'comint)       ; for interactive REPL
(require 'easymenu)     ; for menubar features

(require 'elixir-smie)        ; syntax and indentation support
(require 'elixir-mode-setup)  ; Contains only the elixir-mode-setup function.

;;;###autoload
(defvar elixir-mode-hook nil)

;;;###autoload
(defvar elixir-mode-map (make-keymap)
  "Elixir mode keymap.")

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
   '("\\(\\w*\\)\\s-*:?=" . font-lock-variable-name-face)

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

(defun elixir-mode-iex ()
  "Elixir mode interactive REPL."
  (interactive)
  (unless (comint-check-proc "*IEX*")
    (set-buffer
     (apply 'make-comint "IEX"
            elixir-iex-command nil '())))
  (pop-to-buffer "*IEX*"))

(defun elixir-mode-open-modegithub ()
  "Elixir mode open GitHub page."
  (interactive)
  (browse-url "https://github.com/elixir-lang/emacs-elixir"))

(defun elixir-mode-open-elixir-home ()
  "Elixir mode go to language home."
  (interactive)
  (browse-url "http://elixir-lang.org"))

(defun elixir-mode-show-version ()
  "Elixir mode print version."
  (interactive)
  (message (concat "elixir-mode v" elixir-mode-version " " elixir-mode-date " by Humza Yaqoob")))

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
(defun run-elixir-tests ()
  "Run ERT tests for `elixir-mode'."
  (interactive)
  (load "elixir-tests")
  (ert-run-tests-interactively "^elixir-ert-.*$"))

(provide 'elixir-mode)
;;;***

;; Local variables:
;; generated-autoload-file: elixir-mode-setup.el
;;; elixir-mode.el ends here
