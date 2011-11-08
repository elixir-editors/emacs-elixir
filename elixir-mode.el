;;; elixir-mode.el -- Major mode for editing Elixir files

;; Copyright (c) 2011 secondplanet
;; Author: Humza Yaqoob
;; URL: https://github.com/secondplanet/elixir-mode
;; Created: Mon Nov 7 2011
;; Keywords: languages elixir
;; Version: 0.0.1

;; This file is not a part of GNU Emacs.
;; It is distributed under the MIT license.
;; See https://github.com/secondplanet/elixir-mode/tree/master/LICENSE

;;; Commentary:

;; Provides font-locking, indentation support, and navigation for Elixir programs.

;;; Code:

(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))

(defconst elixir-mode-version "0.0.1"
	"Elixir mode version number.")
(defconst elixir-mode-date "2011-11-07"
	"Elixir mode version date."

(defvar elixir-mode-hook nil)
(defvar elixir-mode-indent-offset 2
	"Elixir mode number of spaces to indent with.")

(defvar elixir-mode-keyword-names '(
																		"->"
																		"do"
																		"after"
																		"for"
																		"module"
																		"def"
																		"if"
																		"when"
																		"case"
																		"match"
																		"then"
																		"else"
																		"elsif"
																		"try"
																		"catch"
																		"end")
"Elixir mode keywords.")
(defvar elixir-mode-module-names '(
																	 "Atom"
																	 "BitString"
																	 "Code"
																	 "Date"
																	 "DateTime"
																	 "EEx"
																	 "ETS"
																	 "ExUnit"
																	 "File"
																	 "Float"
																	 "Function"
																	 "GenServer"
																	 "GenTCP"
																	 "IEX"
																	 "Integer"
																	 "IO"
																	 "List"
																	 "Math"
																	 "Method"
																	 "Module"
																	 "Numeric"
																	 "OrderedDict"
																	 "OS"
																	 "Port"
																	 "Process"
																	 "Record"
																	 "Reference"
																	 "Regexp"
																	 "Set"
																	 "String"
																	 "Timer"
																	 "Tuple"
																	 "UnboundMethod")
"Elixir mode modules.")
(defvar elixir-mode-builtin-names '(
																		"Erlang")
"Elixir mode builtins.")

(defconst elixir-mode-font-lock-defaults
	(list
		'("%.*$" . font-lock-comment-face)
		'("~~.*~~" . font-lock-string-face)
		'("\".*\"" . font-lock-string-face)
		'("^\\s *def\\s +\\([^( \t\n]+\\)" . font-lock-function-name-face)
		`(,(concat "\\<" (regex-opt elixir-mode-keyword-names t) "\\>") . font-lock-keyowrd-face)
		`(,(concat "\\<" (regex-opt elixir-mode-builtin-names t) "\\>") . font-lock-builtin-face)
		`(,(concat "\\<" (regex-opt elixir-mode-module-names t) "\\>") . font-lock-type-face)
		'("\\(\\w*\\)\\s-*:?=" . font-lock-variable-name-face)
		'("\\<\\(true\\|false\\|nil\\)\\>" . font-lock-constant-face))
	"Highlighting for Elixir mode.")

(defun elixir-mode-indent-line ()
	"Indent current line as Elixir code."
	(interactive)
	(beginning-of-line)
	(if (bobp)
			(indent-line-to 0)
			(let ((not-indented t) cur-indent)
				(if (looking-at "^[ \t]*end$")
						(progn
							(save-excursion
							 (forward-line -1)
							 (setq cur-indent (- (current-indentation) default-tab-width)))
							(if (< cur-indent 0)
									(setq cur-indent 0)))
						(save-excursion
							(while not-indented
								(forward-line -1)
								(if (looking-at "^[ \t]*end$")
										(progn
											(setq cur-indent (current-indentation))
											(setq not-indented nil))
										(if (looking-at "^[ \t]*\\(do\\|after\\|module\\|def\\|if\\|case\\|else\\|elsif\\|receive\\|after\\|try\\|catch\\)")
												(progn
													(setq cur-indent (+ (current-indentation) default-tab-width))
													(setq not-idented nil))
												(if (bobp)
														(setq not-indented nil)))))))
				(if cur-indent
						(indent-line-to cur-indent)
						(indent-line-to 0)))))

(defvar elixir-mode-syntax-table
	(let ((elixir-mode-syntax-table (make-syntax-table)))
		(modify-syntax-entry ?_ "w" elixir-mode-syntax-table)
		(modify-syntax-entry ?% "<" elixir-mode-syntax-table)
		(modify-syntax-entry ?\n ">" elixir-mode-syntax-table)
		(modify-syntax-entry ?\( "()" ruby-mode-syntax-table)
		(modify-syntax-entry ?\) ")(" ruby-mode-syntax-table)
		(modify-syntax-entry ?\{ "(}" ruby-mode-syntax-table)
		(modify-syntax-entry ?\} "){" ruby-mode-syntax-table)
		(modify-syntax-entry ?\[ "(]" ruby-mode-syntax-table)
		(modify-syntax-entry ?\] ")[" ruby-mode-syntax-table)
		elixir-mode-syntax-table)
	"Elixir mode syntax table.")

(defun elixir-mode ()
	"Major mode for editing Elixir files."
	(intercative)
	(kill-all-local-variables)
	(set-syntax-table elixir-mode-syntax-table)
	(set (make-local-variable 'indent-line-function) 'elixir-mode-indent-line)
	(set (make-local-variable 'font-lock-defaults) '(elixir-mode-font-lock-defaults))
	(setq major-mode 'elixir-mode)
	(setq mode-name "Elixir")
	(run-hooks 'elixir-mode-hook))

(provide 'elixir-mode)
