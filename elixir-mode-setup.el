;;; elixir-mode-setup.el --- Load and initialize major mode for Elixir files

(defun elixir-mode-setup ()
  (let ((mode-version "1.1.0")
        (mode-date    "2013-04-06"))
    (if (boundp 'elixir-mode-version)
        (display-warning :warning
                         (format "elixir-mode version %s is already present! This is version %s (%s)"
                                 elixir-mode-version mode-version mode-date))
      (progn
        (defconst elixir-mode-version mode-version
          "Elixir mode version number.")
        (defconst elixir-mode-date mode-date
          "Elixir mode version date (bumped infrequently)."))))

  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode)))


;;;### (autoloads (run-elixir-tests elixir-mode) "elixir-mode" "elixir-mode.el"
;;;;;;  (20834 17836))
;;; Generated autoloads from elixir-mode.el

(defvar elixir-mode-hook nil)

(defvar elixir-mode-map (make-keymap) "\
Elixir mode keymap.")

(autoload 'elixir-mode "elixir-mode" "\
Major mode for editing Elixir files.

\(fn)" t nil)

(autoload 'run-elixir-tests "elixir-mode" "\
Run ERT tests for `elixir-mode'.

\(fn)" t nil)

;;;***

(provide 'elixir-mode-setup)
;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8
;; End:
;;; elixir-mode-setup.el ends here
