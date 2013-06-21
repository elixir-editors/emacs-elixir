(require 'ert)
(require 'ert-x)

(defmacro* elixir-deftest (name args &body body)
  (declare (indent 2)
           (&define :name test name sexp
                    [&optional [":documentation" stringp]]
                    [&optional [":expected-result" sexp]]
                    def-body))
  `(ert-deftest ,(intern (format "elixir-ert-%s" name)) ()
     ""
     ,@args
     (let ((elixir-smie-verbose-p t))
       ,@body)))

(defmacro* elixir-ert-with-test-buffer ((&rest args) initial-contents &body body)
  (declare (indent 2))
  `(ert-with-test-buffer (,@args)
     (elixir-mode)
     (insert ,initial-contents)
     ,@body))

(load "test/elixir-mode-indentation-tests.el")
(load "test/elixir-mode-tokenizer-hl-tests.el")

(provide 'elixir-mode-tests)
;;; elixir-mode-tests.el ends here
