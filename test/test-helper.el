;;; test-helper.el --- Test helper

;;; Commentary:
;;

;;; Code:

(require 'ert-x)          ; `ert-with-test-buffer'
(require 'cl-lib)         ; `cl-defmacro'

(message "Running tests on Emacs %s" emacs-version)

;; The test fixtures assume an indentation width of 2, so we need to set that
;; up for the tests.
(setq-default default-tab-width 2
              indent-tabs-mode nil)

;; Load the elixir-mode under test
(require 'elixir-mode)

;; Helpers

(cl-defmacro elixir-deftest (name args &body body)
  (declare (indent 2))
  `(ert-deftest ,(intern (format "elixir-ert-%s" name)) ()
     ""
     ,@args
     (let ((elixir-smie-verbose-p t))
       ,@body)))

(cl-defmacro elixir-ert-with-test-buffer ((&rest args) initial-contents &body body)
  (declare (indent 2))
  `(ert-with-test-buffer (,@args)
     (elixir-mode)
     (insert ,initial-contents)
     ,@body))

(defmacro elixir-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (elixir-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(cl-defmacro elixir-def-indentation-test (name args initial-contents expected-output)
  (declare (indent 2))
  `(elixir-deftest ,name ,args
     (elixir-ert-with-test-buffer (:name ,(format "(Expected)" name))
         ,initial-contents
       (let ((indented (ert-buffer-string-reindented)))
         (delete-region (point-min) (point-max))
         (insert ,expected-output)
         (ert-with-test-buffer (:name ,(format "(Actual)" name))
           (elixir-mode)
           (insert indented)
           (should (equal indented ,expected-output)))))))

(when (s-contains? "--win" (getenv "ERT_RUNNER_ARGS"))
  (defun ert-runner/run-tests-batch-and-exit (selector)
    (ert-run-tests-interactively selector)))

(provide 'test-helper)

;;; test-helper.el ends here
