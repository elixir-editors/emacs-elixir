
(require 'ert-x)

(message "Running tests on Emacs %s" emacs-version)

;; The test fixtures assume an indentation width of 2, so we need to set that
;; up for the tests.
(setq-default default-tab-width 2
              indent-tabs-mode nil
              elixir-smie-verbose-p t)

;; Load the elixir-mode under test
(require 'elixir-mode)

;; Helpers

(defmacro* elixir-test-with-test-buffer ((&rest args) content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENTS."
  (declare (debug t)
           (indent 2))
  `(ert-with-test-buffer (,@args)
     (insert ,content)
     (elixir-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(defun elixir-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (elixir-test-with-test-buffer () content
        (get-text-property pos 'face))
    (get-text-property pos 'face)))

(defmacro* elixir-deftest (name &body body)
  (declare (indent 2)
           (&define :name test name sexp
                    [&optional [":documentation" stringp]]
                    [&optional [":expected-result" sexp]]
                    def-body))
  `(ert-deftest ,(intern (format "elixir-ert-%s" name)) ()
     ,@body))
