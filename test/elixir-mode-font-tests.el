;; `elixir-test-with-temp-buffer' and `elixir-test-face-at' are both slightly
;; modified versions of the original at
;; https://github.com/lunaryorn/puppet-mode/blob/master/test/puppet-mode-test.el
(defmacro elixir-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENTS."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
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
      (elixir-test-with-temp-buffer content
        (get-text-property pos 'face))
    (get-text-property pos 'face)))

(ert-deftest elixir-mode-syntax-table/fontify-regex ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "match = ~r/foo/"
   (should (eq (elixir-test-face-at 1) 'font-lock-variable-name-face))
   (should (eq (elixir-test-face-at 9) 'font-lock-builtin-face))
   (should (eq (elixir-test-face-at 12) 'font-lock-string-face))
   ;; no face for regex delimiters
   (should (eq (elixir-test-face-at 15) nil))))

(ert-deftest elixir-mode-syntax-table/fontify-modules-and-types ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
   "defmodule Application.Behavior do
  use Application.Behaviour"
   (should (eq (elixir-test-face-at 1) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 11) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 22) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 23) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 32) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 37) 'font-lock-keyword-face))
   (should (eq (elixir-test-face-at 41) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 52) 'font-lock-type-face))
   (should (eq (elixir-test-face-at 53) 'font-lock-type-face))))

;; This SHOULD fail, but it does not. `?<' is fontified like a builtin, but
;; the characters afterward are not.
(ert-deftest elixir-mode-syntax-table/fontify-regex-with-question-mark ()
  :tags '(fontification syntax-table)
  (elixir-test-with-temp-buffer
      "ptn = ~r/^user-mnl6yssn: (?<ct>\d+)$/mg"
    (should (eq (elixir-test-face-at 10) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 28) 'font-lock-string-face))
    (should (eq (elixir-test-face-at 31) 'font-lock-string-face))))
