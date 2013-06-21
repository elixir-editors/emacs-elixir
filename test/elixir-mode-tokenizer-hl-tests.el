;;; High-level Tokenizer tests:

(elixir-deftest skip-forwards-over-comments ()
  (elixir-ert-with-test-buffer ()
      "foo # comment\nbar"
    (goto-char (point-min))
    (should (equal "foo" (elixir-smie-next-token t)))
    (should (equal ";" (elixir-smie-next-token t)))
    (should (equal "bar" (elixir-smie-next-token t)))
    (should (equal (point-max) (point)))))

(elixir-deftest skip-backwards-over-comments ()
  (elixir-ert-with-test-buffer ()
      "foo # comment\nbar"
    (goto-char (point-max))
    (should (equal "bar" (elixir-smie-next-token nil)))
    (should (equal ";" (elixir-smie-next-token nil)))
    (should (equal "foo" (elixir-smie-next-token nil)))
    (should (equal (point-min) (point)))))

(elixir-deftest treat-comments-the-same-as-whitespace-backwards ()
  (let ((skipped-to-posn))
    (elixir-ert-with-test-buffer (:name "With comment")
        "function do # test\n  bar\nend"
      (goto-char (point-min))
      (forward-line 1)
      (should (equal "do" (elixir-smie-next-token nil)))
      (setq skipped-to-posn (point)))
    (elixir-ert-with-test-buffer (:name "Without comment")
        "function do     \n  bar\nend"
      (goto-char (point-min))
      (forward-line 1)
      (should (equal "do" (elixir-smie-next-token nil)))
      (should (equal skipped-to-posn (point))))))
