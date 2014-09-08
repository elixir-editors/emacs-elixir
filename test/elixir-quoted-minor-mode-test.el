
(ert-deftest elixir-quoted-minor-mode/base ()
  (elixir-test-with-temp-buffer
   "sum(1, 2)"
   (elixir-mode-string-to-quoted-on-current-line)
   (should (string= (buffer-name) elixir-quoted--buffer-name))
   (should (search-forward "{:sum, [line: 1], [1, 2]}"  nil t))
   (should (eq 'quit-window (key-binding "q")))
   (call-interactively (key-binding "q"))
   (should-not (string= (buffer-name) elixir-quoted--buffer-name))))

(ert-deftest elixir-quoted-minor-mode/read-only ()
  (elixir-test-with-temp-buffer
   "sum(1,2)"
   (elixir-mode-string-to-quoted-on-current-line)
   (should buffer-read-only)
   (should-error (call-interactively (key-binding "w")))))

(ert-deftest elixir-quoted-minor-mode/multiple-invocation ()
  (let ((test-buffer (current-buffer)))
    (elixir-test-with-temp-buffer
     "sum(1,2)"
     (elixir-mode-string-to-quoted-on-current-line)
     (should (search-forward "{:sum, [line: 1], [1, 2]}"  nil t))
     (pop-to-buffer test-buffer)
     (erase-buffer)
     (insert "sum(3, 2)")
     (elixir-mode-string-to-quoted-on-current-line)
     (should-not (search-forward "{:sum, [line: 1], [1, 2]}"  nil t))
     (should (search-forward "{:sum, [line: 1], [3, 2]}"  nil t)))))

(ert-deftest elixir-quoted-minor-mode/indentation ()
  (elixir-test-with-temp-buffer
   ""
   (insert "if a do\n")
   (insert "  b\n")
   (insert "else\n")
   (insert "  c\n")
   (insert "end")
   (elixir-mode-string-to-quoted-on-region (point-min) (point-max))
   (should (search-forward "{:if, [line: 1],\n"  nil t))
   (should (search-forward " [{:a, [line: 1], nil}"  nil t))))

(ert-deftest elixir-quoted-minor-mode/undo ()
  (let ((test-buffer (current-buffer)))
    (elixir-test-with-temp-buffer
     "sum(1, 2)"
     (elixir-mode-string-to-quoted-on-current-line)
     (should-error (undo))
     (pop-to-buffer test-buffer)
     (erase-buffer)
     (insert "sum(3, 2)")
     (elixir-mode-string-to-quoted-on-current-line)
     (should-error (undo)))))
