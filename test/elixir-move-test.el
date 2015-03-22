;;; elixir-move-test.el --- Quoted minor mode testsuite

;;; Commentary:
;;

;;; Code:

(defvar elixir-teststring-1 "
defmodule MyApp.Mixfile do
  def project do
    [app: :my_app,
     version: \"1.0.0\",
     aliases: aliases]
  end

  defp aliases do
    [c: \"compile\",
     hello: &hello/1]
  end

  defp hello(_) do
    Mix.shell.info \"Hello world\"
  end
end

defmodule MyApp.Mixfile do
  def project do
    [app: :my_app,
     version: \"1.0.0\",
     deps: deps]
  end

  defp deps do
    [{:ecto, \"~> 0.3.0\"},
     {:plug, github: \"elixir-lang/plug\"}]
  end
end
")

(defvar elixir-debug-p t)

(ert-deftest elixir-statement-backward-test ()
  (with-temp-buffer
    (insert elixir-teststring-1)
    (elixir-mode)
    (when elixir-debug-p (switch-to-buffer (current-buffer))
          (font-lock-fontify-buffer))
    (elixir-statement-backward)
    (should (eq (char-after) ?e))
    (elixir-statement-backward)
    (should (eq (char-after) ?e))
    (elixir-statement-backward)
    (should (eq (char-after) ?\[))
    (elixir-statement-backward)
    (should (eq (char-after) ?d))
    (elixir-statement-backward)
    (should (eq (char-after) ?e))
    (elixir-statement-backward)
    (should (eq (char-after) ?\[))
    ))

(ert-deftest elixir-beginning-of-statement-test ()
  (elixir-test-with-temp-buffer
   "\"\"\"foo\"bar\"baz\"\"\"
defmodule Hello do
  IO.puts \"Defining the function world\"

  def world do
    IO.puts \"Hello World\"
  end

  IO.puts \"Function world defined\"
end
"
   (elixir-mode)
   (goto-char (point-max))
   (when elixir-debug-p (switch-to-buffer (current-buffer))
         (font-lock-fontify-buffer))
   (elixir-statement-backward)
   (should (eq (char-after) ?e))
   (elixir-statement-backward)
   (should (eq (char-after) ?I))
   (elixir-statement-backward)
   (should (eq (char-after) ?e))
   (elixir-statement-backward)
   (should (eq (char-after) ?I))
   (elixir-statement-backward)
   (should (eq (char-after) ?d))
   (elixir-statement-backward)
   (should (eq (char-after) ?I))
   (elixir-statement-backward)
   (should (eq (char-after) ?d))
   (elixir-statement-backward)
   (should (eq (char-after) ?\"))
   (should-not (elixir-statement-backward))
))

(ert-deftest elixir-end-of-statement-test ()
  (elixir-test-with-temp-buffer
   "\"\"\"foo\"bar\"baz\"\"\"
defmodule Hello do
  IO.puts \"Defining the function world\"

  def world do
    IO.puts \"Hello World\"
  end

  IO.puts \"Function world defined\"
end
"
   (elixir-mode)
   (when elixir-debug-p (switch-to-buffer (current-buffer))
         (font-lock-fontify-buffer))
   (elixir-statement-forward)
   (should (eq (char-before) ?\"))
   (elixir-statement-forward)
   (should (eq (char-before) ?o))
   (elixir-statement-forward)
   (should (eq (char-before) ?\"))
   (elixir-statement-forward)
   (should (eq (char-before) ?o))
   (elixir-statement-forward)
   (should (eq (char-before) ?\"))
   (elixir-statement-forward)
   (should (eq (char-before) ?d))
   (elixir-statement-forward)
   (should (eq (char-before) ?\"))
   (elixir-statement-forward)
   (should (eq (char-before) ?d))
   (should-not (elixir-statement-forward))
   ))

(provide 'elixir-move-test)

;;; elixir-move-test.el ends here
