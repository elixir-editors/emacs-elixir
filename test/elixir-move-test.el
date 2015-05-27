;;; elixir-move-test.el --- Quoted minor mode testsuite

;;; Commentary:
;;

;;; Code:

(defvar elixir-mode-teststring-1 "
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

(defvar elixir-mode-debug-p t)

(ert-deftest elixir-mode-statement-backward-test ()
  :tags '(navigation)
  (with-temp-buffer
    (insert elixir-mode-teststring-1)
    (elixir-mode)
    (when elixir-mode-debug-p (switch-to-buffer (current-buffer))
          (font-lock-fontify-buffer))
    (elixir-mode-statement-backward)
    (should (eq (char-after) ?e))
    (elixir-mode-statement-backward)
    (should (eq (char-after) ?e))
    (elixir-mode-statement-backward)
    (should (eq (char-after) ?\[))
    (elixir-mode-statement-backward)
    (should (eq (char-after) ?d))
    (elixir-mode-statement-backward)
    (should (eq (char-after) ?e))
    (elixir-mode-statement-backward)
    (should (eq (char-after) ?\[))
    ))

(ert-deftest elixir-mode-beginning-of-statement-test ()
  :tags '(navigation)
  (elixir-test-with-temp-buffer
   "\"\"\"foo\"bar\"baz\"\"\"
defmodule Hello do
  @moduledoc \"\"\"
  Simple implementation  \*\*CRDT\*\* :
  \"\"\"
  IO.puts \"Defining the function world\"

  def world do
    IO.puts \"Hello World\"
  end

  IO.puts \"Function world defined\"
end
"
   (elixir-mode)
   (goto-char (point-max))
   (when elixir-mode-debug-p (switch-to-buffer (current-buffer))
         (font-lock-fontify-buffer))
   (elixir-mode-statement-backward)
   (should (eq (char-after) ?e))
   (elixir-mode-statement-backward)
   (should (eq (char-after) ?I))
   (elixir-mode-statement-backward)
   (should (eq (char-after) ?e))
   (elixir-mode-statement-backward)
   (should (eq (char-after) ?I))
   (elixir-mode-statement-backward)
   (should (eq (char-after) ?d))
   (elixir-mode-statement-backward)
   (should (eq (char-after) ?I))
   (elixir-mode-statement-backward)
   (should (eq (char-after) ?@))
   (elixir-mode-statement-backward)
   (should (eq (char-after) ?d))
   (elixir-mode-statement-backward)
   (should (eq (char-after) ?\"))
   (should-not (elixir-mode-statement-backward))
))

(ert-deftest elixir-mode-end-of-statement-test ()
  :tags '(navigation)
  (elixir-test-with-temp-buffer
   "\"\"\"foo\"bar\"baz\"\"\"
defmodule Hello do
  # Some comment
  IO.puts \"Defining the function world\"

  def world do
    IO.puts \"Hello World\"
  end

  IO.puts \"Function world defined\"
end
"
   (elixir-mode)
   (when elixir-mode-debug-p (switch-to-buffer (current-buffer))
         (font-lock-fontify-buffer))
   (elixir-mode-statement-forward)
   (should (eq (char-before) ?\"))
   (elixir-mode-statement-forward)
   (should (eq (char-before) ?o))
   (elixir-mode-statement-forward)
   (should (eq (char-before) ?\"))
   (elixir-mode-statement-forward)
   (should (eq (char-before) ?o))
   (elixir-mode-statement-forward)
   (should (eq (char-before) ?\"))
   (elixir-mode-statement-forward)
   (should (eq (char-before) ?d))
   (elixir-mode-statement-forward)
   (should (eq (char-before) ?\"))
   (elixir-mode-statement-forward)
   (should (eq (char-before) ?d))
   (should-not (elixir-mode-statement-forward))
   ))

(provide 'elixir-move-test)

;;; elixir-move-test.el ends here
