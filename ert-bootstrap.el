;; Add the current directory to load path.
(add-to-list 'load-path (file-name-directory
                         (or load-file-name buffer-file-name)))
;; The test fixtures assume an indentation width of 2, so we need to set that
;; up for the tests.
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(require 'elixir-mode)
