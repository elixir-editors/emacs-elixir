;;; elixir-format.el --- Emacs plugin to mix format Elixir files

;; Copyright 2017-2018 Anil Wadghule, Christian Kruse

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; The elixir-format function formats the elixir files with Elixir's `mix format`
;; command

;; e.g.
;; M-x elixir-format
;;

(require 'ansi-color)

(defcustom elixir-format-elixir-path "elixir"
  "Path to the Elixir interpreter."
  :type 'string
  :group 'elixir-format)

(defcustom elixir-format-mix-path "/usr/bin/mix"
  "Path to the 'mix' executable."
  :type 'string
  :group 'elixir-format)

(defcustom elixir-format-arguments nil
  "Additional arguments to 'mix format'"
  :type '(repeat string)
  :group 'elixir-format)

(defcustom elixir-format-hook nil
  "Hook called by `elixir-format'."
  :type 'hook
  :group 'elixir-format)


;;; Code

(defun elixir-format--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun elixir-format--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function.

Shamelessly stolen from go-mode (https://github.com/dominikh/go-mode.el)"
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun elixir-format--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer.
Shamelessly stolen from go-mode (https://github.com/dominikh/go-mode.el)"

  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in elixir-format--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (elixir-format--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (elixir-format--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in elixir-format--apply-rcs-patch"))))))))
  )

;;;###autoload
(defun elixir-format (&optional is-interactive)
  (interactive "p")

  (let ((outbuff (get-buffer-create "*elixir-format-output*"))
        (errbuff (get-buffer-create "*elixir-format-errors*"))
        (tmpfile (make-temp-file "elixir-format" nil ".ex"))
        (our-elixir-format-arguments (list elixir-format-mix-path "format"))
        (output nil))

    (unwind-protect
        (save-restriction
          (with-current-buffer outbuff
            (erase-buffer))

          (with-current-buffer errbuff
            (setq buffer-read-only nil)
            (erase-buffer))

          (write-region nil nil tmpfile)

          (run-hooks 'elixir-format-hook)

          (when elixir-format-arguments
            (setq our-elixir-format-arguments (append our-elixir-format-arguments elixir-format-arguments)))
          (setq our-elixir-format-arguments (append our-elixir-format-arguments (list tmpfile)))

          (if (zerop (apply #'call-process elixir-format-elixir-path nil errbuff nil our-elixir-format-arguments))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil outbuff nil "-n" "-" tmpfile))
                    (message "File is already formatted")
                  (progn
                    (elixir-format--apply-rcs-patch outbuff)
                    (message "mix format applied")))
                (kill-buffer errbuff))

            (progn
              (with-current-buffer errbuff
                (setq buffer-read-only t)
                (ansi-color-apply-on-region (point-min) (point-max))
                (special-mode))

              (if is-interactive
                  (display-buffer errbuff)
                (error "elixir-format failed: see %s" (buffer-name errbuff)))))

          (delete-file tmpfile)
          (kill-buffer outbuff)))))

(provide 'elixir-format)

;;; elixir-format.el ends here
