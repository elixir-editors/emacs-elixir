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

(defcustom elixir-format-elixir-path ""
  "Path to the Elixir executable. Usually it is /usr/local/bin/elixir. You can type `which elixir` in terminal to find out the elixir binary path in your system.

Customize the elixir path

In Emacs, run following command to customize option

M-x customize-option

Customize-variable: elixir-format-elixir-path"
  :type 'string
  :group 'elixir-format)

(defcustom elixir-format-mix-path ""
  "Path to the 'mix' executable. Usually it is /usr/local/bin/mix. You can type `which mix` in terminal to find out the mix binary path in your system

Customize the mix path

In Emacs, run following command to customize option

M-x customize-option

Customize-variable: elixir-format-mix-path"
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


;;; Code:

(defun elixir-format--errbuff ()
  (get-buffer-create "*elixir-format-errors*"))

(defun elixir-format--outbuff ()
  (get-buffer-create "*elixir-format-output*"))

(defun elixir-format--clean-output-buffers ()
  (with-current-buffer (elixir-format--outbuff)
    (erase-buffer))

  (with-current-buffer (elixir-format--errbuff)
    (setq buffer-read-only nil)
    (erase-buffer)))

;;;###autoload
(defun elixir-format (&optional called-interactively-p)
  (interactive "p")

  (let ((tmpfile (make-temp-file "elixir-format" nil ".ex"))
        (our-elixir-format-arguments (list elixir-format-mix-path "format"))
        (output nil))

    (if (elixir-format--elixir-and-mix-path-not-set-p)
        (elixir-format--display-missing-configuration-error called-interactively-p)
      (unwind-protect
          (save-restriction
            (elixir-format--clean-output-buffers)
            (elixir-format--run-format called-interactively-p tmpfile))))))

(defun elixir-format--elixir-and-mix-path-not-set-p ()
  (or (= (length elixir-format-mix-path) 0)
      (= (length elixir-format-elixir-path) 0)))

(defun elixir-format--display-missing-configuration-error (called-interactively-p)
  (with-current-buffer (elixir-format--errbuff)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Elixir or mix binary path not set, please check the documentation for `elixir-format-elixir-path` and `elixir-format-mix-path` with `C-h v <VARNAME> RET` to set the variables appropriately")
    (setq buffer-read-only t)
    (ansi-color-apply-on-region (point-min) (point-max))
    (special-mode)
    (if called-interactively-p
        (display-buffer elixir-format--errbuff)
      (error "elixir-format-configuration-missing: see %s" (buffer-name (elixir-format--errbuff))))))

(defun elixir-format--run-format (called-interactively-p tmpfile)
  (write-region nil nil tmpfile)

  (run-hooks 'elixir-format-hook)

  (when elixir-format-arguments
    (setq our-elixir-format-arguments (append our-elixir-format-arguments elixir-format-arguments)))
  (setq our-elixir-format-arguments (append our-elixir-format-arguments (list tmpfile)))

  (if (zerop (apply #'call-process elixir-format-elixir-path nil (elixir-format--errbuff) nil our-elixir-format-arguments))
      (elixir-format--call-format-command tmpfile)
    (elixir-format--failed-to-format called-interactively-p))

  (delete-file tmpfile)
  (kill-buffer (elixir-format--outbuff)))

(defun elixir-format--call-format-command (tmpfile)
  (if (zerop (call-process-region (point-min) (point-max) "diff" nil (elixir-format--outbuff) nil "-n" "-" tmpfile))
      (message "File is already formatted")
    (elixir-format--apply-rcs-patch (elixir-format--outbuff))
    (message "elixir-format format applied"))
  (kill-buffer (elixir-format--errbuff)))

(defun elixir-format--failed-to-format (called-interactively-p)
  (with-current-buffer (elixir-format--errbuff)
    (setq buffer-read-only t)
    (ansi-color-apply-on-region (point-min) (point-max))
    (special-mode))

  (if called-interactively-p
      (display-buffer (elixir-format--errbuff))
    (error "elixir-format failed: see %s" (buffer-name (elixir-format--errbuff)))))

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
              (error "Invalid rcs patch or internal error in elixir-format--apply-rcs-patch")))))))))

(provide 'elixir-format)

;;; elixir-format.el ends here
