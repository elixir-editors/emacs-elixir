;;; elixir-intern.el --- Helpers functions

;; Copyright (C) 2015  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar elixir-max-specpdl-size max-specpdl-size
  "Protect against eternal loop")

(defmacro elixir-escaped ()
  "Return t if char is preceded by an odd number of backslashes. "
  `(save-excursion
     (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defmacro elixir-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line. "
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (elixir-escaped))))

(defmacro elixir-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line. "
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (elixir-escaped))))

(defun elixir--skip-to-comment-or-comma ()
  "Returns position if comment or semicolon found. "
  (let ((orig (point)))
    (cond ((and done (< 0 (abs (skip-chars-forward "^#," (line-end-position))))
		(member (char-after) (list ?# ?\,)))
	   (when (eq ?\, (char-after))
	     (skip-chars-forward "," (line-end-position))))
	  ((and (< 0 (abs (skip-chars-forward "^#," (line-end-position))))
		(member (char-after) (list ?# ?\,)))
	   (when (eq ?\, (char-after))
	     (skip-chars-forward "," (line-end-position))))
	  ((not done)
	   (end-of-line)))
    (skip-chars-backward " \t" (line-beginning-position))
    (and (< orig (point))(setq done t)
	 done)))

(defun elixir--skip-to-comma-backward (&optional limit)
  "Fetch the beginning of statement after a comma.

Returns position reached if point was moved. "
  (let ((orig (point)))
    (and (< 0 (abs (skip-chars-backward "^," (or limit (line-beginning-position)))))
	 (skip-chars-forward " \t" (line-end-position))
	 (setq done t)
	 (and (< (point) orig) (point)))))

(provide 'elixir-intern)
;;; elixir-intern.el ends here
