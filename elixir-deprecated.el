;;; elixir-deprecated.el --- Functionality to display deprecated messages

;; Copyright 2011-2014 secondplanet
;;           2013-2014 Matt DeBoard, Samuel Tonini, Andreas Fuchs
;; Authors: Humza Yaqoob,
;;          Andreas Fuchs <asf@boinkor.net>,
;;          Matt DeBoard
;;          Samuel Tonini <tonini.samuel@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Functionality to display deprecated messages

;;; Code:

(defface elixir-deprecated--warning-face
  '((t (:inherit font-lock-variable-name-face :bold t :foreground "red")))
  ""
  :group 'elixir-deprecated)

(defface elixir-deprecated--function-face
  '((t (:inherit font-lock-variable-name-face :bold t :foreground "green")))
  ""
  :group 'elixir-deprecated)

(defun elixir-deprecated--warning (function-name message)
  (let ((buffer (get-buffer "*Warnings*")))
    (when buffer
      (kill-buffer buffer))
    (display-warning :deprecated
		     (concat "\n\n"
			     (propertize "DEPRECATION WARNING: "
					 'face 'elixir-deprecated--warning-face)
			     (propertize (format "[ %s ]\n\n" function-name)
					 'face 'elixir-deprecated--function-face)
			     message)) :warning))

;; DEPRECATED MESSAGES FOR RELEASE 3.0.0

(defvar elixir-deprecated--alchemist-message "This function will be removed in version 3.0.0.\n
Please use the package *alchemist.el* for compilation functionality.\n
Alchemist: http://www.github.com/tonini/alchemist.el")

(defun elixir-deprecated-use-alchemist (function-name)
  (elixir-deprecated--warning function-name
			      elixir-deprecated--alchemist-message))

(provide 'elixir-deprecated)
