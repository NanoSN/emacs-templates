;;; sample-special-mode.el ---
;;
;; Filename: sample-special-mode.el
;; Description: Sample special mode similar to dired-mode and other read only
;; modes.
;; Author: Sam Elkhateeb
;; Maintainer: Sam Elkhateeb
;; Copyright (C) 2013, NanoSN inc., all rights reserved.
;; Created:
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'rx)


;;; Variables and Customizations
(defgroup sample nil
  "A sample user interface to be used as a template."
  :group 'tools)

(defvar sample-command-mode-map nil
  "Keymap for sample major mode.")


;;; Font faces section
(defface sample-keywords-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "salmon")))
  "Sample mode face used to highlight keywords."
  :group 'sample)

(defconst sample-command-font-lock-keywords
  `(
    ;; keywords sample
    (,(rx symbol-start (or "sample" "command" "execute") symbol-end)
     . sample-keywords-face)

    ;; regexp sample
    (,(rx symbol-start "execute" (1+ space) (group (1+ word)))
     (1 font-lock-type-face))
    )
  "Sample font face definitions.")


(defun sample-command () ;; (arg)
  "Entry point into sample-command mode."
  (interactive)
  ;; (interactive "DSelect directory: ") ; ask user for input
  ;;TODO: more interactive samples

  (let ((buffer (get-buffer-create "*sample command*")))
    (switch-to-buffer buffer)
    (sample-command-mode)
    (goto-char (point-min))))

(unless sample-command-mode-map
  (let ((map (make-keymap)))
        (define-key map "q" 'bury-buffer)
        (define-key map "\r" 'bury-buffer)
        (setq sample-command-mode-map map)))

;; sample mode should only run in the *sample command* buffer
(put 'sample-command-mode 'mode-class 'special)

(defun sample-command-mode ()
  "Major mode for interacting with sample command.
Commands:
\\{sample-command-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "Sample Command"
        major-mode 'sample-command-mode
        font-lock-defaults '(sample-command-font-lock-keywords)
        ;; goal-column 10
        ;;
        ;; Any more local variables goes here
        ;;
        buffer-read-only t)
  (use-local-map sample-command-mode-map)
  (let ((buffer-read-only nil))
    (erase-buffer)
    ;; TODO: do actual work to current buffer
    (insert "This is a sample command\n")
    (insert "execute this\n")
    (insert (format-time-string "%Y-%m-%dT%T"))
    ))
