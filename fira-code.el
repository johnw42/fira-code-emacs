;;; fira-code.el --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Emacs mode for displaying Fira Code ligatures using modified
;;; version of Fira Code called Fira Emacs.
;;;
;;; Code:

(require 'ligature-font)

(eval-when-compile
  (require 'cl))
(require 'dash)

(load "fira-code-data")

(defvar-local fira-code--disable-funcs nil)

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :global nil
  (ligature-font--set-or-restore fira-code-mode fira-code--disable-funcs
    (ligature-font-char-list fira-code--data))
  (when fira-code-mode
    (unless ligature-font-mode
      (push (lambda () (ligature-font-mode 0))
            fira-code--disable-funcs))
    (ligature-font-mode)))

(defun fira-code-enable (&optional face)
  (set-face-attribute 'default nil
                      :family (or face "Fira Emacs"))
  (fira-code-mode))

(provide 'fira-code)
