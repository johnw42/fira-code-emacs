;;; fira-code.el --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Emacs mode for displaying Fira Code ligatures using modified
;;; version of Fira Code called Fira Emacs.
;;;
;;; Originally derived from code posted by Emmanuel Touzery
;;; at <https://emacs.stackexchange.com/a/26824>.
;;;
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'dash)

(load "fira-code-data")

(defconst fira-code--word-ligatures
  (-keep
   (-lambda ([glyph input-string])
     (and (string-match-p "\\.liga$" glyph)
          (string-match-p "^[[:alpha:]]+$" input-string)
          input-string))
   fira-code--data)
  "List of ligatures that should be recognized when the occur
within a word.")

(defvar fira-code-enable-substitution-predicate
  'fira-code--default-enable-substitution-predicate
  "Predicate to decide whether to enable a substitution from
`fira-code--data'.  The arguments are the name of the glyph and
the characters to be replaced.

This predicate is evaluated once for each possible substitution
whenever `fira-code-mode' is activated.  This predicate is
generally less useful than `fira-code-compose-predicate', but it
is needed in situations where Fira Code provides multiple glyphs
that can be subsituted for a particular input sequence, and it
can be used to optimize screen refreshes by excluding
substitutions that are never desired in any context.

If this function returns a string, that string will be replaced
instead of the default string takenf rom `fira-code--data' This
could be used, for example, to replace \"*\" rather than \"x\"
with the x.multiply glyph.")

(defvar fira-code-compose-predicate
  'fira-code--default-compose-predicate
  "Predicate to decide whether a particular sequence of
characters should be replaced with a prettier alternative.  The
arguments are the start and end positions of the characters to be
replaced, plus a string containing the characters themselves.

This predicate is evaluated before each string of characters is
replaced with a glyph while `fira-code-mode' is active.  See also
`fira-code-enable-substitution-predicate'.")

(defun fira-code--default-enable-substitution-predicate
    (name input-string)
  (let ((default-enabled
          (or
           ;; Enable most ligatures.
           (when (string-match-p ".*\\.liga$" name)
             (not (member name '("less_equal.liga"
                                 "greater_equal.liga"))))

           ;; Turn on certain alternative glyphs.
           (member name '("at.ss06"
                          "less_equal.ss02"
                          "geter_equal.ss02")))))
    (cond
     ;; Haskell-specific settings:
     ((derived-mode-p 'haskell-mode)
      (cl-case input-string
        ("$" t)                         ; use alterantive $
        ("/=" "!=")                     ; "not equal" is /=
        ("!=" nil)                      ; != is not special
        (t default-enabled)))
     (t default-enabled))))

(defun fira-code--default-compose-predicate
    (start end input-string)  
  (condition-case nil
      (and
       ;; Turn off composition in strings.
       (not (nth 3 (syntax-ppss)))

       ;; Prevent portions of words from being transformed.  This can
       ;; happen with, for example, the default transformations in
       ;; python-mode, which replace "or" with "∨".  Without this
       ;; check, "for" would be rendered as "f∨".  As a special case,
       ;; input strings in fira-code--word-ligatures are allowed, since
       ;; they are intended to appear as parts of words.
       (or (not (string-match-p "^[[:alnum:]_]+$" input-string))
           (member input-string fira-code--word-ligatures)
           (condition-case nil
               (and (not (string-match-p
                          "[[:alnum:]_]"
                          (buffer-substring (1- start) start)))
                    (not (string-match-p
                          "[[:alnum:]_]"
                          (buffer-substring end (1+ end)))))
             (args-out-of-range nil)))

       ;; Prevent long sequences of repeating characters from being
       ;; turned into a weird combination of ligatures, such as when a
       ;; long line of = characters appears in a comment.
       (condition-case nil
           (not (or (equal input-string
                           (buffer-substring (1- start) (1- end)))
                    (equal input-string
                           (buffer-substring (1+ start) (1+ end)))))
         (args-out-of-range t)))))

(defun fira-code--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (-keep
   (-lambda ([name input-string output-string])
     (let ((pred-result
            (funcall fira-code-enable-substitution-predicate
                     name input-string)))
       (when pred-result
         (when (stringp pred-result)
           (setq input-string pred-result))
         (cons input-string
               (append '(?\s (Br . Br))
                       (cl-loop for n
                                from 2
                                to (string-width input-string)
                                append '(?\s (Br . Bl)))
                       (list (aref output-string 0)))))))
   list))

(defvar-local fira-code--old-prettify-alist nil)

(defun fira-code--prettify-symbols-compose-predicate (start end input-string)
  (funcall fira-code-compose-predicate start end input-string))

(defun fira-code--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (fira-code--make-alist fira-code--data) fira-code--old-prettify-alist))
  (setq-local prettify-symbols-compose-predicate
              'fira-code--prettify-symbols-compose-predicate)
  (prettify-symbols-mode t))

(defun fira-code--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code--old-prettify-alist)
  (kill-local-variable 'prettify-symbols-compose-predicate)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter "  "
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code--enable)
    (fira-code--disable)))

(provide 'fira-code)
