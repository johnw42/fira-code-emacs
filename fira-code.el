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

(defvar fira-code-math-symbol-modes '(haskell-mode))
(defconst fira-code--math-symbols
  '(">>="
    "<<="
    "|=")
  "List of math symbols that clash with common operators.")

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
          (and
           (or
            ;; Enable most ligatures.
            (when (string-match-p ".*\\.liga$" name)
              (not (member name '("less_equal.liga"
                                  "greater_equal.liga"))))

            ;; Turn on certain alternative glyphs.
            (member name '("at.ss06"
                           "less_equal.ss02"
                           "geter_equal.ss02")))
           (or (not (member input-string fira-code--math-symbols))
               (-some 'derived-mode-p fira-code-math-symbol-modes)))))
    (cond
     ;; Haskell-specific settings:
     ((derived-mode-p 'haskell-mode)
      (pcase input-string
        ("$" t)                         ; use alternate $
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
     (cl-assert (= 1 (length output-string)))
     (let ((pred-result
            (funcall fira-code-enable-substitution-predicate
                     name input-string)))
       (when pred-result
         (when (stringp pred-result)
           (setq input-string pred-result))
         (cons input-string
               (append
                (apply 'append
                       (-repeat (- (length input-string) 1)
                                '(?\s (Br . Bl))))
                (list ?\s '(Br . Br)
                      (aref output-string 0)))))))
   list))

(defvar-local fira-code--disable-funcs nil)
;; 
(defun fira-code--prettify-symbols-compose-predicate (start end input-string)
  (funcall fira-code-compose-predicate start end input-string))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter "  "
  (cl-macrolet ((set-for-mode
                 (var expr)
                 `(progn
                    (push
                     (if (and (local-variable-if-set-p ',var)
                              (not (local-variable-p ',var)))
                         (lambda () (kill-local-variable ',var))
                       (let ((old-val ,var))
                         (lambda () (setq ,var old-val))))
                     fira-code--disable-funcs)
                    (setq ,var ,expr))))
    (if fira-code-mode
        (progn
          (set-for-mode prettify-symbols-alist
                        (nconc (fira-code--make-alist fira-code--data)
                               prettify-symbols-alist))
          (set-for-mode prettify-symbols-compose-predicate
                        'fira-code--prettify-symbols-compose-predicate)
          (unless prettify-symbols-mode
            (prettify-symbols-mode)
            (push (lambda () (prettify-symbols-mode 0))
                  fira-code--disable-funcs)))
      (while fira-code--disable-funcs
        (funcall (pop fira-code--disable-funcs))))))

(provide 'fira-code)
