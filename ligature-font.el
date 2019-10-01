;;; ligature-font.el --- Summary -*- lexical-binding: t -*-
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

(defvar-local ligature-font-char-list nil)

(defconst ligature-font--safe-ligatures
  '("www" "Fl" "Tl" "fl")
  "Ligatures that should always used regardless of context.")

(defvar ligature-font-math-symbol-modes '(haskell-mode)
  "List of modes where math symbol ligatures should be used.")

(defconst ligature-font--math-symbols
  '(">>=" "<<=" "|=")
  "List of math symbols that clash with common operators.")

(defvar ligature-font-enable-substitution-predicate
  'ligature-font--default-enable-substitution-predicate
  "Predicate to decide whether to enable a substitution from
`ligature-font-char-list'.  The arguments are the name of the
glyph and the characters to be replaced.

This predicate is evaluated once for each possible substitution
whenever `ligature-font-mode' is activated.  This predicate is
generally less useful than `ligature-font-compose-predicate', but it
is needed in situations where Fira Code provides multiple glyphs
that can be subsituted for a particular input sequence, and it
can be used to optimize screen refreshes by excluding
substitutions that are never desired in any context.

If this function returns a string, that string will be replaced
instead of the default string taken from
`ligature-font-char-list' This could be used, for example, to
replace \"*\" rather than \"x\" with the x.multiply glyph.")

(defvar ligature-font-compose-predicate
  'ligature-font--default-compose-predicate
  "Predicate to decide whether a particular sequence of
characters should be replaced with a prettier alternative.  The
arguments are the start and end positions of the characters to be
replaced, plus a string containing the characters themselves.

This predicate is evaluated before each string of characters is
replaced with a glyph while `ligature-font-mode' is active.  See also
`ligature-font-enable-substitution-predicate'.")

(defun ligature-font--default-enable-substitution-predicate
    (name input-string)
  (let ((default-enabled
          (and
           (or
            ;; Enable most ligatures.
            (when (string-match-p ".*\\.liga$" name)
              (not (member name '("less_equal.liga"
                                  "greater_equal.liga"
                                  ))))

            ;; Turn on certain alternative glyphs.
            (member name '("at.ss06"
                           "less_equal.ss02"
                           "greater_equal.ss02"
                           )))
           (or (not (member input-string ligature-font--math-symbols))
               (-some 'derived-mode-p ligature-font-math-symbol-modes)))))
    (cond
     ;; Haskell-specific settings:
     ((derived-mode-p 'haskell-mode)
      (pcase input-string
        ("$" t)                         ; use alternate $
        ("/=" "!=")                     ; "not equal" is /=
        ("!=" nil)                      ; != is not special
        (_ default-enabled)))
     (t default-enabled))))

(defun ligature-font--default-compose-predicate
    (start end input-string)
  (cl-macrolet
      ((check-char-sequence
        (char-regex)
        `(or (not (string-match-p ,(concat "^" char-regex "+$")
                                  input-string))
             (and (or (<= start (point-min))
                      (not (string-match-p
                            ,char-regex
                            (buffer-substring-no-properties
                             (1- start)
                             start))))
                  (or (>= end (point-max))
                      (not (string-match-p
                            ,char-regex
                            (buffer-substring-no-properties
                             end
                             (1+ end)))))))))
    (or
     ;; Always enable ligatures for input strings in ligature-font--safe-ligatures.
     (member input-string ligature-font--safe-ligatures)

     (and
      ;; Prevent composition in strings.
      (not (nth 3 (syntax-ppss)))

      ;; Prevent words in comments from being converted to symbols.
      (or (not (nth 4 (syntax-ppss)))
          (not (string-match-p "^[[:alnum:]_]+$" input-string)))

      ;; Prevent portions of words from being transformed.  This can
      ;; happen with, for example, the default transformations in
      ;; python-mode, which replace "or" with "∨".  Without this
      ;; check, "for" would be rendered as "f∨".  As a special case,
      ;; input strings in ligature-font-word-ligatures are allowed, since
      ;; they are intended to appear as parts of words.
      (check-char-sequence "[[:alnum:]_]")

      ;; Prevent parts of puncutation sequences from being turned into
      ;; ligatures.  This typically happens when a long sequence of
      ;; repeating characters is used as a "page break" in a comment.
      (check-char-sequence "[[:punct:]]")))))

;; Compile ligature-font--default-compose-predicate because contains macro calls
;; and it is executed a lot.
(byte-compile 'ligature-font--default-compose-predicate)

(defun ligature-font--make-composition (input-string output-char)
  (cons input-string
        (append
         (apply 'append
                (-repeat (- (length input-string) 1)
                         '(?\s (Br . Bl))))
         (list ?\s '(Br . Br) output-char))))

(defun ligature-font--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (-keep
   (-lambda ([name input-string output-string])
     (cl-assert (= 1 (length output-string)))
     (let ((pred-result
            (funcall ligature-font-enable-substitution-predicate
                     name input-string)))
       (when pred-result
         (when (stringp pred-result)
           (setq input-string pred-result))
         (ligature-font--make-composition input-string
                             (aref output-string 0)))))
   list))

(defun ligature-font--prettify-symbols-compose-predicate (start end input-string)
  (funcall ligature-font-compose-predicate start end input-string))

(defun ligature-font--and-or-hack (alist)
  "If ALIST maps \"and\" and \"or\" to ∧ and ∨, and we have
ligatures for /\\ and \\/, use the ligatures instead."
  (or
   (-when-let* ((big-and (cdr (assoc "/\\" alist)))
                (big-or (cdr (assoc "\\/" alist)))
                (small-and (cdr (assoc "and" alist)))
                (small-or (cdr (assoc "or" alist))))
     (when t (and (equal ?∧ small-and)
                  (equal ?∨ small-or))
           (-cons*
            ;; Build a composition rule that centers the "and"
            ;; ligature in the middle of three columns, assuming the
            ;; ligature itself uses two columns.
            `("and" ?\s (Br . Bl) ?\s (Br . Bc)
              ,(-last-item big-and) (Br . Bc) ?\s)
            ;; The "or" ligature uses the same amount of space as the
            ;; word "or", so we don’t need to do anything special.
            (cons "or" big-or)
            (--remove (member (car it) '("and" "or"))
                      alist))))
   alist))

(defun ligature-font--restore-func (var)
  (let ((old-val (symbol-value var)))
    (if (and (local-variable-if-set-p var)
             (not (local-variable-p var)))
        (lambda () (kill-local-variable var))
      (lambda () (set var old-val)))))

(defmacro ligature-font--set-or-restore (cond-expr restore-funcs-var
                                      &rest clauses)
  (declare (indent 2))
  (let* ((clauses (--map (if (symbolp it) (list it)
                           (cl-assert (<= 1 (length it) 2))
                           it)
                         clauses))
         (set-exprs
          (--map
           (let* ((var (car it))
                  (value-exprs (cdr it))
                  (save-expr
                   `(push (chromium--restore-func ',var)
                          ,restore-funcs-var)))
             (if value-exprs
                 `(progn ,save-expr
                         (setq ,var ,(car value-exprs)))
               save-expr))
           clauses)))
    `(if ,cond-expr
         (progn ,@set-exprs)
       (while ,restore-funcs-var
         (funcall (pop ,restore-funcs-var))))))

(defvar-local ligature-font--disable-funcs nil)

(define-minor-mode ligature-font-mode
  "Fira Code ligatures minor mode"
  :lighter "  "
  (ligature-font--set-or-restore ligature-font-mode ligature-font--disable-funcs
    (prettify-symbols-unprettify-at-point
     (or prettify-symbols-unprettify-at-point t))
    (prettify-symbols-alist
     (ligature-font--and-or-hack
      (nconc (ligature-font--make-alist ligature-font-char-list)
             prettify-symbols-alist)))
    (prettify-symbols-compose-predicate
     'ligature-font--prettify-symbols-compose-predicate))
  (when ligature-font-mode
    (unless prettify-symbols-mode
      (push (lambda () (prettify-symbols-mode 0))
            ligature-font--disable-funcs))
    (prettify-symbols-mode)))

(provide 'ligature-font)
