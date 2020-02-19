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
(defvar-local ligature-font-word-ligatures nil)

(defvar ligature-font-math-symbol-modes '(haskell-mode))
(defconst ligature-font--math-symbols
  '(">>="
    "<<="
    "|=")
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
                                  "greater_equal.liga"))))

            ;; Turn on certain alternative glyphs.
            (member name '("at.ss06"
                           "less_equal.ss02"
                           "geter_equal.ss02")))
           (or (not (member input-string ligature-font--math-symbols))
               (-some 'derived-mode-p ligature-font-math-symbol-modes)))))
    (cond
     ;; Haskell-specific settings:
     ((derived-mode-p 'haskell-mode)
      (pcase input-string
        ("$" t)                         ; use alternate $
        ("/=" "!=")                     ; "not equal" is /=
        ("!=" nil)                      ; != is not special
        (t default-enabled)))
     (t default-enabled))))

(defun ligature-font--default-compose-predicate
    (start end input-string)
  ;; This function is called thousands for times for every screen
  ;; update, so it needs to be very fast.  It uses to main
  ;; optimizations to achieve that goal:
  ;;
  ;; - It uses ‘string-match’ rather than than ‘looking-at’, because
  ;;   moving the cursor around is slow.
  ;;
  ;; - Rather than calling ‘syntax-ppss’ to look for commments and
  ;;   strings, it looks for font properties previously set by
  ;;   font-lock mode.
  ;;
  ;; It also contains some questionable optimizations:
  ;;
  ;; - It’s always byte-compiled.
  ;;
  ;; - It sets ‘inhibit-changing-match-data’ and calls ‘string-match’
  ;;   rather than calling ‘string-match-p’.
  (let* ((font-prop (get-text-property start 'face))
         (in-string (eq font-prop 'font-lock-string-face))
         (in-comment (memq font-prop '(font-lock-comment-face
                                       font-lock-doc-face)))
         (char-before
          (if (> start (point-min))
              (buffer-substring-no-properties (1- start) start)
            ""))
         (char-after
          (if (< end (point-max))
              (buffer-substring-no-properties end (1+ end))
            ""))
         (inhibit-changing-match-data t))
    (cl-macrolet
        ((check-char-sequence
          (char-regex)
          `(or (not (string-match ,char-regex
                                  input-string))
               (and (not (string-match ,char-regex
                                       char-before))
                    (not (string-match ,char-regex
                                       char-after))))))

      (and
       ;; Prevent composition in strings.
       (not in-string)

       ;; Prevent words in comments from being converted to symbols.
       (not (and in-comment
                 (string-match "^[[:alnum:]_]+$" input-string)))

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
       ;;
       ;; The regex includes all ASCII punctuation characters except
       ;; for brackets.  Brackets are omitted so expressions like
       ;; (<= x y) are handled correctly.
       (check-char-sequence "[-!\"#$%&'*+,./:;<=>?@\\^|~]")))))

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
         (cons input-string
               (append
                (apply 'append
                       (-repeat (- (length input-string) 1)
                                '(?\s (Br . Bl))))
                (list ?\s '(Br . Br)
                      (aref output-string 0)))))))
   list))

(defun ligature-font--prettify-symbols-compose-predicate (start end input-string)
  ;; This function gets called a lot with the empty string as an
  ;; argument, so we check for that case first and immediately return
  ;; nil as an optimization.
  (and (not (string= input-string ""))
       (funcall ligature-font-compose-predicate start end input-string)))

(defvar-local ligature-font--disable-funcs nil)

(defmacro ligature-font--set-with-restore (var expr)
  (let ((temp-var (cl-gensym (format "old-%s" var))))
    `(prog1
         (if (and (local-variable-if-set-p ',var)
                  (not (local-variable-p ',var)))
             (lambda () (kill-local-variable ',var))
           (let ((,temp-var ,var))
             (lambda () (setq ,var ,temp-var))))
       (setq ,var ,expr))))

(define-minor-mode ligature-font-mode
  "Fira Code ligatures minor mode"
  :lighter "  "
  (if ligature-font-mode
      (progn
        (push (ligature-font--set-with-restore
               prettify-symbols-alist
               (nconc (ligature-font--make-alist ligature-font-char-list)
                      prettify-symbols-alist))
              ligature-font--disable-funcs)
        (push (ligature-font--set-with-restore
               prettify-symbols-compose-predicate
               'ligature-font--prettify-symbols-compose-predicate)
              ligature-font--disable-funcs)
        (unless prettify-symbols-mode
          (push (lambda () (prettify-symbols-mode 0))
                ligature-font--disable-funcs))
        (prettify-symbols-mode))
    (while ligature-font--disable-funcs
      (funcall (pop ligature-font--disable-funcs)))))

(provide 'ligature-font)
