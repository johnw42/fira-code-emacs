;;; fira-code.el --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'dash)

(defconst fira-code--pua-start #xE100)

(defun fira-code--make-alist (list)
  "Generate prettify-symbols alist from LIST.

Each item in LISP may be either a string representing a sequence
of characters to be turned into a ligature, an element to be
inserted literally as a ‘cdr’ in the alist, or a zero-argument
function that returns one of the other types.  The order of the
items correponds to the orders of code points assigned in the
Fira Emacs font, starting from #xE100, part of the Unicode
private-use area.

Using a function value is useful to choose or enable ligatures
selectively depending on the current mode.  For instance, the
\"not equal\" ligature is normally used for !=, to use it instead
for /= in Haskell, replace the != and /= entries with

  (lambda () (if (derived-mode-p 'haskell-mode) \"/=\" \"!=\"))

  and

  (lambda () (if (derived-mode-p 'haskell-mode) nil \"/=\"))

Returns an alist whose keys are successive characters starting
from #xE100, in the Unicode private-use area.  This corresponds
to the order of characters in the Fira Emacs font."
  (let ((idx -1))
    (-filter
     'identity
     (-map
      (lambda (s)
        (cl-incf idx)
        (let ((code (list (decode-char 'ucs (+ fira-code--pua-start idx)))))
          (when (functionp s)
            (setq s (funcall s)))
          (cond
           ((stringp s)
            (let* ((width (string-width s))
                   (prefix ())
                   (suffix (list ?\s (cons 'Br 'Br)))
                   (n 1))
              (while (< n width)
                (cl-callf append prefix (list ?\s (cons 'Br 'Bl)))
                (cl-incf n))
              (cons s (append prefix suffix code))))
           ((null s) nil)
           ((consp s)
            (append s code)))))
      list))))

(defconst fira-code--ligatures
  (fira-code--make-alist
   (list
    "Fl" "Tl" "fl" "www" "--" "---" "-->" "-|" "->" "->>" "-<" "-<<" "-~"
    "{|" "[|" "]#" ".-" ".." "..." "..<" ".?" ".=" "::" ":::" "::=" ":=" ":>" ":<" ";;"
    "!!" "!!." "!=" "!==" "?." "?:" "??" "?=" "**" "***" "*>" "*/"
    "#(" "#{" "#[" "#:" "#!" "#?" "##" "###" "####" "#=" "#_" "#_("
    "/*" "/=" "/==" "/>" "//" "///" "/\\" "\\/" "_|_" "__" "&&"
    "|-" "|->" "|}" "|]" "||" "||-" "|||>" "||=" "||>" "|=" "|=>" "|>" "$>"
    "++" "+++" "+>" "=:=" "=!=" "==" "===" "==>" "=>" "=>>" "=<<" "=/="
    ">-" ">->" ">:" ">=" ">=>" ">>" ">>-" ">>=" ">>>"
    "<-" "<--" "<-|" "<->" "<-<" "<:" "<!--" "<*" "<*>" "<|" "<||" "<|||" "<|>"
    "<$" "<$>" "<+" "<+>" "<=" "<=|" "<==" "<==>" "<=>" "<=<"
    "<>" "<<" "<<-" "<<=" "<<<" "<~" "<~>" "<~~" "</" "</>"
    "~-" "~@" "~=" "~>" "~~" "~~>" "^=" "%%")))

(defvar-local fira-code--old-prettify-alist nil)

(defun fira-code--prettify-symbols-compose-predicate (start end match)
  (or (prettify-symbols-default-compose-p start end match)
      ;; Allow a comment-start token to be composed.
      (and (nth 4 (syntax-ppss))
           (= start (nth 8 (syntax-ppss))))))

(defun fira-code--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append fira-code--ligatures fira-code--old-prettify-alist))
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
  :lighter " æ"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code--enable)
    (fira-code--disable)))

(provide 'fira-code)
