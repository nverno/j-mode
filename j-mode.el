;;; j-mode.el --- Major mode for editing J programs

;; Copyright (C) 2012 Zachary Elliott
;;
;; Authors: Zachary Elliott <ZacharyElliott1@gmail.com>
;; URL: http://github.com/zellio/j-mode
;; Version: 1.1.1
;; Keywords: J, Langauges

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock and basic REPL integration for the
;; [J programming language](http://www.jsoftware.com)

;;; Installation

;; The only method of installation is to check out the project, add it to the
;; load path, and load normally. This may change one day.
;;
;; Put this in your emacs config
;;   (add-to-list 'load-path "/path/to/j-mode/")
;;   (load "j-mode")
;;
;; Add for detection of j source files if the auto-load fails
;;   (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode)))

;;; License:

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:
(require 'j-console)

(defgroup j nil
  "Major mode for editing J source files."
  :group 'languages
  :prefix "j-")

(defconst j-mode-version "1.1.1"
  "`j-mode' version")


;; ------------------------------------------------------------
;;* User Variables

;; Required eval depth for older systems
;; (setq max-lisp-eval-depth (max 500 max-lisp-eval-depth))


;; ------------------------------------------------------------
;;* Font Lock
(defgroup j-font-lock nil
  "font-lock extension for j-mode"
  :group 'j
  :prefix "j-font-lock-")

(defgroup j-faces nil
  "Faces for j-font-lock"
  :group 'j
  :group 'j-font-lock)

(defface j-verb-face
  `((t (:foreground "Red")))
  "Font Lock mode face used to higlight verbs"
  :group 'j-faces)

(defface j-adverb-face
  `((t (:foreground "Green")))
  "Font Lock mode face used to higlight adverbs"
  :group 'j-faces)

(defface j-conjunction-face
  `((t (:foreground "Blue")))
  "Font Lock mode face used to higlight conjunctions"
  :group 'j-faces)

(defface j-other-face
  `((t (:foreground "Black")))
  "Font Lock mode face used to higlight others"
  :group 'j-faces)

(defvar j-font-lock-constants '())

(defvar j-font-lock-control-structures
  '("assert."  "break."  "continue."  "while."  "whilst."  "for."  "do."  "end."
    "if."  "else."  "elseif."  "return."  "select."  "case."  "fcase."  "throw."
    "try."  "catch."  "catchd."  "catcht."  "end."
    ;; "for_[a-zA-Z]+\\."  "goto_[a-zA-Z]+\\."  "label_[a-zA-Z]+\\."
    ))

(defvar j-font-lock-foreign-conjunctions
  '("0!:" "1!:" "2!:" "3!:" "4!:" "5!:" "6!:" "7!:" "8!:" "9!:" "11!:" "13!:"
    "15!:" "18!:" "128!:" ))

(defvar j-font-lock-len-3-verbs
  '("_9:" "p.." "{::"))
(defvar j-font-lock-len-2-verbs
  '("x:" "u:" "s:" "r." "q:" "p:" "p." "o." "L." "j." "I." "i:" "i." "E." "e."
    "C." "A." "?." "\":" "\"." "}:" "}." "{:" "{." "[:" "/:" "\\:" "#:" "#." ";:" ",:"
    ",." "|:" "|." "~:" "~." "$:" "$." "^." "%:" "%." "-:" "-." "*:" "*."  "+:"
    "+." "_:" ">:" ">." "<:" "<."))
(defvar j-font-lock-len-1-verbs
  '("?" "{" "]" "[" ":" "!" "#" ";" "," "|" "$" "^" "%" "-" "*" "+" ">" "<" "="))
(defvar j-font-lock-verbs
  (append j-font-lock-len-3-verbs j-font-lock-len-2-verbs j-font-lock-len-1-verbs))

(defvar j-font-lock-len-2-adverbs
  '("t:" "t." "M." "f." "b." "/."))
(defvar j-font-lock-len-1-adverbs
  '("}" "." "\\" "/" "~"))
(defvar j-font-lock-adverbs
  (append j-font-lock-len-2-adverbs j-font-lock-len-1-adverbs))

(defvar j-font-lock-len-3-others
  '("NB."))
(defvar j-font-lock-len-2-others
  '("=." "=:" "_." "a." "a:"))
(defvar j-font-lock-len-1-others
  '("_" ))
(defvar j-font-lock-others
  (append j-font-lock-len-3-others j-font-lock-len-2-others j-font-lock-len-1-others))

(defvar j-font-lock-len-3-conjunctions
  '("&.:"))
(defvar j-font-lock-len-2-conjunctions
  '("T." "S:" "L:" "H." "D:" "D." "d." "&:" "&." "@:" "@." "`:" "!:" "!." ";."
    "::" ":." ".:" ".." "^:"))
(defvar j-font-lock-len-1-conjunctions
  '("&" "@" "`" "\"" ":" "."))
(defvar j-font-lock-conjunctions
  (append j-font-lock-len-3-conjunctions
          j-font-lock-len-2-conjunctions
          j-font-lock-len-1-conjunctions))


(defvar j-font-lock-keywords
  `(
    ("\\([_a-zA-Z0-9]+\\)\s*\\(=[.:]\\)"
     (1 font-lock-variable-name-face) (2 j-other-face))

    (,(regexp-opt j-font-lock-foreign-conjunctions) . font-lock-warning-face)
    (,(concat (regexp-opt j-font-lock-control-structures)
              "\\|\\(?:\\(?:for\\|goto\\|label\\)_[a-zA-Z]+\\.\\)")
     . font-lock-keyword-face)
    (,(regexp-opt j-font-lock-constants) . font-lock-constant-face)
    (,(regexp-opt j-font-lock-len-3-verbs) . j-verb-face)
    (,(regexp-opt j-font-lock-len-3-conjunctions) . j-conjunction-face)
    ;;(,(regexp-opt j-font-lock-len-3-others) . )
    (,(regexp-opt j-font-lock-len-2-verbs) . j-verb-face)
    (,(regexp-opt j-font-lock-len-2-adverbs) . j-adverb-face)
    (,(regexp-opt j-font-lock-len-2-conjunctions) . j-conjunction-face)
    ;;(,(regexp-opt j-font-lock-len-2-others) . )
    (,(regexp-opt j-font-lock-len-1-verbs) . j-verb-face)
    (,(regexp-opt j-font-lock-len-1-adverbs) . j-adverb-face)
    (,(regexp-opt j-font-lock-len-1-conjunctions) . j-conjunction-face)
    ;;(,(regexp-opt j-font-lock-len-1-other) . )
    ) "J Mode font lock keys words")

(defun j-font-lock-syntactic-face-function (state)
  "Function for detection of string vs. Comment Note: J comments
are three chars longs, there is no easy / evident way to handle
this in emacs and it poses problems"
  (if (nth 3 state) font-lock-string-face
    (let* ((start-pos (nth 8 state)))
      (and (<= (+ start-pos 3) (point-max))
           (eq (char-after start-pos) ?N)
           (string= (buffer-substring-no-properties
                     start-pos (+ start-pos 3)) "NB.")
           font-lock-comment-face))))


;; ------------------------------------------------------------
;;* Major Mode

;; Syntax
(defvar j-font-lock-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\{ "."   table)
    (modify-syntax-entry ?\} "."   table)
    (modify-syntax-entry ?\[ "."   table)
    (modify-syntax-entry ?\] "."   table)
    (modify-syntax-entry ?\" "."   table)
    (modify-syntax-entry ?\\ "."   table)
    (modify-syntax-entry ?\. "w"   table)
    (modify-syntax-entry ?\: "w"   table)
    (modify-syntax-entry ?\( "()"  table)
    (modify-syntax-entry ?\) ")("  table)
    (modify-syntax-entry ?\' "\""  table)
    (modify-syntax-entry ?\N "w 1" table)
    (modify-syntax-entry ?\B "w 2" table)
    (modify-syntax-entry ?\n ">"   table)
    (modify-syntax-entry ?\r ">"   table)
    table)
  "Syntax table for j-mode")

;; Menu
(defvar j-mode-menu
  '("J"
    ["Start J Console" j-console t]
    ["Execute Buffer" j-console-execute-buffer t]
    ["Execute Region" j-console-execute-region t]
    ["Execute Line" j-console-execute-line t]
    "---"
    ["J Symbol Look-up" j-help-lookup-symbol t]
    ["J Symbol Dynamic Look-up" j-help-lookup-symbol-at-point t]
    ["Help on J-mode" describe-mode t]))

(defvar j-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil j-mode-menu)
    (define-key map (kbd "C-c C-z") 'j-console)
    (define-key map (kbd "C-c C-c") 'j-console-execute-buffer)
    (define-key map (kbd "C-c C-r") 'j-console-execute-region)
    (define-key map (kbd "C-c C-l") 'j-console-execute-line)
    (define-key map (kbd "C-c h")   'j-help-lookup-symbol)
    (define-key map (kbd "C-c C-h") 'j-help-lookup-symbol-at-point)
    map)
  "Keymap for J major mode")

;;;###autoload
(define-derived-mode j-mode prog-mode "J"
  "Major mode for editing J.\n
\\{j-mode-map}"
  (kill-all-local-variables)
  (set-syntax-table j-font-lock-syntax-table)
  (set (make-local-variable 'comment-start) "NB. ")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)NB. *")
  (set (make-local-variable 'font-lock-comment-start-skip) "NB. *")
  (set (make-local-variable 'font-lock-defaults)
       '(j-font-lock-keywords
         nil nil nil nil
         ;;(font-lock-mark-block-function . mark-defun)
         (font-lock-syntactic-face-function
          . j-font-lock-syntactic-face-function))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))

(provide 'j-mode)

;;; j-mode.el ends here
