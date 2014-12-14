;;; shakespeare-mode.el --- A major mode for editing Shakespearean templates.
;;
;; Copyright (C) 2014 Cody Reichert
;;
;; Author: Cody Reichert
;; URL: http://github.com/CodyReichert/shakespeare-mode
;; Keywords: shakespeare hamlet lucius julius mode
;; Version: DEV
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:
;; A major mode that provides syntax highlighting an indentation for
;; editing Shakespearean templates (hamlet, lucius, julius).
;;
;; Currently, this mode support almost all of the features provided
;; with Shakespeare templates. Most notable (see the README for a complete list):
;;
;; - Variable interpolation syntax highlighting
;;   #{..}, @{..}, ^{..}, _{..}
;; - Control flow statements syntax highlighting
;;   $if, $forall, $maybe, etc
;; - Indentation for these modes (mostly derived from their parent modes,
;;   but it works well).
;;
;; There are two packages that helped me out greatly while creating this one
;; that I'd like to give credit to since I borrowed a few line from each
;; of them (also see the README):
;;
;; * hamlet-mode (https://github.com/lightquake/hamlet-mode)
;; * less-css-mode (https://github.com/purcell/less-css-mode)
;;
;; Submit issues at https://github.com/CodyReichert/shakespeare-mode
;; or email me directly at cody@reichertbrothers.com.
;;
;;; Code:



;;; REQUIREMENTS
;; sgml mode for hamlet
(require 'sgml-mode)

;; javascript-mode for hamlet
(autoload 'javascript-mode "javascript-mode" "Javascript-Mode." t)

;; css mode for lucius
(require 'css-mode)
(unless (or (boundp 'css-navigation-syntax-table)
            (functionp 'css-smie-rules))
  (error "Wrong css-mode.el.  Please use the version bundled with Emacs >= 23"))



;;; Show Hooks
;; a hook allows users to run code when this mode is started.
(defvar shakespeare-mode-hook nil)
;; (defvar shakespeare-hamlet-mode-hook nil)
;; (defvar shakespeare-lucius-mode-hook nil)
;; (defvar shakespeare-julius-mode-hook nil)



;;; Key Maps
;; There is one mode-map for all three file types.
;; An example key is defined.
(defvar shakespeare-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Shakespeare major mode.")


;;; Keyword Highlighting
;; hamlet
(defconst shakespeare-hamlet-name-regexp "[_:[:alpha:]][-_.:[:alnum:]]*")
(defconst shakespeare-hamlet-font-lock-keywords
  `(
    ;; Tag names.
    ("^!!!$" . font-lock-keyword-face)
    (,(concat "</?\\(" shakespeare-hamlet-name-regexp "\\)")
     1 font-lock-function-name-face)
    ;; Attributes: name=val, #id, or .class.
    (,(concat "\\(?:^\\|[ \t]\\)\\(?:\\("
              shakespeare-hamlet-name-regexp "\\)=\\([^@^ \r\n]*\\)\\|<?\\([.#]"
              shakespeare-hamlet-name-regexp "\\)\\)")
     (1 font-lock-variable-name-face nil t) ; Attribute names
     (2 font-lock-string-face nil t) ; Attribute values
     (3 font-lock-variable-name-face nil t)) ; #id and .class
    ;; interpolated variables
    ("\\([_@^#]{[^}]+}\\)" . font-lock-preprocessor-face)
    ("^[ \t]*\\($\\w+\\)" . font-lock-keyword-face)))

;;  lucius
(defconst shakespeare-lucius-font-lock-keywords
  '(
    ;; Variables
    ("@[a-z_-][a-z-_0-9]*" . font-lock-constant-face)
    ("&" . font-lock-preprocessor-face)
    ;; hamlet interpolation and control flow keywords
    ("\\([@^#]{[^}]+}\\)" . font-lock-preprocessor-face)
    ("^[ \t]*\\($\\w+\\)" . font-lock-keyword-face)
    ;; Mixins
    ("\\(?:[ \t{;]\\|^\\)\\(\\.[a-z_-][a-z-_0-9]*\\)[ \t]*;"
     . (1 font-lock-keyword-face))))

;; julius
;; hamlet interpolation and control flow keywords
(defconst shakespeare-julius-font-lock-keywords
  '(
    ("\\([@^#]{[^}]+}\\)" . font-lock-preprocessor-face)
    ("^[ \t]*\\($\\w+\\)" . font-lock-keyword-face)))



;;; Syntax Tables
;; The lucius and julius syntax tables are derived from their parent-mode
;;
;; Hamlet syntax table
(defvar shakespeare-hamlet-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?\\ "w" st)
    st)
  "The shakespeare mode syntax table.")



;;; Derive Modes
(define-derived-mode shakespeare-hamlet-mode sgml-mode "Shakespeare (Hamlet)"
  "A major mode for shakespearean hamlet files.
  \\{shakespeare-mode-map}"
  (use-local-map shakespeare-mode-map) ;; show mode map
  (setq font-lock-defaults '(shakespeare-hamlet-font-lock-keywords))
  (set (make-local-variable 'sgml-basic-offset) 2))

(define-derived-mode shakespeare-lucius-mode css-mode "Shakespeare (Lucius)"
  "A major mode for shakespearean lucius files.
  \\{shakespeare-mode-map}"
  (use-local-map shakespeare-mode-map) ;; show mode map
  (font-lock-add-keywords nil shakespeare-lucius-font-lock-keywords)
  ;; syntax for cpp comments
  (modify-syntax-entry ?/ ". 124b" shakespeare-lucius-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" shakespeare-lucius-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" shakespeare-lucius-mode-syntax-table)
  ;; Special chars that sometimes come at the beginning of words.
  (modify-syntax-entry ?. "'" shakespeare-lucius-mode-syntax-table)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  ;; indentation
  (set (make-local-variable 'indent-line-function) 'shakespeare-lucius-indent-line))

(define-derived-mode shakespeare-julius-mode javascript-mode "Shakespeare (Julius)"
  "A major mode for shakespearean julius files.
  \\{shakespeare-mode-map}"
  (use-local-map shakespeare-mode-map)
  (font-lock-add-keywords nil shakespeare-julius-font-lock-keywords))



;;; Indtentaion rules
;; lucius indentation
(defun shakespeare-lucius-indent-line ()
  "Indent current line according to LESS CSS indentation rules."
  (let ((css-navigation-syntax-table shakespeare-lucius-mode-syntax-table))
    (css-indent-line)))



;;; Load em up
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . shakespeare-hamlet-mode))
(add-to-list 'auto-mode-alist '("\\.lucius\\'" . shakespeare-lucius-mode))
(add-to-list 'auto-mode-alist '("\\.julius\\'" . shakespeare-julius-mode))

(provide 'shakespeare-mode)
;;; shakespeare-mode ends here
