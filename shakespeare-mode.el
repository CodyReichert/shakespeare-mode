;;; shakespeare-mode.el --- A major mode for editing Shakespearean templates.
;;
;; Copyright (C) 2014 - 2015 Cody Reichert
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
;;   #{..}, @{..}, ^{..}, _{..}, @?{..}
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

;; javascript-mode for julius
(autoload 'javascript-mode "javascript-mode" "Javascript-Mode." t)

;; css mode for lucius
(require 'css-mode)
(unless (or (boundp 'css-navigation-syntax-table)
            (functionp 'css-smie-rules))
  (error "Wrong css-mode.el.  Please use the version bundled with Emacs >= 23"))



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
              shakespeare-hamlet-name-regexp "\\)=\\([^@#^ \r\n]*\\)\\|<?\\([.#]"
              shakespeare-hamlet-name-regexp "\\)\\)")
     (1 font-lock-variable-name-face nil t) ; Attribute names
     (2 font-lock-string-face nil t) ; Attribute values
     (3 font-lock-variable-name-face nil t)) ; #id and .class
    ;; interpolated variables
    ("\\([_@^#][?]?{[^}]+}\\)" . font-lock-preprocessor-face)
    ("^[ \t]*\\($\\w+\\)" . font-lock-keyword-face)))

;; hamlet indentation
(defun shakespeare-hamlet-mode--count-indent ()
  "It just counts indent of current line."
  (let ((count 0))
    (save-excursion
      (beginning-of-line)
      (while (string-match (rx blank) (char-to-string (char-after)))
    	(setq count (+ count 1))
    	(forward-char))
      count)))

(defun shakespeare-hamlet-mode--set-indent (indent-count)
  "Set indent of current line to indent-count."
  (save-excursion
    (back-to-indentation)
    (delete-region
     (point-at-bol)
     (point))
    (cl-loop repeat indent-count
	     do (insert-before-markers " "))
    ))

(defun shakespeare-hamlet-mode--blank-line-p ()
  "Return t if the line with the cursor is blank."
    (if (= (line-beginning-position) (line-end-position))
	t
      nil))

(defun shakespeare-hamlet-mode--count-indent-of-previous-line ()
  "Count indent of previous non-blank line."
  (save-excursion
    (forward-line -1)
    (while (shakespeare-hamlet-mode--blank-line-p)
      (forward-line -1))
    (shakespeare-hamlet-mode--count-indent)))

(defun shakespeare-hamlet-mode--indent-deeper ()
  "Add 2 spaces to current line's indent."
  (let ((indent-of-current-line (shakespeare-hamlet-mode--count-indent)))
    (shakespeare-hamlet-mode--set-indent (+ indent-of-current-line sgml-basic-offset))))

(defun shakespeare-hamlet-mode--indent-shallower ()
  "Remove 2 spaces from current line's indent."
  (let ((indent-of-current-line (shakespeare-hamlet-mode--count-indent)))
    (shakespeare-hamlet-mode--set-indent (- indent-of-current-line sgml-basic-offset))))

(defun shakespeare-hamlet-mode-indent-line ()
  "Cycle the indent like hyai-mode.
If current line's indent is deeper than previous line's, set current line's indent to zero.
Else, indent current line deeper."
  (interactive)
  (let* ((indent-of-previous-line (shakespeare-hamlet-mode--count-indent-of-previous-line))
	 (maximum-indent (+ sgml-basic-offset indent-of-previous-line))
	 (indent-of-current-line (shakespeare-hamlet-mode--count-indent)))

    (if (>= indent-of-current-line maximum-indent)
	(shakespeare-hamlet-mode--set-indent 0)
      (shakespeare-hamlet-mode--indent-deeper))))

(defun shakespeare-hamlet-mode-indent-backward ()
  "Similar to `shakespeare-hamlet-mode-indent-line', but cycle inversely."
  (interactive)
  (let* ((indent-of-previous-line (shakespeare-hamlet-mode--count-indent-of-previous-line))
	 (maximum-indent (+ sgml-basic-offset indent-of-previous-line))
	 (indent-of-current-line (shakespeare-hamlet-mode--count-indent)))

      (if (= 0 indent-of-current-line)
	  (shakespeare-hamlet-mode--set-indent maximum-indent) ; if the indent is zero, cycle to max
	(shakespeare-hamlet-mode--indent-shallower)) ; else, indent shallower
    ))

(defun shakespeare-hamlet-mode--indent-as-previous-line ()
  "Indent current line exactly as deep as previous line."
  (shakespeare-hamlet-mode--set-indent (shakespeare-hamlet-mode--count-indent-of-previous-line)))

(defun shakespeare-hamlet-mode--cursor-is-before-indent-beginning-p ()
  "If the cursor is more left than first non-blank character in the line, return t."
  (let ((cursor-point (point))
	(indent-head-point
	 (save-excursion
	   (back-to-indentation)
	   (point))))
    (if (<= cursor-point indent-head-point)
	t
      nil)))

(defun shakespeare-hamlet-mode--previous-line-is-control-syntax-p ()
  "If previous line begins with $if, $forall, $maybe, etc, return t."
  (save-excursion
    (forward-line -1)
    (while (shakespeare-hamlet-mode--blank-line-p)
      (forward-line -1))
    (back-to-indentation)
    (string= "$" (char-to-string (char-after)))))

(defun shakespeare-hamlet-mode-newline-and-indent ()
  "Insert newline and indent it without touching previous line. It is intended to bind to RET."
  (interactive)
  (let ((beginning-of-indent
	 (set-marker (make-marker)
		     (save-excursion
		       (back-to-indentation)
		       (point)))))

    (if (shakespeare-hamlet-mode--cursor-is-before-indent-beginning-p)
	(progn
	  (save-excursion
	   (beginning-of-line)
	   (newline))
	  (goto-char beginning-of-indent))
      (progn
	(newline)
	(shakespeare-hamlet-mode--indent-as-previous-line)
	(when (shakespeare-hamlet-mode--previous-line-is-control-syntax-p)
	  (shakespeare-hamlet-mode--indent-deeper))))))

(defun shakespeare-hamlet-mode-indent-region (beg end)
  "Just add indent on each line in the region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (shakespeare-hamlet-mode--indent-deeper)
      (forward-line))))

(defun shakespeare-hamlet-mode-indent-region-backward (beg end)
  "just remove indent from each line in the region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (shakespeare-hamlet-mode--indent-shallower)
      (forward-line))))

(defun shakespeare-hamlet-backtab ()
  "If region is active, indent the region backward.
If region is not active, just indent the line backward.
It is intended to bind to backtab (shift-TAB)."
  (interactive)
  (if (region-active-p)
      (shakespeare-hamlet-mode-indent-region-backward (region-beginning) (region-end))
    (shakespeare-hamlet-mode-indent-backward)))

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
  '(("\\([@^#]{[^}]+}\\)" . font-lock-preprocessor-face)
    ("^[ \t]*\\($\\w+\\)" . font-lock-keyword-face)))



;;; Minor Mode for mode-maps and hooks
;;;###autoload
(define-minor-mode shakespeare-mode
  "Shakespeare Mode minor mode for keymaps and mode-hooks."
  :init nil
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))))



;;; Derive Major Modes

;;;###autoload
(define-derived-mode shakespeare-hamlet-mode sgml-mode "Shakespeare (Hamlet)"
  "A major mode for shakespearean hamlet files.
  \\{shakespeare-mode-map}"
  :keymap shakespeare-hamlet-mode-map
  (shakespeare-mode 1)
  (setq-local indent-line-function 'shakespeare-hamlet-mode-indent-line)
  (setq-local indent-region-function 'shakespeare-hamlet-indent-region)
  (setq font-lock-defaults '(shakespeare-hamlet-font-lock-keywords))
  (set (make-local-variable 'sgml-basic-offset) 2))

(define-key shakespeare-hamlet-mode-map (kbd "<backtab>") 'shakespeare-hamlet-backtab)
(define-key shakespeare-hamlet-mode-map (kbd "RET") 'shakespeare-hamlet-mode-newline-and-indent)



;;;###autoload
(define-derived-mode shakespeare-lucius-mode css-mode "Shakespeare (Lucius)"
  "A major mode for shakespearean lucius files.
  \\{shakespeare-mode-map}"
  (shakespeare-mode 1)
  (font-lock-add-keywords nil shakespeare-lucius-font-lock-keywords)
  ;; syntax for cpp comments
  (modify-syntax-entry ?/ ". 124b" shakespeare-lucius-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" shakespeare-lucius-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" shakespeare-lucius-mode-syntax-table)
  ;; Special chars that sometimes come at the beginning of words.
  (modify-syntax-entry ?. "'" shakespeare-lucius-mode-syntax-table)
  (set (make-local-variable 'comment-start) "/*")
  (set (make-local-variable 'comment-end) "*/")
  ;; indentation
  (set (make-local-variable 'indent-line-function) 'shakespeare-lucius-indent-line))

;;;###autoload
(define-derived-mode shakespeare-julius-mode javascript-mode "Shakespeare (Julius)"
  "A major mode for shakespearean julius files.
  \\{shakespeare-mode-map}"
  (shakespeare-mode 1)
  (font-lock-add-keywords nil shakespeare-julius-font-lock-keywords))



;;; Indtentaion rules
;; lucius indentation
(defun shakespeare-lucius-indent-line ()
  "Indent current line according to LESS CSS indentation rules."
  (let ((css-navigation-syntax-table shakespeare-lucius-mode-syntax-table))
    (if (fboundp 'css-indent-line)
        (css-indent-line)
      (smie-indent-line))))

;;; Load em up
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . shakespeare-hamlet-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lucius\\'" . shakespeare-lucius-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.julius\\'" . shakespeare-julius-mode))

(provide 'shakespeare-mode)
;;; shakespeare-mode ends here
