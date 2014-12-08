;;; Package --- shakespeare-mode.el
;;; Summary --- major mode for shakespeare templates
;;; Commentary:
;;; An Emacs mode for hamlet, lucius, and julius files
;;; Cody Reichert 2014
;;;
;;; Code:



;;; REQUIREMENTS
;; require css and js2 mode for syntax and indentation in lucius and julius files
(require 'sgml-mode)
(require 'css-mode)
(unless (or (boundp 'css-navigation-syntax-table)
            (functionp 'css-smie-rules))
  (error "Wrong css-mode.el.  Please use the version bundled with Emacs >= 23"))



;;; Show Hooks
;; a hook allows users to run code when this mode is started.
(defvar shakespeare-mode-hook nil)
(defvar shakespeare-hamlet-mode-hook nil)
(defvar shakespeare-lucius-mode-hook nil)
(defvar shakespeare-julius-mode-hook nil)



;;; Key Maps
;; An example key is defined.
(defvar shakespeare-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Shakespeare major mode.")



;;; Keyword syntax highlighting
;; hamlet
(defconst shakespeare-hamlet-name-regexp "[_:[:alpha:]][-_.:[:alnum:]]*")
(defconst shakespeare-hamlet-font-lock-keywords
  `(
    ("^!!!$" . font-lock-keyword-face)
    ;; Tag names.
    (,(concat "</?\\(" shakespeare-hamlet-name-regexp "\\)") 1 font-lock-function-name-face)
    ;; Attributes: name=val, #id, or .class.
    (,(concat "\\(?:^\\|[ \t]\\)\\(?:\\("
              shakespeare-hamlet-name-regexp "\\)=\\([^@^ \r\n]*\\)\\|<?\\([.#]"
              shakespeare-hamlet-name-regexp "\\)\\)")
     (1 font-lock-variable-name-face nil t) ; Attribute names
     (2 font-lock-string-face nil t) ; Attribute values
     (3 font-lock-variable-name-face nil t)) ; #id and .class
    ;; interpolated variables
    ("\\([@^#]{[^}]+}\\)" . font-lock-preprocessor-face)
    ("^[ \t]*\\($\\w+\\)" . font-lock-keyword-face)))

;;  lucius
(defconst shakespeare-lucius-font-lock-keywords
  '(;; Variables
    ("@[a-z_-][a-z-_0-9]*" . font-lock-constant-face)
    ("&" . font-lock-preprocessor-face)

    ("\\([@^#]{[^}]+}\\)" . font-lock-preprocessor-face)
    ("^[ \t]*\\($\\w+\\)" . font-lock-keyword-face)
    ;; Mixins
    ("\\(?:[ \t{;]\\|^\\)\\(\\.[a-z_-][a-z-_0-9]*\\)[ \t]*;" . (1 font-lock-keyword-face))))



;;; Syntax Tables
;; Hamlet syntax table
(defvar shakespeare-hamlet-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?\\ "w" st)
    st)
  "The shakespeare mode syntax table.")

;; jucius syntax table
(defvar shakespeare-julius-mode-syntax-table)



;;; Derive Modes
;; derive hamlet mode
(define-derived-mode shakespeare-hamlet-mode sgml-mode "Shakespeare - Hamlet"
  "A major mode for hamlet, lucius, and julius files.
  \\{shakespeare-mode-map}"
  ;;(kill-all-local-variables) ;; kill all local variables before loading ours
  (set-syntax-table shakespeare-hamlet-mode-syntax-table)
  (use-local-map shakespeare-mode-map) ;; show mode map
  (set (make-local-variable 'font-lock-defaults) ;; set local variables
       '(shakespeare-hamlet-font-lock-keywords))
  (setq major-mode 'shakespeare-hamlet-mode) ;; display mode in mode-line
  (set (make-local-variable 'sgml-basic-offset) 2)
  (setq mode-name "Shakespeare (hamlet)") ;; sets the name in the mode-line
  (run-hooks 'shakespeare-hamlet-mode-hook))

;;derive lucius mode
(define-derived-mode shakespeare-lucius-mode css-mode "Shakespeare - Lucius"
  "A major mode for hamlet, lucius, and julius files.
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
  (set (make-local-variable 'indent-line-function) 'shakespeare-lucius-indent-line)
  (setq major-mode 'shakespeare-lucius-mode)
  (setq mode-name "Shakespeare (lucius)") ;; sets the name in the mode-line
  (run-hooks 'shakespeare-lucius-mode-hook))



;;; Indtentaion rules
;; lucius indentation
(defun less-css-indent-line ()
  "Indent current line according to LESS CSS indentation rules."
  (let ((css-navigation-syntax-table shakespeare-lucius-mode-syntax-table))
    (css-indent-line)))



;;; Load em up
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . shakespeare-hamlet-mode))
(add-to-list 'auto-mode-alist '("\\.lucius\\'" . shakespeare-lucius-mode))

(provide 'shakespeare-mode)

;;; shakespeare-mode ends here
