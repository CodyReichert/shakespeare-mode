;;; Package --- shakespeare-mode.el
;;; Summary --- major mode for shakespeare templates
;;; Commentary:
;;; An Emacs mode for hamlet, lucius, and julius files
;;; Cody Reichert 2014
;;;
;;; Code:



;;; REQUIREMENTS
;; require css and js2 mode for syntax and indentation in lucius and julius files
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

    (shakespeare-interpolation-regex . font-lock-preprocessor-face)
    (shakespeare-control-flow-regex . font-lock-keyword-face)))

;;  lucius
(defconst shakespeare-lucius-font-lock-keywords
  '(;; Variables
    ("@[a-z_-][a-z-_0-9]*" . font-lock-constant-face)
    ("&" . font-lock-preprocessor-face)

    (shakespeare-interpolation-regex . font-lock-preprocessor-face)
    (shakespeare-control-flow-regex . font-lock-keyword-face)

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

;; lucius syntax table

;; lucius syntax table
(defvar shakespeare-julius-mode-syntax-table)



;;; Derive Modes
;; derive hamlet mode
(define-derived-mode shakespeare-hamlet-mode fundamental-mode "Shakespeare - Hamlet"
  "A major mode for hamlet, lucius, and julius files.
  \\{shakespeare-mode-map}"

  (kill-all-local-variables) ;; kill all local variables before loading ours
  (set-syntax-table shakespeare-hamlet-mode-syntax-table)
  (use-local-map shakespeare-mode-map) ;; show mode map
  (set (make-local-variable 'font-lock-defaults) ;; set local variables
       '(shakespeare-hamlet-font-lock-keywords))
  (setq major-mode 'shakespeare-hamlet-mode) ;; display mode in mode-line
  (setq mode-name "Shakespeare (hamlet)") ;; sets the name in the mode-line
  (run-hooks 'shakespeare-hamlet-mode-hook))

;;derive lucius mode
(define-derived-mode shakespeare-lucius-mode css-mode "Shakespeare - Lucius"
  "A major mode for hamlet, lucius, and julius files."
  (kill-all-local-variables) ;; kill all local variables before loading ours
  (use-local-map shakespeare-mode-map) ;; show mode map
  (set (make-local-variable 'font-lock-defaults) ;; set local variables
       '(shakespeare-lucius-font-lock-keywords))

  (setq major-mode 'shakespeare-lucius-mode) ;; display mode in mode-line
  (setq mode-name "Shakespeare (lucius)") ;; sets the name in the mode-line
  (run-hooks 'shakespeare-lucius-mode-hook))



;;; Load em up
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . shakespeare-hamlet-mode))
(add-to-list 'auto-mode-alist '("\\.lucius\\'" . shakespeare-lucius-mode))

(provide 'shakespeare-mode)
;;; shakespeare-mode ends here
