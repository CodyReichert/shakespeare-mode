;;; Package --- shakespeare-mode.el
;;; Summary --- major mode for shakespeare templates
;;; Commentary:
;;; An Emacs mode for hamlet, lucius, and julius files
;;; Cody Reichert 2014
;;;
;;; Code:

;; require css and js2 mode for syntax and indentation in
;; lucius and julius files (they work so well, why rewrite them).
(require 'css-mode)
(unless (or (boundp 'css-navigation-syntax-table)
            (functionp 'css-smie-rules))
  (error "Wrong css-mode.el. Please use the version bundled with Emacs >= 23"))

;; a main hook that allows users to run code when this mode
;; is started.
(defvar shakespeare-mode-hook nil)

;; create a keymap for the mode. An example key is defined.
(defvar shakespeare-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Shakespeare major mode.")

(defvar shakespeare-interpolation-regex
  "\\([@^#]{[^}]+}\\)")

(defvar shakespeare-control-flow-regex
  "^[ \t]*\\($\\w+\\)")

;; Syntax Highlighting
;; Give emacs any keywords that need to be highlighted
(defconst shakespeare-hamlet-name-regexp "[_:[:alpha:]][-_.:[:alnum:]]*")

(defconst shakespeare-hamlet-font-lock-keywords
  `(
    ;; Doctype declaration.
    ("^!!!$" . font-lock-keyword-face)
    ;; Tag names.
    (,(concat "</?\\(" hamlet//name-regexp "\\)") 1 font-lock-function-name-face)

    ;; Attributes can be either name=val, #id, or .class.
    (,(concat "\\(?:^\\|[ \t]\\)\\(?:\\("
              hamlet//name-regexp "\\)=\\([^@^ \r\n]*\\)\\|<?\\([.#]"
              hamlet//name-regexp "\\)\\)")
     (1 font-lock-variable-name-face nil t) ; Attribute names
     (2 font-lock-string-face nil t) ; Attribute values
     (3 font-lock-variable-name-face nil t)) ; #id and .class

    (shakespeare-interpolation-regex . font-lock-preprocessor-face)
    (shakespeare-control-flow-regex . font-lock-keyword-face)))

;; Hamlet syntax table
(defvar shakespeare-hamlet-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?\\ "w" st)
    st)
  "The shakespeare mode syntax table.")

;; derive hamlet mode
(define-derived-mode shakespeare-hamlet-mode fundamental-mode "Shakespeare"
  "A major mode for hamlet, lucius, and julius files."
  (kill-all-local-variables) ;; kill all local variables before loading ours
  (set-syntax-table shakespeare-hamlet-mode-syntax-table)
  (use-local-map shakespeare-mode-map) ;; show mode map
  (set (make-local-variable 'font-lock-defaults) ;; set local variables
       '(shakespeare-hamlet-font-lock-keywords))
  (setq major-mode 'shakespeare-mode) ;; display mode in mode-line
  (setq mode-name "Shakespeare") ;; sets the name in the mode-line
  (run-hooks 'shakespeare-mode-hook))

;; derive lucius mode
;; (define-derived-mode shakespeare-mode fundamental-mode "Shakespeare"
;;   "A major mode for hamlet, lucius, and julius files."
;;   (kill-all-local-variables) ;; kill all local variables before loading ours
;;   (set-syntax-table shakespeare-mode-syntax-table)
;;   (use-local-map shakespeare-mode-map) ;; show mode map
;;   (set (make-local-variable 'font-lock-defaults) ;; set local variables
;;        '(shakespeare-hamlet-font-lock-keywords))
;;   (setq major-mode 'shakespeare-mode) ;; display mode in mode-line
;;   (setq mode-name "Shakespeare") ;; sets the name in the mode-line
;;   (run-hooks 'shakespeare-mode-hook))

;; derive lucius mode

;; tell emacs to run this mode for the following file types
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . shakespeare-hamlet-mode))

(provide 'shakespeare-mode)
;;; shakespeare-mode ends here
