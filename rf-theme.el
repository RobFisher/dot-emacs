(deftheme rf-theme
    "Rob's Emacs theme")

(defvar rf-fg "#ffffff")
(defvar rf-bg "#2b2b2b")

(custom-theme-set-faces
 'rf-theme
   ;;; list of custom faces
 `(default ((t (:foreground ,rf-fg :background ,rf-bg))))
 `(cursor ((t (:foreground ,rf-fg :background "black"))))
 `(mode-line ((t (:foreground "#ffffff" :background "#323232"))))
 `(region ((t (:background "#6b6b00"))))
 `(font-lock-builtin-face ((t (:foreground "#d57fa4"))))
 `(font-lock-comment-face ((t (:foreground "#ffc36c"))))
 `(font-lock-function-name-face ((t (:foreground "#867f44"))))
 `(font-lock-keyword-face ((t (:foreground "#ffffff"))))
 `(font-lock-string-face ((t (:foreground "#bcd09b"))))
 `(font-lock-type-face ((t (:foreground"#ffffff"))))
 `(font-lock-constant-face ((t (:foreground "#ffa685"))))
 `(font-lock-variable-name-face ((t (:foreground "#ffbca3"))))
 `(minibuffer-prompt ((t (:foreground "#ffffff" :bold t))))
 `(font-lock-warning-face ((t (:foreground "red" :bold t))))
 )

(provide-theme 'rf-theme)
