; To set the font, put the following in ~/.Xresources :
;     emacs.font:             6x13
; Then run xrdb -merge ~/.Xresources

; For Emacs 23 on Ubuntu:
; sudo apt-get install emacs emacs-goodies-el cscope cscope-e

; For Emacs 24 on Ubuntu:
; https://launchpad.net/~cassou/+archive/emacs
; sudo add-apt-repository ppa:cassou/emacs
; sudo apt-get update
; sudo apt-get install emacs24 emacs24-el emacs24-common-non-dfsg

(desktop-save-mode 1)

'(load-file "/usr/share/emacs/site-lisp/xcscope.el")
(require 'xcscope)
(set-face-attribute 'cscope-line-face nil :foreground "PaleGreen")

; http://jasonm23.github.io/emacs-theme-editor/
(defun sweyla764430 ()
  (interactive)
  (color-theme-install
   '(sweyla764430
      ((background-color . "#2b2b2b")
      (background-mode . light)
      (border-color . "#323232")
      (cursor-color . "#ffffff")
      (foreground-color . "#ffffff")
      (mouse-color . "black"))
     (fringe ((t (:background "#323232"))))
     (mode-line ((t (:foreground "#ffffff" :background "#323232"))))
     (region ((t (:background "#6b6b00"))))
     (font-lock-builtin-face ((t (:foreground "#d57fa4"))))
     (font-lock-comment-face ((t (:foreground "#ffc36c"))))
     (font-lock-function-name-face ((t (:foreground "#867f44"))))
     (font-lock-keyword-face ((t (:foreground "#ffffff"))))
     (font-lock-string-face ((t (:foreground "#bcd09b"))))
     (font-lock-type-face ((t (:foreground"#ffffff"))))
     (font-lock-constant-face ((t (:foreground "#ffa685"))))
     (font-lock-variable-name-face ((t (:foreground "#ffbca3"))))
     (minibuffer-prompt ((t (:foreground "#ffffff" :bold t))))
     (font-lock-warning-face ((t (:foreground "red" :bold t))))
     )))
(provide 'sweyla764430)

(require 'color-theme)
(color-theme-initialize)
(sweyla764430)

;; ibuffer mode (built in)
;; http://ergoemacs.org/emacs/emacs_buffer_management.html
(defalias 'list-buffers 'ibuffer)

;; GNU Global
;; http://www.gnu.org/software/global/globaldoc.html
;; Don't use Debian package; too old; build source.
;; Avoid symlinked duplicates by writing output of find in gtags.files
;; but also avoid lines starting with . which are comments in this file.
;; run gtags in top level directory to make index files.
;; key bindings from: http://bocaiwen.blogspot.co.uk/2012/04/customize-gtags-mode-key-binding.html
(defun enable-gtags-mode ()
  (gtags-mode 1)
  (setq gtags-mode-overriding-map (make-sparse-keymap))
  (define-prefix-command 'meta-dot-map)
  (define-key meta-dot-map (kbd "RET") 'gtags-find-tag)
  (define-key meta-dot-map "r" 'gtags-find-rtag)
  (define-key meta-dot-map "p" 'gtags-find-pattern)
  (define-key gtags-mode-overriding-map "\e." 'meta-dot-map)
  (define-key gtags-mode-overriding-map "\e*" 'gtags-pop-stack)
  (setq minor-mode-overriding-map-alist
    (cons (cons 'gtags-mode gtags-mode-overriding-map) minor-mode-overriding-map-alist)))

(autoload 'gtags-mode "gtags" "" t)

;; autocomplete
;; http://cx4a.org/software/auto-complete/manual.html
;; wget http://cx4a.org/pub/auto-complete/auto-complete-1.3.1.tar.bz2
;; tar -jxvf auto-complete-1.3.1.tar.bz2
;; cd auto-complete-1.3.1/
;; /usr/share/emacs24/site-lisp
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/emacs24/site-lisp/ac-dict")
(ac-config-default)

;; log4j mode (log higlighting and filtering)
;; http://log4j-mode.sourceforge.net/
(autoload 'log4j-mode "log4j-mode" "" t)

;; flymake with gccrec
;; On flymake: http://www.emacswiki.org/emacs/FlyMake
;; On gccrec: http://cx4a.org/software/gccsense/manual.html#gccrec
;; Note I am using gccrec without GCCSense.
;; I leave flymake mode off by default; enable it per buffer with
;; M-x flymake-mode
(defun gccrec-command (filename tempfile &rest rest)
  (append `(,"/usr/bin/gccrec"
            ,"-r"
            ,"-a"
            ,tempfile
            ,filename
            "-fsyntax-only")
          rest))

(defun gccrec-flymake-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (command (gccrec-command buffer-file-name temp-file)))
    (list (car command) (cdr command))))

(defun gccrec-flymake-setup ()
  (interactive)
  (require 'flymake)
  (push '("\\.\\(?:c\\|cc\\|cpp\\|cxx\\|C\\|CC\\)$" gccrec-flymake-init) flymake-allowed-file-name-masks))

;; C mode customisation
(setq c-mode-common-hook
      '(lambda ()
         (enable-gtags-mode)
         (gccrec-flymake-setup)
         ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "linux"))))
 '(global-cwarn-mode t)
 '(indent-tabs-mode nil)
 '(tool-bar-mode nil)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((t (:foreground "pale green")))))
