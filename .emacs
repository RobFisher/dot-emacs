; To set the font, put the following in ~/.Xresources :
;     emacs.font:             6x13
; Then run xrdb -merge ~/.Xresources

; For Emacs 23 on Ubuntu:
; sudo apt-get install emacs emacs-goodies-el

; For Emacs 24 on Ubuntu:
; https://launchpad.net/~cassou/+archive/emacs
; sudo add-apt-repository ppa:cassou/emacs
; sudo apt-get update
; sudo apt-get install emacs24 emacs24-el emacs24-common-non-dfsg

;; Give precedence to loading lisp files from ~/.emacs.d/lisp and its
;; subdirectories.
;; http://www.emacswiki.org/emacs/LoadPath
(let ((default-directory "~/.emacs.d/lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(desktop-save-mode 1)

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

;; ido and idomenu
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;; http://www.emacswiki.org/emacs/idomenu.el
(require 'ido)
(ido-mode t)
(autoload 'idomenu "idomenu" nil t)

;; C mode customisation
(setq c-mode-common-hook
      '(lambda ()
         (enable-gtags-mode)
         (gccrec-flymake-setup)
         (local-set-key (kbd "C-x i") 'idomenu)
         ))

;; occur from isearch
;; http://www.emacswiki.org/emacs/OccurFromIsearch
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; find XML hierarchy
;; http://www.emacswiki.org/NxmlMode
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (condition-case nil
                   (progn
                     (nxml-backward-up-element) ; always returns nil
                     t)       
                 (error nil))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (message "/%s" (mapconcat 'identity path "/"))))))

;; Smex for enhanced M-x
;; https://github.com/nonsequitur/smex/
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "linux"))))
 '(global-cwarn-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(tool-bar-mode nil)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((t (:foreground "pale green")))))
