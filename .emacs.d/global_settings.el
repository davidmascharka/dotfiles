;; Hide those annoying backup files
(defconst emacs-backup-dir "~/.emacs.backup/" "Directory backup files.")
(defconst emacs-autosave-dir "~/.emacs.autosave/" "Directory auto-save files.")

;; Stop that noob shit at startup
(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(use-package perl6-mode
  :ensure t
  :defer t)

;;; Make numbers pop a bit more
(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;;; Make nested ((((([[[[{}]]]]))))) look alright
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;; Stop the mouse from doing anything
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

;; Keep backup(~) files in specified folder
(setq backup-directory-alist `((".*" . ,emacs-backup-dir)))

;; Set auto-save directory
(setq auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t)))

;; Change options yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Clipboard enabled
(setq select-enable-clipboard t)

;; Check if the font exists and set it
(defvar my/font-type "Hack:style=Regular:antialiasing=True:hinting=True")
(defun font-exists-p (font)
  "Check if FONT exists."
       (if (null (x-list-fonts font))
           nil
         t))

(font-exists-p my/font-type)

;; Set font for standalone GUI emacs
(if (display-graphic-p)
    (if (font-exists-p my/font-type)
        (set-face-attribute 'default nil :font my/font-type)))
(set-face-attribute 'default nil :height 120)

;; Set font for generated frames (daemon)
(defun my/set-frame-font (frame)
  "Set FRAME font if font exists."
  (select-frame frame)
  (if (font-exists-p my/font-type)
      (set-frame-font my/font-type)))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/set-frame-font))

;; UTF-8 encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Display column and line numbers
(line-number-mode 1)
(column-number-mode 1)

;; Syntax highlighting
(global-font-lock-mode 1)

;; Require a newline at the end of files
(setq require-final-newline t)

;; Change buffer listing to ibuffer
(defalias 'list-buffers 'ibuffer)

;; No tab indents (use spaces instead)
(setq-default indent-tabs-mode nil)

;; Comment autofill
(defun comment-auto-fill-only-comments ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda () (not (eq (get-text-property (point) 'face)
       'font-lock-comment-face)))))

;; Prompt before closing emacs
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are sure you want to exit Emacs? "))
      (save-buffers-kill-emacs)
    (message "Canceled exit")))
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;; Autofill comments
(setq comment-auto-fill-only-comments t)

;; Nice-to-have keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode) ; auto-refresh ibuffer :B1:

(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c s") 'eshell)

;; ===========================================================================
;;                          PACKAGE SPECIFICS
;; ===========================================================================
;; Emacs theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package hlinum
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'hlinum-activate))

;; ein
;; ---------------------------------------------------------------------------
(use-package ein
  :ensure t
  :commands (ein:notebooklist-open))

;; use shackle to force newly-opened notebooks to open in the current buffer instead
;; of splitting off into a new window
;; https://github.com/millejoh/emacs-ipython-notebook/issues/75#issuecomment-178583636
(use-package shackle
  :ensure t
  :init
  (setq shackle-rules '(("\\`\\*ein: .+?\\.ipynb\\*\\'" :regexp t :same t)))
  (shackle-mode))


;; Ibuffer-vc
;; ---------------------------------------------------------------------------
(use-package ibuffer-vc
  :demand t
  :ensure t
  :config
  (defun my-ibuffer-vc-hook ()
    ;; ibuffer-vc-set-filter-groups-by-vc-root without buffer update for popwin
    (setq ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root))
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-hook 'my-ibuffer-vc-hook))

;; Hippie-expand
;; ---------------------------------------------------------------------------
(use-package hippie-expand
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill))
  :bind
  (("M-/" . hippie-expand)))

;; Fill Column Indicator
;; ---------------------------------------------------------------------------
(use-package fill-column-indicator
  :ensure t
  :init
  (setq-default fill-column 120)
  (setq-default fci-rule-color "gray")
  (add-hook 'prog-mode-hook 'fci-mode))

;; Ivy
;; ---------------------------------------------------------------------------
(use-package ivy
  :ensure t
  :demand t
  :diminish ivy-mode
  :init
  (setq ivy-height 15)
  (setq ivy-count-format "[%d/%d] ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file)
   ("C-c l" . counsel-locate))
  :config
  (ivy-mode 1))

;; Projectile
;; ---------------------------------------------------------------------------
(use-package projectile
  :after ivy
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode)
  (if (featurep 'ivy)
      (setq projectile-completion-system 'ivy))
  (setq projectile-indexing-method 'alien))

(use-package counsel-projectile
  :after projectile
  :ensure t
  :config
(counsel-projectile-mode))

;; Magit
;; ---------------------------------------------------------------------------
(use-package magit
  :if (not (version< emacs-version "24.4"))
  :ensure t
  :bind
  (("C-c m" . magit-status)))

(add-hook 'after-save-hook 'magit-after-save-refresh-status t)

(use-package forge
  :after magit
  :ensure t)

;; Company
;; ---------------------------------------------------------------------------
(use-package company
  :ensure t
  :diminish company-mode
  :bind
  (("C-<tab>" . company-complete))
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.5
        company-echo-delay 0
        company-show-numbers t)
  :config
  (global-company-mode 1))

;; Flycheck
;; ---------------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :diminish flycheck-mode)

;; Markdown-Mode
;; ---------------------------------------------------------------------------
(use-package markdown-mode
  :if (eq system-type 'gnu/linux)
  :ensure t
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "/usr/bin/pandoc"))

;; yasnippet
;; ---------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :init (yas-global-mode))

;; blacken
;; ---------------------------------------------------------------------------
(use-package blacken
  :ensure t
  :bind
  (("C-c b" . blacken-buffer))

  )

(provide 'global_settings)
