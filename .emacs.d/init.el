;; Set emacs directory
(defconst dot-d-dir "~/.emacs.d/" ".emacs.d location")

;; Package management
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

;; Check for network connectivity
(defvar my-online-p nil)
(unless (condition-case
            nil (delete-process (make-network-process
                                 :name "my-check-internet"
                                 :host "elpa.gnu.org"
                                 :service 80)) (error t))
  (setq my-online-p t))

(when my-online-p
  (package-refresh-contents))
(package-initialize)

;; Add packages to load
(add-to-list 'load-path (concat dot-d-dir "packages"))

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)
(require 'cl)

;; Compile elisp
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; Load configurations
(load (concat dot-d-dir "global_settings.el"))
(load (concat dot-d-dir "cpp_settings.el"))
(load (concat dot-d-dir "python_settings.el"))
(load (concat dot-d-dir "org_settings.el"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("649ca960922e2176a451db44624bc4dbcd282bc1660d2621793145232f688836" default)))
 '(package-selected-packages
   (quote
    (yaml-mode kaolin-themes kaolin-theme use-package org-bullets markdown-mode magit ibuffer-vc flycheck fill-column-indicator counsel-projectile company auto-compile ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
