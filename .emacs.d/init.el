(package-initialize)

;; set emacs directory
(defconst dot-d-dir "~/.emacs.d/" ".emacs.d location")

;; Package management
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

;; Add packages to load
(add-to-list 'load-path (concat dot-d-dir "packages"))

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(require 'bind-key)
(require 'cl-lib)

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

;; Machine-specific settings
(if (file-exists-p (concat dot-d-dir "private-settings.el"))
    (load (concat dot-d-dir "private-settings.el")))

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(setq custom-file (concat dot-d-dir "custom.el"))
