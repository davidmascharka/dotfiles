;; Set emacs directory
(defconst dot-d-dir "~/.emacs.d/" ".emacs.d location")

;; Machine-specific settings
(load (concat dot-d-dir "private-settings.el"))

;; Package management
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

;; Check for network connectivity
;;(defvar my-online-p nil)
;;(unless (condition-case
;;            nil (delete-process (make-network-process
;;                                 :name "my-check-internet"
;;                                 :host "elpa.gnu.org"
;;                                 :service 80)) (error t))
;;  (setq my-online-p t))

(when my-online-p
  (package-refresh-contents))
(package-refresh-contents)

;; Add packages to load
(add-to-list 'load-path (concat dot-d-dir "packages"))

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile (require 'use-package))
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
   '("f6c0353ac9dac7fdcaced3574869230ea7476ff1291ba8ed62f9f9be780de128" "cdc4f03a0fd22165f884b2a39468f848794200bd3ed175ab2272b18981da0673" "6eab6a2f7831e60eb8e53bdab0a49000adc7c494ac5ea9e3a0ac61496b16ba16" "7f6796a9b925f727bbe1781dc65f7f23c0aa4d4dc19613aa3cf96e41a96651e4" "65d4e1535e8fa5d40b9a95d30ed0e95b3bac2b905e905f4397e0425a51addc55" "08e47c1b4faf013eadb945fd15748fe4bc98435c75c0e3014541ecdb5adf7196" "fec6c786b1d3088091715772839ac6051ed972b17991af04b50e9285a98c7463" "293b55c588c56fe062afe4b7a3a4b023712a26d26dc69ee89c347b30283a72eb" "9a58c408a001318ce9b4eab64c620c8e8ebd55d4c52327e354f24d298fb6978f" "8ad35d6c2b35eacc328b732f0a4fe263abd96443a5075aa53b8535a9e8cb7eaf" "b23dbe539cffb25891752772d81fc76133aa73a769a409bfee8547cefc218cf4" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "355e1c0eb7cd20794e983b4c6f5c0c978a85b159d6aadb2fae15faa25fb344e5" "b447129c5efd45c7dcfaaf99b94caf479637ff205b4e5b566efc7ce5496272ab" "628cc301749fc392d32c26ba913402967a17d9f44c0a2b5e4c77850b50b5588b" "9313e289c6d49c96e686ca377e35406dda0d851e26beb3493b427281dbd4fec8" "8e23afd1939fb702db93df19271f48d44b22db8683fa9d1dab87a1f84a6484dc" default))
 '(help-at-pt-display-when-idle '(flymake-diagnostic) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.1)
 '(package-selected-packages
   '(pyvenv virtualenv company-lsp lsp-python lsp-ui lsp-mode eglot yaml-mode python-mode shackle jedi writegood-mode rainbow-delimiters highlight-numbers perl6-mode perl6-model magic-latex-buffer kaolin-themes org-bullets markdown-mode flycheck company magit counsel-projectile projectile ivy use-package ibuffer-vc fill-column-indicator auto-compile))
 '(tramp-syntax 'default nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
