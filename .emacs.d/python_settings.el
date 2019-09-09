(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (if (executable-find "ipython")
      (setq-default python-shell-interpreter "ipython"
                    python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                    python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
                    python-shell-prompt-output-regexp "Out \\[0-9]+\\]: "
                    python-shell-completion-setup-code
                    "from IPython.core.completerlib import module_completion"
                    python-shell-completion-string-code
                    "';'.join(get_ipython().Completer.all_completions('''%s''))\n"))
  (setq-default python-indent 4
                python-shell-interpreter-args "--simple-prompt -i")
  (setq python-shell-prompt-detect-failure-warning nil))

;; display line number
(add-hook 'python-mode-hook (lambda () (linum-mode 1)))

;; add blacken to use black for formatting
(use-package blacken
  :after python
  :no-require t)

(use-package flycheck
  :after python
  :no-require t
  :config
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package company-jedi
  :after python
  :no-require t
  :config
  (add-to-list 'company-backends 'company-jedi))
