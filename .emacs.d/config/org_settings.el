(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (setq org-log-done t)
  ;; set TODO keywords
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "RECOMMENDATION" "|" "DONE" "CANCELED")))
  (add-hook 'org-mode-hook #'org-indent-mode)
  ;; no prompt for execution
  (setq org-config-babel-evaluate nil))

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; capture templates
(if (and (boundp 'my/org-task-file) (boundp 'my/org-notes-files))
    (setq org-capture-templates
          (("t"
            "Task"
            entry
            (file my/org-task-file)
            "* TODO %?\nCAPTURED: %<%Y-%m-%d %H:%M>")
           ("T"
            "Detailed Task"
            entry
            (file my/org-task-file)
            "* TODO %^(Task)\nCAPTURED: %<%Y-%m-%d %H:%M>\n%?\n")
           ("r"
            "Recommendation"
            entry
            (file my/org-notes-file)
            "* RECOMMENDATION %? :%^(Type):\nCAPTURED: %<%Y-%m-%d %H:%M>\n%?\n"))))

;; Make org file bullets look nice
(use-package org-bullets
  :after org
  :ensure t
  :config
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
