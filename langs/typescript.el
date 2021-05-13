(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'"))


(use-package tide
  :ensure t
  :after (typescript-mode web-mode company flycheck)
  :custom (tide-node-executable "/Users/manuel/.nvm/versions/node/v14.17.0/bin/node" "Node executable")
  :hook ((typescript-mode . tide-setup)
         (web-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (web-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package eldoc
  :ensure t
  :after tide
  :hook ((tide-mode . eldoc-mode)))
