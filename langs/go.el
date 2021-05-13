(use-package go-mode
  :ensure t
  :mode "\\.go\\"
  :hook (go-mode . (lambda () (local-set-key (kbd "M-.") #'godef-jump-other-window))))
