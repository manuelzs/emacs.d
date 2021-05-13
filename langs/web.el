(use-package prettier-js
  :ensure t
  :hook ((js2-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package web-mode
  :ensure t
  :mode ("\\.js$" "\\.jsx$" "\\.vue$" "\\.tsx\\'" "\\.ts\\'")
  :init
  (progn
    (use-package add-node-modules-path
      :ensure t
      :hook ((web-mode . add-node-modules-path))))
  :config
  (progn
    (setq web-mode-code-indent-offset 2
          web-mode-markup-indent-offset 2)))

(use-package company-web
  :ensure t
  :after company web-mode
  :bind (:map web-mode-map ("C-'" . company-web-html)))
