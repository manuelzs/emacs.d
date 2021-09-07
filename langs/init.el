;;; lang-init --- Programming languages config

;;; Commentary:

;;; Code:
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens :ensure t
  :config
  (progn
    (define-key smartparens-mode-map (kbd "C-c C-s s") 'sp-forward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-c C-s b") 'sp-forward-barf-sexp)
    (define-key smartparens-mode-map (kbd "C-c C-s k") 'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd "C-c C-s c") 'sp-copy-sexp))
  :hook (emacs-lisp-mode . smartparens-strict-mode))

(use-package flycheck
  :ensure t
  :init
  (progn
    (use-package flycheck-mypy
      :ensure t)
    (global-flycheck-mode))
  ;; :custom ((flycheck-javascript-eslint-executable . "/usr/local/bin/eslint")
  ;;          (flycheck-python-flake8-executable . "/Users/manuel/.virtualenvs/emacs/bin/flake8")
  ;;          (flycheck-python-mypy-executable . "/Users/manuel/.virtualenvs/emacs/bin/mypy")
  ;;          (flycheck-python-pycompile-executable . "/Users/manuel/.virtualenvs/melt/bin/python3"))
  )

(load-file ' "~/.emacs.d/langs/web.el")
(load-file ' "~/.emacs.d/langs/typescript.el")
(load-file ' "~/.emacs.d/langs/python.el")
(load-file ' "~/.emacs.d/langs/go.el")

(use-package pug-mode :ensure t)
(use-package gradle-mode :ensure t)
(use-package dart-mode :ensure t)
(use-package php-mode :ensure t)
(use-package swift-mode :ensure t)
(use-package graphviz-dot-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package clojure-mode :ensure t)
(use-package elm-mode :ensure t)
(use-package groovy-mode :ensure t)
(use-package haskell-mode :ensure t)

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "/opt/homebrew/bin/markdown" "Markdown binary location"))

(use-package protobuf-mode :ensure t)
(use-package rust-mode :ensure t)
(use-package sass-mode :ensure t)
(use-package graphql-mode :ensure t)
(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . terraform-format-on-save-mode))

(provide 'init)
;;; init.el ends here
