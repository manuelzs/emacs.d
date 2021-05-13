;;; Python -- lang config

;;; Commentary:

;;; Code:
(use-package python
  :custom ((python-shell-interpreter . "ipython")
           (python-shell-interpreter-args . "--simple-prompt")))

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode)
  :custom ((blacken-only-if-project-is-blackened t)))

;; ;; Python autocomplete
;; (flycheck-add-mode 'python-flake8 'python-mode)
;; ;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook 'flycheck-mode)
;; (setq jedi:complete-on-dot t)
;; (add-hook 'python-mode-hook 'blacken-mode)

;; ;; Isort
;; ;; (add-hook 'before-save-hook 'py-isort-before-save)

(provide 'Python)
;;; Python.el ends here
