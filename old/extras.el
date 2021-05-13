;; ;; Guru mode with warnings only
;; ;; (guru-global-mode +1)
;; ;; (setq guru-warn-only t)

;; ;; Magit GH pulls
;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; (defun basestone-widget-switch-file ()
;;   "BaseStone switch files"
;;   (interactive)
;;   (let ((other-file-name
;;          (if (string-equal "index.js" (file-name-nondirectory buffer-file-name))
;;              "template.mustache"
;;            "index.js")))
;;     (find-file-other-window (concat default-directory other-file-name))))

;; (global-set-key (kbd "<f5>") 'basestone-widget-switch-file)


;; ;; Webpack
;; ;; (load-file "~/.emacs.d/webpack-server.el")
;; ;; (setq webpack-env-var "BASESTONE_TARGET")

;; ;; (defun webpack-list-targets ()
;; ;;   (let* (
;; ;;          (config-dir-path (concat (webpack-server-project-root) "config"))
;; ;;          (config-files (directory-files config-dir-path nil directory-files-no-dot-files-regexp)))
;; ;;     (mapcar 'file-name-sans-extension config-files)))

;; ;; (defun webpack-setup-env ()
;; ;;   (interactive)
;; ;;   (setenv webpack-env-var (completing-read "Target: " (webpack-list-targets))))

;; ;; (defun webpack-setup-env-if-empty ()
;; ;;   (if (not (locate-file "node" exec-path exec-suffixes 1))
;; ;;       (init-env-path))
;; ;;   (if (not (getenv webpack-env-var)) (webpack-setup-env))
;; ;;   (message "Running with env %s" (getenv webpack-env-var)))

;; ;; (require 'webpack-server)
;; ;; (add-hook 'webpack-server-before-start-hook 'webpack-setup-env-if-empty)


;; (defun collapse-lines ()
;;   (interactive)
;;   (back-to-indentation)
;;   (let ((beg (point)))
;;     (end-of-line 0)
;;     (delete-region beg (point))))

;; (global-set-key (kbd "C-c C-w") 'collapse-lines)

;; ;; Restclient mode
;; (add-to-list 'auto-mode-alist '("\\.rest" . restclient-mode))

;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory) "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js" root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (defun my/use-flow-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory) "node_modules"))
;;          (flow (and root
;;                     (expand-file-name "node_modules/flow-bin/vendor/flow" root))))
;;     (when (and flow (file-executable-p flow))
;;       (setq-local flycheck-javascript-flow-executable flow))))

;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
;; (add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules)

;; ;; Docker mode
;; (use-package docker
;;   :ensure t
;;   :bind ("C-c d" . docker))



