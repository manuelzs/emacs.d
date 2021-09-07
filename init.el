(setq backup-directory-alist `(("." . "~/.emacs_backups")))
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(define-coding-system-alias 'UTF-8 'utf-8)
(put 'downcase-region 'disabled nil)

(require 'server)
(unless (server-running-p server-name) (server-start))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;(eval-when-compile
;;  (require 'use-package))
;;(require 'diminish)
;;(require 'bind-key)

;; Theme
(use-package molokai-theme 
  :ensure t
  :load-path "themes"
  :init
  (setq molokai-theme-kit t)
  :config
  (load-theme 'molokai t))


;; Auth token example
(use-package auth-source
  :ensure t
  :config
  (progn
    (setq magit-circleci-token
      (funcall
       (plist-get
        (nth 0 (auth-source-search
                :host "circleci.com/api"
                :requires '(user secret)))
        :secret)))))

(use-package ido
  :ensure t
  :init
  (progn
    (ido-mode 1)
    (use-package ido-vertical-mode
      :ensure t
      :init (ido-vertical-mode 1)
      :config
      (progn
        (setq ido-vertical-define-keys 'C-n-and-C-p-only)))
    (use-package flx-ido
      :ensure t
      :init (flx-ido-mode 1))
    ;; (use-package ido-ubiquitous
    ;;   :ensure t)
    (use-package smex
      :ensure t
      :init (smex-initialize)
      :bind ("M-x" . smex))
    (use-package idomenu
      :ensure t
      :init (global-set-key (kbd "C-c C-j") 'idomenu)))
  :config
  (progn
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-case-fold t
          ido-create-new-buffer 'always
          ido-use-filename-at-point nil
          ido-max-prospects 10)
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(load-file ' "~/.emacs.d/local-packages/move-lines.el")
(use-package move-lines
  :init (move-lines-binding))

;; Browse kill ring
(use-package browse-kill-ring
  :ensure t
  :init (global-set-key (kbd "C-c k") 'browse-kill-ring))

;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind-keymap (("C-c p" . projectile-command-map)
                ("s-p" . projectile-command-map))
  :delight (concat " " (projectile-project-name))
  :custom ((projectile-keymap-prefix (kbd "C-c p"))
           (projectile-switch-project-action 'projectile-vc)
           (projectile-use-git-grep t)))

(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :config (global-set-key (kbd "M-*") 'anzu-replace-at-cursor-thing))

(use-package magit
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(use-package powerline
  :ensure t
  :config
  (progn
    (setq powerline-utf-8-separator-left 9622
          powerline-utf-8-separator-right 9623)))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :init
  (progn
    (use-package company-tabnine
      :ensure t
      :config (add-to-list 'company-backends #'company-tabnine))
    (use-package company-quickhelp
      :ensure t
      :hook (company-mode . company-quickhelp-mode)
      :custom ((company-quickhelp-delay 1))))
  :config
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 1
          company-show-numbers t
          company-tooltip-align-annotations t
          company-tooltip-limit 20
          company-begin-commands '(self-insert-command)
          company-tooltip-idle-delay 0.2)))

;; Expand region
(use-package expand-region
  :ensure t
  :config
  (progn 
    (global-set-key (kbd "<f9>") 'er/expand-region)))

(load-file ' "~/.emacs.d/langs/init.el")

;; Restclient mode
(use-package restclient :ensure t)

(use-package xterm-color :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; Use spaces instead of tabs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(package-selected-packages
   '(xterm-color molokai-theme flycheck-mypy yasnippet-snippets yaml-mode web-mode use-package tide terraform-mode swift-mode smex smartparens sass-mode rust-mode restclient rainbow-delimiters pug-mode protobuf-mode projectile prettier-js powerline php-mode idomenu ido-vertical-mode haskell-mode groovy-mode graphviz-dot-mode graphql-mode gradle-mode forge flx-ido expand-region elm-mode editorconfig dockerfile-mode docker dart-mode company-web company-tabnine company-quickhelp clojure-mode browse-kill-ring blacken anzu add-node-modules-path))
 '(prettier-js-command "/Users/manuel/.yarn/bin/prettier")
 '(require-final-newline t)
 '(scroll-step 1)
 '(split-height-threshold 100)
 '(split-width-threshold 190)
 '(tab-width 4)
 '(typescript-indent-level 2))


;; Enable mouse
(xterm-mouse-mode 1)

;; Trackpad scroll
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)

;; Restart emacs server
(load-file "~/.emacs.d/local-packages/restart-server.el")

;; Send to slack
;; (load-file "~/.emacs.d/post_to_slack.el")
;; (slack-load-users-and-channels)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Todo: check issues when loading as server
;; (setq node-path "/Users/manuel/.nvm/versions/node/v14.17.0/bin")
;; (unless (string-match-p (regexp-quote node-path) (getenv "PATH"))
;;   (progn 
;;     (setenv "PATH" (concat (getenv "PATH") ":" node-path))
;;     (add-to-list 'exec-path 'node-path)))


;; ;; Yasnippet
;; ;; Todo: check issues when loading as server
;; (use-package yasnippet
;;   :ensure t
;;   :init
;;   (progn
;;     (use-package yasnippet-snippets
;;       :ensure t)
;;     (yas-global-mode 1))
;;   :config
;;   (progn
;;     (setq yas-snippet-dirs "~/.emacs.d/yas/snippets/")))

