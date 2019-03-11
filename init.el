(setq backup-directory-alist `(("." . "~/.emacs_backups")))

(defun init-env-path ()
  (interactive)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))

(when (memq window-system '(mac ns)) (init-env-path))

(define-minor-mode silent-mode
  "Silent mode
Disables backup creation and auto saving."

  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  "[Silent]"
  ;; The minor mode bindings.
  nil

  (if (symbol-value silent-mode)
      (progn
	;; disable backups
	(set 'backup-inhibited t)
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)
	  )

	(custom-set-variables
	 '(ido-enable-last-directory-history nil)
	 '(ido-record-commands nil)
	 '(ido-max-work-directory-list 0)
	 '(ido-max-work-file-list 0))
	)

    ;resort to default value of backup-inhibited
    (set 'backup-inhibited nil)

    ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1)
      )
    )

  )

(define-coding-system-alias 'UTF-8 'utf-8)

;; Smpartparens for elisp mode
(add-to-list 'load-path "~/.emacs.d/elpa/dash-20190128.1920/")
(add-to-list 'load-path "~/.emacs.d/elpa/smartparens-20170723.1205/")
(require 'smartparens)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(define-key smartparens-mode-map (kbd "C-c C-s s") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-c C-s b") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-c C-s k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-c C-s c") 'sp-copy-sexp)

(add-to-list 'load-path "~/.emacs.d/ruby-mode/")
;; Load ruby mode when needed
(autoload 'ruby-mode "ruby-mode" "Ruby mode" t )
;; Assign .rb and .rake files to use ruby mode
(setq auto-mode-alist (cons '("\\.rb\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
;; Show syntax highlighting when in ruby mode
(add-hook 'ruby-mode-hook '(lambda () (font-lock-mode 1)))


(put 'downcase-region 'disabled nil)

;; html-mode for JSTs
(add-to-list 'auto-mode-alist '("\\.jst" . html-mode))

;; Adding IDO mode
(add-to-list 'load-path "~/.emacs.d/elpa/ido-vertical-mode-20180618.2101/")
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)


;; Scala mode
(add-to-list 'load-path "~/.emacs.d/scala-mode/")
(require 'scala-mode-auto)

;; (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;;(load "~/.emacs.d/my-packages.el")

;; Show column
(setq column-number-mode t)

(add-to-list 'load-path "~/.emacs.d/less/")
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))

;; Scroll one line - dont jump
(setq scroll-step 1)

;; yaml-mode
(add-to-list 'load-path "~/.emacs.d/yaml/")
(require 'yaml-mode)
;; Salt extensions
(add-to-list 'auto-mode-alist '("\\.sls" . yaml-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212121" "#CC5542" "#6aaf50" "#7d7c61" "#5180b3" "#DC8CC3" "#9b55c3" "#bdbdb3"])
 '(ac-go-gocode-bin "/Users/manuel/go/bin/gocode")
 '(browse-url-browser-function (quote browse-url-default-macosx-browser))
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-input-method "TeX")
 '(emms-player-vlc-command-name "/Applications/VLC.app/Contents/MacOS/VLC")
 '(epa-pinentry-mode (quote loopback))
 '(fci-rule-color "#2e2e2e")
 '(ido-enable-last-directory-history nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0)
 '(ido-record-commands nil)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-global-externs
   (list "window" "define" "require" "module" "exports" "process" "Buffer" "__dirname" "Parse" "sessionStorage" "localStorage" "describe" "it" "FileReader" "analytics" "setTimeout" "btoa" "atob" "FormData" "xdescribe" "xit" "context" "beforeEach"))
 '(package-selected-packages
   (quote
    (ac-js2
     ack-and-a-half
     afternoon-theme
     alect-themes
     ample-theme
     ample-zen-theme
     async
     auto-complete
     auto-package-update
     butler
     clj-refactor
     clojure-mode
     clojure-snippets
     coffee-mode
     color-theme
     color-theme-sanityinc-solarized
     color-theme-sanityinc-tomorrow
     color-theme-solarized
     company
     company-go
     concurrent
     ctable
     dash
     deferred
     direx
     docker
     ein
     emms
     emojify
     exec-path-from-shell
     f
     feature-mode
     fill-column-indicator
     flx-ido
     flycheck
     flycheck-flow
     flycheck-mypy
     flycheck-package
     flymake
     flymake-jshint
     flymake-json
     flymake-python-pyflakes
     fsm
     gist
     git
     gitty
     go-autocomplete
     go-direx
     go-dlv
     go-mode
     go-projectile
     go-stacktracer
     gotest
     govet
     groovy-mode
     guru-mode
     haskell-mode
     ido-vertical-mode
     jedi
     jedi ido-vertical-mode
     jenkins
     json-mode
     karma
     magit-gh-pulls
     markdown-mode
     neotree
     nose
     pivotal-tracker
     popwin
     powerline
     projectile
     protobuf-mode
     pycoverage
     rainbow-delimiters
     regex-tool
     request
     request-deferred
     restclient
     rich-minority
     sass-mode
     smartparens
     solarized-theme
     soundklaus
     string-utils
     tabulated-list
     tern js2-mode
     tern-auto-complete
     terraform-mode
     tide
     tldr
     twittering-mode
     typescript-mode
     use-package
     uuid
     uuidgen
     web-beautify
     web-mode
     webpack-server
     websocket
     wsd-mode)))
 '(powerline-utf-8-separator-left 9622)
 '(powerline-utf-8-separator-right 9623)
 '(projectile-keymap-prefix (kbd "C-c p"))
 '(projectile-switch-project-action (quote projectile-vc))
 '(projectile-use-git-grep t)
 '(python-shell-interpreter "ipython")
 '(python-shell-interpreter-args "--simple-prompt")
 '(require-final-newline t)
 '(send-mail-function (quote mailclient-send-it))
 '(soundklaus-access-token (get-string-from-file "~/.soundcloud_token"))
 '(split-height-threshold 100)
 '(split-width-threshold 190)
 '(tab-width 4)
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#dd5542")
     (40 . "#CC5542")
     (60 . "#fb8512")
     (80 . "#baba36")
     (100 . "#bdbc61")
     (120 . "#7d7c61")
     (140 . "#6abd50")
     (160 . "#6aaf50")
     (180 . "#6aa350")
     (200 . "#6a9550")
     (220 . "#6a8550")
     (240 . "#6a7550")
     (260 . "#9b55c3")
     (280 . "#6CA0A3")
     (300 . "#528fd1")
     (320 . "#5180b3")
     (340 . "#6380b3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(web-mode-markup-indent-offset 2)
 '(webpack-server-host "0.0.0.0"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :background "color-22"))))
 '(diff-removed ((t (:inherit diff-changed :background "color-88"))))
 '(ediff-current-diff-A ((t (:background "#5f0000"))))
 '(ediff-current-diff-B ((t (:background "#005f00"))))
 '(ediff-even-diff-A ((t (:background "color-233" :foreground "color-252"))))
 '(ediff-even-diff-B ((t (:background "color-233" :foreground "White"))))
 '(ediff-odd-diff-A ((t (:background "color-234" :foreground "White"))))
 '(ediff-odd-diff-B ((t (:background "color-234" :foreground "color-252"))))
 '(font-lock-function-name-face ((t (:foreground "color-33"))))
 '(font-lock-keyword-face ((t (:foreground "brightmagenta"))))
 '(font-lock-string-face ((t (:foreground "color-128"))))
 '(font-lock-type-face ((t (:foreground "color-34"))))
 '(font-lock-variable-name-face ((t (:foreground "color-39"))))
 '(highlight ((t (:background "color-235"))))
 '(magit-diff-added ((t (:background "#222222" :foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:background "#111111" :foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t (:background "color-234" :foreground "color-245"))))
 '(magit-diff-hunk-heading ((t (:background "color-235" :foreground "color-251"))))
 '(magit-diff-removed ((t (:background "#222222" :foreground "#cc2222"))))
 '(magit-diff-removed-highlight ((t (:background "#111111" :foreground "#cc2222"))))
 '(magit-section-heading ((t (:foreground "color-208" :weight bold))))
 '(magit-section-highlight ((t (:background "color-234"))))
 '(menu ((t (:background "color-235"))))
 '(minibuffer-prompt ((t (:foreground "brightblue"))))
 '(mode-line ((t (:background "color-234" :foreground "color-249" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:foreground "color-197"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "color-233" :foreground "color-238" :box (:line-width -1 :color "grey75") :weight light))))
 '(region ((t (:background "color-234"))))
 '(rst-level-1 ((t (:background "color-234"))))
 '(rst-level-2 ((t (:background "color-234"))))
 '(rst-level-3 ((t (:background "color-234"))))
 '(secondary-selection ((t (:background "color-130"))))
 '(smerge-lower ((t (:background "color-232"))))
 '(smerge-markers ((t (:background "color-234"))))
 '(smerge-mine ((t (:background "color-233"))) t)
 '(smerge-other ((t (:background "color-233"))) t)
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "color-235"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "color-235"))))
 '(smerge-upper ((t (:background "color-232"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "color-243"))))
 '(widget-field ((t (:background "color-235" :foreground "color-246")))))

;; Clean whitespace on save
;;(add-hook 'before-save-hook 'whitespace-cleanup)
;; This might add tabs instead of spaces

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Mustache mode
(add-to-list 'load-path "~/.emacs.d/mustache/")
(require 'mustache-mode)

(projectile-global-mode)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(load-file "~/.emacs.d/projectile-local.el")
(projectile-bs-global-mode)

;; Guru mode with warnings only
;; (guru-global-mode +1)
;; (setq guru-warn-only t)

;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yas/snippets"))
(yas-global-mode 1)

(require 'flx-ido)
(flx-ido-mode 1)

;; Python autocomplete
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq jedi:complete-on-dot t)

;; JS Tern
;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))
;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))
;; (setq tern-command '("/usr/local/bin/tern"))

;; JS2
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
;; (setq js2-highlight-level 3)


;; Auto complete mode
(require 'auto-complete)
(add-to-list 'ac-modes 'javascript-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'coffee-mode)
(global-auto-complete-mode t)

;; Coffee Options

;; GO
(require 'go-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") #'godef-jump)))


(defun my-go-mode-hook ()
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  ;; Use company mode
  (set (make-local-variable 'company-backends) '(company-mode))
  (company-mode))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;; Go Test
(define-key go-mode-map (kbd "C-c t f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-c t t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-c t p") 'go-test-current-project)
(define-key go-mode-map (kbd "C-c C-c") 'go-run)

;; Enable mouse
(xterm-mouse-mode 1)

;; Clojure
(setq cider-lein-command "/usr/local/bin/lein")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)

;; Hoplon
(add-to-list 'auto-mode-alist '("\\.hl$" . clojure-mode))

;;(load-file "~/.emacs.d/elpa/spinner-1.7.1/spinner.el")
;; (require 'clj-refactor)

;; (defun my-clojure-mode-hook ()
;;    (clj-refactor-mode 1)
;;    (cljr-add-keybindings-with-prefix "C-c C-m"))

;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Set fill column
(setq-default fill-column 80)

;; Copy region to clipboard
(defun copy-region-to-clipboard ()
  (interactive)
  (if (eq system-type 'darwin)
      (shell-command-on-region
       (region-beginning) (region-end)
       "pbcopy && echo 'Region copied to clipboard'")
    (message "Clipboard copy not supported")))

(global-set-key (kbd "C-c C-k") 'copy-region-to-clipboard)

;; Jenkins
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun jenkins-start ()
  "Start jenkins with user token"
  (interactive)
  (setq jenkins-api-token (get-string-from-file "~/.jenkins_token"))
  (setq jenkins-url "http://cm.basestone.io:8080/")
  (setq jenkins-username "manuel")
  (jenkins))

;; Magit GH pulls
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(load-file "~/.emacs.d/post_to_slack.el")
(slack-load-users-and-channels)

;; Trackpad scroll
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)




(defun basestone-widget-switch-file ()
  "BaseStone switch files"
  (interactive)
  (let ((other-file-name
         (if (string-equal "index.js" (file-name-nondirectory buffer-file-name))
             "template.mustache"
           "index.js")))
    (find-file-other-window (concat default-directory other-file-name))))

(global-set-key (kbd "<f5>") 'basestone-widget-switch-file)


;; Webpack
;; (load-file "~/.emacs.d/webpack-server.el")
;; (setq webpack-env-var "BASESTONE_TARGET")

;; (defun webpack-list-targets ()
;;   (let* (
;;          (config-dir-path (concat (webpack-server-project-root) "config"))
;;          (config-files (directory-files config-dir-path nil directory-files-no-dot-files-regexp)))
;;     (mapcar 'file-name-sans-extension config-files)))

;; (defun webpack-setup-env ()
;;   (interactive)
;;   (setenv webpack-env-var (completing-read "Target: " (webpack-list-targets))))

;; (defun webpack-setup-env-if-empty ()
;;   (if (not (locate-file "node" exec-path exec-suffixes 1))
;;       (init-env-path))
;;   (if (not (getenv webpack-env-var)) (webpack-setup-env))
;;   (message "Running with env %s" (getenv webpack-env-var)))

;; (require 'webpack-server)
;; (add-hook 'webpack-server-before-start-hook 'webpack-setup-env-if-empty)


(defun collapse-lines ()
  (interactive)
  (back-to-indentation)
  (let ((beg (point)))
    (end-of-line 0)
    (delete-region beg (point))))

(global-set-key (kbd "C-c C-w") 'collapse-lines)

(require 'flycheck)
;; (add-to-list 'js-mode-hook 'flycheck-mode)
(add-to-list 'python-mode-hook 'flycheck-mode)
(setq-default flycheck-disabled-checkers '(javascript-jscs))

;; Typescript
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx" . typescript-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(add-hook 'web-mode-hook
          (lambda ()
            (let ((ext (file-name-extension buffer-file-name)))
              (when (or (string-equal "jsx" ext) (string-equal "js" ext))
                (flycheck-mode)
                (yas-activate-extra-mode 'js-mode)))))

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'yas-extra-modes) 'js-mode)))
(yas-reload-all 1)

;; Flyckech
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'python-flake8 'python-mode)


;; Restclient mode
(add-to-list 'auto-mode-alist '("\\.rest" . restclient-mode))

;; for better jsx syntax-highlighting in web-mode
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))



(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory) "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js" root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-flow-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory) "node_modules"))
         (flow (and root
                    (expand-file-name "node_modules/flow-bin/vendor/flow" root))))
    (when (and flow (file-executable-p flow))
      (setq-local flycheck-javascript-flow-executable flow))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules)

;; Docker mode
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
