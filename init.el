(setq backup-directory-alist `(("." . "~/.emacs_backups")))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))
;;   (exec-path-from-shell-copy-env "GOPATH")
;;   (add-to-list 'load-path (getenv "GOPATH")))


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

;; Add pony mode
(add-to-list 'load-path "~/.emacs.d/pony-mode/src/")
(if (not (file-exists-p "~/.emacs.d/pony-mode/src/pony-mode.elc"))
    (byte-compile-file "~/.emacs.d/pony-mode/src/pony-mode.el"))
(require 'pony-mode)

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
(add-to-list 'load-path "~/.emacs.d/elpa/ido-vertical-mode-20160429.1037/")
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)


;; Scala mode
(add-to-list 'load-path "~/.emacs.d/scala-mode/")
(require 'scala-mode-auto)

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(load "~/.emacs.d/my-packages.el")

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
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-input-method "TeX")
 '(ido-enable-last-directory-history nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0)
 '(ido-record-commands nil)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-global-externs
   (list "window" "define" "require" "module" "exports" "process" "Buffer" "__dirname" "Parse" "sessionStorage" "localStorage" "describe" "it" "FileReader" "analytics" "setTimeout" "btoa" "atob" "FormData" "xdescribe" "xit" "context" "beforeEach"))
 '(powerline-utf-8-separator-left 9622)
 '(powerline-utf-8-separator-right 9623)
 '(projectile-use-git-grep t)
 '(python-shell-interpreter "ipython")
 '(require-final-newline t)
 '(send-mail-function (quote mailclient-send-it))
 '(split-height-threshold 100)
 '(split-width-threshold 190)
 '(webpack-server-host "0.0.0.0"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :background "color-22"))))
 '(diff-removed ((t (:inherit diff-changed :background "color-88"))))
 '(font-lock-function-name-face ((t (:foreground "color-33"))))
 '(font-lock-string-face ((t (:foreground "color-128"))))
 '(highlight ((t (:background "color-235"))))
 '(magit-diff-added ((t (:background "#222222" :foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:background "#111111" :foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t (:background "color-234" :foreground "color-245"))))
 '(magit-diff-removed ((t (:background "#222222" :foreground "#cc2222"))))
 '(magit-diff-removed-highlight ((t (:background "#111111" :foreground "#cc2222"))))
 '(magit-section-highlight ((t (:background "color-234"))))
 '(minibuffer-prompt ((t (:foreground "Blue2"))))
 '(region ((t (:background "color-234"))))
 '(secondary-selection ((t (:background "color-130"))))
 '(smerge-markers ((t (:background "color-234"))))
 '(smerge-mine ((t (:background "color-233"))))
 '(smerge-other ((t (:background "color-233")))))

;; Python Tools
;; (add-to-list 'load-path "~/.emacs.d/pytools/")

;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "~/.emacs.d/pytools/pyflymake.py" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pylint-init)))

;; Activate flymake on file visit
;;(add-hook 'find-file-hook 'flymake-find-file-hook)

;;(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;;    (setq flymake-check-was-interrupted t))
;;(ad-activate 'flymake-post-syntax-check)

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

(load-file "~/.emacs.d/projectile-local.el")
(projectile-bs-global-mode)

;; Guru mode with warnings only
;; (guru-global-mode +1)
;; (setq guru-warn-only t)

;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yas"))
(yas-global-mode 1)

(require 'flx-ido)
(flx-ido-mode 1)

;; Python autocomplete
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; JS Tern
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))
(setq tern-command '("/usr/local/bin/tern"))

;; JS2
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)


;; Auto complete mode
(require 'auto-complete)
(add-to-list 'ac-modes 'javascript-mode)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'coffee-mode)
(global-auto-complete-mode t)

;; Load Flymake
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

;; Coffee Options

;; GO
(require 'go-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") #'godef-jump)))

;; (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")

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
  (local-set-key (kbd "M-.") 'godef-jump))
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

(load-file "~/.emacs.d/elpa/spinner-1.7.1/spinner.el")
(require 'clj-refactor)

(defun my-clojure-mode-hook ()
   (clj-refactor-mode 1)
   (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Set fill column
(setq-default fill-column 80)

;; Copy region to clipboard
(global-set-key
 (kbd "C-c C-k")
 (lambda ()
   (interactive)
   (if (eq system-type 'darwin)
       (shell-command-on-region (region-beginning) (region-end) "pbcopy")
     (message "Clipboard copy not supported"))))

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
