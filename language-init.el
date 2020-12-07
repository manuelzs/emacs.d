;; Smpartparens for elisp mode
(add-to-list 'load-path "~/.emacs.d/elpa/dash-20180910.1856/")
(require 'dash)
(add-to-list 'load-path "~/.emacs.d/elpa/smartparens-20190128.1747/")
(require 'smartparens)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(define-key smartparens-mode-map (kbd "C-c C-s s") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-c C-s b") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-c C-s k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-c C-s c") 'sp-copy-sexp)

;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yas/snippets"))
(yas-global-mode 1)

;; ;; Flyckech
(require 'flycheck)

;; ;; (add-to-list 'js-mode-hook 'flycheck-mode)
(setq-default flycheck-disabled-checkers '(javascript-jscs python-pylint))
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; Auto complete mode
(require 'auto-complete)
;; (add-to-list 'ac-modes 'javascript-mode)
;; (add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'coffee-mode)
;; (global-auto-complete-mode t)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "<f9>") 'er/expand-region)

;; PYTHON
;; Python autocomplete
(flycheck-add-mode 'python-flake8 'python-mode)
;; (add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'blacken-mode)

;; Isort
;; (add-hook 'before-save-hook 'py-isort-before-save)

;; RUBY
(add-to-list 'load-path "~/.emacs.d/ruby-mode/")
;; Load ruby mode when needed
(autoload 'ruby-mode "ruby-mode" "Ruby mode" t )
;; Assign .rb and .rake files to use ruby mode
(setq auto-mode-alist (cons '("\\.rb\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
;; Show syntax highlighting when in ruby mode
(add-hook 'ruby-mode-hook '(lambda () (font-lock-mode 1)))

;; html-mode for JSTs
(add-to-list 'auto-mode-alist '("\\.jst" . html-mode))

;; Scala mode
(add-to-list 'load-path "~/.emacs.d/scala-mode/")
(require 'scala-mode-auto)

;; (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Less
(add-to-list 'load-path "~/.emacs.d/less/")
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))


;; yaml-mode
(add-to-list 'load-path "~/.emacs.d/yaml/")
(require 'yaml-mode)

;; Salt extensions
(add-to-list 'auto-mode-alist '("\\.sls" . yaml-mode))

;; Mustache mode
(add-to-list 'load-path "~/.emacs.d/mustache/")
(require 'mustache-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; for better jsx syntax-highlighting in web-mode
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(add-hook 'web-mode-hook
          (lambda ()
            (let ((ext (file-name-extension buffer-file-name)))
              (when (or (string-equal "jsx" ext) (string-equal "js" ext))
                (flycheck-mode)
                (setup-tide-mode)
                (yas-activate-extra-mode 'js-mode)))))

;; ;; Prettier for web-mode
;; (require 'prettier-js)
;; (defun web-mode-init-prettier-hook ()
;;   (message "PRETTIER HOOK")
;;   (add-node-modules-path)
;;   (message "PRETTIER HOOK add node_modules")
;;   (prettier-js-mode)
;;   (message "PRETTIER HOOK DONE"))
;; (add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

;; Terraform (HCL)

(require 'terraform-mode)
(add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)

;; ;; (add-hook 'web-mode-hook
;; ;;           (lambda ()
;; ;;             (set (make-local-variable 'yas-extra-modes) 'js-mode)))
;; (yas-reload-all 1)

;; ;; Elm
;; (require 'elm-mode)
;; (require 'company)
;; (add-hook 'flycheck-mode-hook #'flycheck-elm-setup)
;; (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

;; (add-to-list 'company-backends 'company-elm)
;; (add-to-list 'elm-mode-hook 'flycheck-mode)
;; (add-to-list 'elm-mode-hook 'company-mode)

;; ;; JS Tern
;; ;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))
;; ;; (eval-after-load 'tern
;; ;;   '(progn
;; ;;      (require 'tern-auto-complete)
;; ;;      (tern-ac-setup)))
;; ;; (setq tern-command '("/usr/local/bin/tern"))

;; ;; JS2
;; ;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; ;; (add-hook 'js2-mode-hook 'ac-js2-mode)
;; ;; (setq js2-highlight-level 3)

;; GO
(require 'go-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") #'godef-jump-other-window)))

(add-to-list 'load-path "/Users/manuel/go/src/golang.org/x/lint/misc/emacs/")
(require 'golint)


(defun my-go-mode-hook ()
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;;   ;; Customize compile command to run go build
  ;;   (if (not (string-match "go" compile-command))
  ;;       (set (make-local-variable 'compile-command)
  ;;            "go generate && go build -v && go test -v && go vet"))
  ;;   ;; Godef jump key binding
  ;;   (local-set-key (kbd "M-.") 'godef-jump)
  ;;   ;; Use company mode
  ;;   (set (make-local-variable 'company-backends) '(company-mode))
  ;;   (company-mode)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;; Go Test
(define-key go-mode-map (kbd "C-c t f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-c t t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-c t p") 'go-test-current-project)
(define-key go-mode-map (kbd "C-c C-c") 'go-run)

;; ;; Clojure
;; (setq cider-lein-command "/usr/local/bin/lein")
;; (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; (add-hook 'clojure-mode-hook 'smartparens-strict-mode)

;; ;; Hoplon
;; (add-to-list 'auto-mode-alist '("\\.hl$" . clojure-mode))

;; ;;(load-file "~/.emacs.d/elpa/spinner-1.7.1/spinner.el")
;; ;; (require 'clj-refactor)

;; ;; (defun my-clojure-mode-hook ()
;; ;;    (clj-refactor-mode 1)
;; ;;    (cljr-add-keybindings-with-prefix "C-c C-m"))

;; ;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; ;; Typescript
;; (require 'typescript-mode)
;; (add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx" . typescript-mode))

;; TabNine
(require 'company-tabnine)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;(flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (auto-complete-mode -1)
  (company-mode +1)
  (add-to-list 'company-backends #'company-tabnine))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-hook 'web-mode-hook
           (lambda ()
             (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                       (string-equal "ts" (file-name-extension buffer-file-name)))
               (yas-activate-extra-mode 'js-mode)
               (setup-tide-mode))))


;; (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . rome-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . rome-mode))



