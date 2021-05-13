
;; ;; Flyckech
;; ;; (add-to-list 'js-mode-hook 'flycheck-mode)
(setq-default flycheck-disabled-checkers '(javascript-jscs python-pylint))
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)


(add-hook 'web-mode-hook
           (lambda ()
             (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                       (string-equal "ts" (file-name-extension buffer-file-name)))
               (yas-activate-extra-mode 'js-mode))))





;; (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Less
(add-to-list 'load-path "~/.emacs.d/less/")
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))


(require 'web-mode)
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
                (yas-activate-extra-mode 'js-mode)))))



;; GO

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
