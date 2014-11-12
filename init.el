(setq backup-directory-alist `(("." . "~/.emacs_backups")))

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
(require 'ido)
(ido-mode t)


;; Scala mode
(add-to-list 'load-path "~/.emacs.d/scala-mode/")
(require 'scala-mode-auto)


;; js2 mode
(add-to-list 'load-path "~/.emacs.d/js2/")
(if (not (file-exists-p "~/.emacs.d/js2/js2-mode.elc"))
    (byte-compile-file "~/.emacs.d/js2/js2-mode.el"))

(autoload 'js2-mode "js2-mode" nil t)

(require 'js2-mode)

;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(when (not (daemonp))
  (require 'zone)
  (zone-when-idle 180))

(add-to-list 'load-path "~/.emacs.d/elpa/")
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
(add-to-list 'load-path "~/.emacs.d/")
(require 'yaml-mode)
;; Salt extensions
(add-to-list 'auto-mode-alist '("\\.sls" . yaml-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(python-shell-interpreter "ipython")
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :background "color-22"))))
 '(diff-removed ((t (:inherit diff-changed :background "color-88"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue2"))))
 '(font-lock-string-face ((t (:foreground "color-128"))))
 '(highlight ((t (:background "color-235"))))
 '(minibuffer-prompt ((t (:foreground "Blue2"))))
 '(region ((t (:background "color-234")))))

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

;; Mustache mode
(add-to-list 'load-path "~/.emacs.d/mustache/")
(require 'mustache-mode)

(projectile-global-mode)

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
