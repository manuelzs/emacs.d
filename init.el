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