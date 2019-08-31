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
