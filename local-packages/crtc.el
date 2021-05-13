;; Copy region to clipboard
(defun copy-region-to-clipboard ()
  (interactive)
  (if (eq system-type 'darwin)
      (shell-command-on-region
       (region-beginning) (region-end)
       "pbcopy && echo 'Region copied to clipboard'")
    (message "Clipboard copy not supported")))

(global-set-key (kbd "C-c C-k") 'copy-region-to-clipboard)
