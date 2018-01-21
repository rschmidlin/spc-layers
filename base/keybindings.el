(global-set-key (kbd "C-M-q") 'insert-commercial-at)
(global-set-key (kbd "C-M-+") 'insert-tilde)
(global-set-key (kbd "C-M-7") 'insert-left-curly-brace)
(global-set-key (kbd "C-M-8") 'insert-left-squared-bracket)
(global-set-key (kbd "C-M-9") 'insert-right-squared-bracket)
(global-set-key (kbd "C-M-0") 'insert-right-curly-brace)
(global-set-key (kbd "C-M-ß") 'insert-backslash)
(global-set-key (kbd "C-M-<") 'insert-pipe)

;; Special stuff
(global-set-key (kbd "M-1") 'neotree-toggle)
(when (configuration-layer/package-usedp 'magit)
  (global-set-key (kbd "C-x g") 'magit-status))
