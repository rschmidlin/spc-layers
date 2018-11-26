(defvar boon-keybinding-minor-mode-map  (make-sparse-keymap))

;; Avoid x-esc-esc to lead to repeat command
(global-set-key (kbd "C-x <escape> <escape>") nil)

;; Make line movement consistent in the minibuffer
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-o") 'next-line)
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "M-i") 'dired-previous-line)))
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "M-o") 'dired-next-line)))
(add-hook 'helm-mode-hook (lambda () (define-key helm-find-files-map (kbd "M-i") 'helm-previous-line)))
(add-hook 'helm-mode-hook (lambda () (define-key helm-generic-files-map (kbd "M-i") 'helm-previous-line)))
(add-hook 'helm-mode-hook (lambda () (define-key helm-map (kbd "M-i") 'helm-previous-line)))
(add-hook 'helm-mode-hook (lambda () (define-key helm-map (kbd "M-o") 'helm-next-line)))

;; Elscreen usage
(define-key boon-command-map (kbd "ü") 'elscreen-clone)
(define-key boon-command-map (kbd "Ü") 'elscreen-goto)
(define-key boon-command-map (kbd "+") 'elscreen-kill)
(define-key boon-command-map (kbd "*") 'elscreen-kill-others)

;; Access spacemacs menu
(define-key boon-command-map (kbd spacemacs-boon-leader-key) 'spacemacs-cmds)

(defmacro get-quoted-major-mode-map-prefix-symbol()
  `(intern (concatenate 'string "spacemacs-" (symbol-name major-mode) "-map-prefix")))

(defun get-major-mode-map-prefix ()
  (and (boundp (get-quoted-major-mode-map-prefix-symbol)) (get-quoted-major-mode-map-prefix-symbol)))

(defun set-major-mode-map-prefix()
  (interactive)
  (let ((current-major-mode (get-major-mode-map-prefix)))
    (when current-major-mode
      (define-key boon-command-map (kbd spacemacs-boon-major-mode-leader-key) current-major-mode))))

(add-hook 'buffer-list-update-hook 'set-major-mode-map-prefix)
 
;; Use M-SPC to go back to command mode
(define-key boon-keybinding-minor-mode-map (kbd "M-SPC") 'boon-set-command-state)

;; Special help keys like pressing escape for C-g, TAB for searching further
(add-hook 'ivy-mode-hook (lambda () (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)))
(define-key isearch-mode-map (kbd "TAB") 'isearch-repeat-forward)

;; Also define commands for C-x that are available from x in Boon
;; (global-set-key (kbd "C-x o") 'ace-window)
(define-key boon-keybinding-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key boon-command-map (kbd "x x") 'helm-M-x)
(global-set-key (kbd "C-x x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(define-key boon-command-map (kbd "x b") 'helm-mini)

;; Define new commands for command mode
(define-key boon-command-map (kbd ",") 'ace-window)
(define-key boon-command-map (kbd "r") 'helm-swoop)
(define-key boon-command-map (kbd "m") 'split-window-below)
(define-key boon-command-map (kbd "M") 'split-window-right)
(define-key boon-command-map (kbd ".") 'ace-delete-window)
(define-key boon-command-map (kbd ":") 'delete-other-windows)
(define-key boon-command-map (kbd "T") 'query-replace)
(define-key boon-command-map (kbd "_") 'undo-tree-redo)
(define-key boon-command-map (kbd "M-_") 'undo-tree-visualize)
(define-key boon-command-map (kbd "M-f") 'browse-kill-ring)
(define-key boon-goto-map (kbd "i") 'helm-imenu)

;; New keys on C-x or C-c groups avoiding necessity of pressing control
(global-set-key (kbd "C-x t") 'vr/query-replace)
(global-set-key (kbd "C-x ö") 'save-buffer)
(global-set-key (kbd "C-x j") 'helm-find-files)
(global-set-key (kbd "C-x p") 'recenter-top-bottom)
(global-set-key (kbd "C-x c") 'eval-last-sexp)
(global-set-key (kbd "C-x y") 'comment-dwim)
(global-set-key (kbd "C-x w") 'find-alternate-file)
(global-set-key (kbd "C-x i") 'ibuffer)
(define-key boon-keybinding-minor-mode-map (kbd "C-c C-r") 'helm-ag)

;; Buffer and window control
(define-key boon-keybinding-minor-mode-map (kbd "C-c C-l") 'make-frame)
(define-key boon-keybinding-minor-mode-map (kbd "C-c C-k") 'delete-frame)
(global-set-key (kbd "C-x l") 'xah-new-empty-buffer)

;; Help menu on J
(defvar boon-help-map)
(define-prefix-command 'boon-help-map)
(set-keymap-parent boon-help-map help-map)
(define-key boon-command-map (kbd "J") boon-help-map)

;; Include extended indexer navigation for Boon
(define-key boon-keybinding-minor-mode-map (kbd "C-c C-z") 'xref-peek-definitions)
(define-key boon-keybinding-minor-mode-map (kbd "C-c C-f") 'projectile-find-file)
(define-key boon-keybinding-minor-mode-map (kbd "C-c C-i") 'cscope-find-functions-calling-this-function)
(define-key boon-keybinding-minor-mode-map (kbd "C-c C-o") 'cscope-find-called-functions)
(define-key boon-keybinding-minor-mode-map (kbd "C-c C-p") 'cscope-find-this-symbol)

(define-key boon-keybinding-minor-mode-map (kbd "C-M-0") 'insert-right-curly-brace)

(define-key boon-command-map (kbd "z") 'raul-find-definitions)
(define-key boon-command-map (kbd "Z") 'raul-find-references)
(define-key boon-command-map (kbd "N") 'raul-pop-marker)

;; Start org-mode
(define-key boon-keybinding-minor-mode-map (kbd "C-c C-w") 'org-capture)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'load-current-file)
(add-hook 'c-mode-hook (lambda () (define-key c-mode-map (kbd "C-c C-c") 'compile)))
(add-hook 'c++-mode-hook (lambda () (define-key c++-mode-map (kbd "C-c C-c") 'compile)))
(add-hook 'python-mode-hook (lambda () (define-key python-mode-map (kbd "C-c C-c") 'raul-send-buffer-to-python)))

(define-minor-mode boon-keybinding-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter boon-keybinding " boon-keybinding"
  :keymap boon-keybinding-minor-mode-map)

(provide 'boon-keybinding)
