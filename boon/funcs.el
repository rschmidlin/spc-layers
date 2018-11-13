(when (configuration-layer/package-usedp 'boon)
  
  (defun raul-find-definitions ()
    (interactive)
    (cond
     ((eq major-mode 'python-mode) (anaconda-mode-find-definitions))
     ((eq major-mode 'c++-mode) (if (not (eq system-type 'windows-nt))
                                    (rtags-find-symbol-at-point)
                                  (ggtags-find-tag-dwim (ggtags-read-tag 'definition current-prefix-arg))))
     ((eq major-mode 'c-mode) (ggtags-find-tag-dwim (ggtags-read-tag 'definition current-prefix-arg)))
     (t (xref-find-definitions (xref--read-identifier "Find definitions of: ")))))

  (defun raul-find-references ()
    (interactive)
    (cond
     ((eq major-mode 'python-mode) (anaconda-mode-find-references))
     ((eq major-mode 'c++-mode) (if (not (eq system-type 'windows-nt))
                                    (rtags-find-references-at-point)
                                  (ggtags-find-reference (ggtags-read-tag 'reference current-prefix-arg))))
     ((eq major-mode 'c-mode) (ggtags-find-reference (ggtags-read-tag 'reference current-prefix-arg)))
     (t (xref-find-references (xref--read-identifier "Find references of: ")))))

  (defun raul-pop-marker ()
    (interactive)
    (cond
     ((eq major-mode 'python-mode) (xref-pop-marker-stack))
     ((eq major-mode 'c++-mode) (if (not (eq system-type 'windows-nt))
                                    (rtags-location-stack-back)
                                  (ggtags-prev-mark)))
     ((eq major-mode 'c-mode) (ggtags-prev-mark))
     (t (xref-pop-marker-stack))))
  
  ;; Load current file
  (defun load-current-file ()
    "Execute file corresponding to current buffer"
    (interactive)
    (load-file (buffer-file-name)))

  (defun raul-send-buffer-to-python ()
    "Send complete buffer to Python"
    (interactive)
    (python-shell-send-buffer t))
  
  (defun xah-new-empty-buffer ()
    "Create a new empty buffer.
     New buffer will be named “untitled” or “untitled<2>”,
     “untitled<3>”, etc.

     URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
     Version 2016-12-27"
    (interactive)
    (let (($buf (generate-new-buffer "untitled")))
      (switch-to-buffer $buf)
      (funcall initial-major-mode)
      (setq buffer-offer-save t)))

  (defun load-current-file ()
    "Execute file corresponding to current buffer"
    (interactive)
    (load-file (buffer-file-name)))

  (defun use-special-mode-p (old-function &rest arguments)
    "Function to substitute boon-special-mode-p and disallow use of special mode for some major-modes"
    (or (memq major-mode boon-new-special-list)
        (and (apply old-function arguments)
             (not (memq major-mode boon-non-special-list)))))

  (defun insert-commercial-at()
    "Insert a commercial at before point."
    (interactive)
    (insert "@"))

  (defun insert-tilde()
    "Insert a tilde before point."
    (interactive)
    (insert "~"))

  (defun insert-left-curly-brace()
    "Insert a left curly brace before point."
    (interactive)
    (insert "{"))

  (defun insert-right-curly-brace()
    "Insert a right curly brace before point."
    (interactive)
    (insert "}"))

  (defun insert-left-squared-bracket()
    "Insert a left square bracket before point."
    (interactive)
    (insert "["))

  (defun insert-right-squared-bracket()
    "Insert a right square bracket before point."
    (interactive)
    (insert "]"))

  (defun insert-backslash()
    "Insert a backslash before point."
    (interactive)
    (insert "\\"))

  (defun insert-pipe()
    "Insert a pipe before point."
    (interactive)
    (insert "|"))
  
  (defun make-peek-frame (find-definition-function &rest args)
  "Make a new frame for peeking definition"
  (when (or (not (fboundp 'rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let (summary
          doc-frame
          x y
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; 1. Find the absolute position of the current beginning of the symbol at point, ;;
          ;; in pixels.                                                                     ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (abs-pixel-pos (save-excursion
                           (beginning-of-thing 'symbol)
                           (window-absolute-pixel-position))))
      (setq x (car abs-pixel-pos))
      ;; (setq y (cdr abs-pixel-pos))
      (setq y (+ (cdr abs-pixel-pos) (frame-char-height)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 2. Create a new invisible frame, with the current buffer in it. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq doc-frame (make-frame '((minibuffer . nil)
                                    (name . "*RTags Peek*")
                                    (width . 80)
                                    (visibility . nil)
                                    (height . 15))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 3. Position the new frame right under the beginning of the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (set-frame-position doc-frame x y)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 4. Jump to the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (with-selected-frame doc-frame
        (apply find-definition-function args)
        (read-only-mode)
        (when (boundp 'semantic-stickyfunc-mode) (semantic-stickyfunc-mode -1)))
      ;; (recenter-top-bottom 0))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 5. Make frame visible again ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (make-frame-visible doc-frame))))

(defun xref-peek-definitions ()
  "Peek at definition using xref-find-definitions"
  (interactive)
  (let ((func (lambda ()
				(raul-find-definitions))))
	(make-peek-frame func)))

; Generate cscope.files from a directory list
(defun build-cscope-file (directories &optional target-directory)
  "Generate cscope.file for a list of DIRECTORIES, optionally in TARGET-DIRECTORY."
  (let
	  (
	   (file (if target-directory
				 (concat target-directory "/cscope.files")
			   "cscope.files"))
	   )
	(shell-command (concat "rm -rf " file))
	(let ((command ""))
	  (dolist (dir directories)
        (setq command "")
		(setq command (concat command "find " dir " -name *.cpp >> " file " && "))
		(setq command (concat command "find " dir " -name *.hpp >> " file " && "))
		(setq command (concat command "find " dir " -name *.tpp >> " file " && "))
		(setq command (concat command "find " dir " -name *.c >> " file " && "))
		(setq command (concat command "find " dir " -name *.h >> " file " && "))
        (setq command (substring command 0 -4))
        (shell-command command))))
  (message "cscope file generated"))

 ; Functions to create Ctags and Cscope files
(defun build-ctags-from-list (filename &optional target-directory)
  (interactive "f")
  (if target-directory
	  (call-process path-to-ctags nil (get-buffer-create "process-output") t "-e" "--extra=+fq" "-L" filename "-f" (concat target-directory "/TAGS"))
	(call-process path-to-ctags nil (get-buffer-create "process-output") t "-e" "--extra=+fq" "-L" filename)))

(defun build-cscope-from-list (filename &optional target-directory)
  (interactive "f")
  (if target-directory
	  (let ((default-directory target-directory))
		(call-process "cscope" nil (get-buffer-create "process-output") t "-U" "-b" "-i" filename))
	(call-process "cscope" nil (get-buffer-create "process-output") t "-U" "-b" "-i" filename))
	(message (concat "Cscope file built successfully for " filename)))

(defun build-gtags-from-list (filename &optional target-directory)
  (interactive "f")
  (if target-directory
	  (let ((default-directory target-directory))
		(call-process "gtags" nil (get-buffer-create "process-output") t "-f" filename))
	(call-process "gtags" nil (get-buffer-create "process-output") t "-f" filename))
  (message (concat "GNU Global tags built successfully for " filename)))

  )
