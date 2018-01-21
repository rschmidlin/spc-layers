(when (configuration-layer/package-usedp 'boon)

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

  )
