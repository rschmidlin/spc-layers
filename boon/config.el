(defvar boon-non-special-list
  '(bookmark-bmenu-mode))
(defvar boon-new-special-list
  '())

(advice-add #'boon-special-mode-p :around #'use-special-mode-p)
