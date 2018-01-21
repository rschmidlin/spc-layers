;; Configure Emacs to use find and grep from MSYS
(setenv "PATH"
        (concat
         ;; Change this with your path to MSYS bin directory
         "C:\\MinGW\\msys\\1.0\\bin;"
         "/usr/local/bin:"
         (getenv "PATH")))

;; Include AG executable
(defvar ag-executable "c:/Users/SESA452110/MyPrograms/bin/ag.exe")

;; Path to ctags
(defvar path-to-ctags "c:/Users/SESA452110/MyPrograms/bin/ctags.exe")

(when (configuration-layer/package-usedp 'xcscope)
  (setq cscope-do-not-update-database t))

