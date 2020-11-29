;;; init.el --- Emacs Configuration File
;; -*- lexical-binding: t; -*-
;;
;; Author: Fernando Carmona Varo <ferkiwi@gmail.com>
;; URL: https://github.com/Ferk/xdg_config/raw/master/HOME/.emacs.d/init.el

;;; Code:

;;; Startup optimizations
(defvar init-startup-time (current-time)) ; measure time taken
;; Temporarily unset to prevent Emacs from loading extra files during startup
(defvar last-file-name-handler-alist file-name-handler-alist)
;; The garbage collector eats up a lot of time during startup, so up its
;; memory threshold to prevent it from running at all
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
;;(setq load-prefer-newer noninteractive)
;; --------

;; Replace the annoying "yes or no" questions to a single keystroke "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable over-write mode! Never good! Pain in the posterior!
(put 'overwrite-mode 'disabled t)

;;(require 'semantic-ia)
;;(require 'semantic-gcc)


;;;; Hooks!
;; Visual lines
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
;; But truncated for dired
(add-hook 'dired-mode-hook
          (lambda () (setq truncate-lines t)))
;; auto update the pdf when regenerated
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;; C-mode Hooks
(add-hook 'c-mode-hook
          (lambda ()

            ;; When no makefile, just compile it with "make -k $file"
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (concat "make -k "
                           (file-name-sans-extension buffer-file-name)))
              ;;(flymake-mode)
              )

            ;; highlighting for preprocessor
            (cpp-highlight-buffer t)

            ;; activate semantic code helpers
            ;;(semantic-load-enable-excessive-code-helpers)
            ;;(semantic-load-enable-all-exuberent-ctags-support)

            ));; end of C-mode Hooks


;; outline mode for folding code, will be used for every font-locked mode
(add-hook 'c-mode-hook 'outline-minor-mode)
(add-hook 'shell-script-mode-hook 'outline-minor-mode)
;; electric indent mode
(add-hook 'c-mode-hook 'electric-indent-mode)
(add-hook 'shell-script-mode-hook 'electric-indent-mode)

;;; Set up which modes to use for which file extensions
(setq auto-mode-alist
      (append
       '(
         ("\\.h$"             . c++-mode)
         ("\\.dps$"           . pascal-mode)
         ("\\.py$"            . python-mode)
         ("\\.rpy$"           . python-mode)
         ("\\.Xdefaults$"     . xrdb-mode)
         ("\\.Xenvironment$"  . xrdb-mode)
         ("\\.Xresources$"    . xrdb-mode)
         ("\\.tei$"           . xml-mode)
         ("\\.php$"           . php-mode)
         ("\\.clp$"           . clips-mode)
         ("\\.jl$"            . sawfish-mode)
         ("\\.md$"            . markdown-mode)
         ("\\.po$\\|\\.po\\." . po-mode)
         ("/[Mm]akefile\\."   . makefile-mode)
         ("/crontab"          . crontab-mode)
         ) auto-mode-alist))
(modify-coding-system-alist 'file "\\.po$\\|\\.po\\."
                            'po-find-file-coding-system)


;;(load-file "~/.emacs.d/cedet-1.1/common/cedet.elc")


;;;; Functions

(autoload 'outline-show-subtree "outline-mode")
(autoload 'outline-show-entry "outline-mode")
(autoload 'outline-show-children "outline-mode")
(defun show-sublevel ()
  "Progressivelly unfolds the current level.
First showing the childs and then the whole subtree if the command is issued a second time."
  (interactive)
  (if (eq last-command this-command)
      (outline-show-subtree)
    (or (outline-show-entry) (outline-show-children))))

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (local-set-key [M-S-left] 'hide-subtree)
            (local-set-key [M-S-right] 'show-sublevel)
            (local-set-key [M-S-up] 'outline-previous-visible-heading)
            (local-set-key [M-S-down] 'outline-next-visible-heading)
            ))


(defun shell-command-general (command arg)
  "Run shell command on region.
Run COMMAND in the shell with ARG arguments, with the region text as input
\(if selected), then replace the region with the output of the command (if
no argument was passed to this function)."
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (let ((begin (if mark-active (region-beginning) 0))
        (end (if mark-active (region-end) 0)))
    (if (= begin end)
        ;; No active region
        (shell-command command arg)
      ;; Active region
      (if (eq arg nil)
          (shell-command-on-region begin end command t t)
        (shell-command-on-region begin end command)))))
(global-set-key [f3] 'shell-command-general)

(defun uniq-lines ()
  "Search for duplicated lines in region (or whole buffer) and deletes them."
  (interactive)
  (let ((begin (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max)))
        (count 0)
        )
    (save-excursion
      (save-restriction
        (narrow-to-region begin end)
        (goto-char (point-min))
        (while (not (eobp))
          (kill-line 1)
          (yank)
          (let ((next-line (point)))
            (while
                (re-search-forward
                 (format "^%s" (regexp-quote (car kill-ring))) nil t)
              (replace-match "" nil nil)
              (setq count (+ count 1)))
            (goto-char next-line)))))
    (princ (format "%d duplicated lines found" count))))

;;; indent buffer
;;;###autoload
(defun indent-whole-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil)
  ;;(untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defalias 'iwb 'indent-whole-buffer)
(define-key global-map
  [menu-bar edit indent] '("Indent the whole buffer" . indent-whole-buffer))

;; Compilation
(define-key global-map
  "\C-cc" 'compile)

;;; eTAGS
(defun create-tags (dir-name langs)
  "Create/update tags file and load it silently.
Argument DIR-NAME Directory where to create/update tags file.
Argument LANGS Languages to tag from (none for default)."
  (interactive "DDirectory: \nsLanguages to tag from (none for default): ")
  (or langs (setq langs "Make,Java,Lua,Lisp,C,C++,PHP"))
  ;;(setq langmap "c:.c.h")
  (and
   (shell-command
    ;;(format "ctags -f %s/TAGS -e -R %s" dir-name (directory-file-name dir-name))
    (concat "cd " dir-name " && ctags -eR " ;;--langmap=" langmap
            (and langs (concat " --languages=" langs))))

   ;;(format "cd %s && ctags -eR --languages=\"%s\"" dir-name langs))
   (let ((tags-revert-without-query t))  ; don't query, revert silently
     (visit-tags-table dir-name nil))))

(defadvice find-tag (around refresh-tags activate)
  "Rerun etags and reload tags if tag not found and redo `find-tag'.
If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             ;;(create-tags "." (concat "*." extension))
             ad-do-it))))

;;; Use the C-w Unix tty behaviour of deleting word backward
(defadvice kill-region (before unix-werase activate compile)
  "Kill selected region.
When called interactively with no active region, delete a single word backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

;;; autocompile
(add-hook 'after-save-hook 'autocompile)
;;;###autoload
(defun autocompile ()
  "Byte-compile the current file if matching '~/.emacs*.el'.
This is useful for making sure you didn't make some stupid mistake when
configuring, and also it will make Emacs load faster."
  (interactive)
  (require 'bytecomp)
  (if
      (and (string-match (concat (getenv "HOME") "/\.emacs.*\.el")
                         (buffer-file-name))
           (not (string-match "\.dir-locals" (buffer-file-name)))
           (not (string-match "\.emacs\.d/elpa" (buffer-file-name))))
      (byte-compile-file (buffer-file-name))))


;;;;
;; (defadvice barf-if-buffer-read-only (before ask-rooting-if-non-writable activate)
;;   (and buffer-read-only
;;        (not (file-writable-p (buffer-file-name)))
;;        (y-or-n-p "No permissions, switch to root? ")
;;        (open-as-root)))
;;;; (barf-if-buffer-read-only)
;;;;;
;;(add-hook 'before-save-hook 'switch-to-root)
(defun open-as-root ()
  "Using tramp, switch to editting the current file as root."
  (interactive)
  (set-visited-file-name
   (concat "/sudo::" (buffer-file-name)))
  ;;(and (file-writable-p (buffer-file-name))
  (setq buffer-read-only nil));)
(defalias 'sudo 'open-as-root)

;; (defun smart-comint-up ()
;;    "Implement the behaviour of the up arrow key in comint mode.  At
;; the end of buffer, do comint-previous-input, otherwise just move in
;; the buffer."
;;    (interactive)
;;    (if (= (point) (point-max))
;;        (comint-previous-input 1)
;;      (previous-line 1)))

;; (defun smart-comint-down ()
;;    "Implement the behaviour of the down arrow key in comint mode.  At
;; the end of buffer, do comint-next-input, otherwise just move in the
;; buffer."
;;    (interactive)
;;      (if (= (point) (point-max))
;;      (comint-next-input 1)
;;        (forward-line 1)))

;; (eval-after-load "gud"
;;   '(progn
;;      (define-key gud-mode-map (kbd "<up>") 'smart-comint-up)
;;      (define-key gud-mode-map (kbd "<down>") 'smart-comint-down)))

;;; from purcell/emacs.d
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      (assoc package package-archive-contents)
    (progn
      (or (assoc package package-archive-contents) no-refresh (package-refresh-contents))
      (package-install package))))


;;;;;;;;;;;
;;; Init customizations

;; Create cache directory if it doesn't exist
(mkdir "~/.cache/emacs" 't)

;; Write customize options for this machine in a different file
;; (this file won't be under version control to allow for specific installations
;; to override the settings)
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file)
  (customize-save-customized))

;; Custom-theme files
;; load from themes/ subdirectory (and not just .emacs.d)
(setq custom-theme-directory "~/.emacs.d/themes/")

;; Default themes to load if no other was set
(with-demoted-errors
    (and (eq custom-enabled-themes '())
         (load-theme 'darkclean)
         (load-theme 'config-base))) ; all my custom-izable configuration is in here, this is not a color theme

(global-set-key (kbd "<M-gr>") (quote rgrep))
;;(global-set-key "\347r" (quote rgrep))

(require 'uniquify)


;;; Extra package installation
(with-demoted-errors
    (progn ; don't abort if some cannot be installed

      ;; gnu elpa packages:
      (require-package 'flymake  '(1  0)    t)
      (require-package 'ack      '(1 10)    t)
      (require-package 'org      '(9  3)    t)

      (and (require-package 'js2-mode '(20190219) t)
           (add-hook 'js2-mode-hook 'flymake-jslint-load))

      ;; melpa packages
      (require-package 'auto-complete   '(1  5)     t)
      (require-package 'smart-tabs-mode '(1  1)     t)
      (require-package 'git-commit      '(2 90)     t)
      (require-package 'gitconfig-mode  '(1  2)     t)
      (require-package 'markdown-mode   '(2  4)     t)
      (require-package 'yaml-mode       '(0  0)     t)
      (require-package 'po-mode         '(0 21)     t)
      (require-package 'php-mode        '(1 23)     t)
      (require-package 'go-mode         '(1  5)     t)
      (require-package 'lua-mode        '(20201010) t)

      (and (require-package 'flymake-shell  '(0 8) t)
           (add-hook 'sh-mode-hook 'flymake-shell-load))

      (and (require-package 'flymake-eslint '(1 5) t)
           (add-hook 'js-mode 'flymake-eslint-enable)
           (add-hook 'js2-mode 'flymake-eslint-enable))

      (and (require-package 'unicode-fonts  '(0 4) t)
           (unicode-fonts-setup))

      )) ; end of package setup



;;; Reset startup optimizations

;; after startup, it is important you reset gc to some reasonable default. A large
;; gc-cons-threshold will cause freezing and stuttering during long-term
;; interactive use. I find these are nice defaults:
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold 16777216
         gc-cons-percentage 0.1
         file-name-handler-alist last-file-name-handler-alist))
    (message "---- Emacs init-el loaded! [seconds taken: %.3f] ----"
            (time-to-seconds (time-since init-startup-time))))

;;; init.el ends here
