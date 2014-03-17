;;
;; Emacs Configuration File
;;
;; Author: Fernando Carmona Varo <ferkiwi@gmail.com>
;; URL: https://github.com/Ferk/xdg_config/raw/master/HOME/.emacs.d/init.el
;; Related searches: argen teens, argen-teens, argen-teens mureil

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
         ("\\.rpy$"            . python-mode)
         ("\\.Xdefaults$"     . xrdb-mode)
         ("\\.Xenvironment$"  . xrdb-mode)
         ("\\.Xresources$"    . xrdb-mode)
         ("\\.tei$"           . xml-mode)
         ("\\.php$"           . php-mode)
         ("\\.clp$"           . clips-mode)
         ("\\.jl$"            . sawfish-mode)
         ("\\.md$"            . markdown-mode)
         ("\\.po$\\|\\.po\\." . po-mode)
         ("/[Mm]akefile\\." . makefile-mode)
         ("/crontab" . crontab-mode)
         ) auto-mode-alist))
(modify-coding-system-alist 'file "\\.po$\\|\\.po\\."
                            'po-find-file-coding-system)


;;(load-file "~/.emacs.d/cedet-1.1/common/cedet.elc")


;;;; Functions

(defun show-sublevel ()
  "Progressivelly unfolds the current level. First showing the childs and then the whole subtree if the command is issued a second time."
  (interactive)
  (if (eq last-command this-command)
      (show-subtree)
    (or (show-entry) (show-children))))

(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (local-set-key [S-M-left] 'hide-subtree)
	    (local-set-key [S-M-right] 'show-sublevel)
	    (local-set-key [S-M-up] 'outline-previous-visible-heading)
	    (local-set-key [S-M-down] 'outline-next-visible-heading)
	    ))


(defun shell-command-general (command arg)
  "Run a shell command passing the text of the region (if
selected) to it. Ant then replace the region with the output of the command (if no argument was passed to this function)."
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
  "Searches for duplicated lines in region (or whole buffer if no
mark active) and deletes them."
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
  "Indent the whole buffer"
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
  "Create/update tags file and load it silently."
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
  "Rerun etags and reload tags if tag not found and redo find-tag.
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
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

;;; autocompile
(add-hook 'after-save-hook 'autocompile)
;;;###autoload
(defun autocompile ()
  "Byte-compile the current file if it matchs ~/.emacs*.el
This is useful for making sure you didn't make some stupid mistake when
configuring, and also it will make emacs load faster."
  (interactive)
  (require 'bytecomp)
  (if
      (string-match (concat (getenv "HOME") "/\.emacs.*\.el")
                    (buffer-file-name))
      (byte-compile-file (buffer-file-name))))


;;;;
;; (defadvice barf-if-buffer-read-only (before ask-rooting-if-non-writable activate)
;;   (and buffer-read-only
;;        (not (file-writable-p (buffer-file-name)))
;;        (y-or-n-p "No permissions, switch to root?")
;;        (open-as-root)))
;;;; (barf-if-buffer-read-only)
;;;;;
;;(add-hook 'before-save-hook 'switch-to-root)
(defun open-as-root ()
  "Using tramp, switches to editting the current file as root."
  (interactive)
   (set-visited-file-name
    (concat "/sudo::" (buffer-file-name)))
   ;;(and (file-writable-p (buffer-file-name))
	 (setq buffer-read-only nil));)


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
;; 	(comint-next-input 1)
;;        (forward-line 1)))

;; (eval-after-load "gud"
;;   '(progn 
;;      (define-key gud-mode-map (kbd "<up>") 'smart-comint-up)
;;      (define-key gud-mode-map (kbd "<down>") 'smart-comint-down)))

;;;;;;;;;;;
(global-set-key (kbd "<M-gr>") (quote rgrep))
;;(global-set-key "\347r" (quote rgrep))

;; Create cache director if it doesn't exist
(mkdir "~/.cache/emacs" 't)

;; Write customize options for this machine in a different file 
;; (this file won't be under version control to allow for specific installations
;; to override the settings)
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file)
  (customize-save-customized))

;; Custom-theme files will be used instead to set settings
;; theme/ directory will be usedfor them (and not just .emacs.d)
(setq custom-theme-directory "~/.emacs.d/themes/")

;; Default themes to load if no other was set
(and (eq custom-enabled-themes '())
     (load-theme 'darkclean)
     (load-theme 'config-base))


