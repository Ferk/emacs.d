;;; config-base-theme.el --- Custom configuration options

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Fernando Carmona Varo <ferkiwi@gmail.com>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; <http://www.gnu.org/licenses/>.

;;;


(deftheme config-base
  "Custom configuration options.")

(provide-theme 'config-base)
(custom-theme-set-variables 'config-base

 ;; Avoid startup screen
 '(inhibit-startup-screen t)

 ;; Custom settings that are machine-specific and not to be versioned, will be added to custom.el
 '(make-backup-files nil)
 '(backup-directory-alist (quote (("~/.emacs.d/backups/"))))
 '(clean-buffer-list-delay-special 900)

 ;; Modeline and Window appearance options
 '(column-number-mode t)
 '(scroll-bar-mode (quote right))
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tooltip-delay 0.4)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(speedbar-directory-button-trim-method (quote trim))

 ;; Buffer switching
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-work-directory-list 64)
 '(ido-max-work-file-list 64)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-virtual-buffers t)
 '(iswitchb-buffer-ignore (quote ("^ " "^*Messages" "^*Completions")))
 '(iswitchb-use-virtual-buffers t nil (recentf))
 '(completion-ignored-extensions (quote (".o" ".elc" ".class" "java~" ".ps" ".abs" ".mx" ".~jv" ".elf" ".bin")))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))

 ;; Parenthesis highlight
 '(show-paren-delay 0.125)
 '(show-paren-mode t)
 '(show-paren-ring-bell-on-mismatch nil)
 '(show-paren-style (quote expression))

 ;; Dired options
 '(dired-auto-revert-buffer (quote dired-directory-changed-p))
 '(dired-guess-shell-alist-user (quote (("\\.\\(gz\\|bz2\\|lzma\\|\\tar\\|zip\\|rar\\)" "unp -U" "xdg-open") ("." "xdg-open"))))
 '(dired-isearch-filenames (quote dwim))
 '(dired-listing-switches "-alhG --time-style=iso")

 ;; Eshell
 '(eshell-visual-commands (quote ("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "alsamixer" "wicd-curses")))
 '(comint-input-ignoredups t)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input (quote this))

 ;; Compilation / Debugging
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-threshold 2)
 '(compilation-window-height 6)
 '(flymake-gui-warnings-enabled nil)
 '(gdb-show-main t)

 ;; Grep
 '(grep-command "grep . -nHIr -e ")
 '(grep-scroll-output t)
 
 ;; kill-ring
 '(kill-do-not-save-duplicates t)
 '(mark-even-if-inactive t)

 ;; Org
 '(org-agenda-files (quote ("~/org/home.org" "~/org/work.org")))
 '(org-mode-hook nil t)
 '(org-src-fontify-natively t)
 '(org-startup-with-inline-images t)

 ;; Outline
 '(outline-regexp "\\([*^L]+\\|.*{{{\\|.*#{\\)")

 ;; Additional package sources
 '(package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		      ("elpa" . "http://tromey.com/elpa/")
		      ("marmalade" . "http://marmalade-repo.org/packages/")
		      ))

 ;; Recent files
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 128)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf")

 ;; TAGs
 '(tags-case-fold-search nil)
 '(tags-revert-without-query t)

 ;; Uniquify buffer names
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify) "distinguish duplicate filenames renaming the buffers to contain part of the path")

 ;; Avoid warnings when opening symlinks to version control files
 '(vc-follow-symlinks t)

 ;; Other
 '(visible-bell t)
 '(x-select-enable-clipboard t)
 '(mouse-avoidance-mode (quote animate) nil (avoid))

 '(require-final-newline t)

 '(savehist-mode t nil (savehist))

 '(midnight-delay 1800)
 '(midnight-mode t nil (midnight))

 '(c-default-style "linux")
 
 '(cpp-edit-list '(("1" default font-lock-comment-face t) 
		   ("0" font-lock-comment-face default both)
		   ))
 '(cpp-known-face (quote default))
 '(cpp-unknown-face (quote default))

 ;;;
 ;;; these settings might require some extra package to be installed through package-el
 ;;;
 ;; auto-complete
 '(global-auto-complete-mode t)
 ;; yasnippet
 '(yas-global-mode t)
 ;; flymake-jslint
 '(js-mode-hook (quote (flymake-jslint-load)))
 '(flymake-jslint-args '("--white" "--undef" "--nomen" "--regexp" "--plusplus" "--bitwise" "--newcap" "--sloppy" "--vars" "--eqeq" "--node" "--browser"))
 ;; flymake-shell
 '(sh-mode-hook (quote (flymake-shell-load)))
 ;; -- end
)

;; since these hooks have no customize entry, they
;; are not loaded without including them here
(add-hook 'js-mode-hook 'flymake-jslint-load)
(add-hook 'js2-mode-hook 'flymake-jslint-load)
