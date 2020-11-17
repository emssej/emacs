;;; Requires
(require 'exwm-config "~/.config/emacs/exwm-config.el")
;;; Variables
(custom-set-variables
 '(backup-directory-alist '(("." . "~/.config/emacs/backups")))
 '(blink-cursor-mode t)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(cursor-type 'bar)
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes t)
 '(global-so-long-mode t)
 '(global-tab-line-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(modus-operandi-theme-bold-constructs t)
 '(modus-operandi-theme-faint-syntax t)
 '(modus-operandi-theme-slanted-constructs t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(mouse-yank-at-point t)
 '(prog-mode-hook
   '(flyspell-prog-mode display-line-numbers-mode electric-pair-mode show-paren-mode hl-line-mode))
 '(safe-local-variable-values '((eval outline-hide-sublevels 6)))
 '(savehist-mode t)
 '(select-enable-primary t)
 '(size-indication-mode t)
 '(tab-line-close-tab-function 'kill-buffer)
 '(tab-line-exclude-modes nil)
 '(tab-line-tabs-function
   '(lambda nil
      (seq-filter
       (lambda
	 (buffer)
	 (when
	     (not
	      (string=
	       (substring
		(buffer-name buffer)
		0 1)
	       " "))
	   buffer))
       (sort
	(buffer-list)
	(lambda
	  (first last)
	  (string<
	   (buffer-name first)
	   (buffer-name last)))))))
 '(text-mode-hook
   '(turn-on-flyspell turn-on-auto-fill text-mode-hook-identify delete-selection-mode))
 '(tool-bar-mode nil))
;;; Functions
(defalias 'yes-or-no-p 'y-or-n-p)
;;; File local variables
;; Local variables:
;; eval: (outline-minor-mode)
;; eval: (outline-hide-sublevels 6)
;; End:
