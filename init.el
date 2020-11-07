;;; Requires
(require 'exwm-config "~/.emacs.d/exwm-config.el")
;;; Variables
(custom-set-variables
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(blink-cursor-mode t)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(cursor-type 'bar)
 '(global-so-long-mode t)
 '(global-tab-line-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(mouse-yank-at-point t)
 '(prog-mode-hook
   '(flyspell-prog-mode display-line-numbers-mode electric-pair-mode show-paren-mode hl-line-mode))
 '(rcirc-authinfo '(("rizon" nickserv "" "")))
 '(rcirc-fill-flag nil)
 '(rcirc-log-flag t)
 '(rcirc-server-alist
   '(("" :nick "" :port 9999 :user-name "" :full-name "" :channels
      ("" "")
      :encryption tls :server-alias "")))
 '(rcirc-time-format "[%H:%M:%S] ")
 '(rcirc-track-minor-mode t)
 '(safe-local-variable-values '((eval outline-hide-sublevels 6)))
 '(savehist-mode t)
 '(select-enable-primary t)
 '(size-indication-mode t)
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
