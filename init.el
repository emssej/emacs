;;; Variables
(custom-set-variables
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(mouse-yank-at-point t)
 '(prog-mode-hook
   (quote
    (flyspell-prog-mode display-line-numbers-mode electric-pair-mode show-paren-mode hl-line-mode)))
 '(rcirc-authinfo
   (quote
    (("rizon" nickserv "" ""))))
 '(rcirc-fill-flag nil)
 '(rcirc-log-flag t)
 '(rcirc-server-alist
   (quote
    (("irc.rizon.net" :nick "" :port 9999 :user-name "" :full-name "" :channels
      ("" "")
      :encryption tls :server-alias ""))))
 '(rcirc-time-format "[%H:%M:%S] ")
 '(rcirc-track-minor-mode t)
 '(safe-local-variable-values (quote ((eval outline-hide-sublevels 6))))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(select-enable-primary t)
 '(size-indication-mode t)
 '(text-mode-hook
   (quote
    (turn-on-flyspell turn-on-auto-fill text-mode-hook-identify delete-selection-mode)))
 '(tool-bar-mode nil)
 '(x-gtk-use-system-tooltips nil))
;;; Functions
(defalias 'yes-or-no-p 'y-or-n-p)
;;; File local variables
;; Local variables:
;; eval: (outline-minor-mode)
;; eval: (outline-hide-sublevels 6)
;; End:
