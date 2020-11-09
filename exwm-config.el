;;; exwm-config.el --- Emacs Desktop Environment

;;; Commentary:
;; 

;;; Code:

;;; Setup, configuration

(setq exwm-config--exwm-loaded (cond ((locate-library "exwm") (require 'exwm))))
(setq exwm-config--exwm-systemtray-loaded (cond ((locate-library "exwm-systemtray") (require 'exwm-systemtray))))

(when exwm-config--exwm-loaded (exwm-enable))
(when exwm-config--exwm-systemtray-loaded (exwm-systemtray-enable))

(when exwm-config--exwm-loaded
  (setq display-time-default-load-average nil
	display-time-format " [%R] "
	exwm-input-global-keys
	`((,(kbd "<XF86AudioLowerVolume>") . exwm-config--alsa-lower-volume)
	  (,(kbd "<XF86AudioRaiseVolume>") . exwm-config--alsa-raise-volume)
	  (,(kbd "<XF86AudioMute>") . exwm-config--alsa-message-volume-state)
	  (,(kbd "s-p") . exwm-config--bufferless-shell-command)
	  (,(kbd "s-v") . exwm-config-mplayer-remote)
	  (,(kbd "s-V") . exwm-config-mplayer-local)))
  (display-battery-mode 1)
  (display-time-mode 1)
  (fringe-mode 1)
  (add-hook 'exwm-update-class-hook (lambda nil (exwm-workspace-rename-buffer exwm-class-name))))

;;; Workaround so EXWM can show the tab line.
(defun exwm-layout--show (id &optional window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (exwm--log "Show #x%x in %s" id window)
  (let* ((edges (window-inside-absolute-pixel-edges window))
         (x (pop edges))
         (y (pop edges))
         (width (- (pop edges) x))
         (height (- (pop edges) y))
         frame-x frame-y frame-width frame-height)
    (with-current-buffer (exwm--id->buffer id)
      (when exwm--floating-frame
        (setq frame-width (frame-pixel-width exwm--floating-frame)
              frame-height (+ (frame-pixel-height exwm--floating-frame)
                              ;; Use `frame-outer-height' in the future.
                              exwm-workspace--frame-y-offset))
        (when exwm--floating-frame-position
          (setq frame-x (elt exwm--floating-frame-position 0)
                frame-y (elt exwm--floating-frame-position 1)
                x (+ x frame-x (- exwm-layout--floating-hidden-position))
                y (+ y frame-y (- exwm-layout--floating-hidden-position)))
          (setq exwm--floating-frame-position nil))
        (exwm--set-geometry (frame-parameter exwm--floating-frame
                                             'exwm-container)
                            frame-x frame-y frame-width frame-height))
      (when (exwm-layout--fullscreen-p)
        (with-slots ((x* x)
                     (y* y)
                     (width* width)
                     (height* height))
            (exwm-workspace--get-geometry exwm--frame)
          (setq x x*
                y y*
                width width*
                height height*)))
      (when (bound-and-true-p tab-line-mode)
	(setq y (+ y (frame-char-height))))
      (exwm--set-geometry id x y width height)
      (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:NormalState)
      (setq exwm--ewmh-state
            (delq xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state))
      (exwm-layout--set-ewmh-state id)
      (exwm-layout--auto-iconify)))
  (xcb:flush exwm--connection))

;;; EXWM utilities.

(defun exwm-config--bufferless-shell-command (command)
  "Run asynchronous shell COMMAND without buffer."
  (interactive (list (read-shell-command "[EXWM] Bufferless shell command: ")))
  (start-process-shell-command command nil command))

(defun exwm-config--alsa-raise-volume nil
  "Raise the ALSA volume and display current percentage."
  (interactive)
  (shell-command-to-string "amixer -q set Master 5%+")
  (message "[ALSA] Volume at %s"
	   (let ((output (shell-command-to-string "amixer -M get Master")))
	     (save-match-data
	       (string-match "\\([0-9]+%\\)" output)
	       (match-string 1 output)))))

(defun exwm-config--alsa-lower-volume nil
  "Lower the ALSA volume and display current percentage."
  (interactive)
  (shell-command-to-string "amixer -q set Master 5%-")
  (message "[ALSA] Volume at %s"
	   (let ((output (shell-command-to-string "amixer -M get Master")))
	     (save-match-data
	       (string-match "\\([0-9]+%\\)" output)
	       (match-string 1 output)))))

(defun exwm-config--alsa-message-volume-state nil
  "Get the current volume state."
  (interactive)
  (shell-command-to-string "amixer -q set Master toggle")
  (message "[ALSA] Volume %s"
	   (substring (let ((output (shell-command-to-string "amixer get Master")))
			(save-match-data
			  (string-match "\\(\\[on\\]\\|\\[off\\]\\)" output)
			  (match-string 1 output)))
		      1 -1)))

(defun exwm-config-mplayer-local (path)
  "View video at PATH using MPlayer."
  (interactive "fFile: ")
  (start-process-shell-command "MPlayer" nil (concat "mplayer " path)))

(defun exwm-config-mplayer-remote (url)
  "View video at URL using MPlayer."
  (interactive "MURL: ")
  (start-process-shell-command "MPlayer" nil (concat "youtube-dl -q -o- " url " | mplayer -")))

(defun exwm-config-mplayer-cd nil
  "Listen to a CD using MPlayer."
  (interactive)
  (start-process-shell-command "MPlayer" nil "mplayer cdda://"))

(provide 'exwm-config)

;;; exwm-config.el ends here
