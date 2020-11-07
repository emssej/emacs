;;; exwm-config.el --- Emacs Desktop Environment

;;; Commentary:
;; 

;;; Code:

;;; Setup, configuration

(setq exwm-config--exwm-loaded (require 'exwm))
(setq exwm-config--exwm-systemtray-loaded (require 'exwm-systemtray))

(when exwm-config--exwm-loaded (exwm-enable))
(when exwm-config--exwm-systemtray-loaded (exwm-systemtray-enable))

(when exwm-config--exwm-loaded
  (setq display-time-default-load-average nil
	display-time-format " [%R]"
	exwm-input-global-keys
	`((,(kbd "<XF86AudioLowerVolume>") . exwm-config--alsa-message-volume-value)
	  (,(kbd "<XF86AudioRaiseVolume>") . exwm-config--alsa-message-volume-value)
	  (,(kbd "<XF86AudioMute>") . exwm-config--alsa-message-volume-state)
	  (,(kbd "s-p") . exwm-config--bufferless-shell-command)
	  (,(kbd "s-v") . exwm-config-mplayer-remote)
	  (,(kbd "s-V") . exwm-config-mplayer-local)))
  (display-battery-mode 1)
  (display-time-mode 1)
  (fringe-mode 1)
  (add-hook 'exwm-update-class-hook (lambda nil (exwm-workspace-rename-buffer exwm-class-name))))

;;; EXWM utilities.

(defun exwm-config--bufferless-shell-command (command)
  "Run asynchronous shell COMMAND without buffer."
  (interactive (list (read-shell-command "[EXWM] Bufferless shell command: ")))
  (start-process-shell-command command nil command))

(defun exwm-config--alsa-message-volume-value nil
  "Get the current volume percentage."
  (interactive)
  (message "[ALSA] Volume—%s"
	   (let ((output (shell-command-to-string "amixer -M get Master")))
	     (save-match-data
	       (string-match "\\([0-9]+%\\)" output)
	       (match-string 1 output)))))

(defun exwm-config--alsa-message-volume-state nil
  "Get the current volume state."
  (interactive)
  (message "[ALSA] Volume—%s"
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
  (start-process-shell-command "MPlayer" nil (concat "youtube-dl -q -o- " url " | mplayer -cache 4096 -")))

(defun exwm-config-mplayer-cd nil
  "Listen to a CD using MPlayer."
  (interactive)
  (start-process-shell-command "MPlayer" nil "mplayer cdda://"))

(provide 'exwm-config)

;;; exwm-config.el ends here
