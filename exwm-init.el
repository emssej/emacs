;;; exwm-init.el --- EXWM config file  -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO: add alt-tabbing

(setq exwm-init--exwm-loaded (cond ((locate-library "exwm") (require 'exwm))))
(setq exwm-init--exwm-systemtray-loaded (cond ((locate-library "exwm-systemtray") (require 'exwm-systemtray))))

(when exwm-init--exwm-loaded (exwm-enable))
(when exwm-init--exwm-systemtray-loaded (exwm-systemtray-enable))

(when exwm-init--exwm-loaded
  (custom-set-variables
   '(display-battery-mode t)
   '(display-time-format " [%R]")
   '(display-time-mode t)
   '(exwm-input-global-keys
     `((,(kbd "s-<f2>") . exwm-init--start-program)
       (,(kbd "s-<f4>") . kill-this-buffer)))
   '(scroll-bar-mode nil)))

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
	(setq y (+ 2 y (frame-char-height))))
      (exwm--set-geometry id x y width height)
      (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:NormalState)
      (setq exwm--ewmh-state
            (delq xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state))
      (exwm-layout--set-ewmh-state id)
      (exwm-layout--auto-iconify)))
  (xcb:flush exwm--connection))

(defun exwm-init--start-program (program)
  "Run PROGRAM without a buffer."
  (interactive (list (read-shell-command "Start program: ")))
  (start-process-shell-command program nil program))

(provide 'exwm-init)

;;; exwm-init.el ends here
