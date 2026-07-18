;;; emacs-nxs-transparency.el --- Frame transparency for GUI and terminal  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Sets frame transparency for both GUI (alpha-background) and
;; terminal (ANSI escape) Emacs sessions.  Supports macOS alpha
;; workaround.

;;; Code:

(use-package emacs-nxs-transparency
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-nxs/clear-terminal-background-color (&optional frame)
    "Unsets the background color in terminal mode, including line-number face."
    (interactive)
    (or frame (setq frame (selected-frame)))
    (unless (display-graphic-p frame)
      ;; Set the terminal to a transparent version of the background color
      (send-string-to-terminal
       (format "\033]11;[90]%s\033\\"
               (face-attribute 'default :background)))
      (set-face-background 'default "unspecified-bg" frame)
      (set-face-background 'line-number "unspecified-bg" frame)
      (set-face-background 'line-number-current-line "unspecified-bg" frame)))

  (defun emacs-nxs/transparency-set (&optional frame)
    "Set frame transparency. If FRAME is nil, applies to all existing frames."
    (interactive)
    (unless (display-graphic-p frame)
      (emacs-nxs/clear-terminal-background-color)
      (add-hook 'window-setup-hook 'emacs-nxs/clear-terminal-background-color)
      (add-hook 'ef-themes-post-load-hook 'emacs-nxs/clear-terminal-background-color))

    (if frame
        (progn
          (when (eq system-type 'darwin)
            (set-frame-parameter frame 'alpha '(90 90)))
          (set-frame-parameter frame 'alpha-background 85))

      ;; Apply to all frames if no frame is passed
      (dolist (frm (frame-list))
        (when (eq system-type 'darwin)
          (set-frame-parameter frm 'alpha '(90 90)))
        (set-frame-parameter frm 'alpha-background 85))))

  (defun emacs-nxs/transparency-unset ()
    "Unset frame transparency (Graphical Mode)."
    (interactive)
    (when (eq system-type 'darwin)
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'alpha-background 100)))

  (when emacs-nxs-enable-transparency

    (add-hook 'after-init-hook #'emacs-nxs/transparency-set)
    (add-hook 'after-make-frame-functions #'emacs-nxs/transparency-set)))

(provide 'emacs-nxs-transparency)
;;; emacs-nxs-transparency.el ends here
