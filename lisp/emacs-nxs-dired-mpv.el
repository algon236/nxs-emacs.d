;;; emacs-nxs-dired-mpv.el --- Audio player for Dired using mpv  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: multimedia, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Play audio files directly from Dired using mpv with IPC control.
;;
;; TLDR: M-x dired
;;       mark files with `m'
;;       C-c m to open the music player with the selected files
;;       RET will add the marked files and start playing
;;       You can control this mpv session from anywhere using C-c m

;;; Code:

(use-package emacs-nxs-mpv-player
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-nxs/mpv-process nil
    "Process object for the currently running mpv instance.")

  (defvar emacs-nxs/mpv-ipc-socket
    (expand-file-name "mpv-socket" (temporary-file-directory))
    "Path to mpv's IPC UNIX domain socket.")
  ;; defvar won't overwrite an existing binding on config reload, so force it:
  (setq emacs-nxs/mpv-ipc-socket
        (expand-file-name "mpv-socket" (temporary-file-directory)))

  (defvar emacs-nxs/mpv-ipc-process nil
    "Persistent IPC connection to the running mpv instance.")

  (defvar emacs-nxs/mpv-audio-extensions
    '(;; Lossy compressed
      "mp3" "mp2" "mp1" "ogg" "oga" "opus" "aac" "m4a" "m4b" "wma"
      "mpc" "mp+" "mpp" "spx" "amr" "ra" "rm"
      ;; Lossless compressed
      "flac" "ape" "wv" "tta" "alac"
      ;; Uncompressed
      "wav" "aiff" "aif" "au" "snd" "caf" "voc"
      ;; Containers / encoded streams
      "mka" "ac3" "eac3" "dts" "adts" "dsf" "dff"
      ;; MIDI & tracker modules
      "mid" "midi" "mod" "xm" "it" "s3m" "stm"
      ;; Playlists
      "m3u" "m3u8" "pls" "xspf")
    "Audio file extensions recognised by emacs-nxs-mpv-player.")

  (defun emacs-nxs/mpv-play-files ()
    "Play marked audio files in Dired using mpv with IPC."
    (interactive)
    (unless (derived-mode-p 'dired-mode)
      (user-error "Not in a Dired buffer"))
    (let* ((all-files (dired-get-marked-files))
           (files (seq-filter
                   (lambda (f)
                     (or (file-directory-p f)
                         (member (downcase (or (file-name-extension f) ""))
                                 emacs-nxs/mpv-audio-extensions)))
                   all-files)))
      (when (null files)
        (user-error "No supported audio files in selection"))
      (when (process-live-p emacs-nxs/mpv-ipc-process)
        (delete-process emacs-nxs/mpv-ipc-process)
        (setq emacs-nxs/mpv-ipc-process nil))
      (when (file-exists-p emacs-nxs/mpv-ipc-socket)
        (delete-file emacs-nxs/mpv-ipc-socket))
      (when (process-live-p emacs-nxs/mpv-process)
        (kill-process emacs-nxs/mpv-process))
      (setq emacs-nxs/mpv-process
            (apply #'start-process
                   "mpv" "*mpv*"
                   "mpv"
                   "--no-video"
                   (concat "--input-ipc-server=" emacs-nxs/mpv-ipc-socket)
                   files))
      (run-with-timer 0.7 nil #'emacs-nxs/mpv-show-status)
      (run-with-timer 0.7 nil #'emacs-nxs/mpv-maybe-refresh-playlist)))

  (defun emacs-nxs/mpv-stop ()
    "Stop mpv playback."
    (interactive)
    (when (process-live-p emacs-nxs/mpv-ipc-process)
      (delete-process emacs-nxs/mpv-ipc-process)
      (setq emacs-nxs/mpv-ipc-process nil))
    (when (process-live-p emacs-nxs/mpv-process)
      (kill-process emacs-nxs/mpv-process)
      (setq emacs-nxs/mpv-process nil))
    (message "⏹  Stopped")
    ;; Socket is gone; update the playlist buffer directly if visible.
    (when (get-buffer-window "*mpv-playlist*" t)
      (with-current-buffer "*mpv-playlist*"
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "⏹  Playback stopped.\n")
          (goto-char (point-min))))))

  (defun emacs-nxs/mpv-send-command (json-cmd)
    "Send JSON-CMD to mpv via a persistent IPC socket connection."
    (unless (process-live-p emacs-nxs/mpv-ipc-process)
      (setq emacs-nxs/mpv-ipc-process nil)
      (when (file-exists-p emacs-nxs/mpv-ipc-socket)
        (condition-case err
            (setq emacs-nxs/mpv-ipc-process
                  (make-network-process
                   :name "mpv-ipc"
                   :family 'local
                   :service emacs-nxs/mpv-ipc-socket
                   :filter #'ignore
                   :sentinel (lambda (p _e)
                               (unless (process-live-p p)
                                 (setq emacs-nxs/mpv-ipc-process nil)))))
          (error
           (message "❌ mpv IPC connect error: %s" (error-message-string err))))))
    (if (process-live-p emacs-nxs/mpv-ipc-process)
        (process-send-string emacs-nxs/mpv-ipc-process (concat json-cmd "\n"))
      (message "❌ mpv IPC socket not found or unreachable: %s" emacs-nxs/mpv-ipc-socket)))


  (defun emacs-nxs/mpv-read-property (prop)
    "Query mpv for PROP via a fresh IPC connection, returning its value or nil."
    (when (file-exists-p emacs-nxs/mpv-ipc-socket)
      (let ((result 'pending) (buf ""))
        (condition-case nil
            (let ((proc (make-network-process
                         :name "mpv-query"
                         :family 'local
                         :service emacs-nxs/mpv-ipc-socket
                         :filter (lambda (_p chunk)
                                   (setq buf (concat buf chunk))
                                   (ignore-errors
                                     (let* ((json-object-type 'alist)
                                            (json-array-type  'list)
                                            (json-key-type    'symbol)
                                            (data (json-read-from-string buf)))
                                       (setq result (alist-get 'data data))))))))
              (process-send-string
               proc (format "{\"command\":[\"get_property\",\"%s\"]}\n" prop))
              (let ((tries 20))
                (while (and (eq result 'pending) (> tries 0))
                  (accept-process-output proc 0.05)
                  (setq tries (1- tries))))
              (when (process-live-p proc) (delete-process proc)))
          (error nil))
        (unless (eq result 'pending) result))))

  (defun emacs-nxs/mpv-show-status ()
    "Show current track name and play/pause state in the minibuffer."
    (when (process-live-p emacs-nxs/mpv-process)
      (let* ((title  (emacs-nxs/mpv-read-property "media-title"))
             (paused (emacs-nxs/mpv-read-property "pause")))
        (when title
          (message "%s  %s" (if (eq paused t) "⏸" "▶") title)))))

  (defun emacs-nxs/mpv-show-playlist ()
    "Show the current mpv playlist in a readable buffer."
    (interactive)
    (let ((buf (get-buffer-create "*mpv-playlist*"))
          (socket emacs-nxs/mpv-ipc-socket)
          (output ""))
      (if (file-exists-p socket)
          (let ((proc
                 (make-network-process
                  :name "mpv-ipc-playlist"
                  :family 'local
                  :service socket
                  :nowait nil
                  :filter (lambda (_proc chunk)
                            (setq output (concat output chunk))))))
            (process-send-string proc
                                 "{\"command\": [\"get_property\", \"playlist\"]}\n")
            (sleep-for 0.1)
            (delete-process proc)

            (let ((paused (emacs-nxs/mpv-read-property "pause")))
              (with-current-buffer buf
                (let ((inhibit-read-only t)
                      (json-object-type 'alist)
                      (json-array-type 'list)
                      (json-key-type 'symbol))
                  (erase-buffer)
                  (let* ((json-data (ignore-errors (json-read-from-string output)))
                         (playlist (alist-get 'data json-data)))
                    (if playlist
                        (progn
                          (insert "MPV Playlist:\n\n")
                          (cl-loop for i from 0
                                   for entry in playlist do
                                   (let ((current (eq (alist-get 'current entry) t)))
                                     (insert
                                      (format "%s %s. %s\n"
                                              (if current
                                                  (if (eq paused t) "⏸ " "▶ ")
                                                "  ")
                                              (1+ i)
                                              (file-name-nondirectory
                                               (alist-get 'filename entry)))))))
                      (insert "Error: failed to parse playlist or playlist is empty."))))
                (special-mode)
                (goto-char (point-min))))
            (display-buffer buf))
        (message "Error: mpv IPC socket not found at %s" socket))))

  (defun emacs-nxs/mpv-maybe-refresh-playlist ()
    "Refresh *mpv-playlist* silently only if it is visible in a window."
    (when (get-buffer-window "*mpv-playlist*" t)
      (emacs-nxs/mpv-show-playlist)))

  (defun emacs-nxs/mpv-toggle-playlist ()
    "Toggle the *mpv-playlist* window open or closed."
    (interactive)
    (let ((win (get-buffer-window "*mpv-playlist*" t)))
      (if win
          (delete-window win)
        (emacs-nxs/mpv-show-playlist))))

  (defun emacs-nxs/mpv-quit-transient ()
    "Quit the mpv transient, closing the playlist window if open."
    (interactive)
    (when-let* ((win (get-buffer-window "*mpv-playlist*" t)))
      (delete-window win))
    (transient-quit-all))

  (require 'transient)

  (transient-define-prefix emacs-nxs/mpv-transient ()
    "MPV Controls"
    [["Controls"
      ("SPC" "⏸  Pause/Resume"
       (lambda () (interactive)
         (emacs-nxs/mpv-send-command "{\"command\": [\"cycle\", \"pause\"]}")
         (run-with-timer 0.15 nil #'emacs-nxs/mpv-show-status)
         (run-with-timer 0.15 nil #'emacs-nxs/mpv-maybe-refresh-playlist))
       :transient t)
      ("x" "  ⏹  Stop" emacs-nxs/mpv-stop :transient t)
      ("n" "  ⏭  Next"
       (lambda () (interactive)
         (emacs-nxs/mpv-send-command "{\"command\": [\"playlist-next\"]}")
         (run-with-timer 0.4 nil #'emacs-nxs/mpv-show-status)
         (run-with-timer 0.4 nil #'emacs-nxs/mpv-maybe-refresh-playlist))
       :transient t)
      ("p" "  ⏮  Previous"
       (lambda () (interactive)
         (emacs-nxs/mpv-send-command "{\"command\": [\"playlist-prev\"]}")
         (run-with-timer 0.4 nil #'emacs-nxs/mpv-show-status)
         (run-with-timer 0.4 nil #'emacs-nxs/mpv-maybe-refresh-playlist))
       :transient t)
      ("l" "  ↺  Loop file"
       (lambda () (interactive)
         (emacs-nxs/mpv-send-command
          "{\"command\": [\"cycle-values\", \"loop-file\", \"no\", \"inf\"]}")
         (run-with-timer 0.15 nil
                         (lambda ()
                           (when (process-live-p emacs-nxs/mpv-process)
                             (let ((state (emacs-nxs/mpv-read-property "loop-file")))
                               (message "↺  Loop: %s"
                                        (if (equal state "inf") "on" "off")))))))
       :transient t)]
     ["Playlist"
      ("RET" "▶  Play files"   emacs-nxs/mpv-play-files :transient t)
      ("L"   "  ☰  Playlist"     emacs-nxs/mpv-toggle-playlist :transient t)
      ("q"   "  ×  Quit"         emacs-nxs/mpv-quit-transient)]])

  (defun emacs-nxs/mpv-dired-setup ()
    (global-set-key (kbd "C-c m") #'emacs-nxs/mpv-transient))

  (add-hook 'dired-mode-hook #'emacs-nxs/mpv-dired-setup))

(provide 'emacs-nxs-dired-mpv)
;;; emacs-nxs-dired-mpv.el ends here
