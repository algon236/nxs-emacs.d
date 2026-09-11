;;; emacs-nxs-gutter.el --- Git diff gutter indicators in buffers  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: vc, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Displays git diff indicators in the left margin of file-visiting
;; buffers.  Shows added, changed, and deleted lines with colored
;; symbols.  Refreshes on save, revert, and focus changes.

;;; Code:

(use-package emacs-nxs-gutter
  :if emacs-nxs-enable-buffer-gutter
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-nxs/goto-next-hunk ()
    "Jump cursor to the closest next hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (line-numbers (mapcar #'car git-gutter-diff-info))
           (sorted-line-numbers (sort line-numbers '<))
           (next-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the next closest line number
                (cl-find-if (lambda (line) (> line current-line)) sorted-line-numbers)
              ;; If the current line is in the list, find the next line number that is not consecutive
              (let ((last-line nil))
                (cl-loop for line in sorted-line-numbers
                         when (and (> line current-line)
                                   (or (not last-line)
                                       (/= line (1+ last-line))))
                         return line
                         do (setq last-line line))))))

      (when next-line-number
        (goto-char (point-min))
        (forward-line (1- next-line-number)))))

  (defun emacs-nxs/goto-previous-hunk ()
    "Jump cursor to the closest previous hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (line-numbers (mapcar #'car git-gutter-diff-info))
           (sorted-line-numbers (sort line-numbers '<))
           (previous-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the previous closest line number
                (cl-find-if (lambda (line) (< line current-line)) (reverse sorted-line-numbers))
              ;; If the current line is in the list, find the previous line number that has no direct predecessor
              (let ((previous-line nil))
                (dolist (line sorted-line-numbers)
                  (when (and (< line current-line)
                             (not (member (1- line) line-numbers)))
                    (setq previous-line line)))
                previous-line))))

      (when previous-line-number
        (goto-char (point-min))
        (forward-line (1- previous-line-number)))))


  (defvar-local emacs-nxs/git-gutter-timer nil)
  (defvar-local git-gutter-diff-info nil)

  (defun emacs-nxs/git-gutter-eligible-p ()
    "Return the Git root for a local visited file, otherwise nil."
    (and buffer-file-name (not (file-remote-p buffer-file-name))
         (file-exists-p buffer-file-name)
         (vc-git-root buffer-file-name)))

  (defun emacs-nxs/git-gutter-parse-diff (text)
    "Classify hunks in TEXT using both old and new line counts."
    (let ((start 0) result)
      (while (string-match
              "^@@ -[0-9]+\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
              text start)
        (let ((old-count (if (match-string 1 text)
                             (string-to-number (match-string 1 text)) 1))
              (line (string-to-number (match-string 2 text)))
              (new-count (if (match-string 3 text)
                             (string-to-number (match-string 3 text)) 1)))
          (if (zerop new-count)
              (push (cons (max 1 line) "deleted") result)
            (dotimes (offset new-count)
              (push (cons (+ line offset)
                          (if (zerop old-count) "added" "changed")) result))))
        (setq start (match-end 0)))
      (nreverse result)))

  (defvar-local emacs-nxs/git-gutter-process nil)

  (defun emacs-nxs/git-gutter-render (lines-status)
    "Render LINES-STATUS in the current buffer without running external tools."
    (remove-overlays (point-min) (point-max) 'emacs-nxs--git-gutter-overlay t)
    (setq git-gutter-diff-info lines-status)
    (save-excursion
      (dolist (entry lines-status)
        (goto-char (point-min))
        (forward-line (1- (car entry)))
        (let ((overlay (make-overlay (line-beginning-position) (line-beginning-position)))
              (face (pcase (cdr entry)
                      ("added" 'success) ("changed" 'warning) (_ 'error))))
          (overlay-put overlay 'emacs-nxs--git-gutter-overlay t)
          (overlay-put overlay 'before-string
                       (propertize " " 'display
                                   `((margin left-margin)
                                     ,(propertize "┃" 'face face))))))))

  (defun emacs-nxs/git-gutter-cancel-process ()
    "Cancel an obsolete diff, without rendering its result."
    (when (processp emacs-nxs/git-gutter-process)
      (let ((proc emacs-nxs/git-gutter-process))
        (setq emacs-nxs/git-gutter-process nil)
        (when (process-live-p proc) (delete-process proc)))))

  (defun emacs-nxs/git-gutter-add-mark (&rest _args)
    "Refresh a local Git buffer asynchronously, using only the latest result."
    (interactive)
    (emacs-nxs/git-gutter-cancel-process)
    (emacs-nxs/git-gutter-render nil)
    (when-let* ((root (emacs-nxs/git-gutter-eligible-p)))
      (let* ((buf (current-buffer))
             (file buffer-file-name)
             (tick (buffer-chars-modified-tick))
             (default-directory root)
             (output (generate-new-buffer " *NXS git diff*")))
        (condition-case err
            (setq emacs-nxs/git-gutter-process
                  (make-process
                   :name "nxs-git-gutter" :buffer output :noquery t
                   :connection-type 'pipe
                   :command (list "git" "diff" "--no-ext-diff" "--no-color"
                                  "--unified=0" "--" (file-relative-name file root))
                   :sentinel
                   (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (unwind-protect
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (when (eq proc emacs-nxs/git-gutter-process)
                                 (setq emacs-nxs/git-gutter-process nil)
                                 (when (and (eq (process-status proc) 'exit)
                                            (zerop (process-exit-status proc))
                                            (equal file buffer-file-name)
                                            (= tick (buffer-chars-modified-tick)))
                                   (emacs-nxs/git-gutter-render
                                    (with-current-buffer output
                                      (emacs-nxs/git-gutter-parse-diff (buffer-string))))))))
                         (when (buffer-live-p output) (kill-buffer output)))))))
          (error (kill-buffer output)
                 (message "Git gutter: %s" (error-message-string err))))
        (add-hook 'kill-buffer-hook #'emacs-nxs/git-gutter-cancel-process nil t))))

  (defun emacs-nxs/git-gutter-cancel-timer ()
    "Cancel this buffer's pending refresh."
    (when (timerp emacs-nxs/git-gutter-timer)
      (cancel-timer emacs-nxs/git-gutter-timer))
    (setq emacs-nxs/git-gutter-timer nil))

  (defun emacs-nxs/timed-git-gutter-on ()
    "Coalesce repeated refresh requests for a local Git buffer."
    (emacs-nxs/git-gutter-cancel-timer)
    (when (emacs-nxs/git-gutter-eligible-p)
      (let ((buf (current-buffer)))
        (setq emacs-nxs/git-gutter-timer
              (run-with-idle-timer
               0.2 nil
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (setq emacs-nxs/git-gutter-timer nil)
                     (emacs-nxs/git-gutter-add-mark)))))))
      (add-hook 'kill-buffer-hook #'emacs-nxs/git-gutter-cancel-timer nil t)))

  (defun emacs-nxs/git-gutter-off ()
    "Remove all `emacs-nxs--git-gutter-overlay' marks and other overlays."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (emacs-nxs/git-gutter-cancel-timer)
        (emacs-nxs/git-gutter-cancel-process)
        (setq git-gutter-diff-info nil)
        (remove-overlays (point-min) (point-max) 'emacs-nxs--git-gutter-overlay t)))
    (remove-hook 'find-file-hook #'emacs-nxs/timed-git-gutter-on)
    (remove-hook 'after-save-hook #'emacs-nxs/timed-git-gutter-on)
    (remove-hook 'after-revert-hook #'emacs-nxs/timed-git-gutter-on)
    (remove-function after-focus-change-function #'emacs-nxs/git-gutter-refresh-visible)
    (remove-hook 'window-selection-change-functions #'emacs-nxs/git-gutter-on-window-switch))

  (defun emacs-nxs/git-gutter-on ()
    (interactive)
    (add-hook 'find-file-hook #'emacs-nxs/timed-git-gutter-on)
    (add-hook 'after-save-hook #'emacs-nxs/timed-git-gutter-on)
    (add-hook 'after-revert-hook #'emacs-nxs/timed-git-gutter-on)
    (add-function :after after-focus-change-function #'emacs-nxs/git-gutter-refresh-visible)
    (add-hook 'window-selection-change-functions #'emacs-nxs/git-gutter-on-window-switch)
    (when (not (string-match-p "^\\*" (buffer-name))) ; avoid *scratch*, etc.
      (emacs-nxs/git-gutter-add-mark)))

  (defun emacs-nxs/git-gutter-refresh-visible ()
    "Refresh gutter marks in all visible file-visiting buffers.
Runs after Emacs regains focus (e.g. switching back from terminal
after git add/commit, or after an external tool modifies files)."
    (when (frame-focus-state)
      (dolist (win (window-list))
        (let ((buf (window-buffer win)))
          (when (and (buffer-file-name buf)
                     (not (string-match-p "^\\*" (buffer-name buf)))
                     (vc-git-root (buffer-file-name buf)))
            (with-current-buffer buf
              (emacs-nxs/timed-git-gutter-on)))))))

  (defun emacs-nxs/git-gutter-on-window-switch (_frame)
    "Refresh gutter marks in the newly selected window's buffer.
Called by `window-selection-change-functions' on C-x o, etc."
    (let ((buf (window-buffer (selected-window))))
      (when (and (buffer-file-name buf)
                 (not (string-match-p "^\\*" (buffer-name buf)))
                 (vc-git-root (buffer-file-name buf)))
        (with-current-buffer buf
          (emacs-nxs/timed-git-gutter-on)))))

  (global-set-key (kbd "M-9") 'emacs-nxs/goto-previous-hunk)
  (global-set-key (kbd "M-0") 'emacs-nxs/goto-next-hunk)
  (global-set-key (kbd "C-c g p") 'emacs-nxs/goto-previous-hunk)
  (global-set-key (kbd "C-c g r") 'emacs-nxs/git-gutter-off)
  (global-set-key (kbd "C-c g g") 'emacs-nxs/git-gutter-on)
  (global-set-key (kbd "C-c g n") 'emacs-nxs/goto-next-hunk)

  (add-hook 'after-init-hook #'emacs-nxs/git-gutter-on))

(provide 'emacs-nxs-gutter)
;;; emacs-nxs-gutter.el ends here
