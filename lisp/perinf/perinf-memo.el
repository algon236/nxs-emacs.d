;;; perinf-memo.el --- Simple categorized Org capture -*- lexical-binding: t; -*-

;; Copyright (C) 2026, Niels Søndergaard, Nivaa, Denmark.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Memos are plain Org entries with an explicit, stable CATEGORY for statistics.
;; The stored category is independent of the interface language.

;;; Code:

(require 'org-capture)
(require 'org-id)
(require 'autoinsert)
(require 'perinf-project)

(defun perinf-memo--file ()
  "Return the memo file in the current project, requiring a valid project."
  (unless (and perinf-current-project
               (perinf-project-p perinf-current-project))
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (expand-file-name "data/memos.org" perinf-current-project))

(defun perinf-memo--target ()
  "Visit the memo target without prompting for a generic new-file template."
  (let ((auto-insert nil))
    (set-buffer (find-file-noselect (perinf-memo--file))))
  (goto-char (point-max)))

(defun perinf-memo-capture-template (key)
  "Return a minimal memo capture template using KEY."
  (list key (perinf-i18n 'memo.capture) 'entry
        '(function perinf-memo--target)
        "* %?\nSCHEDULED: %^t\n:PROPERTIES:\n:ID: %(org-id-new)\n:CATEGORY: Husk\n:CREATED: %U\n:END:\n"
        :empty-lines 1))

;;;###autoload
(defun perinf-capture-memo ()
  "Capture a memo: write text and finish with C-c C-c."
  (interactive)
  (perinf-memo--file)
  (let ((org-capture-templates (list (perinf-memo-capture-template "H"))))
    (org-capture nil "H")))

;;;###autoload
(defun perinf-open-memos ()
  "Open the current project's memo file."
  (interactive)
  (let ((auto-insert nil))
    (find-file (perinf-memo--file))))

(defun perinf-memo-list (&optional project)
  "Return memo plists from PROJECT, including date and optional time.
Read a visiting buffer when available, without changing existing entries."
  (let* ((perinf-current-project (or project perinf-current-project))
         (file (perinf-memo--file))
         (visiting (find-buffer-visiting file)))
    (when (or visiting (file-readable-p file))
      (with-temp-buffer
        (if visiting
            (insert (with-current-buffer visiting
                      (save-restriction (widen) (buffer-string))))
          (insert-file-contents file))
        (delay-mode-hooks (org-mode))
        (let (memos)
          (org-map-entries
           (lambda ()
             (when (and (equal (org-entry-get nil "CATEGORY") "Husk")
                        (not (org-entry-is-done-p))
                        (not (org-in-archived-heading-p)))
               (let ((scheduled (org-entry-get nil "SCHEDULED")))
                 (push (list :type 'memo :file file
                             :id (org-entry-get nil "ID")
                             :position (point)
                             :title (org-get-heading t t t t)
                             :scheduled scheduled
                             :time (and scheduled
                                        (org-time-string-to-time scheduled)))
                       memos)))))
          (nreverse memos))))))

(defvar perinf-memo-after-close-hook nil
  "Hook run after a memo has been completed and saved.")

(defvar-local perinf-memo--previous-header nil
  "Header displayed before memo controls were enabled.")

(defvar perinf-memo--header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'perinf-memo--close-click)
    map)
  "Mouse map for the memo close button.")

(defun perinf-memo--close-click (event)
  "Close the memo in the window receiving mouse EVENT."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (perinf-memo-close-and-save)))

(define-minor-mode perinf-memo-controls-mode
  "Show a close-and-save button above the opened memo."
  :lighter nil
  (if perinf-memo-controls-mode
      (progn
        (setq perinf-memo--previous-header header-line-format)
        (setq header-line-format
              '(:eval (propertize
                       (concat "  [ " (perinf-i18n 'memo.close-save) " ]  ")
                       'face 'link 'mouse-face 'highlight
                       'local-map perinf-memo--header-map))))
    (setq header-line-format perinf-memo--previous-header)))

(defun perinf-memo-close-and-save ()
  "Complete the memo at point, save it and close its window."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) buffer-file-name
               (equal (org-entry-get nil "CATEGORY") "Husk"))
    (user-error "%s" (perinf-i18n 'memo.missing)))
  (let ((org-inhibit-logging t)
        (org-log-done nil))
    (org-todo (car org-done-keywords)))
  (save-buffer)
  (perinf-memo-controls-mode -1)
  (run-hooks 'perinf-memo-after-close-hook)
  (quit-window))

(defun perinf-memo-open (memo)
  "Open the entry described by MEMO, preferring its stable ID."
  (find-file (plist-get memo :file))
  (widen)
  (goto-char (point-min))
  (let ((id (plist-get memo :id)))
    (if id
        (if (re-search-forward
             (concat "^[ \t]*:ID:[ \t]+" (regexp-quote id) "[ \t]*$") nil t)
            (org-back-to-heading t)
          (user-error "%s" (perinf-i18n 'memo.missing)))
      (goto-char (plist-get memo :position))))
  (org-fold-show-context)
  (org-fold-show-entry)
  (unless perinf-memo-controls-mode
    (perinf-memo-controls-mode 1)))

(provide 'perinf-memo)
;;; perinf-memo.el ends here
