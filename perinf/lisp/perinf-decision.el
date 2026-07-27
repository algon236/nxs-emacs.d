;;; perinf-decision.el --- Decision workflow -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'seq)
(require 'perinf-date)
(require 'perinf-i18n)
(require 'perinf-storage)

(defun perinf-decision--setting (property)
  "Return current project PROPERTY as a symbol."
  (intern (alist-get
           property
           (perinf-storage-read-project perinf-current-project))))

(defun perinf-decision-create (&optional minutes-id)
  "Interactively register a decision, optionally sourced from MINUTES-ID."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((date-format (perinf-decision--setting 'DATE_FORMAT))
         (minutes
          (and minutes-id
               (seq-find
                (lambda (candidate)
                  (equal (perinf-object-id candidate) minutes-id))
                (perinf-storage-list 'minutes perinf-current-project))))
         (meeting-id
          (and minutes
               (alist-get 'MEETING_ID (perinf-object-properties minutes))))
         (meeting
          (and meeting-id
               (seq-find
                (lambda (candidate)
                  (equal (perinf-object-id candidate) meeting-id))
                (perinf-storage-list 'meeting perinf-current-project))))
         (default-date
          (and meeting
               (substring
                (alist-get 'START_AT (perinf-object-properties meeting))
                0 10)))
         (title (string-trim
                 (read-string (perinf-i18n 'decision.title-prompt))))
         (date
          (perinf-date-normalize
           (read-string
            (format "%s (%s): "
                    (perinf-i18n 'decision.date-prompt)
                    (perinf-i18n
                     (intern (format "setting.%s" date-format))))
            (and default-date
                 (perinf-date-format default-date date-format)))
           date-format))
         (rationale
          (string-trim
           (read-string (perinf-i18n 'decision.rationale-prompt))))
         (decision
          (perinf-storage-create
           'decision
           `((title . ,title)
             (date . ,date)
             (rationale . ,rationale)
             (meeting-id . ,meeting-id)
             (minutes-id . ,minutes-id))
           perinf-current-project)))
    (message "%s" (perinf-i18n 'decision.created))
    (when (fboundp 'perinf-core-records)
      (perinf-core-records))
    decision))

(defun perinf-decision-create-from-minutes (minutes-id)
  "Register a decision sourced from final-approved MINUTES-ID."
  (interactive)
  (let ((minutes
         (seq-find
          (lambda (candidate)
            (equal (perinf-object-id candidate) minutes-id))
          (perinf-storage-list 'minutes perinf-current-project))))
    (unless minutes
      (signal 'perinf-object-not-found (list minutes-id)))
    (unless (eq (perinf-object-status minutes) 'final-approved)
      (user-error "%s" (perinf-i18n 'decision.requires-approved-minutes)))
    (perinf-decision-create minutes-id)))

(provide 'perinf-decision)

;;; perinf-decision.el ends here
