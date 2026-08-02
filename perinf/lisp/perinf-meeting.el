;;; perinf-meeting.el --- Meeting workflow for Personal Work and Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'perinf-date)
(require 'perinf-i18n)
(require 'perinf-storage)
(require 'perinf-time)
(require 'seq)
(require 'subr-x)

(defun perinf-meeting--setting (property)
  "Return PROPERTY from the current project metadata."
  (alist-get property
             (perinf-storage-read-project perinf-current-project)
             nil nil #'eq))

;;;###autoload
(defun perinf-meeting-create ()
  "Interactively create a meeting in the current Personal Work and Information System project."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((date-format-name (perinf-meeting--setting 'DATE_FORMAT))
         (time-format-name (perinf-meeting--setting 'TIME_FORMAT))
         (date-format (intern date-format-name))
         (time-format (intern time-format-name))
         (title (read-string (perinf-i18n 'meeting.title-prompt)))
         (date-text
          (read-string
           (format "%s (%s): "
                   (perinf-i18n 'meeting.date-prompt)
                   (perinf-i18n
                    (intern (format "setting.%s" date-format-name))))))
         (date (perinf-date-normalize date-text date-format))
         (start
          (perinf-time-normalize
           (read-string
            (format "%s (%s): "
                    (perinf-i18n 'meeting.start-prompt)
                    (perinf-i18n
                     (intern (format "setting.%s" time-format-name)))))
           time-format))
         (finish
          (perinf-time-normalize
           (read-string
            (format "%s (%s): "
                    (perinf-i18n 'meeting.finish-prompt)
                    (perinf-i18n
                     (intern (format "setting.%s" time-format-name)))))
           time-format))
         (location (read-string (perinf-i18n 'meeting.location-prompt)))
         (meeting
          (perinf-storage-create
           'meeting
           `((title . ,title)
             (date . ,date)
             (start-time . ,start)
             (finish-time . ,finish)
             (location . ,location))
           perinf-current-project)))
    (message "%s" (perinf-i18n 'meeting.created))
    (when (fboundp 'perinf-core-meetings)
      (perinf-core-meetings))
    meeting))

(defun perinf-meeting--select-meeting ()
  "Prompt for a meeting and return its ID."
  (let* ((meetings
          (perinf-storage-list 'meeting perinf-current-project))
         (choices
          (mapcar
           (lambda (meeting)
             (let ((start
                    (alist-get 'START_AT
                               (perinf-object-properties meeting))))
               (cons (format "%s — %s"
                             (perinf-object-title meeting)
                             (substring start 0 10))
                     (perinf-object-id meeting))))
           meetings)))
    (unless choices
      (user-error "%s" (perinf-i18n 'meeting.none)))
    (cdr (assoc
          (completing-read
           (perinf-i18n 'meeting.select-prompt) choices nil t)
          choices))))

(defun perinf-meeting--select-person ()
  "Prompt for a person and return its ID."
  (let* ((people
          (seq-filter
           (lambda (person)
             (eq (perinf-object-status person) 'active))
           (perinf-storage-list 'person perinf-current-project)))
         (choices
          (mapcar (lambda (person)
                    (cons (perinf-object-title person)
                          (perinf-object-id person)))
                  people)))
    (unless choices
      (user-error "%s" (perinf-i18n 'person.none)))
    (cdr (assoc
          (completing-read
           (perinf-i18n 'person.select-prompt) choices nil t)
          choices))))

(defun perinf-meeting--select-role ()
  "Prompt for a participant role and return its internal symbol."
  (let ((choices
         `((,(perinf-i18n 'role.participant) . participant)
           (,(perinf-i18n 'role.chair) . chair)
           (,(perinf-i18n 'role.secretary) . secretary)
           (,(perinf-i18n 'role.guest) . guest))))
    (cdr (assoc
          (completing-read
           (perinf-i18n 'role.prompt) choices nil t
           (perinf-i18n 'role.participant))
          choices))))

;;;###autoload
(defun perinf-meeting-add-participant (&optional meeting-id)
  "Add a registered person to MEETING-ID."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((selected-meeting (or meeting-id (perinf-meeting--select-meeting)))
        (person-id (perinf-meeting--select-person))
        (role (perinf-meeting--select-role)))
    (perinf-storage-add-child
     selected-meeting 'participants 'participant
     `((PERSON_ID . ,person-id)
       (PARTICIPANT_ROLE . ,role)
       (ATTENDANCE_STATUS . invited))
     perinf-current-project)
    (message "%s" (perinf-i18n 'meeting.participant-added))
    (when (fboundp 'perinf-core-meetings)
      (perinf-core-meetings))))

(defun perinf-meeting--select-agenda-kind ()
  "Prompt for an agenda kind and return its internal symbol."
  (let ((choices
         `((,(perinf-i18n 'common.cancel) . cancel)
           (,(perinf-i18n 'agenda.kind.information) . information)
           (,(perinf-i18n 'agenda.kind.discussion) . discussion)
           (,(perinf-i18n 'agenda.kind.decision) . decision)
           (,(perinf-i18n 'agenda.kind.election) . election)
           (,(perinf-i18n 'agenda.kind.approval) . approval)
           (,(perinf-i18n 'agenda.kind.other) . other))))
    (cdr (assoc
          (completing-read
           (perinf-i18n 'agenda.kind-prompt)
           choices nil t
           (perinf-i18n 'agenda.kind.discussion))
          choices))))

(defun perinf-meeting--read-agenda-text (prompt)
  "Read required agenda text with an explicit empty-input cancel path."
  (let ((value (string-trim (read-string prompt))))
    (unless (string-empty-p value) value)))

;;;###autoload
(defun perinf-meeting-add-agenda-item (&optional meeting-id)
  "Add an agenda item to MEETING-ID."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((selected-meeting (or meeting-id (perinf-meeting--select-meeting)))
         (number (perinf-meeting--read-agenda-text
                  (perinf-i18n 'agenda.number-prompt)))
         (title (and number
                     (perinf-meeting--read-agenda-text
                      (perinf-i18n 'agenda.title-prompt))))
         (kind (and title (perinf-meeting--select-agenda-kind))))
    (if (or (null number) (null title) (eq kind 'cancel))
        (message "%s" (perinf-i18n 'agenda.cancelled))
      (perinf-storage-add-child
       selected-meeting 'agenda 'agenda-item
       `((title . ,title)
         (AGENDA_NUMBER . ,number)
         (AGENDA_KIND . ,kind))
       perinf-current-project)
      (message "%s" (perinf-i18n 'agenda.created))
      (when (fboundp 'perinf-core-meetings)
        (perinf-core-meetings)))))

;;;###autoload
(defun perinf-meeting-attach-document (meeting-id &optional agenda-item-id)
  "Attach a document to MEETING-ID or optional AGENDA-ITEM-ID."
  (interactive (list (perinf-meeting--select-meeting) nil))
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((source-file
         (read-file-name (perinf-i18n 'document.file-prompt) nil nil t)))
    (perinf-storage-attach-document
     meeting-id agenda-item-id source-file perinf-current-project)
    (message "%s" (perinf-i18n 'document.attached))
    (when (fboundp 'perinf-core-meetings)
      (perinf-core-meetings))))

;;;###autoload
(defun perinf-meeting-attach-audio (&optional meeting-id)
  "Attach an existing audio file to MEETING-ID."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((selected-meeting
          (or meeting-id (perinf-meeting--select-meeting)))
         (source-file
          (read-file-name
           (perinf-i18n 'audio.file-prompt)
           nil nil t)))
    (perinf-storage-attach-audio
     selected-meeting source-file perinf-current-project)
    (message "%s" (perinf-i18n 'audio.attached))
    (when (fboundp 'perinf-core-meetings)
      (perinf-core-meetings))))

;;;###autoload
(defun perinf-meeting-import-transcript (&optional meeting-id)
  "Import a raw text transcript for MEETING-ID."
  (interactive)
  (unless (and (boundp 'perinf-current-project) perinf-current-project)
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((selected-meeting
          (or meeting-id (perinf-meeting--select-meeting)))
         (source-file
          (read-file-name
           (perinf-i18n 'transcript.file-prompt)
           nil nil t)))
    (perinf-storage-import-transcript
     selected-meeting source-file perinf-current-project)
    (message "%s" (perinf-i18n 'transcript.imported))
    (when (fboundp 'perinf-core-meetings)
      (perinf-core-meetings))))

(defun perinf-meeting-import-generated-minutes (&optional meeting-id)
  "Import externally generated draft minutes for MEETING-ID."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((selected-meeting (or meeting-id (perinf-meeting--select-meeting)))
         (source-file
          (read-file-name (perinf-i18n 'minutes.file-prompt) nil nil t)))
    (perinf-storage-import-generated-minutes
     selected-meeting source-file perinf-current-project)
    (message "%s" (perinf-i18n 'minutes.imported))
    (when (fboundp 'perinf-core-meetings)
      (perinf-core-meetings))))

(defun perinf-meeting-approve-minutes (minutes-id)
  "Ask for human confirmation and approve MINUTES-ID."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (when (yes-or-no-p (perinf-i18n 'minutes.approve-confirmation))
    (let ((approved-by
           (string-trim
            (read-string (perinf-i18n 'minutes.approved-by-prompt)
                         user-full-name))))
      (perinf-storage-approve-minutes
       minutes-id approved-by perinf-current-project)
      (message "%s" (perinf-i18n 'minutes.approved))
      (perinf-core-meetings))))

(defun perinf-meeting-submit-minutes (minutes-id)
  "Submit MINUTES-ID for final human approval."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (when (yes-or-no-p (perinf-i18n 'minutes.submit-confirmation))
    (let ((submitted-by
           (string-trim
            (read-string (perinf-i18n 'minutes.submitted-by-prompt)
                         user-full-name))))
      (perinf-storage-submit-minutes
       minutes-id submitted-by perinf-current-project)
      (message "%s" (perinf-i18n 'minutes.submitted))
      (perinf-core-meetings))))

(defun perinf-meeting-edit-minutes (minutes-id)
  "Open the Org file containing MINUTES-ID for human review."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((minutes
         (seq-find
          (lambda (candidate)
            (equal (perinf-object-id candidate) minutes-id))
          (perinf-storage-list 'minutes perinf-current-project))))
    (unless minutes
      (signal 'perinf-object-not-found (list minutes-id)))
    (find-file (perinf-object-file minutes))
    (goto-char (point-min))
    (when (perinf-storage--find-id minutes-id)
      (org-fold-show-subtree))))

(defun perinf-meeting-reject-minutes (minutes-id)
  "Reject MINUTES-ID and record the human review decision."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((rejected-by
          (string-trim
           (read-string (perinf-i18n 'minutes.rejected-by-prompt)
                        user-full-name)))
         (reason
          (string-trim
           (read-string (perinf-i18n 'minutes.rejection-reason-prompt)))))
    (when (yes-or-no-p (perinf-i18n 'minutes.reject-confirmation))
      (perinf-storage-reject-minutes
       minutes-id rejected-by reason perinf-current-project)
      (message "%s" (perinf-i18n 'minutes.rejected))
      (perinf-core-meetings))))

(defun perinf-meeting-start (&optional meeting-id)
  "Mark MEETING-ID as in progress."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((selected-meeting
         (or meeting-id (perinf-meeting--select-meeting))))
    (perinf-storage-set-meeting-status
     selected-meeting 'in-progress perinf-current-project)
    (message "%s" (perinf-i18n 'meeting.started))
    (perinf-core-meetings)))

(defun perinf-meeting-finish (&optional meeting-id)
  "Mark MEETING-ID as held."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((selected-meeting
         (or meeting-id (perinf-meeting--select-meeting))))
    (perinf-storage-set-meeting-status
     selected-meeting 'held perinf-current-project)
    (message "%s" (perinf-i18n 'meeting.finished))
    (perinf-core-meetings)))

(defun perinf-meeting-postpone (&optional meeting-id)
  "Mark MEETING-ID as postponed."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((selected-meeting
         (or meeting-id (perinf-meeting--select-meeting))))
    (perinf-storage-set-meeting-status
     selected-meeting 'postponed perinf-current-project)
    (message "%s" (perinf-i18n 'meeting.postponed))
    (perinf-core-meetings)))

(defun perinf-meeting-resume-planning (&optional meeting-id)
  "Return postponed MEETING-ID to planned status."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((selected-meeting
         (or meeting-id (perinf-meeting--select-meeting))))
    (perinf-storage-set-meeting-status
     selected-meeting 'planned perinf-current-project)
    (message "%s" (perinf-i18n 'meeting.planned-again))
    (perinf-core-meetings)))

(defun perinf-meeting-cancel (&optional meeting-id)
  "Mark MEETING-ID as cancelled after human confirmation."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let ((selected-meeting
         (or meeting-id (perinf-meeting--select-meeting))))
    (when (yes-or-no-p (perinf-i18n 'meeting.cancel-confirmation))
      (perinf-storage-set-meeting-status
       selected-meeting 'cancelled perinf-current-project)
      (message "%s" (perinf-i18n 'meeting.cancelled))
      (perinf-core-meetings))))

(defun perinf-meeting-set-attendance
    (meeting-id participant-id attendance)
  "Set PARTICIPANT-ID attendance in MEETING-ID to ATTENDANCE."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (perinf-storage-set-attendance
   meeting-id participant-id attendance perinf-current-project)
  (message "%s" (perinf-i18n 'attendance.updated))
  (let ((meeting
         (seq-find
          (lambda (candidate)
            (equal (perinf-object-id candidate) meeting-id))
          (perinf-storage-list 'meeting perinf-current-project))))
    (if meeting
        (perinf-core-show-object meeting)
      (perinf-core-meetings))))

(provide 'perinf-meeting)

;;; perinf-meeting.el ends here
