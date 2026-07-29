;;; perinf.el --- Org-backed work management core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Personal Work and Information System contributors

;; Author: Personal Work and Information System contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, calendar, convenience
;; URL: https://example.invalid/perinf
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Personal Work and Information System presents tasks, meetings, people, transcripts, and minutes while
;; keeping ordinary Org files as the persistent source of truth.

;;; Code:

(let ((root (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (directory '("lisp" "schema" "locales"))
    (add-to-list 'load-path (expand-file-name directory root))))

(require 'perinf-core)

;;;###autoload
(defalias 'perinf #'perinf-core-open)

;;;###autoload
(defalias 'perinf-create-project #'perinf-core-create-project)

;;;###autoload
(defalias 'perinf-open-project #'perinf-core-select-project)

;;;###autoload
(defalias 'perinf-home #'perinf-core-home)

;;;###autoload
(defalias 'perinf-work #'perinf-core-work)

;;;###autoload
(defalias 'perinf-meetings #'perinf-core-meetings)

;;;###autoload
(defalias 'perinf-records #'perinf-core-records)

;;;###autoload
(defalias 'perinf-administration #'perinf-core-administration)

;;;###autoload
(defalias 'perinf-create-task #'perinf-task-create)

;;;###autoload
(defalias 'perinf-complete-task #'perinf-task-complete)

;;;###autoload
(defalias 'perinf-create-task-from-decision
  #'perinf-task-create-from-decision)

;;;###autoload
(defalias 'perinf-assign-task #'perinf-task-assign)

;;;###autoload
(defalias 'perinf-set-task-context #'perinf-task-set-context)

;;;###autoload
(defalias 'perinf-create-meeting #'perinf-meeting-create)

;;;###autoload
(defalias 'perinf-create-person #'perinf-person-create)

;;;###autoload
(defalias 'perinf-create-decision #'perinf-decision-create)

;;;###autoload
(defalias 'perinf-create-context #'perinf-context-create)

;;;###autoload
(defalias 'perinf-create-decision-from-minutes
  #'perinf-decision-create-from-minutes)

;;;###autoload
(defalias 'perinf-search #'perinf-core-search)

;;;###autoload
(defalias 'perinf-add-meeting-participant
  #'perinf-meeting-add-participant)

;;;###autoload
(defalias 'perinf-add-agenda-item
  #'perinf-meeting-add-agenda-item)

;;;###autoload
(defalias 'perinf-attach-meeting-audio
  #'perinf-meeting-attach-audio)

;;;###autoload
(defalias 'perinf-import-meeting-transcript
  #'perinf-meeting-import-transcript)

;;;###autoload
(defalias 'perinf-import-generated-minutes
  #'perinf-meeting-import-generated-minutes)

;;;###autoload
(defalias 'perinf-approve-minutes
  #'perinf-meeting-approve-minutes)

;;;###autoload
(defalias 'perinf-submit-minutes
  #'perinf-meeting-submit-minutes)

;;;###autoload
(defalias 'perinf-edit-minutes
  #'perinf-meeting-edit-minutes)

;;;###autoload
(defalias 'perinf-reject-minutes
  #'perinf-meeting-reject-minutes)

;;;###autoload
(defalias 'perinf-start-meeting #'perinf-meeting-start)

;;;###autoload
(defalias 'perinf-finish-meeting #'perinf-meeting-finish)

;;;###autoload
(defalias 'perinf-set-meeting-attendance
  #'perinf-meeting-set-attendance)

(provide 'perinf)

;;; perinf.el ends here
