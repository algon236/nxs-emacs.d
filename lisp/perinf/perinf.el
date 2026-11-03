;;; perinf.el --- Org-backed work management core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Personal Work and Information System contributors
;; Copyright (C) 2026, Niels Søndergaard, Nivaa, Denmark.

;; Author: Niels Søndergaard <niels@algon.dk>
;; Assisted-by: Codex:GPT-6
;; Version: 1.0.2
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, calendar, convenience
;; URL: https://github.com/algon236/perinf
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is part of Personal Work and Information System.
;;
;; Personal Work and Information System is free software: you can redistribute
;; it and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; Personal Work and Information System is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Personal Work and Information System presents tasks, meetings, people,
;; transcripts, and minutes while keeping ordinary Org files as the
;; persistent source of truth.

;;; Code:

(require 'perinf-core)

;;;###autoload (autoload 'perinf "perinf" nil t)
(defalias 'perinf #'perinf-core-open)

;;;###autoload (autoload 'perinf-create-project "perinf" nil t)
(defalias 'perinf-create-project #'perinf-core-create-project)

;;;###autoload (autoload 'perinf-open-project "perinf" nil t)
(defalias 'perinf-open-project #'perinf-core-select-project)

;;;###autoload (autoload 'perinf-home "perinf" nil t)
(defalias 'perinf-home #'perinf-core-home)

;;;###autoload (autoload 'perinf-work "perinf" nil t)
(defalias 'perinf-work #'perinf-core-work)

;;;###autoload (autoload 'perinf-meetings "perinf" nil t)
(defalias 'perinf-meetings #'perinf-core-meetings)

;;;###autoload (autoload 'perinf-people "perinf" nil t)
(defalias 'perinf-people #'perinf-core-people)

;;;###autoload (autoload 'perinf-records "perinf" nil t)
(defalias 'perinf-records #'perinf-core-records)

;;;###autoload (autoload 'perinf-administration "perinf" nil t)
(defalias 'perinf-administration #'perinf-core-administration)

;;;###autoload (autoload 'perinf-create-task "perinf" nil t)
(defalias 'perinf-create-task #'perinf-task-create)

;;;###autoload (autoload 'perinf-complete-task "perinf" nil t)
(defalias 'perinf-complete-task #'perinf-task-complete)

;;;###autoload
(defun perinf-start-task-timer (task-id)
  "Start TASK-ID's work timer."
  (interactive (list (perinf-task--select-timer-task t)))
  (perinf-task-toggle-timer task-id t)
  (perinf-task-maybe-associate-current-buffer task-id))

;;;###autoload
(defun perinf-stop-task-timer (task-id)
  "Stop TASK-ID's work timer."
  (interactive (list (perinf-task--select-timer-task nil)))
  (perinf-task-toggle-timer task-id nil))

;;;###autoload
(defun perinf-reset-task-timer (task-id)
  "Reset TASK-ID's work timer without ending the task."
  (interactive (list (perinf-task--select-open-task)))
  (perinf-task-reset-timer task-id))

;;;###autoload (autoload 'perinf-associate-buffer-with-task "perinf" nil t)
(defalias 'perinf-associate-buffer-with-task #'perinf-task-associate-buffer)

;;;###autoload (autoload 'perinf-dissociate-buffer-from-task "perinf" nil t)
(defalias 'perinf-dissociate-buffer-from-task
  #'perinf-task-dissociate-current-buffer)

;;;###autoload (autoload 'perinf-create-task-from-decision "perinf" nil t)
(defalias 'perinf-create-task-from-decision
  #'perinf-task-create-from-decision)

;;;###autoload (autoload 'perinf-assign-task "perinf" nil t)
(defalias 'perinf-assign-task #'perinf-task-assign)

;;;###autoload (autoload 'perinf-set-task-context "perinf" nil t)
(defalias 'perinf-set-task-context #'perinf-task-set-context)

;;;###autoload (autoload 'perinf-create-meeting "perinf" nil t)
(defalias 'perinf-create-meeting #'perinf-meeting-create)

;;;###autoload (autoload 'perinf-edit-meeting "perinf" nil t)
(defalias 'perinf-edit-meeting #'perinf-meeting-edit)

;;;###autoload (autoload 'perinf-create-person "perinf" nil t)
(defalias 'perinf-create-person #'perinf-person-create)

;;;###autoload (autoload 'perinf-create-person-group "perinf" nil t)
(defalias 'perinf-create-person-group #'perinf-person-group-create)

;;;###autoload (autoload 'perinf-create-decision "perinf" nil t)
(defalias 'perinf-create-decision #'perinf-decision-create)

;;;###autoload (autoload 'perinf-create-context "perinf" nil t)
(defalias 'perinf-create-context #'perinf-context-create)

;;;###autoload (autoload 'perinf-create-decision-from-minutes "perinf" nil t)
(defalias 'perinf-create-decision-from-minutes
  #'perinf-decision-create-from-minutes)

;;;###autoload (autoload 'perinf-search "perinf" nil t)
(defalias 'perinf-search #'perinf-core-search)

;;;###autoload (autoload 'perinf-add-meeting-participant "perinf" nil t)
(defalias 'perinf-add-meeting-participant
  #'perinf-meeting-add-participant)

;;;###autoload (autoload 'perinf-add-agenda-item "perinf" nil t)
(defalias 'perinf-add-agenda-item
  #'perinf-meeting-add-agenda-item)

;;;###autoload (autoload 'perinf-attach-meeting-audio "perinf" nil t)
(defalias 'perinf-attach-meeting-audio
  #'perinf-meeting-attach-audio)

;;;###autoload (autoload 'perinf-attach-meeting-document "perinf" nil t)
(defalias 'perinf-attach-meeting-document
  #'perinf-meeting-attach-document)

;;;###autoload (autoload 'perinf-import-meeting-transcript "perinf" nil t)
(defalias 'perinf-import-meeting-transcript
  #'perinf-meeting-import-transcript)

;;;###autoload (autoload 'perinf-import-generated-minutes "perinf" nil t)
(defalias 'perinf-import-generated-minutes
  #'perinf-meeting-import-generated-minutes)

;;;###autoload (autoload 'perinf-approve-minutes "perinf" nil t)
(defalias 'perinf-approve-minutes
  #'perinf-meeting-approve-minutes)

;;;###autoload (autoload 'perinf-submit-minutes "perinf" nil t)
(defalias 'perinf-submit-minutes
  #'perinf-meeting-submit-minutes)

;;;###autoload (autoload 'perinf-edit-minutes "perinf" nil t)
(defalias 'perinf-edit-minutes
  #'perinf-meeting-edit-minutes)

;;;###autoload (autoload 'perinf-reject-minutes "perinf" nil t)
(defalias 'perinf-reject-minutes
  #'perinf-meeting-reject-minutes)

;;;###autoload (autoload 'perinf-start-meeting "perinf" nil t)
(defalias 'perinf-start-meeting #'perinf-meeting-start)

;;;###autoload (autoload 'perinf-finish-meeting "perinf" nil t)
(defalias 'perinf-finish-meeting #'perinf-meeting-finish)

;;;###autoload (autoload 'perinf-set-meeting-attendance "perinf" nil t)
(defalias 'perinf-set-meeting-attendance
  #'perinf-meeting-set-attendance)

(provide 'perinf)

;;; perinf.el ends here
