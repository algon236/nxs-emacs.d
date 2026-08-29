;;; perinf-properties.el --- Property registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026, Niels Søndergaard, Nivaa, Denmark.
;; Author: Niels Søndergaard, mail: niels<at>algon.dk

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

;;; Code:

(defconst perinf-property-definitions
  '((ID :type identifier :access system :required t)
    (PERINF_TYPE :type enum :access system :required t)
    (PERINF_STATUS :type enum :access controlled :required t)
    (CREATED_AT :type datetime :access system :required t)
    (MODIFIED_AT :type datetime :access system :required t)
    (CONTEXT_ID :type identifier :access user :required nil)
    (SCHEMA_VERSION :type integer :access system :required t)
    (PROJECT_ID :type identifier :access system :required t)
    (PROJECT_TITLE :type string :access user :required t)
    (AUDIO_ID :type identifier :access controlled :required nil)
    (AUDIO_STATUS :type enum :access controlled :required nil)
    (MEETING_ID :type identifier :access controlled :required nil)
    (PARENT_TYPE :type enum :access controlled :required nil)
    (PARENT_ID :type identifier :access controlled :required nil)
    (AGENDA_ITEM_ID :type identifier :access controlled :required nil)
    (FILE_REFERENCE :type file-reference :access controlled :required nil)
    (ORIGINAL_FILE_NAME :type string :access system :required nil)
    (CHECKSUM_SHA256 :type checksum :access system :required nil)
    (FILE_SIZE_BYTES :type integer :access system :required nil)
    (TRANSCRIPT_ID :type identifier :access controlled :required nil)
    (TRANSCRIPT_STATUS :type enum :access controlled :required nil)
    (TRANSCRIPTION_METHOD :type enum :access system :required nil)
    (MINUTES_ID :type identifier :access controlled :required nil)
    (MINUTES_STATUS :type enum :access controlled :required nil)
    (ACTUAL_START_AT :type datetime :access system :required nil)
    (ACTUAL_FINISH_AT :type datetime :access system :required nil)
    (TASK_TIMER_STARTED_AT :type datetime :access controlled :required nil)
    (TASK_WORK_SECONDS :type integer :access controlled :required nil)
    (TASK_LAST_ACTIVITY_AT :type datetime :access system :required nil)
    (TASK_LAST_ACTIVITY_RESOURCE :type string :access system :required nil)
    (TASK_ACTIVITY_FILES :type string-list :access controlled :required nil)
    (TASK_ACTIVITY_BUFFERS :type string-list :access controlled :required nil)
    (DECIDED_ON :type date :access user :required nil)
    (RATIONALE :type string :access user :required nil)
    (DECISION_ID :type identifier :access controlled :required nil)
    (ASSIGNEE_ID :type identifier :access controlled :required nil)
    (ASSIGNEE_IDS :type identifier-list :access controlled :required nil)
    (MEMBER_IDS :type identifier-list :access controlled :required nil)
    (GENERATION_METHOD :type enum :access system :required nil)
    (APPROVED_AT :type datetime :access system :required nil)
    (APPROVED_BY :type string :access controlled :required nil)
    (SUBMITTED_AT :type datetime :access system :required nil)
    (SUBMITTED_BY :type string :access controlled :required nil)
    (REJECTED_AT :type datetime :access system :required nil)
    (REJECTED_BY :type string :access controlled :required nil)
    (REJECTION_REASON :type string :access controlled :required nil)
    (SOURCE_CHECKSUM_SHA256 :type checksum :access system :required nil)
    (CONTENT_CHECKSUM_SHA256 :type checksum :access system :required nil)
    (APPROVED_CONTENT_CHECKSUM_SHA256
     :type checksum :access system :required nil))
  "Authoritative bootstrap property definitions.")

(provide 'perinf-properties)

;;; perinf-properties.el ends here
