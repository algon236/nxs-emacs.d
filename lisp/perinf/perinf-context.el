;;; perinf-context.el --- Context workflow -*- lexical-binding: t; -*-

;; Copyright (C) 2026, Niels Søndergaard, Nivaa, Denmark.
;; Author: Niels Søndergaard <niels@algon.dk>
;; Assisted-by: Codex:GPT-6

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

;; Context workflow.

;;; Code:

(require 'perinf-i18n)
(require 'perinf-storage)

(defun perinf-context-create ()
  "Interactively create a context in the current project."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((title
          (string-trim
           (read-string (perinf-i18n 'context.title-prompt))))
         (description
          (string-trim
           (read-string (perinf-i18n 'context.description-prompt))))
         (context
          (perinf-storage-create
           'context
           `((title . ,title) (description . ,description))
           perinf-current-project)))
    (message "%s" (perinf-i18n 'context.created))
    (when (fboundp 'perinf-core-records)
      (perinf-core-records))
    context))

(provide 'perinf-context)

;;; perinf-context.el ends here
