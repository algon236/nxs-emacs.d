;;; perinf-project-schema.el --- Project metadata schema -*- lexical-binding: t; -*-

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

;; Project metadata schema.

;;; Code:

(defconst perinf-current-schema-version 1
  "Current persistent project schema version.")

(defconst perinf-project-required-metadata
  '(ID PERINF_TYPE PERINF_STATUS PROJECT_ID PROJECT_TITLE SCHEMA_VERSION
       INTERFACE_LANGUAGE DATE_FORMAT TIME_FORMAT CREATED_AT)
  "Required properties in `perinf-project.org'.")

(provide 'perinf-project-schema)

;;; perinf-project-schema.el ends here
