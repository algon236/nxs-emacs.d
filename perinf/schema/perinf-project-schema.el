;;; perinf-project-schema.el --- Project metadata schema -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defconst perinf-current-schema-version 1
  "Current persistent project schema version.")

(defconst perinf-project-required-metadata
  '(ID PERINF_TYPE PERINF_STATUS PROJECT_ID PROJECT_TITLE SCHEMA_VERSION
       INTERFACE_LANGUAGE DATE_FORMAT TIME_FORMAT CREATED_AT)
  "Required properties in `perinf-project.org'.")

(provide 'perinf-project-schema)

;;; perinf-project-schema.el ends here
