;;; perinf-object-types.el --- Object type registry -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defconst perinf-object-types
  '(project-metadata context person person-group task meeting meeting-series participant
    agenda-item document audio-recording transcript corrected-transcript
    minutes-series minutes minutes-section decision task-proposal
    decision-proposal history-entry)
  "Language-independent object types in the initial Personal Work and Information System core.")

(provide 'perinf-object-types)

;;; perinf-object-types.el ends here
