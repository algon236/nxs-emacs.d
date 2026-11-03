;;; flick.el --- flick choose font  -*- lexical-binding: t; byte-compile-warnings: (not free-vars unresolved make-local); -*-

;; Copyright (C) Copenhagen,  2026  Niels Søndergaard

;; Author: Niels Søndergaard <niels@algon.dk>
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ;;; flick.el --- Pick font families and sizes with completion -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Vegas Harmon

;; Author: Vegas Harmon <vegasharmon@protonmail.com>
;; Maintainer: Vegas Harmon <vegasharmon@protonmail.com>
;; URL: https://gitlab.com/vegasharmon/flick
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces, convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Choose a font family from the installed ones, with completion, then a size
;; in points, and apply both to a group of faces.
;;
;; A group of faces is a role.  The default roles are:
;;
;;   monospace       `default' and `fixed-pitch'; candidates are monospace
;;   variable-pitch  `variable-pitch'
;;   serif           `fixed-pitch-serif'
;;
;; Commands:
;;
;;   `flick-choose-font'                 the monospace role
;;   `flick-choose-variable-pitch-font'  the variable-pitch role
;;   `flick-choose'                      any role, read first
;;   `flick-refresh-cache'               rescan for monospace families
;;
;; By default the choice is applied with `set-face-attribute' on every frame.
;; If your config already owns its fonts (fallback fontsets, per-monitor
;; sizing, a theme hook that reapplies them), set `flick-apply-function'
;; to a function of ROLE, FAMILY and SIZE that records the choice your way.
;;
;; Candidates carry the completion category `font-family', so completion UIs
;; and annotators that know that category (consult, marginalia) apply.
;;
;; With consult installed, the family prompt previews each family on the
;; role's faces as you move through the candidates (see `flick-preview').

;;; Code:

(require 'cl-lib)

(defgroup flick nil
  "Pick font families and sizes with completion."
  :group 'faces
  :prefix "flick-")

(defcustom flick-roles
  '((monospace      :faces (default fixed-pitch) :monospace t)
    (variable-pitch :faces (variable-pitch))
    (serif          :faces (fixed-pitch-serif)))
  "Font roles, as (ROLE . PLIST).
PLIST keys:
  :faces      the faces the role sets; the first one supplies the
              current family and size offered as defaults.
  :monospace  non-nil to offer only monospace families, where they
              can be detected."
  :type '(alist :key-type symbol
                :value-type (plist :options ((:faces (repeat face))
                                             (:monospace boolean)))))

(defcustom flick-apply-function #'flick-set-faces
  "Function that applies a choice.
Called with ROLE (a key of `flick-roles'), FAMILY (a string) and
SIZE (a number of points)."
  :type 'function)

(defcustom flick-preview t
  "Non-nil to preview families on a role's faces while choosing.
Previewing needs consult; without it families are read with plain
`completing-read' and not previewed."
  :type 'boolean)

(defvar flick-family-history nil
  "Minibuffer history of chosen font families.")

(defvar flick--monospace-cache nil
  "Cached result of `flick-monospace-families'.")

;;;; Families

(defun flick-families ()
  "All installed font families, sorted and without duplicates."
  (sort (delete-dups (copy-sequence (font-family-list))) #'string-lessp))

(defun flick--fixed-spacing-p (spacing)
  "Non-nil if SPACING, a font's :spacing, is dual, mono or charcell.
Font backends report it as a number (0 proportional, 90 dual, 100 mono,
110 charcell) or, in XLFD terms, as a symbol."
  (if (integerp spacing)
      (>= spacing 90)
    (memq spacing '(d m c))))

(defun flick--monospace-p (family)
  "Non-nil if FAMILY has dual, mono or charcell spacing."
  (when-let* ((font (ignore-errors (find-font (font-spec :family family)))))
    (flick--fixed-spacing-p (font-get font :spacing))))

(defun flick-monospace-families ()
  "Monospace families where detectable, else every family.
Opening each family is slow, so the result is cached for the session;
`flick-refresh-cache' rescans."
  (or flick--monospace-cache
      (setq flick--monospace-cache
            (let ((all (flick-families)))
              (or (cl-remove-if-not #'flick--monospace-p all) all)))))

(defun flick-refresh-cache ()
  "Rescan the installed fonts for monospace families."
  (interactive)
  (setq flick--monospace-cache nil)
  (message "flick: %d monospace families"
           (length (flick-monospace-families))))

;;;; Reading a choice

(defun flick--role (role)
  "The plist for ROLE, or an error if it isn't in `flick-roles'."
  (or (alist-get role flick-roles)
      (user-error "flick: no role `%s' in `flick-roles'" role)))

(defun flick-current (role)
  "The current (FAMILY . SIZE) of ROLE's first face.
FAMILY is nil when unspecified; SIZE is in points, or nil."
  (let* ((face (car (plist-get (flick--role role) :faces)))
         (family (and face (face-attribute face :family nil t)))
         (height (and face (face-attribute face :height nil t))))
    (cons (and (stringp family) family)
          (and (integerp height) (/ height 10.0)))))

(defun flick--table (candidates)
  "A completion table over CANDIDATES in the `font-family' category.
It sets no sort function, so the completion UI sorts as it likes."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . font-family))
      (complete-with-action action candidates string pred))))

(declare-function consult--read "ext:consult")

(defun flick-read-family (prompt candidates &optional default preview)
  "Read one of CANDIDATES with PROMPT, offering DEFAULT.
PREVIEW, if non-nil, is called with each family the completion UI
selects and with nil to undo the preview.  It is only called when
consult is installed."
  (if (and preview (require 'consult nil t))
      (consult--read candidates
                     :prompt (format-prompt prompt default)
                     :category 'font-family
                     :require-match t
                     :default default
                     :history 'flick-family-history
                     :state (lambda (action family)
                              (pcase action
                                ('preview (funcall preview family))
                                ;; Consult undoes the preview on exit; keep
                                ;; the chosen family shown while the size is
                                ;; read.
                                ('return (when family
                                           (funcall preview family))))))
    (completing-read (format-prompt prompt default)
                     (flick--table candidates)
                     nil t nil 'flick-family-history default)))

(defun flick-read-size (&optional default)
  "Read a positive size in points, offering DEFAULT."
  (let ((default (and default (if (= default (truncate default))
                                  (truncate default)
                                default))))
    (cl-loop for size = (read-number "Size (pt): " default)
             when (> size 0) return size
             do (message "The size must be positive") (sit-for 1))))

(defun flick-read-role ()
  "Read a role from `flick-roles'."
  (intern (completing-read "Font role: "
                           (mapcar (lambda (r) (symbol-name (car r)))
                                   flick-roles)
                           nil t)))

;;;; Applying a choice

(defun flick--previewer (faces)
  "A function that sets FACES to a family, or back to their own with nil."
  (let ((saved (mapcar (lambda (face) (face-attribute face :family)) faces)))
    (lambda (family)
      (cl-mapc (lambda (face old)
                 (set-face-attribute face nil :family (or family old)))
               faces saved))))

(defun flick-set-faces (role family size)
  "Set ROLE's faces to FAMILY at SIZE points, on every frame."
  (dolist (face (plist-get (flick--role role) :faces))
    (set-face-attribute face nil :family family :height (round (* 10 size)))))

;;;; Commands

;;;###autoload
(defun flick-choose (role)
  "Choose a family and size for ROLE and apply them.
Interactively, read ROLE from `flick-roles'."
  (interactive (list (flick-read-role)))
  (let* ((spec (flick--role role))
         (candidates (if (plist-get spec :monospace)
                         (flick-monospace-families)
                       (flick-families)))
         (current (flick-current role))
         (preview (and flick-preview
                       (flick--previewer (plist-get spec :faces))))
         family size)
    ;; A terminal frame lists no fonts; the terminal owns them.
    (unless candidates
      (user-error "flick: no fonts to choose from %s"
                  "(a terminal frame uses the terminal's font)"))
    (unwind-protect
        (setq family (flick-read-family
                      (format "%s font" (capitalize (symbol-name role)))
                      candidates (car current) preview)
              size (flick-read-size (cdr current)))
      ;; Undo any preview, so the apply function starts from the old faces
      ;; and a quit leaves them as they were.
      (when preview
        (funcall preview nil)))
    (funcall flick-apply-function role family size)
    (message "%s font set to %s %spt" role family size)))

;;;###autoload
(defun flick-choose-font ()
  "Choose the monospace font."
  (interactive)
  (flick-choose 'monospace))

;;;###autoload
(defun flick-choose-variable-pitch-font ()
  "Choose the variable-pitch font."
  (interactive)
  (flick-choose 'variable-pitch))

(provide 'flick)
;;; flick.el ends here


;;; Code:



(provide 'flick)

;;; flick.el ends here
