;;; emacs-nxs-org-latex.el --- NXS Org-LaTeX-tabeller -*- lexical-binding: t; -*-

;;; Commentary:

;; Brug tabularrays longtblr som standard ved eksport af Org-tabeller.
;; Den fælles LaTeX-opsætning i ~/include/org/standard.org skjuler
;; longtblrs tomme, automatiske tabeloverskrift (for eksempel "Tabel 1:").
;; Under LaTeX-eksport får den navngivne medicintabel desuden en synlig
;; overskrift og longtblr-miljøet. Org-kildefilen ændres ikke.

;;; Code:

(with-eval-after-load 'ox-latex
  (setq org-latex-default-table-environment "longtblr"
        org-latex-tables-centered nil))

(defun emacs-nxs-org-latex--set-longtblr-no-float (name-regexp)
  "Gør tabellen med et navn svarende til NAME-REGEXP lang og ikke-flydende.
Returnér begyndelsen af tabellens ATTR_LATEX-linje eller nil."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^[ \t]*#\\+NAME:[ \t]*" name-regexp "[ \t]*$") nil t)
    (let ((name-beginning (line-beginning-position))
          attribute-beginning)
      (save-excursion
        (goto-char name-beginning)
        (when (re-search-backward
               "^[ \t]*#\\+ATTR_LATEX:.*:environment[ \t]+\\([^ \t\n]+\\)"
               (max (point-min) (- name-beginning 500)) t)
          (when (string-match-p
                 "\\`[ \t\n]*\\'"
                 (buffer-substring-no-properties
                  (line-end-position) name-beginning))
            (setq attribute-beginning (copy-marker (line-beginning-position)))
            (replace-match "longtblr" t t nil 1)
            (unless (save-excursion
                      (beginning-of-line)
                      (re-search-forward ":float[ \t]+nil" (line-end-position) t))
              (end-of-line)
              (insert " :float nil")))))
      (unless attribute-beginning
        (goto-char name-beginning)
        (setq attribute-beginning (copy-marker (point)))
        (insert "#+ATTR_LATEX: :environment longtblr :float nil\n"))
      attribute-beginning)))

(defun emacs-nxs-org-latex--prepare-medicine-table (backend)
  "Klargør den navngivne medicintabel ved LaTeX-eksport til BACKEND.
Ændringen foretages i Org-eksportens midlertidige buffer, ikke i kildefilen."
  (when (org-export-derived-backend-p backend 'latex)
    (save-excursion
      ;; Den første navngivne tabel er månedens søvntabel.
      (emacs-nxs-org-latex--set-longtblr-no-float "[^ \t\n]+")
      (let ((medicine-attributes
             (emacs-nxs-org-latex--set-longtblr-no-float
              "Status-Medicin-uden-kolonne-2-og-8")))
        (when medicine-attributes
          (goto-char medicine-attributes)
          (insert "#+LATEX: \\begin{center}\\textbf{Medicin Status}\\end{center}\n")
          (set-marker medicine-attributes nil))))))

(add-hook 'org-export-before-parsing-hook
          #'emacs-nxs-org-latex--prepare-medicine-table)

(provide 'emacs-nxs-org-latex)
;;; emacs-nxs-org-latex.el ends here
