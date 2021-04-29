;;; abbrevs.el -*- lexical-binding: t; -*-

(define-abbrev-table 'global-abbrev-table
  '(
    ("ra" "→")
    ("la" "←")
    ("tu" "👍")
    ("td" "👎")
    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
