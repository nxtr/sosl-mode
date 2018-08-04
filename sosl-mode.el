;;; sosl-mode.el --- Salesforce SOSL Major Mode   -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Magnus Nyberg

;; Author: Magnus Nyberg <magnus@nexter.se>
;; Keywords: languages, sosl, apex, force, sfdc, salesforce
;; URL: https://github.com/nxtr/sosl-mode

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing Salesforce Object Search Language
;; (SOSL) code.

;;; Code:

(require 'soql-mode)

(defgroup sosl nil
  "Salesforce Object Search Language (SOSL)."
  :group 'languages
  :group 'salesforce
  :prefix "sosl-")

(defcustom sosl-mode-indent-basic 4
  "Basic amount of indentation."
  :type 'integer)

(defcustom sosl-mode-hook nil
  "Hook called by `sosl-mode'."
  :type 'hook)

(defvar sosl-mode--kwds-regexp
  (eval-when-compile
    (list
     (regexp-opt
      '("FIND" "IN" "ALL" "FIELDS" "NAME" "EMAIL" "PHONE" "SIDEBAR"
        "RETURNING"
        "ORDER" "BY" "ASC" "DESC" "NULLS" "FIRST" "first" "LAST" "last"
        "LIMIT"
        "OFFSET"
        "WHERE" "LIKE" "NOT" "INCLUDES" "includes" "EXCLUDES" "excludes"
        "AND" "OR"
        "null" "NULL" "toLabel" "TRUE" "true" "FALSE" "false"
        "YESTERDAY" "TODAY" "TOMORROW"
        "LAST_WEEK" "THIS_WEEK" "NEXT_WEEK"
        "LAST_MONTH" "THIS_MONTH" "NEXT_MONTH"
        "LAST_90_DAYS" "NEXT_90_DAYS"
        "LAST_N_DAYS" "NEXT_N_DAYS"
        "NEXT_N_WEEKS" "LAST_N_WEEKS"
        "NEXT_N_MONTHS" "LAST_N_MONTHS"
        "THIS_QUARTER" "LAST_QUARTER" "NEXT_QUARTER"
        "NEXT_N_QUARTERS" "LAST_N_QUARTERS"
        "THIS_YEAR" "LAST_YEAR" "NEXT_YEAR"
        "NEXT_N_YEARS" "LAST_N_YEARS"
        "THIS_FISCAL_QUARTER" "LAST_FISCAL_QUARTER" "NEXT_FISCAL_QUARTER"
        "NEXT_N_FISCAL_QUARTERS" "LAST_N_FISCAL_QUARTERS"
        "THIS_FISCAL_YEAR" "LAST_FISCAL_YEAR" "NEXT_FISCAL_YEAR"
        "NEXT_N_FISCAL_YEARS" "LAST_N_FISCAL_YEARS"
        "AT" "ABOVE" "BELOW" "ABOVE_OR_BELOW"
        "FORMAT"
        "WITH" "DATA" "CATEGORY" "HIGHLIGHT" "METADATA" "NETWORK" "SNIPPET"
        "SPELL_CORRECTION"
        "UPDATE" "TRACKING" "VIEWSTAT"
        "USING"
        "convertCurrency"
        "DISTANCE" "GEOLOCATION")
      'words))))

(require 'smie)

(defconst sosl-mode--grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((stmt ("FIND" search-query)
            (stmt "IN" exp)
            (stmt "RETURNING" exp)
            (stmt "LIMIT" num)
            (stmt "OFFSET" num)
            (stmt "ORDER" "BY" field)
            (stmt "WHERE" exp)
            (stmt "UPDATE" "TRACKING")
            (stmt "UPDATE" "VIEWSTAT")
            (stmt "USING" exp)
            (stmt "WITH" exp)
            (stmt "WITH" "DATA" "CATEGORY" exp)
            (stmt "WITH" "METADATA" exp)
            (stmt "WITH" "NETWORK" exp)
            (stmt "WITH" "SNIPPET" exp)
            (stmt "WITH" "SPELL_CORRECTION" exp))
      (search-query)
      (field)
      (fields (field "," field))
      (type)
      (types (type "," type))
      (exp)
      (num))
    '((assoc ",")))))

(defun sosl-mode--rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) sosl-mode-indent-basic)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:list-intro . ,(or `"WHERE")) t)))

(defvar sosl-mode-syntax-table
  (let ((table (make-syntax-table soql-mode-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "|"  table)
    (modify-syntax-entry ?{  "|"  table)
    (modify-syntax-entry ?}  "|"  table)
    table))

;;;###autoload
(define-derived-mode sosl-mode prog-mode "SOSL"
  "Major mode for editing Salesforce Object Search Language (SOSL) code."
  (setq font-lock-defaults '(sosl-mode--kwds-regexp))
  (smie-setup sosl-mode--grammar #'sosl-mode--rules)
  ;; Dummy comment settings
  (setq comment-start "#")
  (setq comment-start-skip "\\`.\\`"))

(provide 'sosl-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; sosl-mode.el ends here
