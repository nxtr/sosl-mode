;;; sosl-mode.el --- Salesforce SOSL Major Mode   -*- lexical-binding: t -*-

;; Copyright (C) 2015 Magnus Nyberg

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

;;;###autoload
(define-derived-mode sosl-mode prog-mode "SOSL"
  "Major mode for editing Salesforce Object Search Language (SOSL) code.")

(provide 'sosl-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; sosl-mode.el ends here
