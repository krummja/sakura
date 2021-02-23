;;; yama.el --- Yama blog content management

;; Copyright (c) 2021 Jonathan Crum

;; Author: Jonathan Crum <crumja4@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (f "0.20.0") (s "1.12.0") (mustache "0.24") (ht "2.2"))

;; URL: https://github.com/krummja/yama

;;; Commentary:

;; Org yama management
;; This file is not a part of GNU Emacs.

;;; License: 

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'f)
(require 'helm)
(require 'ht)
(require 'mustache)
(require 'org)
(require 'ox-html)
(require 'ox-publish)
(require 's)

(defgroup yama nil
  "Yama wiki"
  :group 'org)

(defcustom yama-projects nil
  "List of managed yama projects"
  :group 'yama)

(defcustom yama-pre-publish-hook nil
  "Hook for pre-publish. Functions take no arguments and run in the 
to-be-published buffer."
  :type 'hook
  :group 'yama)

(defcustom pile-post-publish-hook nil
  "Hook for post-publish. Functions take the following arguments:
1. Input file path
2. Output file path
These functions are directly appended to the org-publish-after-publishing-hook."
  :type 'hook
  :group 'yama)

(cl-defmethod yama-project-publish ((pj yama-project) &optional arg)
  "Publish the project."
  (save-excursion
    (with-yama-hooks (org-publish-project (format "yama-%s" (oref pj :name)) arg))))

(defun yama-publish-current-file (arg)
  "Publish only the current file."
  (interactive "P")  ;; arg code letter 'prefix arg converted to number, does not I/O'
  (save-excursion
    (with-yama-hooks (org-publish-current-file arg))))
