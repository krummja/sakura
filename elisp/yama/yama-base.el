;;; yama-base.el --- Base classes -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Jonathan Crum

;; Author: Jonathan Crum <crumja4@gmail.com>

;;; Commentary:

;; Base classes
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

(require 'eieio)
(require 'ox-publish)
(require 'ox-html)

(defclass yama-project ()
  ((name 
    :initarg :name
    :type string
    :documentation "Name of the project.")
   (root-url
    :initarg :root-url
    :type string
    :documentation "URL for the deployed site.")
   (base-url
    :initarg :base-url
    :type string
    :documentation "URL with respect to / at the host.")
   (input-dir
    :initarg :input-dir
    :type string
    :documentation "Root input directory for the project.")
   (output-dir
    :initarg :output-dir
    :type string
    :documentation "Output directory for the built project.")
   (postamble
    :initarg :postamble
    :type string
    :documentation "Postamble for the pages.")
   (preamble
    :initarg :preamble
    :type string
    :documentation "Preamble for the pages."))
  :abstract t
  :documentation "Base project class for yama.")

(defclass yama-project-wiki (yama-project) ()
  :documentation "A wiki project.")

(defclass yama-project-blog (yama-project) ()
  :documentation "A blog type project.")

(defclass yama-project-plain (yama-project) ()
  :documentation "Simple what-you-org-is-what-you-html pages.")

(defclass yama-project-static (yama-project) ()
  :documentation "Static type project.")

(cl-defmethod yama-project-static-config ((proj yama-project))
  "Get org-publish static config for the project."
  `(,(format "yama-%s-static" (oref proj :name))
    :base-directory ,(oref proj :input-dir)
    :base-extension ".*"
    :exclude ".*\.org$\\|.*export\.setup$\\|.*auto/.*\.el$\\|.*\.tex$\\|.*\.bib$"
    :recursive t
    :publishing-directory ,(oref proj :output-dir)
    :publishing-function org-publish-attachment))

(provide 'yama-base)

;;; yama-base.el ends here
