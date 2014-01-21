;; Copyright (C) 2008 Julian Stecklina, Shawn Betts, Ivy Foster
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Use `add-contrib-dir' to add the location stumpwm searches for modules.

;; Code:

(in-package #:stumpwm)

(export '(load-module
          list-modules
	  *contrib-dirs*
	  add-contrib-dir
          find-module))

(defun module-string-as-directory (dir)
  (unless (string= "/" (subseq dir (1- (length dir))))
    (setf dir (concat dir "/")))
  (pathname dir))

(defvar *contrib-dirs*
  (list
  #.(asdf:system-relative-pathname (asdf:find-system :stumpwm)
                                   (make-pathname :directory
                                                  '(:relative "contrib"))))
  "The location of the contrib modules on your system.")

(defcommand add-contrib-dir (dir) ((:string "Directory: "))
    "Sets the location of the contrib modules"
    (let ((module-string (module-string-as-directory dir)))
      (push module-string *contrib-dirs*)))

(define-stumpwm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (list-modules) :require-match t)))

(defun list-modules ()
  "Return a list of the available modules."
  (let ((mod-list) '())
    (dolist (i *contrib-dirs*)
      (dolist (element
               (mapcar 'pathname-name
                       (directory (make-pathname :defaults i
                                                 :name :wild
                                                 :type "lisp"))))
      (push element mod-list)))
    (remove-duplicates mod-list :test #'equal)))

(defun find-module (name)
  "Find module from list avaliable modules FIXME: test"
  (labels
   ((module-p (dir-list name)
              (cond
               ((null dir-list) nil)
               ((probe-file (make-pathname :defaults (car dir-list) :name name :type "lisp"))
                (make-pathname :defaults (pathname (princ-to-string (car dir-list)))
			       :name name
			       :type "lisp"))
               (t (module-p (cdr dir-list) name)))))
   (module-p (reverse *contrib-dirs*) name)))

(defcommand load-module (name) ((:module "Load Module: "))
  "Loads the contributed module with the given NAME."
  ;; FIXME: This should use ASDF in the future. And maybe there should
  ;; be an extra stumpwm-contrib repository.
  (when name
    (let ((module (find-module name)))
      (when module
          (load module)))))

;; End of file
