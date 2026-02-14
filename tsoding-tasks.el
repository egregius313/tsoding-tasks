;;; tsoding-tasks.el --- Based on tsoding's task.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Edward Minnix III
;;
;; Author: Edward Minnix III <egregius313@gmail.com>
;; Maintainer: Edward Minnix III <egregius313@gmail.com>
;; Created: February 13, 2026
;; Modified: February 14, 2026
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/egregius313/tsoding-tasks
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Based on the tsoding video where he attempted to write a task manageer in Emacs
;;
;;  See: https://www.youtube.com/watch?v=QH6KOEVnSZA
;;; Code:
(require 'cl-lib)

(defconst tsoding-tasks--line-pattern
  (rx (group (* any)) "TODO: " (group (* any))))

(defconst tsoding-tasks--huid-pattern
  (rx (= 8 digit) ?- (= 6 digit)))

(defconst tsoding-tasks--task-pattern
  (rx "TASK(" (group (regexp tsoding-tasks--huid-pattern)) ")"))

(defun tsoding-tasks--bounds-of-huid-at-point ()
  (when (thing-at-point-looking-at tsoding-tasks--huid-pattern 15)
    (cons (match-beginning 0) (match-end 0))))

(defun tsoding-tasks--bounds-of-task-at-point ()
  (when (thing-at-point-looking-at tsoding-tasks--task-pattern 21)
    (cons (match-beginning 1) (match-end 1))))

(add-to-list
 'bounds-of-thing-at-point-provider-alist
 (cons
  'tsoding/huid
  #'tsoding-tasks--bounds-of-huid-at-point))

(add-to-list
 'bounds-of-thing-at-point-provider-alist
 (cons
  'tsoding/task
  #'tsoding-tasks--bounds-of-task-at-point))

(defun tsoding-tasks--task-at-point ()
  (or
   (when (thing-at-point-looking-at tsoding-tasks--huid-pattern 15)
     (match-string 0))
   (when (thing-at-point-looking-at tsoding-tasks--task-pattern 15)
     (match-string 1))))

(defun tsoding-tasks-new-huid (&optional check)
  (if (not check)
      (time-stamp-string "%Y%m%d-%H%M%S")
    (cl-loop
     with second = (seconds-to-time 1)
     with db-dir = (tsoding-tasks-find-database)
     for time = (current-time) then (time-add time second)
     for time-string = (time-stamp-string "%Y%m%d-%H%M%S" time)
     while (file-directory-p (file-name-concat db-dir time-string))
     finally return time-string)))

(defun tsoding-tasks--todo-item-at-point ()
  (save-match-data
    (let* ((line (thing-at-point 'line))
           (_ (string-match tsoding-tasks--line-pattern line))
           (prefix (match-string 1 line))
           (suffix (match-string 2 line)))
      (list prefix suffix))))

(defun tsoding-tasks-find-database ()
  (cl-loop
   for dir = default-directory then (file-name-parent-directory dir)
   while dir
   for tasks-dir = (file-name-concat dir "tasks")
   when (file-directory-p tasks-dir)
   return tasks-dir))

(defun tsoding-tasks-find-huid (huid &optional ensure-exists other-window)
  (interactive (list (or
                      (thing-at-point 'tsoding/huid)
                      (read-string "HUID: "))))
  (when-let* ((db-dir (tsoding-tasks-find-database))
              (task-dir (file-name-concat db-dir huid))
              (task-md (file-name-concat task-dir "TASK.md")))
    (when (and ensure-exists
               (not (file-directory-p task-dir)))
      (make-directory task-dir))
    (if other-window
        (find-file-other-window task-md)
      (xref-push-marker-stack)
      (find-file task-md))))

(defun tsoding-tasks-todo-to-task ()
  (interactive)
  (when-let ((db-dir (tsoding-tasks-find-database))
             (todo-item (tsoding-tasks--todo-item-at-point))
             (huid (tsoding-tasks-new-huid t)))
    (cl-multiple-value-bind (prefix suffix) todo-item
      (replace-region-contents
       (line-beginning-position)
       (line-end-position)
       (lambda (&rest _args) (concat prefix "TASK(" huid "): " suffix)))
      (tsoding-tasks-find-huid huid t t)
      (insert (format "# %s\n" suffix))
      (insert "\n")
      (insert "- STATUS: OPEN\n")
      (insert "- PRIORITY: 50\n")
      (save-buffer))))

(defun tsoding-tasks-xref-location-from-huid (huid)
  (xref-make
   (concat "TASK(" huid ")")
   (xref-make-file-location
    (file-name-concat (tsoding-tasks-find-database) huid "TASK.md")
    0
    0)))

(defun tsoding-tasks-xref-backend ()
  (when (thing-at-point 'tsoding/huid)
    'tsoding/task))

(add-hook 'xref-backend-functions 'tsoding-tasks-xref-backend)

(cl-defmethod xref-backend-apropos ((_backend (eql tsoding/task)) s)
  (list (tsoding-tasks-xref-location-from-huid s)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql tsoding/task)))
  (thing-at-point 'tsoding/huid))

(cl-defmethod xref-backend-definitions ((_backend (eql tsoding/task)) id)
  (list (tsoding-tasks-xref-location-from-huid id)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql tsoding/task)) &rest args)
  (when-let ((huid (thing-at-point 'tsoding/huid)))
    (list (cons
           huid
           (xref-make
            (concat "TASK(" huid ")")
            (xref-make-file-location
             (file-name-concat (tsoding-tasks-find-database) huid "TASK.md")
             0
             0))))))

(provide 'tsoding-tasks)
;;; tsoding-tasks.el ends here
