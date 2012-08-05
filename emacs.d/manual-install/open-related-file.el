;;; open-related-file.el --- Open file related to the currently-opened file
;;
;; Copyright (C) 2012 Shunsuke Kirino <shunsuke.kirino@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; open-related-file.el provides a function to open files whose paths
;; are registered as "related".
;;
;;; Install:
;;
;; Copy this file into load-path'ed directory (and byte compile it if you want).
;; Then put the following code into your ~/.emacs:
;;
;;     (require 'open-related-file.el)
;;     (global-set-key (kbd "C-c ; o") 'open-related-file-open) ;; Modify key sequence as you like
;;
;;; Configuration:
;;
;; To use the function you should register groups of file patterns.
;; Set "open-related-file-groups" variable, which is an empty list by default,
;; as a list of the file-groups.
;; You can use the convinient function "open-related-file-append-group" as in the
;; following examples, or directly modify the variable.
;; Note that the files are matched in the order of the list and only the first
;; matched group is used.
;;
;; Example 1: Simple 1-to-1 mapping
;;     (open-related-file-append-group "/home/someone/somewhere/abc.foo" "/home/someone/somewhere/abc.bar")
;;
;; Example 2: Use wildcard "%s"
;;     (open-related-file-append-group "%s.foo" "%s.bar")
;;
;; Example 3: Add a group containing 3 or 4 paths
;;     (open-related-file-append-group "%s.1" "%s.2" "%s.3")
;;     (open-related-file-append-group "%s.1" "%s.2" "%s.3" "%s.4")
;;
;; Example 4: For Rails developers (with Test::Unit)
;;     (open-related-file-append-group "%s/app/controllers/%s.rb" "%s/test/functional/%s_test.rb"  )
;;     (open-related-file-append-group "%s/app/helpers/%s.rb"     "%s/test/unit/helpers/%s_test.rb")
;;     (open-related-file-append-group "%s/app/models/%s.rb"      "%s/test/unit/%s_test.rb"        )
;;     ...
;;
;;; Resulting layout:
;;
;; As a result of a success of "open-related-file-open", window and buffer configuration
;; will become as follows depending on the number of files in the matched group.
;;
;;   [1] 2 files:
;;     |-----------+-----------|
;;     |           |           |
;;     |           |           |
;;     |           |           |
;;     |  file 1   |  file 2   |
;;     |           |           |
;;     |           |           |
;;     |           |           |
;;     |-----------+-----------|
;;
;;   [2] 3 files:
;;     |-----------+-----------|
;;     |           |           |
;;     |           |  file 2   |
;;     |           |           |
;;     |  file 1   |-----------|
;;     |           |           |
;;     |           |  file 3   |
;;     |           |           |
;;     |-----------+-----------|
;;
;;   [3] 4 files:
;;     |-----------+-----------|
;;     |           |           |
;;     |  file 1   |  file 3   |
;;     |           |           |
;;     |-----------+-----------|
;;     |           |           |
;;     |  file 2   |  file 4   |
;;     |           |           |
;;     |-----------+-----------|
;;

(require 'cl)

(defvar open-related-file-groups '())

(defun open-related-file-append-group (&rest new-group)
  "Register a group specified by the given parameters to the bottom of
open-related-file-groups."
  (let ((len (length new-group)))
    (when (< len 2) (error "Too few arguments (at least 2 required)"))
    (when (< 4 len) (error "Too many arguments (at most 4)")))
  (add-to-list 'open-related-file-groups new-group t))

(defun open-related-file-open ()
  "Try to match current-buffer's filepath with elements of 'open-related-file-groups.
Do nothing if none matches. If match found, open the files."
  (interactive)
  (let* ((original-filepath (buffer-file-name))
         (file-group (orf-find-matched-file-paths original-filepath)))
    (when (and file-group
               (loop for f in file-group always (file-exists-p f)))
      (let ((args (cons original-filepath file-group)))
        (when (equal 2 (length file-group))
          (apply 'orf-open-matched-files-2 args))
        (when (equal 3 (length file-group))
          (apply 'orf-open-matched-files-3 args))
        (when (equal 4 (length file-group))
          (apply 'orf-open-matched-files-4 args))
        ))))

;; private functions
(defun orf-find-matched-file-paths (filepath)
  (let* ((matched-group (orf-find-matching-group filepath))
         (matched-patterns (orf-matched-substrings filepath)))
    (loop for f in matched-group collect
          (eval (append '(format f) matched-patterns)))))

(defun orf-find-matching-group (filepath)
  (loop for group in open-related-file-groups do
        (when (orf-test-group-with-filepath group filepath)
          (return group))))

(defun orf-test-group-with-filepath (group filepath)
  (loop for file-pattern in group do
        (let ((file-regexp (orf-construct-path-regexp file-pattern)))
          (when (string-match file-regexp filepath) (return t)))))

(defun orf-construct-path-regexp (path)
  (let* ((path-with-escaped-dots    (replace-regexp-in-string "\\." "\\\\."        path))
         (path-with-regexp-grouping (replace-regexp-in-string "%s"  "\\\\(.*\\\\)" path-with-escaped-dots)))
    (concat "^" path-with-regexp-grouping "$")))

(defun orf-matched-substrings (orig-str)
  (loop for i from 1 while (match-string i orig-str) collect
        (match-string i orig-str)))

(defun orf-open-matched-files-2 (original-filepath file-l file-r)
  (delete-other-windows)
  (split-window-horizontally)
  (find-file file-l)
  (other-window 1)
  (find-file file-r)
  (when (equal original-filepath file-l) (other-window 1)))

(defun orf-open-matched-files-3 (original-filepath file-l file-r1 file-r2)
  (delete-other-windows)
  (split-window-horizontally)
  (find-file file-l)
  (other-window 1)
  (split-window-vertically)
  (find-file file-r1)
  (other-window 1)
  (find-file file-r2)
  (when (equal original-filepath file-l)  (other-window 1))
  (when (equal original-filepath file-r1) (other-window -1)))

(defun orf-open-matched-files-4 (original-filepath file-l1 file-l2 file-r1 file-r2)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-vertically)
  (find-file file-l1)
  (other-window 1)
  (find-file file-l2)
  (other-window 1)
  (split-window-vertically)
  (find-file file-r1)
  (other-window 1)
  (find-file file-r2)
  (when (equal original-filepath file-l1) (other-window 1))
  (when (equal original-filepath file-l2) (other-window 2))
  (when (equal original-filepath file-r1) (other-window -1)))

(provide 'open-related-file)
;;; open-related-file.el ends here
