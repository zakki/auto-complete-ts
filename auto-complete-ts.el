;;; auto-complete-ts.el --- Auto Completion source for typescript for GNU Emacs
;; Copyright (C) 2012 Kensuke Matsuzaki

;; Author: Kensuke Matsuzaki <knsk.mtzk@gmail.com>
;; Keywords: completion, convenience
;; Version: 0.1

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

;; Many code are from Brian Jiang's auto-complete-clang.el

(provide 'auto-complete-ts)
(require 'auto-complete)
(require 'json)

(defcustom ac-ts-node-executable
  (executable-find "node")
  "*Location of node.js executable"
  :group 'auto-complete
  :type 'file)

(defcustom ac-ts-lib-dir
  nil
  "*Location of typing files (*.d.ts) directory."
  :group 'auto-complete
  :type 'file)

(defcustom ac-ts-auto-save t
  "*Determines whether to save the buffer when retrieving completions."
  :group 'auto-complete
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ac-ts-lang-option-function nil
  "*function to return the lang type for option -x."
  :group 'auto-complete
  :type 'function)

(defconst ac-ts-error-buffer-name "*ts error*")

(defvar ac-ts-dir (file-name-directory load-file-name)
  "The root dir of the auto-complete-ts distribution.")

(defun ac-ts-parse-output (prefix)
  (message "ac-ts-parse-output")
  (goto-char (point-min))
  (let* ((json-object (json-read))
		 (member (cdr (assoc 'member json-object)))
		 (member-entries (cdr (assoc 'entries member)))
		 (nomember (cdr (assoc 'nomember json-object)))
		 (nomember-entries (cdr (assoc 'entries nomember))))
	(mapcar (lambda (ent) (cdr (assoc 'name ent)))
			(if member-entries member-entries nomember-entries))))

(defun ac-ts-handle-error (res args)
  (goto-char (point-min))
  (let* ((buf (get-buffer-create ac-ts-error-buffer-name))
         (cmd (concat ac-ts-node-executable " " (mapconcat 'identity args " ")))
         (pattern (format ac-ts-completion-pattern ""))
         (err (if (re-search-forward pattern nil t)
                  (buffer-substring-no-properties (point-min)
                                                  (1- (match-beginning 0)))
                ;; Warn the user more agressively if no match was found.
                (message "ts failed with error %d:\n%s" res cmd)
                (buffer-string))))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (current-time-string)
                (format "\nts failed with error %d:\n" res)
                cmd "\n\n")
        (insert err)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun ac-ts-call-process (prefix &rest args)
  (message "ac-ts-call-process")
  (let ((buf (get-buffer-create "*ts-output*"))
        res)
    (with-current-buffer buf (erase-buffer))
    (setq res (if ac-ts-auto-save
                  (apply 'call-process ac-ts-node-executable nil buf nil args)
                (apply 'call-process-region (point-min) (point-max)
                       ac-ts-node-executable nil buf nil args)))
    (with-current-buffer buf
      (unless (eq 0 res)
        (ac-ts-handle-error res args))
      ;; Still try to get any useful input.
      (ac-ts-parse-output prefix))))


(defsubst ac-ts-build-location (pos)
  (save-excursion
    (goto-char pos)
    (list "--line" (format "%d" (line-number-at-pos))
		  "--col" (format "%d" (1+ (- (point) (line-beginning-position))))
		  "--libdir" (expand-file-name ac-ts-lib-dir)
		  )))

(defsubst ac-ts-build-complete-args (pos)
  (append (list (expand-file-name (concat ac-ts-dir "/isense.js")))
		  (ac-ts-build-location pos)
          (list (if ac-ts-auto-save buffer-file-name "-")
				)))

(defface ac-ts-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for ts candidate"
  :group 'auto-complete)

(defface ac-ts-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the ts selected candidate."
  :group 'auto-complete)

(defsubst ac-in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))

(defun ac-ts-candidate ()
  (message "ac-ts-candidate")
  (unless (ac-in-string/comment)
    (and ac-ts-auto-save
         (buffer-modified-p)
         (basic-save-buffer))
    (save-restriction
      (widen)
      (apply 'ac-ts-call-process
             ac-prefix
             (ac-ts-build-complete-args (- (point) (length ac-prefix)))))))

(defun ac-ts-prefix ()
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (or (eq ?\. c))
          (point)))))

(ac-define-source ts
  '((candidates . ac-ts-candidate)
    (candidate-face . ac-ts-candidate-face)
    (selection-face . ac-ts-selection-face)
	(prefix . ac-ts-prefix)
    (requires . 0)
    ;; (document . ac-ts-document)
    ;; (action . ac-ts-action)
    (cache)
    (symbol . "t")))
