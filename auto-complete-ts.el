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

(defvar ac-ts-debug-mode nil)

;; connect to typescript-tools
(defvar ac-ts-proc nil)
(defvar ac-ts-tss-result nil)

(defun ac-ts-ensure-tss (file-name)
  (unless (and ac-ts-tss-proc
			   (member (process-status ac-ts-tss-proc) '(run stop)))
	(let ((tss (expand-file-name (concat ac-ts-dir "/../typescript-tools/tss.js")))
		  (f (expand-file-name file-name)))
	  (when ac-ts-debug-mode
		(message "tss:%s\nfile:%s" tss f))
	  (setq ac-ts-tss-proc (start-process "node tss.js"
									  "*ac-ts-tss*"
									  ac-ts-node-executable
									  tss f))
	  (set-process-filter ac-ts-tss-proc 'ac-ts-tss-proc-filter))))

(defsubst ac-ts-build-location (file-name pos)
  (save-excursion
    (goto-char pos)
	(format "%d %d %s"
			(line-number-at-pos)
			(1+ (- (point) (line-beginning-position)))
			(expand-file-name file-name))))

(defsubst ac-ts-tss-query (file-name pos)
  (concat "info " (ac-ts-build-location file-name pos) "\r\n"))

(defun ac-ts-tss-proc-filter (proc string)
  (when ac-ts-debug-mode
	(message "ac-ts-tss-proc-filter %s" string))
  (condition-case err
	  (let* ((info (json-read-from-string string))
			 (member (cdr (assoc 'completions info)))
			 (member-entries (cdr (assoc 'entries member)))
			 (completions (mapcar (lambda (ent)
									(let ((name (cdr (assoc 'name ent)))
										  (kind (cdr (assoc 'kind ent)))
										  (type (cdr (assoc 'type ent))))
									  (propertize name 'ac-ts-help
												  (concat kind ":" type))))
								  member-entries)))
		(setq ac-ts-tss-result completions))
	(json-error
	 (progn
	   (message "ac-ts error %s" (error-message-string err))
	   (setq ac-ts-tss-result 1)))))

(defun ac-ts-document (item)
  (if (stringp item)
      (let (s)
        (setq s (get-text-property 0 'ac-ts-help item))
		s))
  ;; (popup-item-property item 'ac-clang-help)
  )

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
  (when ac-ts-debug-mode
	(message "ac-ts-candidate"))
  (unless (ac-in-string/comment)
	(let (file-name)
	  (if ac-ts-auto-save
		  (progn (buffer-modified-p)
				 (basic-save-buffer)
				 (setq file-name buffer-file-name))
		(let ((tmp-file (make-temp-file "auto-complete-ts")))
		  (write-region (point-min) (point-max)
						tmp-file
						t 1)
		  (setq file-name tmp-file)))
	  (prog1
		  (save-restriction
			(widen)
			(ac-ts-ensure-tss file-name)
			(setq ac-ts-tss-result nil)
			(process-send-string ac-ts-tss-proc (ac-ts-tss-query file-name (- (point) (length ac-prefix))))
			(while (not ac-ts-tss-result)
			  (sleep-for 0.1))
			(if (listp ac-ts-tss-result)
				ac-ts-tss-result
			  nil))
		(unless ac-ts-auto-save (delete-file file-name))))))

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
	(document . ac-ts-document)
    ;; (action . ac-ts-action)
    (cache)
    (symbol . "t")))
