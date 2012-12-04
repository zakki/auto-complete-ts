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
(defvar ac-ts-tss-proc nil)
(defvar ac-ts-tss-result nil)

(defun ac-ts-ensure-tss (file-name)
  (unless (and ac-ts-tss-proc
			   (member (process-status ac-ts-tss-proc) '(run stop)))
	(let ((tss (expand-file-name (concat ac-ts-dir "/../typescript-tools/bin/tss.js")))
		  (f (expand-file-name file-name)))
	  (when ac-ts-debug-mode
		(message "tss:%s\nfile:%s" tss f))
	  (setq ac-ts-tss-proc (start-process "node tss.js"
									  "*ac-ts-tss*"
									  ac-ts-node-executable
									  tss f))
	  (set-process-filter ac-ts-tss-proc 'ac-ts-tss-proc-filter)
	  (add-hook 'kill-buffer-hook 'ac-ts--delete-process nil t))))

(defun ac-ts--delete-process ()
  (and ac-ts-tss-proc
       (delete-process ac-ts-tss-proc)))

(defun ac-ts--wait-response ()
  (let ((n 0)
		(seconds 5))
    (while (and (not ac-ts-tss-result) (<= n (* 10 seconds)))
	  (setq n (+ 1 n))
	  (sleep-for 0.1))
	(unless ac-ts-tss-result
	  (message "tss timeout"))))

(defun ac-ts-current-pos ()
  (format "%d %d"
		  (line-number-at-pos)
		  (- (point) (line-beginning-position))))

(defsubst ac-ts-build-location (file-name pos)
  (save-excursion
    (goto-char pos)
	(format "%d %d %s"
			(line-number-at-pos)
			(- (point) (line-beginning-position))
			(expand-file-name file-name))))

(defsubst ac-ts-tss-completions (file-name pos member)
  (let ((cmd (format "completions %s %s\r\n"
					 (if member "true" "false")
					 (ac-ts-build-location file-name pos))))
	(process-send-string ac-ts-tss-proc cmd)
	(ac-ts--wait-response)))

(defsubst ac-ts-tss-update (file-name)
  (setq ac-ts-tss-result nil)
  (if ac-ts-auto-save
	  (progn (buffer-modified-p)
			 (basic-save-buffer)
			 (process-send-string ac-ts-tss-proc "reload\r\n"))
	(progn
	  (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
			 (lines (line-number-at-pos (point-max)))
			 (cmd (format "update %d %s\r\n"
						  lines file-name)))
		(process-send-string ac-ts-tss-proc cmd)
		(save-excursion
		  (goto-char (point-min))
		  (while (not (eobp))
			; (message ">> %s" (buffer-substring-no-properties (point) (point-at-eol)))
			(process-send-string ac-ts-tss-proc
			 (concat (buffer-substring-no-properties (point) (point-at-eol)) "\r\n"))
			(beginning-of-line 2))))))
  (ac-ts--wait-response))

(defun ac-ts-tss-proc-filter (proc string)
  ;; (when ac-ts-debug-mode
  ;; 	(message "ac-ts-tss-proc-filter %s %d"
  ;; 			 (string  )
  ;; 			 (length string)))
  (let ((line))
	(with-current-buffer (process-buffer proc)
	  (let ((moving (= (point) (process-mark proc))))
		(save-excursion
		  ;; Insert the text, advancing the process marker.
		  (goto-char (process-mark proc))
		  (insert string)
		  (set-marker (process-mark proc) (point))
		  (when (= (point) (line-beginning-position))
			(beginning-of-line 0)
			(setq line (buffer-substring-no-properties (point) (point-at-eol)))))
		(if moving (goto-char (process-mark proc)))))
	(when line
	  (condition-case err
		  (cond ((string-match "^\\(loaded\\|reloaded\\|updated\\|TSS\\) .*" line)
				 (setq ac-ts-tss-result 1))
				(t
				 (setq ac-ts-tss-result (json-read-from-string line))))
		(json-error
		 (progn
		   (message "ac-ts error %s" (error-message-string err))
		   (setq ac-ts-tss-result 1)))))))

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

(defun ac-ts--get-completions (info)
  (let ((member-entries (cdr (assoc 'entries info))))
	(mapcar (lambda (ent)
			  (let ((name (cdr (assoc 'name ent)))
					(kind (cdr (assoc 'kind ent)))
					(type (cdr (assoc 'type ent))))
				(propertize name 'ac-ts-help
							(concat kind ":" type))))
			member-entries)))

(defun typescript-tss-prepare ()
  (let ((file-name (expand-file-name (buffer-file-name))))
	(ac-ts-ensure-tss file-name)
	(ac-ts-tss-update file-name)))


(defun typescript-tss-default-command (cmd)
  (typescript-tss-prepare)
  (setq ac-ts-tss-result nil)
  (let* ((file-name (expand-file-name (buffer-file-name)))
		 (pos (ac-ts-build-location file-name (+ (point) 1)))
		 (str (format "%s %s\r\n" cmd pos)))
	(process-send-string ac-ts-tss-proc str)
	(ac-ts--wait-response)
	(message "%s" ac-ts-tss-result)))

(defun typescript-tss-type ()
  (interactive)
  (typescript-tss-default-command "type"))

(defun typescript-tss-symbol ()
  (interactive)
  (typescript-tss-default-command "symbol"))

(defun typescript-tss-definition ()
  (interactive)
  (typescript-tss-default-command "definition"))

(defun ac-ts-candidate ()
  (when ac-ts-debug-mode
	(message "ac-ts-candidate '%s'" ac-prefix))
  (unless (ac-in-string/comment)
	(let ((file-name (expand-file-name (buffer-file-name))))
	  (prog1
		  (save-restriction
			(widen)
			(typescript-tss-prepare)
			(setq ac-ts-tss-result nil)
			(ac-ts-tss-completions file-name
								   (- (point) (length ac-prefix))
								   (eq  ?\. (char-before ac-point)))
			(if (listp ac-ts-tss-result)
				(ac-ts--get-completions ac-ts-tss-result)
			  nil))))))

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
