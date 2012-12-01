(require 'flymake)
(require 'typescript)

(defun flymake-typescript-make-error (obj)
  (mapcar (lambda (ent)
			(let* ((file (cdr (assoc 'file ent)))
				   (line (cdr (assoc 'line1 ent)))
				   (col (cdr (assoc 'col1 ent)))
				   (text (cdr (assoc 'text ent))))
			  (format "%s (%d,%d): %s" file line col text)))
		  obj))

(defun flymake-typescript-check-error ()
  "Show syntax errors."
  (interactive)
  (when ac-ts-debug-mode
	(message "flymake-typescript-check-error"))
  (let ((file-name (expand-file-name (buffer-file-name))))
	(setq flymake-last-change-time nil)
	(save-restriction
	  (setq flymake-check-start-time (flymake-float-time))
	  (widen)
	  (ac-ts-ensure-tss file-name)
	  (ac-ts-tss-update file-name)
	  (setq ac-ts-tss-result nil)
	  (process-send-string ac-ts-tss-proc "showErrors\r\n")
	  (ac-ts--wait-response)
	  (when (arrayp ac-ts-tss-result)
		(let ((lines (flymake-typescript-make-error ac-ts-tss-result)))
		  (setq flymake-new-err-info
				(flymake-parse-err-lines
				 flymake-new-err-info lines)))
		  ;; (message "lines %s" lines)
		  ;; (message "new err %s" flymake-new-err-info))
		(flymake-post-syntax-check 0 "tss")
        (setq flymake-is-running nil)))))

(when (load "flymake" t)  
  (defun flymake-typescript-init ()
    (run-at-time 0.1 nil 'flymake-typescript-check-error)
    nil)
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.ts\\'" flymake-typescript-init))

  (setq flymake-err-line-patterns
        (cons '("\\(^[^\\.]+.ts\\)[^(]*(\\([0-9]+\\),\\([0-9]+\\)): \\(.+\\)" 1 2 3 4)
              flymake-err-line-patterns)))
