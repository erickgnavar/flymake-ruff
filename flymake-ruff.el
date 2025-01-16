;;; flymake-ruff.el --- A flymake plugin for python files using ruff

;; Copyright © 2023 Erick Navarro
;; Author: Erick Navarro <erick@navarro.io>
;; URL: https://github.com/erickgnavar/flymake-ruff
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "26.1") (project "0.3.0"))

;;; Commentary:

;; Usage:
;;   (require 'flymake-ruff)
;;   (add-hook 'python-mode-hook #'flymake-ruff-load)
;;
;; Or, with use-package:
;;   (use-package flymake-ruff
;;     :ensure t
;;     :hook (python-mode . flymake-ruff-load))

;;; Code:

(require 'project)

(defcustom flymake-ruff-program "ruff"
  "Path to program ruff."
  :group 'flymake-ruff
  :type 'string)

(defcustom flymake-ruff-program-args
  '("check" "--output-format" "concise" "--exit-zero" "--quiet" "-")
  "Flags to be given to \"ruff\"."
  :group 'flymake-ruff
  :type '(list string))

(defvar flymake-ruff--output-regex "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\([A-Za-z0-9]+\\):? \\(.*\\)")

(defvar flymake-ruff--no-config-tramp-dirs
  nil
  "List of TRAMP directories that ruff checked for config in but found none. This prevents repeatedly looking for config files that do not exist.")

(defconst flymake-ruff--default-configs
  '(".ruff.toml" "ruff.toml" "pyproject.toml")
  "Default configuration files supported by Ruff.")

(defun flymake-ruff--get-config ()
  "Look for configuration files supported by Ruff in project root.
When project is on remote host, cache config using `default-directory' as key."
  (if (and (tramp-handle-file-remote-p default-directory))
      (when (not (seq-contains-p flymake-ruff--no-config-tramp-dirs default-directory #'string-equal))
	(let ((cache-dir (expand-file-name 
                          (concat "ruff-config-" (sha1 default-directory))
                          "/tmp")))
          (if (file-directory-p cache-dir)
              ;; Return cached config if exists
              (seq-find #'file-readable-p
			(mapcar (lambda (f)
				  (expand-file-name f cache-dir))
				flymake-ruff--default-configs))
            ;; Create cache and return config path
	    (if-let* ((project-current (project-current))
		      (config (seq-find
                               #'tramp-handle-file-readable-p
                               (mapcar (lambda (f)
                                         (tramp-handle-expand-file-name f (project-root project-current)))
				       flymake-ruff--default-configs)))
		      (temp-file (tramp-handle-expand-file-name (tramp-handle-file-name-nondirectory config) cache-dir))
		      (temp-copy (tramp-handle-file-local-copy config)))
		(progn
		  (make-directory cache-dir)
		  (copy-file temp-copy temp-file t)
		  (delete-file temp-copy)
		  temp-file)
	      (progn
		(push default-directory flymake-ruff--no-config-tramp-dirs)
		nil)))))
    
    ;; Local project path
    (when-let ((project-current (project-current)))
      (seq-find #'file-readable-p
		(mapcar (lambda (f)
			  (expand-file-name f (project-root project-current)))
			flymake-ruff--default-configs)))))

(defun flymake-ruff--check-buffer ()
  "Generate a list of diagnostics for the current buffer."
  (let ((code-buffer (current-buffer))
        (code-filename (buffer-file-name))
        (start-line (line-number-at-pos (point-min) t))
        (code-content (without-restriction
                        (buffer-substring-no-properties (point-min) (point-max))))
        (dxs '()))
    (with-temp-buffer
      (insert code-content)
      ;; check if the current buffer belongs to a project before
      ;; trying to build a path using `project-current' otherwise it
      ;; will fail silently
      (let* ((config (flymake-ruff--get-config))
	     (args (if config
                       ;; for version > 0.5 the work "check" is
                       ;; included so we need to extract it and put it
                       ;; before --config argument
                       (if (member "check" flymake-ruff-program-args)
                           (append `("check" "--config" ,config)
                                   (cdr flymake-ruff-program-args))
                         (append `("--config" ,config)
                                 flymake-ruff-program-args))
                     flymake-ruff-program-args))
             (args (if code-filename
                       (append args `("--stdin-filename" ,code-filename))
                     args)))
        ;; call-process-region will run the program and replace current buffer
        ;; with its stdout, that's why we need to run it in a temporary buffer
        (apply #'call-process-region (point-min) (point-max) flymake-ruff-program t t nil args))
      (goto-char (point-min))
      (while (search-forward-regexp flymake-ruff--output-regex (point-max) t)
        (when (match-string 2)
          (let* ((line (string-to-number (match-string 2)))
                 (col (string-to-number (match-string 3)))
                 (code (match-string 4))
                 (msg (match-string 5))
                 (description (format "Ruff: %s %s" code msg))
                 (region (flymake-diag-region code-buffer (1+ (- line start-line)) col))
                 (dx (flymake-make-diagnostic code-buffer (car region) (cdr region)
                                              :error description)))
            (add-to-list 'dxs dx)))))
    dxs))

;;;###autoload
(defun flymake-ruff-load ()
  "Load hook for the current buffer to tell flymake to run checker."
  (interactive)
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    ;; If cached TRAMP config file exists for current buffer, remove it so that cache can be refreshed on reverting buffer.
    (when-let* ((_remote (file-remote-p default-directory))
		(cache-dir (expand-file-name 
			    (concat "ruff-config-" (sha1 default-directory))
			    "/tmp"))
		(_cache_exist (file-directory-p cache-dir)))
      (delete-directory cache-dir t))
    
    (add-hook 'flymake-diagnostic-functions #'flymake-ruff--run-checker nil t)))

(defun flymake-ruff--run-checker (report-fn &rest _args)
  "Run checker using REPORT-FN."
  (funcall report-fn (flymake-ruff--check-buffer)))

(provide 'flymake-ruff)
;;; flymake-ruff.el ends here
