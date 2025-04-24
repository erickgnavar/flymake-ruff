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

(defconst flymake-ruff-default-severity :warning
  "Default Flymake severity for unmatched Ruff diagnostic codes.")

(defvar flymake-ruff--output-regex "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\([A-Za-z0-9]+\\):? \\(.*\\)")

(defconst flymake-ruff--default-configs
  '(".ruff.toml" "ruff.toml" "pyproject.toml")
  "Default configuration files supported by Ruff.")

(defconst flymake-ruff--severity-map
  '(("E"    . :error)     ; Critical style errors
    ("W"    . :warning)   ; Style warnings
    ("F"    . :error)     ; Logical errors (pyflakes)
    ("B"    . :warning)   ; Bugbears (best practices)
    ("C90"  . :warning)   ; Complexity (mccabe)
    ("N"    . :note)      ; Naming conventions
    ("I"    . :note)      ; Import sorting
    ("UP"   . :note)      ; Python upgrades (pyupgrade)
    ("SIM"  . :note)      ; Simplification
    ("PERF" . :warning))  ; Performance issues
  "Mapping of Ruff diagnostic code prefixes to Flymake severities.")

(defun flymake-ruff--severity-for-code (code)
  "Return Flymake severity for Ruff CODE."
  (let ((matched-prefix (seq-find (lambda (pfx)
                                    (string-prefix-p pfx code))
                                  (mapcar #'car flymake-ruff--severity-map))))
    (or (cdr (assoc matched-prefix flymake-ruff--severity-map))
        flymake-ruff-default-severity)))

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
      (let* ((config (and (project-current)
                          (seq-find
                           #'file-readable-p
                           (mapcar
                            (lambda (f)
                              (expand-file-name f (project-root (project-current))))
                            flymake-ruff--default-configs))))
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
                     args))
             (default-directory (if (project-current)
                                    (project-root (project-current))
                                  default-directory)))
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
                 (severity (flymake-ruff--severity-for-code code))
                 (dx (flymake-make-diagnostic code-buffer (car region) (cdr region)
                                              severity description)))
            (add-to-list 'dxs dx)))))
    dxs))

;;;###autoload
(defun flymake-ruff-load ()
  "Load hook for the current buffer to tell flymake to run checker."
  (interactive)
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (add-hook 'flymake-diagnostic-functions #'flymake-ruff--run-checker nil t)))

(defun flymake-ruff--run-checker (report-fn &rest _args)
  "Run checker using REPORT-FN."
  (funcall report-fn (flymake-ruff--check-buffer)))

(provide 'flymake-ruff)
;;; flymake-ruff.el ends here
