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
  '("--format" "text" "--exit-zero" "--quiet" "-")
  "Flags to be given to \"ruff\"."
  :group 'flymake-ruff
  :type '(list string))

(defvar flymake-ruff--output-regex "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\([A-Z0-9]+\\) \\(.*\\)")

(defconst flymake-ruff--default-configs
  '("pyproject.toml" "ruff.toml" ".ruff.toml")
  "Default configuration files supported by Ruff.")

(defun flymake-ruff--check-buffer ()
  "Generate a list of diagnostics for the current buffer."
  (let ((code-buffer (current-buffer))
        (code-content (buffer-substring-no-properties (point-min) (point-max)))
        (dxs '()))
    (with-temp-buffer
      (insert code-content)
      (let* ((config (seq-find
                      #'file-readable-p
                      (mapcar
                       (lambda (f)
                         (expand-file-name f (project-root (project-current))))
                       flymake-ruff--default-configs)))
             (args (if config
                       (append `("--config" ,config)
                               flymake-ruff-program-args)
                     flymake-ruff-program-args)))
        ;; call-process-region will run the program and replace current buffer
        ;; with its stdout, that's why we need to run it in a temporary buffer
        (apply #'call-process-region (point-min) (point-max) flymake-ruff-program t t nil args))
      (goto-char (point-min))
      (while (search-forward-regexp flymake-ruff--output-regex (point-max) t)
        (when (match-string 2)
          (let* ((line (buffer-substring (match-beginning 2) (match-end 2)))
                 (col (buffer-substring (match-beginning 3) (match-end 3)))
                 (code (buffer-substring (match-beginning 4) (match-end 4)))
                 (description (concat "Ruff: "
                                      (buffer-substring (match-beginning 5) (match-end 5))))
                 (region (flymake-diag-region code-buffer (string-to-number line) (string-to-number col)))
                 (dx (flymake-make-diagnostic code-buffer (car region) (cdr region) :error description)))
            (add-to-list 'dxs dx)))))
    dxs))

;;;###autoload
(defun flymake-ruff-load ()
  "Load hook for the current buffer to tell flymake to run checker."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'flymake-ruff--run-checker nil t))

(defun flymake-ruff--run-checker (report-fn &rest _args)
  "Run checker using REPORT-FN."
  (funcall report-fn (flymake-ruff--check-buffer)))

(provide 'flymake-ruff)
;;; flymake-ruff.el ends here
