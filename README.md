# flymake-ruff

[![MELPA](https://melpa.org/packages/flymake-ruff-badge.svg)](https://melpa.org/#/flymake-ruff)

Flymake plugin to run a linter for python buffers using [ruff](https://github.com/charliermarsh/ruff)

> Make sure you have at least `ruff` `0.1.0` version because there is a [breaking change](https://github.com/astral-sh/ruff/blob/main/BREAKING_CHANGES.md#010) in the output format flag

> If you are using a version of `ruff` < `0.5.0`, set flymake-ruff-program-args to  `'("--output-format" "text" "--exit-zero" "--quiet" "-")`
## Installation

### Cloning the repo

Clone this repo somewhere, and add this to your config:

```elisp
(add-to-list 'load-path "path where the repo was cloned")

(require 'flymake-ruff)
(add-hook 'python-mode-hook #'flymake-ruff-load)
```

### Using use-package

```emacs-lisp
(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))
```

### Using straight.el

```emacs-lisp
(use-package flymake-ruff
  :straight (flymake-ruff
             :type git
             :host github
             :repo "erickgnavar/flymake-ruff"))
```

## Using flymake-ruff with eglot

To use flymake-ruff together with eglot, you should add `flymake-ruff-load` to
`eglot-managed-mode-hook` instead.   For example:

```emacs-lisp
(add-hook 'eglot-managed-mode-hook 'flymake-ruff-load)
```

Or, if you use use-package:

```emacs-lisp
(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))
```

## Excluding Pyright diagnostic notes

Pyright has some diagnostic notes that overlap with diagnostics provided by
ruff. These diagnostic notes can't be disabled via Pyright's config, but you can
exclude them by adding a filter to `eglot--report-to-flymake`. For example, to
remove Pyright's "variable not accessed" notes, add the following:

```emacs-lisp
(defun my-filter-eglot-diagnostics (diags)
    "Drop Pyright 'variable not accessed' notes from DIAGS."
    (list (seq-remove (lambda (d)
                        (and (eq (flymake-diagnostic-type d) 'eglot-note)
                             (s-starts-with? "Pyright:" (flymake-diagnostic-text d))
                             (s-ends-with? "is not accessed" (flymake-diagnostic-text d))))
                      (car diags))))

(advice-add 'eglot--report-to-flymake :filter-args #'my-filter-eglot-diagnostics))
```

