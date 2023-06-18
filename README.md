# flymake-ruff

[![MELPA](https://melpa.org/packages/flymake-ruff-badge.svg)](https://melpa.org/#/flymake-ruff)

Flymake plugin to run a linter for python buffers using [ruff](https://github.com/charliermarsh/ruff)

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
