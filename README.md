# terraform-mode.el ![Tests](https://github.com/emacsorphanage/terraform-mode/workflows/Tests/badge.svg) [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

Major mode of [Terraform](http://www.terraform.io/) configuration file

## Screenshot

![terraform-mode](image/terraform-mode.png)

#### imenu(`helm-imenu`)

![terraform-mode](image/terraform-mode-imenu.png)


## Installation

You can install `terraform-mode.el` from [MELPA](https://melpa.org/) by `package.el`.


## Features

- Syntax highlighting
- Indentation
- imenu
- Formatting using `terraform fmt`
- Block folding
- easier access to Terraform resource documentation

### Block folding

`terraform-mode` sets up `outline-mode` variables for block folding.
To use `outline-mode` for block folding, enable `outline-minor-mode`
in `terraform-mode-hook`:

``` emacs-lisp
(add-hook 'terraform-mode-hook #'outline-minor-mode)
```

You can use `outline-toggle-children` bound to `C-c C-f` to toggle
visibility of a block at point.

We also provide function `terraform-toggle-or-indent` which
folds-or-indents.  It is not bound by default, but you can bind it to
`TAB` or any other key.

### Access to Terraform resource documentation

Within a `resource` or a `data` block, type `C-c C-d C-w` to open a new
browser tab with the resource or data documentation page.

## Customize Variables

#### `terraform-indent-level`(Default: `2`)

Indentation size. You need to call `revert-buffer` if you change this value outer of hook such as `eval-expression`.

#### `terraform-format-on-save`(Default `nil`)

Set to `t` to automatically format the buffer on save.

## Sample Configuration

```emacs-lisp
(custom-set-variables
 '(terraform-indent-level 4))
```

With `use-package`

``` emacs-lisp
(use-package terraform-mode
  ;; if using straight
  ;; :straight t

  ;; if using package.el
  ;; :ensure t
  :custom (terraform-indent-level 4)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )

  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))
```

## See Also

- [hcl-mode](https://github.com/syohex/emacs-hcl-mode)

This major-mode inherits from hcl-mode. Most of syntax features, like highlighting, indentation are implemented in hcl-mode.

[melpa-link]: https://melpa.org/#/terraform-mode
[melpa-stable-link]: https://stable.melpa.org/#/terraform-mode
[melpa-badge]: https://melpa.org/packages/terraform-mode-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/terraform-mode-badge.svg
