# terraform-mode.el [![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

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


## Customize Variables

#### `terraform-indent-level`(Default: `2`)

Indentation size. You need to call `revert-buffer` if you change this value outer of hook such as `eval-expression`.

## Sample Configuration

```lisp
(custom-set-variables
 '(terraform-indent-level 4))
```

## See Also

- [hcl-mode](https://github.com/syohex/emacs-hcl-mode)

This major-mode inherits from hcl-mode. Most of syntax features, like highlighting, indentation are implemented in hcl-mode.

[travis-badge]: https://travis-ci.org/syohex/emacs-terraform-mode.svg
[travis-link]: https://travis-ci.org/syohex/emacs-terraform-mode
[melpa-link]: https://melpa.org/#/terraform-mode
[melpa-stable-link]: https://stable.melpa.org/#/terraform-mode
[melpa-badge]: https://melpa.org/packages/terraform-mode-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/terraform-mode-badge.svg
