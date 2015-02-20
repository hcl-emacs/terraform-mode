# terraform-mode.el [![Build Status](https://travis-ci.org/syohex/emacs-terraform-mode.svg)](https://travis-ci.org/syohex/emacs-terraform-mode)

Major mode of [Terraform](http://www.terraform.io/) configuration file


## Screenshot

![terraform-mode](image/terraform-mode.png)

#### imenu(`helm-imenu`)

![terraform-mode](image/terraform-mode-imenu.png)


## Installation

You can install `terraform-mode.el` from [MELPA](http://melpa.milkbox.net/) by `package.el`.


## Features

- Syntax highlighting
- Indentation
- imenu


## Customize Variables

#### `terraform-indent-level`

Indentation size

## Sample Configuration

```lisp
(custom-set-variables
 '(terraform-indent-level 4))
```
