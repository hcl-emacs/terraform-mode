;;; terraform-mode.el --- Major mode for terraform configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-terraform-mode
;; Version: 0.06
;; Package-Requires: ((emacs "24.3") (hcl-mode "0.03"))

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

;;; Commentary:

;; Major mode of terraform configuration file. terraform-mode provides
;; syntax highlighting, indentation function and formatting.

;; Format the current buffer with terraform-format-buffer. To always
;; format terraform buffers when saving, use:
;;   (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'hcl-mode)

(defgroup terraform nil
  "Major mode of Terraform configuration file."
  :group 'languages)

(defcustom terraform-indent-level 2
  "The tab width to use when indenting."
  :type 'integer)

(defun terraform--list-to-regexp-or-list (list)
  "Turn input list into a regex or statement.
Argument LIST input list"
  (concat "\\(" (string-join list "\\|") "\\)"))

(defface terraform--resource-name-face
  '((t :foreground "pink"))
  "Face for resource names."
  :group 'terraform-mode)

(defvar terraform--resource-name-face 'terraform--resource-name-face)

(defface terraform--resource-type-face
  '((t :foreground "medium sea green"))
  "Face for resource names."
  :group 'terraform-mode)

(defvar terraform--resource-type-face 'terraform--resource-type-face)

(setq terraform--block-builtins-without-name-or-type
      '("terraform" "locals" "required_providers"))

(setq terraform--block-builtins-with-name-only
      '("variable" "module" "output"))

(setq terraform--block-builtins-with-type-only
      '("backend" "provider"))

(setq terraform--block-builtins-with-type-and-name
      '("resource" "data"))

(setq terraform--inline-reserved-words-with-name
      '("var" "module" "local"))

(setq terraform--inline-reserved-words-with-type-and-name
      '("data"))

(setq terraform--builtins
      (append terraform--block-builtins-without-name-or-type
	      terraform--block-builtins-with-type-only
	      terraform--block-builtins-with-name-only
	      terraform--block-builtins-with-type-and-name))

(setq terraform--block-builtins-regexp
      (concat "^\\s-*"
	      (terraform--list-to-regexp-or-list terraform--builtins)
	      "\\s-+"))

(setq terraform--block-with-type-only-regexp
      (concat  "^\\s-*"
	       (terraform--list-to-regexp-or-list terraform--block-builtins-with-type-only)
	       "\\(.+\\)\\s-+{"))

(setq terraform--block-with-name-only-regexp
      (concat  "^\\s-*"
	       (terraform--list-to-regexp-or-list terraform--block-builtins-with-name-only)
	       "\\s-+\\([^{]+\\)\\s-*{"))

(setq terraform--block-with-type-and-name-regexp--type-highlight
      (concat "^\\s-*"
	      (terraform--list-to-regexp-or-list terraform--block-builtins-with-type-and-name)
	      "\\s-+\\(.*?\\)\\s-+"))

(setq terraform--block-with-type-and-name-regexp--name-highlight
      (concat terraform--block-with-type-and-name-regexp--type-highlight
	      "\\(.*?\\)\\s-*{"))
       
(setq terraform--inline-reserved-words-with-name-regexp--reserved-word-highlight
      (concat (terraform--list-to-regexp-or-list terraform--inline-reserved-words-with-name)
	      "\\."))

(setq terraform--inline-reserved-words-with-name-regexp--name-highlight
      (concat terraform--inline-reserved-words-with-name-regexp--reserved-word-highlight
	      "\\(.*?\\)\\(\\.\\|$\\)"))

(setq terraform--inline-reserved-words-with-type-and-name-regexp--reserved-word-highlight
      (concat (terraform--list-to-regexp-or-list terraform--inline-reserved-words-with-type-and-name)
	      "\\."))

(setq terraform--inline-reserved-words-with-type-and-name-regexp--type-highlight
      (concat terraform--inline-reserved-words-with-type-and-name-regexp--reserved-word-highlight
	      "\\(.*?\\)\\."))

(setq terraform--inline-reserved-words-with-type-and-name-regexp--name-highlight
      (concat terraform--inline-reserved-words-with-type-and-name-regexp--type-highlight
	      "\\(.*?\\)\\(\\.\\|\\[\\)"))

(setq terraform--inline-each-regexp
      "\\(each\\)\\.\\(key\\|value\\)")

(setq terraform--inline-resource-regexp--resource-highlight
      "=\\s-+\\(.*?\\)\\.")

(setq terraform--inline-resource-regexp--name-highlight
      (concat terraform--inline-resource-regexp--resource-highlight
	      "\\(.*?\\)\\(\\.\\|$\\)"))
	      
(setq terraform-font-lock-keywords
      `((,terraform--block-builtins-regexp 1 font-lock-builtin-face)
	(,terraform--block-with-type-only-regexp 2 terraform--resource-type-face t)
	(,terraform--block-with-name-only-regexp 2 terraform--resource-name-face t)
	(,terraform--block-with-type-and-name-regexp--type-highlight 2 terraform--resource-type-face t)
	(,terraform--block-with-type-and-name-regexp--name-highlight 3 terraform--resource-name-face t)
	(,terraform--inline-reserved-words-with-name-regexp--reserved-word-highlight 1 font-lock-builtin-face)
	(,terraform--inline-reserved-words-with-name-regexp--name-highlight 2 terraform--resource-name-face t)
	(,terraform--inline-reserved-words-with-type-and-name-regexp--reserved-word-highlight 1 font-lock-builtin-face)
	(,terraform--inline-reserved-words-with-type-and-name-regexp--type-highlight 2 terraform--resource-type-face)
	(,terraform--inline-reserved-words-with-type-and-name-regexp--name-highlight 3 terraform--resource-name-face)
	(,terraform--inline-each-regexp 1 font-lock-builtin-face)
	(,terraform--inline-each-regexp 2 terraform--resource-type-face)
	(,terraform--inline-resource-regexp--resource-highlight 1 terraform--resource-type-face)
	(,terraform--inline-resource-regexp--name-highlight 2 terraform--resource-name-face)
    ,@hcl-font-lock-keywords))

(defun terraform-format-buffer ()
  "Rewrite current buffer in a canonical format using terraform fmt."
  (interactive)
  (let ((buf (get-buffer-create "*terraform-fmt*")))
    (if (zerop (call-process-region (point-min) (point-max)
                                    "terraform" nil buf nil "fmt" "-"))
        (let ((point (point))
              (window-start (window-start)))
          (erase-buffer)
          (insert-buffer-substring buf)
          (goto-char point)
          (set-window-start nil window-start))
      (message "terraform fmt: %s" (with-current-buffer buf (buffer-string))))
    (kill-buffer buf)))

(define-minor-mode terraform-format-on-save-mode
  "Run terraform-format-buffer before saving current buffer."
  :lighter ""
  (if terraform-format-on-save-mode
      (add-hook 'before-save-hook #'terraform-format-buffer nil t)
    (remove-hook 'before-save-hook #'terraform-format-buffer t)))

;;;###autoload
(define-derived-mode terraform-mode hcl-mode "Terraform"
  "Major mode for editing terraform configuration file"

  (setq font-lock-defaults '((terraform-font-lock-keywords)))

  ;; indentation
  (make-local-variable 'terraform-indent-level)
  (setq hcl-indent-level terraform-indent-level)

  ;; imenu
  (setq imenu-generic-expression
        '(("resource" "^resource\\s-+\"[^\"]+\"\\s-+\"\\([^\"]+\\)\"" 1)
          ("data" "^data\\s-+\"[^\"]+\"\\s-+\"\\([^\"]+\\)\"" 1)
          ("provider" "^provider\\s-+\"\\([^\"]+\\)\"" 1)
          ("module" "^module\\s-+\"\\([^\"]+\\)\"" 1)
          ("variable" "^variable\\s-+\"\\([^\"]+\\)\"" 1)
          ("output" "^output\\s-+\"\\([^\"]+\\)\"" 1)))
  (imenu-add-to-menubar "Index"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tf\\(vars\\)?\\'" . terraform-mode))

(provide 'terraform-mode)

;;; terraform-mode.el ends here
