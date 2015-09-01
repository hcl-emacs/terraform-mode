;;; terraform-mode.el --- Major mode for terraform configuration file

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-terraform-mode
;; Version: 0.02
;; Package-Requires: ((cl-lib "0.5"))

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

;;; Code:

(require 'cl-lib)
(require 'rx)

(defgroup terraform nil
  "Major mode of Terraform configuration file."
  :group 'languages)

(defcustom terraform-indent-level 2
  "The tab width to use when indenting."
  :type 'integer
  :group 'terraform)

(defconst terraform--block-regexp
  "^\\s-*\\(provider\\|resource\\|module\\|variable\\|output\\)\\s-+\"")

(defconst terraform--atlas-regexp
  "^\\s-*\\(atlas\\)\\s-*")

(defconst terraform--provisioner-regexp
  "^\\s-+\\(provisioner\\)\\s-+\"")

(defconst terraform--assignment-regexp
  "\\s-*\\([[:word:]]+\\)\\s-*=\\(?:[^>=]\\)")

(defconst terraform--map-regexp
  "\\s-*\\([[:word:]]+\\)\\s-*{")

(defconst terraform--boolean-regexp
  (concat "\\(?:^\\|[^.]\\)"
          (regexp-opt '("true" "false" "on" "off" "yes" "no")
                      'words)))

;; String Interpolation(This regexp is taken from ruby-mode)
(defconst terraform--string-interpolation-regexp
  "\\${[^}\n\\\\]*\\(?:\\\\.[^}\n\\\\]*\\)*}")

(defvar terraform-font-lock-keywords
  `((,terraform--block-regexp 1 font-lock-function-name-face)
    (,terraform--atlas-regexp 1 font-lock-function-name-face)
    (,terraform--provisioner-regexp 1 font-lock-function-name-face)
    (,terraform--assignment-regexp 1 font-lock-variable-name-face)
    (,terraform--boolean-regexp . font-lock-constant-face)
    (,terraform--map-regexp 1 font-lock-type-face)
    (,terraform--string-interpolation-regexp 0 font-lock-variable-name-face t)))

(defsubst terraform--paren-level ()
  (car (syntax-ppss)))

(defsubst terraform--in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

(defun terraform--block-indentation ()
  (let ((curline (line-number-at-pos)))
    (save-excursion
      (condition-case nil
          (progn
            (backward-up-list)
            (unless (= curline (line-number-at-pos))
              (current-indentation)))
        (scan-error nil)))))

(defun terraform--previous-indentation ()
  (save-excursion
    (forward-line -1)
    (let (finish)
      (while (not finish)
        (cond ((bobp) (setq finish t))
              ((terraform--in-string-or-comment-p) (forward-line -1))
              (t
               (let ((line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
                 (if (not (string-match-p "\\`\\s-*\\'" line))
                     (setq finish t)
                   (forward-line -1))))))
      (current-indentation))))

(defun terraform-indent-line ()
  "Indent current line as Terraform configuration."
  (interactive)
  (let* ((curpoint (point))
         (pos (- (point-max) curpoint)))
    (back-to-indentation)
    (if (terraform--in-string-or-comment-p)
        (goto-char curpoint)
      (let ((block-indentation (terraform--block-indentation)))
        (delete-region (line-beginning-position) (point))
        (if block-indentation
            (if (looking-at "[]}]")
                (indent-to block-indentation)
              (indent-to (+ block-indentation terraform-indent-level)))
          (indent-to (terraform--previous-indentation)))
        (when (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))))))

(defun terraform-beginning-of-defun (&optional count)
  (interactive "p")
  (setq count (or count 1))
  (let ((match 0)
        finish)
    (while (and (not finish)
                (re-search-backward terraform--block-regexp nil t))
      (unless (terraform--in-string-or-comment-p)
        (cl-incf match)
        (when (= match count)
          (setq finish t))))))

(defun terraform-end-of-defun (&optional count)
  (interactive "p")
  (let ((paren-level (terraform--paren-level)))
    (when (or (and (looking-at-p "}") (= paren-level 1))
              (= paren-level 0))
      (re-search-forward terraform--block-regexp nil t)))
  (dotimes (_i count)
    (when (looking-at-p terraform--block-regexp)
      (goto-char (line-end-position)))
    (terraform-beginning-of-defun 1)
    (skip-chars-forward "^{")
    (forward-char 1)
    (let ((orig-level (terraform--paren-level)))
      (while (>= (terraform--paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-line +1)))))

(defvar terraform-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-a") 'terraform-beginning-of-defun)
    (define-key map (kbd "C-M-e") 'terraform-end-of-defun)
    map)
  "Keymap for Terraform major mode.")

;;;###autoload
(define-derived-mode terraform-mode prog-mode "Terraform"
  "Major mode for editing terraform configuration file"

  (setq font-lock-defaults '((terraform-font-lock-keywords)))

  (modify-syntax-entry ?_ "w" terraform-mode-syntax-table)

  ;;; Comments
  ;; Single line comment
  (modify-syntax-entry ?# "< b" terraform-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" terraform-mode-syntax-table)

  ;; multiple line comment(/* ... */) taken from `go-mode'
  (modify-syntax-entry ?/  ". 124b" terraform-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23" terraform-mode-syntax-table)

  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")

  ;; indentation
  (make-local-variable 'terraform-indent-level)
  (set (make-local-variable 'indent-line-function) 'terraform-indent-line)

  (set (make-local-variable 'beginning-of-defun-function)
       'terraform-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'terraform-end-of-defun)

  ;; electric-mode
  (set (make-local-variable 'electric-indent-chars)
       (append "{}[]" electric-indent-chars))

  ;; imenu
  (setq imenu-generic-expression
        '(("resource" "^resource\\s-+\"[^\"]+\"\\s-+\"\\([^\"]+\\)\"" 1)
          ("provider" "^provider\\s-+\"\\([^\"]+\\)\"" 1)
          ("module" "^module\\s-+\"\\([^\"]+\\)\"" 1)
          ("variable" "^variable\\s-+\"\\([^\"]+\\)\"" 1)
          ("output" "^output\\s-+\"\\([^\"]+\\)\"" 1)))
  (imenu-add-to-menubar "Index"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tf\\(vars\\)?\\'" . terraform-mode))

(provide 'terraform-mode)

;;; terraform-mode.el ends here
