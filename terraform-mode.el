;;; terraform-mode.el --- Major mode for terraform configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-terraform-mode
;; Version: 0.06
;; Package-Requires: ((emacs "24.3") (hcl-mode "0.03") (dash "2.17.0"))

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
;;   (setq terraform-format-on-save t)

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'hcl-mode)
(require 'dash)
(require 'thingatpt)
(require 'outline)

(defgroup terraform nil
  "Major mode of Terraform configuration file."
  :group 'languages)

(defcustom terraform-indent-level 2
  "The tab width to use when indenting."
  :type 'integer)

(defcustom terraform-format-on-save nil
  "Format buffer on save"
  :type 'boolean
  :group 'terraform-mode)

(defface terraform-resource-type-face
  '((t :inherit font-lock-type-face))
  "Face for resource names."
  :group 'terraform-mode)

(define-obsolete-face-alias 'terraform--resource-type-face 'terraform-resource-type-face "1.0.0")

(defface terraform-resource-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for resource names."
  :group 'terraform-mode)

(define-obsolete-face-alias 'terraform--resource-name-face 'terraform-resource-name-face "1.0.0")

(defface terraform-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for builtins."
  :group 'terraform-mode)

(define-obsolete-face-alias 'terraform--builtin-face 'terraform-builtin-face "1.0.0")

(defface terraform-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for varriables."
  :group 'terraform-mode)

(defconst terraform--constants-regexp
  (concat "\\(?:^\\|[^.]\\)" (regexp-opt '("null") 'words)))

(defconst terraform--block-builtins-without-name-or-type-regexp
  (rx line-start
      (zero-or-more space)
      (group-n 1 (or "terraform" "locals" "required_providers" "atlas" "connection"))
      (or (one-or-more space) "{")))

(defconst terraform--block-builtins-with-type-only
  (rx (or "backend" "provider" "provisioner")))

(defconst terraform--block-builtins-with-type-only--builtin-highlight-regexp
  (eval `(rx line-start
         (zero-or-more space)
         (group-n 1 (regexp ,(eval terraform--block-builtins-with-type-only)))
         (one-or-more space))))

(defconst terraform--block-builtins-with-type-only--resource-type-highlight-regexp
  (eval `(rx (regexp ,(eval terraform--block-builtins-with-type-only--builtin-highlight-regexp))
             (group-n 2 (and (not (any "=")) (+? (not space))))
             (or (one-or-more space) "{"))))

(defconst terraform--block-builtins-with-name-only
  (rx (or "variable" "module" "output")))

(defconst terraform--block-builtins-with-name-only--builtin-highlight-regexp
  (eval `(rx line-start
         (zero-or-more space)
         (group-n 1 (regexp ,(eval terraform--block-builtins-with-name-only)))
         (one-or-more space))))

(defconst terraform--block-builtins-with-name-only--name-highlight-regexp
  (eval `(rx (regexp ,(eval terraform--block-builtins-with-name-only--builtin-highlight-regexp))
         (group-n 2 (+? (not space)))
         (or (one-or-more space) "{"))))

(defconst terraform--block-builtins-with-type-and-name
  (rx (or "data" "resource")))

(defconst terraform--block-builtins-with-type-and-name--builtin-highlight-regexp
  (eval `(rx line-start
         (zero-or-more space)
         (group-n 1 (regexp ,(eval terraform--block-builtins-with-type-and-name)))
         (one-or-more space))))

(defconst terraform--block-builtins-with-type-and-name--type-highlight-regexp
  (eval `(rx (regexp ,(eval terraform--block-builtins-with-type-and-name--builtin-highlight-regexp))
         (group-n 2 "\"" (+? (not space)) "\"")
         (one-or-more space))))

(defconst terraform--block-builtins-with-type-and-name--name-highlight-regexp
  (eval `(rx (regexp ,(eval terraform--block-builtins-with-type-and-name--type-highlight-regexp))
         (group-n 3 (+? (not space)))
         (or (one-or-more space) "{"))))

(defconst terraform--assignment-statement
  (rx line-start
      (zero-or-more space)
      (group-n 1 (minimal-match (one-or-more any)))
      (zero-or-more space)
      "="))

(defvar terraform-font-lock-keywords
  `((,terraform--assignment-statement 1 'terraform-variable-name-face)
    (,terraform--block-builtins-without-name-or-type-regexp 1 'terraform-builtin-face)
    (,terraform--block-builtins-with-type-only--builtin-highlight-regexp 1 'terraform-builtin-face)
    (,terraform--block-builtins-with-type-only--resource-type-highlight-regexp
     (1 'terraform-builtin-face)
     (2 'terraform-resource-type-face t))
    (,terraform--block-builtins-with-name-only--builtin-highlight-regexp 1 'terraform-builtin-face)
    (,terraform--block-builtins-with-name-only--name-highlight-regexp
     (1 'terraform-builtin-face)
     (2 'terraform-resource-name-face t))
    (,terraform--block-builtins-with-type-and-name--builtin-highlight-regexp 1 'terraform-builtin-face)
    (,terraform--block-builtins-with-type-and-name--type-highlight-regexp 2 'terraform-resource-type-face t)
    (,terraform--block-builtins-with-type-and-name--name-highlight-regexp
     (1 'terraform-builtin-face)
     (2 'terraform-resource-type-face t)
     (3 'terraform-resource-name-face t))
    (,terraform--constants-regexp 1 'font-lock-constant-face)
    ,@hcl-font-lock-keywords))

(defun terraform-format-buffer ()
  "Rewrite current buffer in a canonical format using terraform fmt."
  (interactive)
  (let ((buf (get-buffer-create "*terraform-fmt*")))
    (if (zerop (call-process-region (point-min) (point-max)
                                    "terraform" nil buf nil "fmt" "-no-color" "-"))
        (let ((point (point))
              (window-start (window-start)))
          (erase-buffer)
          (insert-buffer-substring buf)
          (when (/= terraform-indent-level 2)
            (indent-region (point-min) (point-max)))
          (goto-char point)
          (set-window-start nil window-start))
      (message "terraform fmt: %s" (with-current-buffer buf (buffer-string))))
    (kill-buffer buf)))

(defun terraform-format-region ()
  "Rewrite current region in a canonical format using terraform fmt."
  (interactive)
  (let ((buf (get-buffer-create "*terraform-fmt*")))
    (when (use-region-p)
    (if (zerop (call-process-region (region-beginning) (region-end)
                                    "terraform" nil buf nil "fmt" "-"))
        (let ((point (region-end))
              (window-start (region-beginning)))
          (delete-region window-start point)
          (insert-buffer-substring buf)
          (goto-char point)
          (set-window-start nil window-start))
      (message "terraform fmt: %s" (with-current-buffer buf (buffer-string))))
    (kill-buffer buf))))

(define-minor-mode terraform-format-on-save-mode
  "Run terraform-format-buffer before saving current buffer."
  :lighter ""
  (if terraform-format-on-save-mode
      (add-hook 'before-save-hook #'terraform-format-buffer nil t)
    (remove-hook 'before-save-hook #'terraform-format-buffer t)))

(defun terraform--generate-imenu ()
  (let ((search-results (make-hash-table :test #'equal))
    (menu-list '()))
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward terraform--block-builtins-with-type-only--resource-type-highlight-regexp nil t)
        (let ((key (match-string 1))
          (location (match-beginning 2))
          (resource-type (replace-regexp-in-string "\"" "" (match-string 2))))
      (-if-let (matches (gethash key search-results))
          (puthash key (push `(,resource-type . ,location) matches) search-results)
        (puthash key `((,resource-type . ,location)) search-results))))


      (goto-char (point-min))
      (while (re-search-forward terraform--block-builtins-with-name-only--name-highlight-regexp nil t)
        (let ((key (match-string 1))
          (location (match-beginning 2))
          (resource-name (replace-regexp-in-string "\"" "" (match-string 2))))
      (-if-let (matches (gethash key search-results))
          (puthash key (push `(,resource-name . ,location) matches) search-results)
        (puthash key `((,resource-name . ,location)) search-results))))

      (goto-char (point-min))
      (while (re-search-forward terraform--block-builtins-with-type-and-name--name-highlight-regexp nil t)
        (let* ((key (match-string 1))
           (location (match-beginning 2))
               (type (match-string 2))
               (name (match-string 3))
           (resource-name (concat (replace-regexp-in-string "\"" "" type)
                      "/"
                      (replace-regexp-in-string "\"" "" name))))
      (-if-let (matches (gethash key search-results))
          (puthash key (push `(,resource-name . ,location) matches) search-results)
        (puthash key `((,resource-name . ,location)) search-results))))

      (maphash (lambda (k v) (push `(,k ,@v) menu-list)) search-results)
      menu-list)))

(defun terraform--extract-provider (resource-name)
  "Return the provider associated with a RESOURCE-NAME."
  (car (split-string resource-name "_")))

(defun terraform--extract-resource (resource-name)
  "Return the resource associated with a RESOURCE-NAME."
  (mapconcat #'identity (cdr (split-string resource-name "_")) "_"))

(defun terraform--get-resource-provider-namespace (provider)
  "Return provider namespace for PROVIDER."
  (let ((provider-info (shell-command-to-string "terraform providers")))
    (with-temp-buffer
      (insert provider-info)
      (goto-char (point-min))
      (when (re-search-forward (concat "/\\(.*?\\)/" provider "\\]") nil t)
        (match-string 1)))))

(defun terraform--get-resource-provider-source (provider &optional dir)
  "Return Terraform provider source for PROVIDER located in DIR.
Terraform provider source is searched in `required_provider' declaration
in current buffer or in other Terraform files located in the same directory
of the file of current buffer.  If still not found, the provider source is
searched by running command `terraform providers'.
The DIR parameter is optional and used only for tests."
  (goto-char (point-min))
  ;; find current directory if it's not specified in arguments
  (if (and (not dir) buffer-file-name) (setq dir (file-name-directory buffer-file-name)))
  (let (tf-files
        ;; try to find provider source in current buffer
        (provider-source (terraform--get-resource-provider-source-in-buffer provider)))
    ;; check if terraform provider-source was found
    (when (and (= (length provider-source) 0) dir)
        ;; find all terraform files of this project. One of them
        ;; should contain required_provider declaration
        (setq tf-files (directory-files dir nil "^[[:alnum:][:blank:]_.-]+\\.tf$")))
    ;; iterate on terraform files until a provider source is found
    (while (and (= (length provider-source) 0) tf-files)
        (with-temp-buffer
          (let* ((file (pop tf-files))
                 (file-path (if dir (concat dir "/" file) file)))
            (insert-file-contents file-path)
            ;; look for provider source in a terraform file
            (setq provider-source (terraform--get-resource-provider-source-in-buffer provider)))))
    provider-source))

(defun terraform--get-resource-provider-source-in-buffer (provider)
  "Search and return provider namespace for PROVIDER in current buffer.
Return nil if not found."
  (goto-char (point-min))
  (if (and (re-search-forward "^terraform[[:blank:]]*{" nil t)
           (re-search-forward "^[[:blank:]]*required_providers[[:blank:]]*{" nil t)
           (re-search-forward (concat "^[[:blank:]]*" provider "[[:blank:]]*=[[:blank:]]*{") nil t)
           (re-search-forward "^[[:blank:]]*source[[:blank:]]*=[[:blank:]]*\"\\([a-z/]+\\)\"" nil t))
      (match-string 1)))

(defun terraform--resource-url (resource doc-dir)
  "Return the url containing the documentation for RESOURCE using DOC-DIR."
  (let* ((provider (terraform--extract-provider resource))
          ;; search provider source in terraform files
         (provider-source (terraform--get-resource-provider-source provider))
         (resource-name (terraform--extract-resource resource)))
    (when (= (length provider-source) 0)
        ;; fallback to old method with terraform providers command
      (setq provider-source (concat
                             (terraform--get-resource-provider-namespace provider)
                             "/" provider)))
    (if (> (length provider-source) 0)
        (format "https://registry.terraform.io/providers/%s/latest/docs/%s/%s"
                provider-source doc-dir resource-name)
      (user-error "Can not determine the provider source for %s" provider))))

(defun terraform--resource-url-at-point ()
  (save-excursion
    (goto-char (line-beginning-position))
    (unless (looking-at-p "^resource\\|^data")
      (re-search-backward "^resource\\|^data" nil t))
    (let ((doc-dir (if (equal (word-at-point) "data") "data-sources" "resources")))
      (forward-symbol 2)
      (terraform--resource-url (thing-at-point 'symbol) doc-dir))))

(defun terraform-open-doc ()
  "Open a browser at the URL documenting the resource at point."
  (interactive)
  (browse-url (terraform--resource-url-at-point)))

(defun terraform-kill-doc-url ()
  "Kill the URL documenting the resource at point (i.e. copy it to the clipboard)."
  (interactive)
  (let* ((url (substring-no-properties (terraform--resource-url-at-point))))
    (kill-new url)
    (message "Copied URL: %s" url)))

(defun terraform-insert-doc-in-comment ()
  "Insert a comment containing an URL documenting the resource at point."
  (interactive)
  (let ((doc-url (terraform--resource-url-at-point)))
    (save-excursion
      (unless (looking-at-p "^resource\\|^data")
        (re-search-backward "^resource\\|^data" nil t))
      (insert (format "# %s\n" doc-url)))))

(defun terraform--outline-level ()
  "Return the depth to which a statement is nested in the outline.

See also `outline-level'."
  (or (cdr (assoc (match-string 1) outline-heading-alist))
      (- (match-end 1) (match-beginning 1))))

(defun terraform--setup-outline-mode ()
  (set (make-local-variable 'outline-level) #'terraform--outline-level)

  (let ((terraform-keywords
         (list "terraform" "locals" "required_providers" "atlas" "connection"
               "backend" "provider" "provisioner"
               "variable" "module" "output"
               "data" "resource")))
    (set (make-local-variable 'outline-regexp)
         (concat
          "^"
          (regexp-opt terraform-keywords 'symbols)
          "[[:blank:]].*{[[:blank:]]*$"))
    (set (make-local-variable 'outline-heading-alist)
         (mapcar
          (lambda (item) (cons item 2))
          terraform-keywords))))

(defun terraform-toggle-or-indent (&optional arg)
  "Toggle visibility of block under point or indent.

If the point is not at the heading, call
`indent-for-tab-command'."
  (interactive)
  (if (and outline-minor-mode (outline-on-heading-p))
      (outline-toggle-children)
    (indent-for-tab-command arg)))

(defvar terraform-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d C-w") #'terraform-open-doc)
    (define-key map (kbd "C-c C-d C-c") #'terraform-kill-doc-url)
    (define-key map (kbd "C-c C-d C-r") #'terraform-insert-doc-in-comment)
    (define-key map (kbd "C-c C-f") #'outline-toggle-children)
    map))

;;;###autoload
(define-derived-mode terraform-mode hcl-mode "Terraform"
  "Major mode for editing terraform configuration file"

  (setq font-lock-defaults '((terraform-font-lock-keywords)))
  (when terraform-format-on-save
    (terraform-format-on-save-mode 1))

  ;; indentation
  (make-local-variable 'terraform-indent-level)
  (setq hcl-indent-level terraform-indent-level)

  ;; outline
  (terraform--setup-outline-mode)

  ;; imenu
  (setq imenu-sort-function 'imenu--sort-by-name)
  (setq imenu-create-index-function 'terraform--generate-imenu)
  (imenu-add-to-menubar "Terraform"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.t\\(f\\(vars\\)?\\|ofu\\)\\'" . terraform-mode))

(provide 'terraform-mode)

;;; terraform-mode.el ends here
