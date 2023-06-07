;;; test-command.el --- Test for terraform-mode commands

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

(require 'ert)
(require 'cl-lib)
(require 'terraform-mode)

(ert-deftest command--beginning-of-defun--from-within-block ()
  "Move to beginning-of-defun"
  (with-terraform-temp-buffer
    "
variable \"ami\" {
    description = \"the AMI to use\"
}

resource \"aws_instance\" \"web\"{
    ami = ${variable.ami}
    count = 2
}
"

    (forward-cursor-on "use")
    (call-interactively 'hcl-beginning-of-defun)
    (should (looking-at "^variable"))))

(ert-deftest command--beginning-of-defun--from-start-of-another-block ()
  "Move to beginning-of-defun"
  (with-terraform-temp-buffer
    "
variable \"ami\" {
    description = \"the AMI to use\"
}

resource \"aws_instance\" \"web\"{
    ami = ${variable.ami}
    count = 2
}
"

    (forward-cursor-on "^resource")
    (call-interactively 'hcl-beginning-of-defun)
    (should (looking-at "^variable"))))

(ert-deftest command--end-of-defun--from-within-block ()
  "Move to end-of-defun"
  (with-terraform-temp-buffer
    "
variable \"ami\" {
    description = \"the AMI to use\"
}
# end1
resource \"aws_instance\" \"web\"{
    ami = ${variable.ami}
    count = 2
}
"

    (forward-cursor-on "use")
    (call-interactively 'hcl-end-of-defun)
    (should (looking-at "^# end1"))))

(ert-deftest command--end-of-defun--from-start-of-block ()
  "Move to end-of-defun"
  (with-terraform-temp-buffer
    "
variable \"ami\" {
    description = \"the AMI to use\"
}
# end1
resource \"aws_instance\" \"web\"{
    ami = ${variable.ami}
    count = 2
}
"

    (forward-cursor-on "^resource")
    (call-interactively 'hcl-end-of-defun)
    (should (eobp))))

(ert-deftest command--open-doc--at-resource-def-line ()
  (with-terraform-temp-buffer
    "
resource \"elasticstack_elasticsearch_security_user\" \"filebeat_writer\" {
    username = \"filebeat_writer\"
    password = random_password.filebeat_writer.result
    roles    = [\"filebeat_writer\"]
    depends_on = [elasticstack_elasticsearch_security_role.filebeat_writer]
}
"

    (forward-cursor-on "security_user")
    (cl-letf (((symbol-function 'terraform--get-resource-provider-namespace) (lambda (prov) "elastic")))
      (should (equal (terraform--resource-url-at-point) "https://registry.terraform.io/providers/elastic/elasticstack/latest/docs/resources/elasticsearch_security_user")))))

(ert-deftest command--open-doc--at-data-resource-def-line ()
  (with-terraform-temp-buffer
    "
data \"aws_subnets\" \"example\" {
  filter {
    name   = \"vpc-id\"
    values = [var.vpc_id]
  }
}
"
    (forward-cursor-on "subnets")
    (cl-letf (((symbol-function 'terraform--get-resource-provider-namespace) (lambda (prov) "hashicorp")))
      (should (equal (terraform--resource-url-at-point) "https://registry.terraform.io/providers/hashicorp/aws/latest/docs/data-sources/subnets")))))

(ert-deftest command--add-comment-doc--at-data-resource-def-line ()
  (with-terraform-temp-buffer
    "
data \"aws_subnets\" \"example\" {
  filter {
    name   = \"vpc-id\"
    values = [var.vpc_id]
  }
}
"
    (forward-cursor-on "filter")
    (cl-letf (((symbol-function 'terraform--get-resource-provider-namespace) (lambda (prov) "hashicorp")))
      (terraform-insert-doc-in-comment)
      (should (equal (search-backward "# https://registry") 2)))))

(ert-deftest command--open-doc--in-body ()
  (with-terraform-temp-buffer
    "
resource \"elasticstack_elasticsearch_security_user\" \"filebeat_writer\" {
    username = \"filebeat_writer\"
    password = random_password.filebeat_writer.result
    roles    = [\"filebeat_writer\"]
    depends_on = [elasticstack_elasticsearch_security_role.filebeat_writer]
}
"

    (forward-cursor-on "random_password")
    (cl-letf (((symbol-function 'terraform--get-resource-provider-namespace) (lambda (prov) "elastic")))
      (should (equal (terraform--resource-url-at-point) "https://registry.terraform.io/providers/elastic/elasticstack/latest/docs/resources/elasticsearch_security_user")))))

;;; test-command.el ends here
