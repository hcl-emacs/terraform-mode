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
(require 'terraform-mode)

(ert-deftest beginning-of-defun ()
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
    (should (looking-at "^variable"))

    (forward-cursor-on "^resource")
    (call-interactively 'hcl-beginning-of-defun)
    (should (looking-at "^variable"))))

(ert-deftest end-of-defun ()
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
    (should (looking-at "^# end1"))

    (forward-cursor-on "^resource")
    (call-interactively 'hcl-end-of-defun)
    (should (eobp))))

;;; test-command.el ends here
