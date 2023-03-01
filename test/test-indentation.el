;;; test-indentation.el --- test for indentation

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

(ert-deftest indentation--no-indentation ()
  "No indentation case"
  (with-terraform-temp-buffer
    "
foo = \"val1\"
bar = \"val2\"
"

    (forward-cursor-on "bar")
    (call-interactively 'indent-for-tab-command)
    (should (= (current-indentation) 0))))

(ert-deftest indentation--no-indentation-with-empty-line ()
  "No indentation case with empty lines"
  (with-terraform-temp-buffer
    "
foo = \"val1\"
    \t
bar = \"val2\"
"

    (forward-cursor-on "bar")
    (call-interactively 'indent-for-tab-command)
    (should (= (current-indentation) 0))))

(ert-deftest indentation--no-indentation-into-comment ()
  "No indentation case into comment"
  (with-terraform-temp-buffer
    "
    foo = 10
/*
  bar = 20
*/
"

    (forward-cursor-on "bar")
    (let ((cur-indent (current-indentation)))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) cur-indent)))))

(ert-deftest indentation--indentation-into-block--in-provider-block ()
  "Indent into block"
  (with-terraform-temp-buffer
    "
provider \"aws\" {
foo = 10
}
"

    (forward-cursor-on "foo")
    (call-interactively 'indent-for-tab-command)
    (should (= (current-indentation) terraform-indent-level))))


(ert-deftest indentation--indentation-into-block--in-variable-block ()
  (with-terraform-temp-buffer
    "
variable \"aws_amis\" {
foo = 10
}
"

    (forward-cursor-on "foo")
    (call-interactively 'indent-for-tab-command)
    (should (= (current-indentation) terraform-indent-level))))

(ert-deftest indentation--indentation-into-block--in-resource-block ()
  (with-terraform-temp-buffer
    "
resource \"aws_security_group\" \"group\" {
foo = 10
}
"

    (forward-cursor-on "foo")
    (call-interactively 'indent-for-tab-command)
    (should (= (current-indentation) terraform-indent-level))))

(ert-deftest indentation--indentation-into-nested-block ()
  "Indent into nested blocks"
  (with-terraform-temp-buffer
    "
resource \"aws_security_group\" \"default\" {
    ingress {
from_port = 22
    }
}
"

    (forward-cursor-on "ingress")
    (let ((cur-indent (current-indentation)))
      (forward-cursor-on "from_port")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) (+ cur-indent terraform-indent-level))))))

(ert-deftest indentation--map-indentation ()
  "Indent for map entry"
  (with-terraform-temp-buffer
    "
map_var {
key = val
}
"

    (forward-cursor-on "key")
    (call-interactively 'indent-for-tab-command)
    (should (= (current-indentation) terraform-indent-level))))

(ert-deftest indentation--array-indentation ()
  "Indent for array element"
  (with-terraform-temp-buffer
    "
array_var [
\"foo\"
]
"

    (forward-cursor-on "foo")
    (call-interactively 'indent-for-tab-command)
    (should (= (current-indentation) terraform-indent-level))))

(ert-deftest indentation--change-indent-size ()
  "Indent for array element"
  (let ((terraform-indent-level 4))
    (with-terraform-temp-buffer
      "
array_var [
\"foo\"
]
"
      (forward-cursor-on "foo")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4)))))

;;; test-indentation.el ends here
