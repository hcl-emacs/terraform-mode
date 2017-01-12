;;; test-highlighting.el --- test for highlighting

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

(ert-deftest boolean-keywords ()
  "Syntax highlight of `boolean' keywords"

  (dolist (keyword '("true" "false" "on" "off" "yes" "no"))
    (with-terraform-temp-buffer
      keyword
      (should (face-at-cursor-p 'font-lock-constant-face)))))

(ert-deftest provider-block ()
  "Syntax highlight of `provider' block"

  (with-terraform-temp-buffer
    "
provider \"aws\" {
    access_key = \"foo\"
}
"

    (forward-cursor-on "provider")
    (should (face-at-cursor-p 'font-lock-function-name-face)))


  (with-terraform-temp-buffer
    "
provider     \"aws\" {
    var = \"foo\"
}
"

    (forward-cursor-on "provider")
    (should (face-at-cursor-p 'font-lock-function-name-face))))

(ert-deftest resource-block ()
  "Syntax highlight of `resource' block"

  (with-terraform-temp-buffer
    "
resource \"aws_security_group\"\"default\" {
    name = \"terraform_example\"
    description = \"Used in terraform\"
}
"

    (forward-cursor-on "resource")
    (should (face-at-cursor-p 'font-lock-function-name-face)))


  (with-terraform-temp-buffer
    "
  resource    \"aws_security_group\"        \"default\"     {
      name = \"terraform_example\"
      description = \"Used in terraform\"
  }
"

    (forward-cursor-on "resource")
    (should (face-at-cursor-p 'font-lock-function-name-face))))

(ert-deftest data-block ()
  "Syntax highlight of 'data' block"

  (with-terraform-temp-buffer
      "
data \"template_file\" \"userdata\" {
    source = \"foo\"
}
"

      (forward-cursor-on "data")
      (should (face-at-cursor-p 'font-lock-function-name-face)))

  (with-terraform-temp-buffer
      "
data   \"template_file\"  \"userdata\"    {
    source = \"foo\"
}
"

      (forward-cursor-on "data")
      (should (face-at-cursor-p 'font-lock-function-name-face))))

(ert-deftest module-block ()
  "Syntax highlight of `module' block"

  (with-terraform-temp-buffer
    "
module \"consul\" {
    source = \"foo\"
}
"

    (forward-cursor-on "module")
    (should (face-at-cursor-p 'font-lock-function-name-face)))


  (with-terraform-temp-buffer
    "
module     \"consul\" {
    source = \"foo\"
}
"

    (forward-cursor-on "module")
    (should (face-at-cursor-p 'font-lock-function-name-face))))

(ert-deftest output-block ()
  "Syntax highlight of `output' block"
  (with-terraform-temp-buffer
    "
output \"address\" {
    value = \"foobar\"
}
"

    (forward-cursor-on "output")
    (should (face-at-cursor-p 'font-lock-function-name-face)))


  (with-terraform-temp-buffer
    "
   output       \"address\"      {
         value = \"foobar\"
   }
"

    (forward-cursor-on "output")
    (should (face-at-cursor-p 'font-lock-function-name-face))))

(ert-deftest provisioner-block ()
  "Syntax highlight of `provisioner' block"
  (with-terraform-temp-buffer
    "
resource \"aws_instance\" \"web\" {
    provisioner \"file\" {
        source = \"hoge.conf\"
    }
}
"

    (forward-cursor-on "provisioner")
    (should (face-at-cursor-p 'font-lock-function-name-face)))


  (with-terraform-temp-buffer
    "
   output       \"address\"      {
         value = \"foobar\"
   }
"

    (forward-cursor-on "output")
    (should (face-at-cursor-p 'font-lock-function-name-face))))

(ert-deftest atlas-block ()
  "Syntax highlight of `atlas' block"
  (with-terraform-temp-buffer
    "
atlas {
    name = \"foo\"
}
"
    (forward-cursor-on "atlas")
    (should (face-at-cursor-p 'font-lock-function-name-face))))

(ert-deftest assignment-statement ()
  "Syntax highlight of assignment statement"
  (with-terraform-temp-buffer
    "
foo = \"var\"
"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-variable-name-face)))

  (with-terraform-temp-buffer
    "
foo=\"var\"
"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-variable-name-face)))

  (with-terraform-temp-buffer
    "
    foo=      \"var\"
"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-variable-name-face)))

  (with-terraform-temp-buffer
    "
output \"name\" {
   bar = \"baz\"
   map {
       hoge = \"${bar}\"
   }
}
"

    (forward-cursor-on "bar")
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "hoge")
    (should (face-at-cursor-p 'font-lock-variable-name-face))))

(ert-deftest string-interpolation ()
  "Syntax highlight of string interpolation"
  (with-terraform-temp-buffer
    "
foo = \"hello world\"
bar = \"${foo}\"
"

    (forward-cursor-on "{foo}")
    (forward-char 1)
    (should (face-at-cursor-p 'font-lock-variable-name-face))))

(ert-deftest single-line-comment ()
  "Syntax highlight of single line comment"

  (with-terraform-temp-buffer
    "# foo" ;; start from beginning of line

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-comment-face)))


  (with-terraform-temp-buffer
    "  bar baz # foo  " ;; start from not beggining of line

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-comment-face))))

(ert-deftest map-statement ()
  "Syntax highlight of map"
  (with-terraform-temp-buffer
    "
resource \"aws_security_group\" \"default\" {
    ingress {
        from_port = 22
        to_port = 22
    }
}
"

    (forward-cursor-on "ingress")
    (should (face-at-cursor-p 'font-lock-type-face))))

(ert-deftest multiple-line-comment ()
  "Syntax highlight of multiple line comment"

  (with-terraform-temp-buffer
    "/* foo */"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-comment-face)))

  (with-terraform-temp-buffer
    "
/*
 foo **
 bar **
 /////
 baz ##
 */"

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-comment-face))

    (forward-cursor-on "bar")
    (should (face-at-cursor-p 'font-lock-comment-face))

    (forward-cursor-on "baz")
    (should (face-at-cursor-p 'font-lock-comment-face))))

(ert-deftest inner-block ()
  "Syntax highlight of special inner block"

  (with-terraform-temp-buffer
    "
provisioner \"file\" {
    source = \"conf/myapp.conf\"
    destination = \"/etc/myapp.conf\"
    connection {
        type = \"ssh\"
        user = \"root\"
        password = \"${var.root_password}\"
    }
}"

    (forward-cursor-on "connection")
    (should (face-at-cursor-p 'font-lock-keyword-face))))

;;; test-highlighting ends here
