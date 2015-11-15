;;; profile-tests.el --- Tests for profile.el

;; Copyright (C) 2013 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Tests for profile.el

;;; Code:

(require 'ert)

(require 'profile)

(eval-when-compile (require 'cl-macs))

(setq profile-binding-alist
      '(("Profile1"
         (user-mail-address . "me@profile1.fr")
         (profile-maildir . "/Maildir1"))
        ("Profile2"
         (user-mail-address . "me@profile2.fr")
         (profile-maildir . "/Maildir2"))))

(ert-deftest profile-tests-profile-names ()
  (should (equal '("Profile1" "Profile2")
                 (profile-names))))

(ert-deftest profile-tests-name ()
  (should (equal "Profile1"
                 (profile--name (car profile-binding-alist)))))

(ert-deftest profile-tests-bindings ()
  (should (equal '((user-mail-address . "me@profile1.fr")
                   (profile-maildir . "/Maildir1"))
                 (profile--bindings (car profile-binding-alist)))))

(ert-deftest profile-tests-binding-value ()
  (should (equal "me@profile1.fr"
                 (profile--binding-value-for-profile
                  (car profile-binding-alist)
                  'user-mail-address))))

(ert-deftest profile-tests-profile-with-name ()
  (should (equal (car profile-binding-alist)
                 (profile--profile-with-name "Profile1"))))

(ert-deftest profile-tests-profiles-for-binding ()
  (should (equal (list (car profile-binding-alist))
                 (profile--profiles-for-binding
                  (cons 'profile-maildir "/Maildir1")))))

(ert-deftest profile-tests-profile-for-binding ()
  (should (equal (car profile-binding-alist)
                 (profile--profile-for-binding
                  (cons 'profile-maildir "/Maildir1")))))

(ert-deftest profile-tests-profile-for-binding-with-nil ()
  (should (null
           (profile--profiles-for-binding
            (cons 'profile-maildir "DOES NOT EXIST")))))

(ert-deftest profile-tests-guess-profile-from-message ()
  (should (equal (car profile-binding-alist)
                 (profile--guess-profile-from-message
                  `(:maildir "/Maildir1")))))

(ert-deftest profile-tests-guess-profile-from-message-subdir ()
  (should (equal (car profile-binding-alist)
                 (profile--guess-profile-from-message
                  `(:maildir "/Maildir1/SubDir")))))

(ert-deftest profile-tests-binding-values-for-profiles ()
  (should (equal '("me@profile1.fr" "me@profile2.fr")
                 (profile-binding-values-for-profiles 'user-mail-address))))

(ert-deftest profile-tests-email-addresses ()
  (should (equal '("me@profile1.fr" "me@profile2.fr")
                 (profile-email-addresses))))

(eval-when-compile (defvar profile-maildir))

(ert-deftest profile-tests-set-profile ()
  (should-not (boundp 'profile-maildir))
  (profile-set-profile (car profile-binding-alist))
  (should (boundp 'profile-maildir))
  (should (equal "/Maildir1" profile-maildir)))

(ert-deftest profile-tests-change-from-in-compose ()
  (with-current-buffer (get-buffer-create "*ert-profile*")
    (insert "From: foo\n--text follows this line--\n")
    (profile--change-from-in-compose "bar" "baz")
    (message-goto-from)
    (message-beginning-of-line)
    (should (looking-at "bar <baz>"))))

(ert-deftest profile-tests-change-from-in-compose-defaults ()
  (with-current-buffer (get-buffer-create "*ert-profile*")
    (insert "From: foo\n--text follows this line--\n")
    (setq user-full-name "bar" user-mail-address "baz")
    (profile--change-from-in-compose)
    (message-goto-from)
    (message-beginning-of-line)
    (cl-assert
     (looking-at "bar <baz>")
     nil
     "Was looking at %s"
     (buffer-substring-no-properties (point) (point-at-eol)))))

(ert-deftest profile-tests-change-signature-in-compose-with-newline ()
  (with-current-buffer (get-buffer-create "*ert-profilel-w-newline*")
    (insert "From: foo\n--text follows this line--\ncontent\n\n-- \nold signature")
    (setq mu4e-compose-signature "new signature")
    (profile--change-signature-in-compose)
    (message-goto-signature)
    (forward-line -3)
    (message-beginning-of-line)
    (should (looking-at "content\n\n-- \nnew signature\n"))))

(ert-deftest profile-tests-change-signature-in-compose-without-newline ()
  (with-current-buffer (get-buffer-create "*ert-profilel-wo-newline*")
    (insert "From: foo\n--text follows this line--\ncontent\n-- \nold signature")
    (setq mu4e-compose-signature "new signature")
    (profile--change-signature-in-compose)
    (message-goto-signature)
    (forward-line -2)
    (message-beginning-of-line)
    (should (looking-at "content\n-- \nnew signature\n"))))

(ert-deftest profile-tests-change-signature-in-compose-without-signature ()
  (with-current-buffer (get-buffer-create "*ert-profilel-wo-signature*")
    (insert "From: foo\n--text follows this line-- \ncontent")
    (setq mu4e-compose-signature "new signature")
    (profile--change-signature-in-compose)
    (message-goto-signature)
    (message-beginning-of-line)
    (should (looking-at "new signature"))))

(provide 'profile-tests)

;;; profile-tests.el ends here
