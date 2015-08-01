;;; profile.el --- Handle multiple sets of Emacs variable bindings

;; Copyright (C) 2015 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 0.1

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

;; Handle multiple sets of Emacs variable bindings.

;;; Code:

(require 'message)
(require 'cus-edit)

(defgroup profile nil
  "Handle multiple sets of Emacs variable bindings."
  :group 'Convenience)

(defconst profile--potential-variables
  '(mu4e-trash-folder
    user-mail-address
    profile-maildir
    mu4e-sent-folder
    mu4e-sent-messages-behavior
    mu4e-drafts-folder
    smtpmail-queue-dir
    smtpmail-local-domain
    smtpmail-smtp-user
    smtpmail-smtp-server
    smtpmail-stream-type
    smtpmail-smtp-service
    )
  "List of variables that a user may want to assign for each profile.")

(defconst profile--variable-options
  (mapcar (lambda (varname) `((variable-item ,varname) ,(custom-variable-type varname)))
          profile--potential-variables)
  "Option list suitable for `defcustom' based on `profile--potential-variables'.")

(defcustom profile-binding-alist nil
  "List of profiles with their bindings."
  :group 'profile
  :type `(alist
          :key-type (string :tag "Profile name")
          :value-type (alist
                       :tag "Variables to set"
                       :key-type variable
                       :options ,profile--variable-options)))

(defun profile-names ()
  "Return a list of all user profile names.
This list is extracted from `profile-binding-alist'."
  (mapcar #'car profile-binding-alist))

(defun profile--choose-profile ()
  "Ask the user to choose an profile from `profile-binding-alist'."
  (let ((profile-names (profile-profile-names)))
    (profile--profile-with-name
     (completing-read "Compose with profile: "
                      profile-names
                      nil t nil nil (car profile-names)))))

(defun profile--name (profile)
  "Return the name of PROFILE."
  (car profile))

(defun profile--bindings (profile)
  "Return an alist of all bindings for PROFILE in `profile-binding-alist."
  (cdr profile))

(defun profile--binding-value-for-profile (profile binding-name)
  "For PROFILE, get the value associated with BINDING-NAME."
  (cdr (assoc binding-name
              (profile--bindings profile))))

(defun profile--profile-with-name (profile-name)
  "Return the profile with PROFILE-NAME."
  (cl-find profile-name
           profile-binding-alist
           :test #'string=
           :key #'car))

(defun profile--binding-name (binding)
  "Return the name of BINDING."
  (car binding))

(defun profile--binding-value (binding)
  "Return the value of BINDING."
  (cdr binding))

(defun profile--profiles-for-binding (binding &optional test)
  "Return all profiles of `profile-binding-alist' with BINDING.
Use TEST to compare the BINDING value against the binding values for each
profile.  TEST is a 2-arg function taking the profile binding value and the
BINDING value as argument in this order.  TEST defaults to `equal'."
  (let ((test (or test #'equal)))
    (cl-remove-if-not
     (lambda (binding-value)
       (funcall test (profile--binding-value binding) binding-value))
     profile-binding-alist
     :key (lambda (profile)
            (profile--binding-value-for-profile profile
                                                (profile--binding-name binding))))))

(defun profile--profile-for-binding (binding &optional test)
  "Return the first profile of `profile-binding-alist' with BINDING.
Use TEST to compare the BINDING value against the binding values for each
profile.  TEST is a 2-arg function taking the profile binding value and the
BINDING value as argument in this order.  TEST defaults to `equal'.
Nil is returned if no profile is found."
  (car (profile--profiles-for-binding binding test)))

(defun profile--guess-profile-from-message (message)
  "Guess the profile related to MESSAGE.
In practice, look at the maildir containing MESSAGE and return the profile
responsible for this maildir by searching a binding of
`profile-maildir' in `profile-binding-alist'."
  (profile--profile-for-binding
   (cons 'profile-maildir
         (plist-get message :maildir))
   (lambda (actual expected)
     (string-match (format "^%s" (regexp-quote expected))
                   actual))))

(defun profile--guess-profile-in-compose ()
  "Guess the profile best suited to compose a new message.
If the new message is an answer to an existing email this function returns
the profile the original email was sent to.  If no profile can be guessed,
return nil."
  (when (and (boundp 'mu4e-compose-parent-message)
             mu4e-compose-parent-message)
    (profile--guess-profile-from-message mu4e-compose-parent-message)))

;;;###autoload
(defun profile-set-profile (profile)
  "Set all bindings of PROFILE."
  (interactive (list (profile--choose-profile)))
  (mapc (lambda (binding) (set (profile--binding-name binding)
                          (profile--binding-value binding)))
        (profile--bindings profile)))

;;;###autoload
(defun profile-set-profile-in-compose ()
  "Set all bindings of the profile best suited to compose."
  (profile-set-profile (or (profile--guess-profile-in-compose)
                           (profile--choose-profile))))

(defun profile--change-from-in-compose (&optional name address)
  "Change the From: of current message using NAME and ADDRESS.
The From: field is changed to be \"USER <ADDRESS>\".

If NAME or ADDRESS are not provided, use the variables `user-full-name' and
`user-mail-address'."
  (save-excursion
    (message-goto-from)
    (message-beginning-of-line)
    (kill-line)
    (insert (format "%s <%s>"
                    (or name user-full-name)
                    (or address user-mail-address)))))

;;;###autoload
(defun profile-set-profile-from-name (profile-name)
  "Set all bindings of profile named PROFILE-NAME."
  (profile-set-profile (profile--profile-with-name profile-name)))

(defun profile-force-profile-in-compose (profile)
  "Bind all PROFILE variables and modify the From: field of current message.
Interactively, ask the user for the profile to use."
  (interactive (list (profile--choose-profile)))
  (profile-set-profile profile)
  (profile--change-from-in-compose))

(provide 'profile)

;;; profile.el ends here

;;  LocalWords:  alist maildir
