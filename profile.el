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
    mu4e-compose-signature
    smtpmail-queue-dir
    smtpmail-local-domain
    smtpmail-smtp-user
    smtpmail-smtp-server
    smtpmail-stream-type
    smtpmail-smtp-service
    )
  "List of variables that a user may want to assign for each profile.")

(defcustom profile-extra-email-addresses '()
  "List of your email addresses not in any profile."
  :group 'profile
  :type '(repeat string))

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

(defcustom profile-noisy-query nil
  "Query to match noisy mailing lists."
  :group 'profile)

(defcustom profile-folder-query-format "folder:\"%s\""
  "Query to match a particular maildir folder.
Use maildir:/\"%s\" in mu4e."
  :group 'profile)

(defun profile-names ()
  "Return a list of all user profile names.
This list is extracted from `profile-binding-alist'."
  (mapcar #'car profile-binding-alist))

(defun profile-email-addresses ()
  "Return a list of all user email addresses.
This list is extracted from `profile-binding-alist' and occurences of
`user-mail-address' in this list.  Use function
`profile-all-email-addresses' to get all addresses returned by
`profile-email-addresses' plus the ones of the variable
`profile-extra-email-addresses'."
  (profile-binding-values-for-profiles 'user-mail-address))

(defun profile-all-email-addresses ()
  "Return all user email addresses.
This results in the concatenation of function `profile-email-addresses' and
variable `profile-extra-email-addresses'."
  (nconc (profile-email-addresses) profile-extra-email-addresses))

(defun profile--choose-profile ()
  "Ask the user to choose an profile from `profile-binding-alist'."
  (let ((profile-names (profile-names)))
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

(defun profile-binding-values-for-profiles (binding-name)
  "Collect all values of BINDING-NAME in `profile-binding-alist'."
  (mapcar
   (lambda (profile)
     (profile--binding-value-for-profile profile binding-name))
   profile-binding-alist))

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

(defun profile--message-from-address ()
  "Return the From email address of the current buffer."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers-or-head)
      (let ((from (message-fetch-field "from")))
        (cadr (mail-extract-address-components from))))))

(defun profile-set-profile-from-message-from-field ()
  "Use `profile-set-profile' based on the From field of current message."
  (require 'cl-extra) ;; for #'cl-equalp
  (profile-set-profile
   (profile--profile-for-binding
    (cons 'user-mail-address (profile--message-from-address))
    #'cl-equalp ;; compare strings case insensitively
    )))


(defun profile-folder-query (maildir)
  "Return the query to match emails in MAILDIR."
  (format profile-folder-query-format maildir))

(defun profile-sent-query ()
  "Return a query matching all emails sent by the user."
  (mapconcat
   (lambda (address) (format "from:%s" address))
   (profile-all-email-addresses)
   " OR "))

(defun profile-inbox-folder-or-tag-query ()
  "Match all messages in inbox."
  (format "(%s OR %s) AND (tag:\\\\Inbox OR NOT %s)"
          (mapconcat
           (lambda (profile-name) (profile-folder-query (format "%s/INBOX" profile-name)))
           (profile-names)
           " OR ")
          (profile-folder-query "GMail/All Mail")
          (profile-folder-query "GMail/All Mail")))

(defun profile-inbox-query ()
  "Match all messages in inbox but noisy mailing lists.
If a message is in a noisy mailing list (as of `profile-noisy-query') and
the user is a direct recipient, this query will still match the message:
that way direct messages are directly visible."
  (if profile-noisy-query
      (format "(%s) AND (NOT (%s) OR recip:damien*)"
              (profile-inbox-folder-or-tag-query)
              profile-noisy-query)
    (profile-inbox-folder-or-tag-query)))

(defun profile-noisy-unarchived-list-query ()
  "Return unarchived emails in noisy mailing lists.
Noisy mailing lists are defined in `profile-noisy-query'."
  (format "(%s) AND (%s)"
          (profile-inbox-folder-or-tag-query)
          profile-noisy-query))


;;;###autoload
(defun profile-set-profile (&optional profile)
  "Set all bindings of PROFILE."
  (interactive (list (profile--choose-profile)))
  (let ((profile (or profile (profile--choose-profile))))
    (mapc (lambda (binding) (set (profile--binding-name binding)
                            (profile--binding-value binding)))
          (profile--bindings profile))))

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

(defun profile--change-signature-in-compose ()
  "Change the signature of the current message.
If the previous signature had no newline between the ending and
the signature, this function removes the blank line before the
new signature."
  (save-excursion
    (set (make-local-variable 'message-signature)
         (if (boundp 'mu4e-compose-signature)
             mu4e-compose-signature
           message-signature))
    (let ((last-line-not-empty nil))
      ;; If the current message has a signature, delete it first
      (if (message-goto-signature)
          (progn
            (forward-line -2)
            ;; Check if the current line is empty. If it is not empty, go to the
            ;; next line and remember the current point
            (unless (equal (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)) "")
              (progn
                (setq last-line-not-empty (point))
                (end-of-line)
                (open-line 1)))
            (delete-region (point) (point-max))))
      (message-insert-signature)
      ;; If there was no blank line between the ending and the signature, the
      ;; following code removes the empty line again that
      ;; `message-insert-signature` includes
      (if last-line-not-empty
          (progn
            (message-goto-signature)
            (forward-line -1)
            (flush-lines "^$" last-line-not-empty (point)))))))

;;;###autoload
(defun profile-set-profile-from-name (profile-name)
  "Set all bindings of profile named PROFILE-NAME."
  (profile-set-profile (profile--profile-with-name profile-name)))

(defun profile-force-profile-in-compose (profile)
  "Bind all PROFILE variables and modify the From: field of current message.
Interactively, ask the user for the profile to use."
  (interactive (list (profile--choose-profile)))
  (profile-set-profile profile)
  (profile--change-from-in-compose)
  (profile--change-signature-in-compose))

(provide 'profile)

;;; profile.el ends here

;;  LocalWords:  alist maildir
