;;; modular.el --- Modularize configuration -*- lexical-binding: t; -*-

;; Author: LostEclair
;; Maintainer: LostEclair
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.4"))
;; License: GPL-3.0-or-later
;; URL: https://github.com/LostEclair/modular

;;; Code:

(defun modular--feature-pattern-validator (symbol value)
  (unless (= 2 (length (split-string value "%s")))
    (error "Pattern must contain only one occurence of %%s"))
  (set-default symbol value))

(defcustom modular-feature-state-variable-pattern "modular-enable-%s"
  "A pattern, which is used when defining feature state variables."
  :type 'string
  :set #'modular--feature-pattern-validator)

(defcustom modular-feature-pattern "setup-%s"
  "A pattern, which is used when requiring feature."
  :type 'string
  :set #'modular--feature-pattern-validator)

(defcustom modular-verbose nil
  "Non-nil to make `modular-require' more verbose"
  :type 'boolean)

(defun modular--extract-feature-state-variable-symbol (name)
  "Returns `modular-feature-state-variable-pattern', which was applied to NAME"
  (intern (format modular-feature-state-variable-pattern name)))

(defun modular--extract-feature-symbol (name)
  "Returns `modular-feature-pattern', which was applied to NAME"
  (intern (format modular-feature-pattern name)))

;;;###autoload
(defmacro modular-define-feature (name &optional default)
  "Generate `modular-enable-NAME' variables with ease."
  (let ((name-string (symbol-name name))
        (symbol-name (modular--extract-feature-state-variable-symbol name)))
    `(defvar ,symbol-name
       ,(if default
            (progn
              (unless (memq default '(:on :off))
                (error "DEFAULT must be either :on or :off"))
              (eq default :on))
          t)
       ,(format "Non-nil to enable %s" name-string))))

;;;###autoload
(defmacro modular-set-feature (feature state)
  "Set FEATURE to STATE. STATE can be `:on' or `:off'"
  (let ((symbol-name (modular--extract-feature-state-variable-symbol feature)))
    (unless (memq state '(:on :off))
      (error "STATE must be either :on or :off"))
    (unless (boundp symbol-name)
      (error "There is no modularity feature with name `%s' (Expected `%s' to be bound in the environment)" feature symbol-name))
    `(setq ,symbol-name ,(eq state :on))))

;;;###autoload
(defmacro modular-require (feature)
  "Check the variable `modular-enable-FEATURE' and if not nil, require the `setup-FEATURE'.

Set `modular-verbose' to non-nil to observe what is being loaded."
  (let* ((variable (modular--extract-feature-state-variable-symbol feature))
         (feature-name (modular--extract-feature-symbol feature)))
    (unless (boundp variable)
      (error "There is no modularity feature with name `%s' (Expected `%s' to be bound in the environment)" feature variable))
    (when (symbol-value variable)
      `(progn
         (when modular-verbose
           (message "Feature `%s' is enabled (requiring %s)" ',feature ',feature-name))
         (condition-case error
             (require ',feature-name)
           (file-error
            (display-warning 'modular (format "Cannot require %s. Is such feature in your load-path?" ',feature-name)
                             :error)))))))

(provide 'modular)
;;; modular.el ends here
