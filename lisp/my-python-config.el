;;; my-python-config.el --- Setup `python-mode'

;;; Commentary:

;;;

;;; Code:

(require 'jedi)

(defun my/python-hook ()
  "My personal `python-mode-hook'."
  (jedi:setup)
  (setq jedi:complete-on-dot t))

(provide 'my-python-config)
;;; my-python-config.el ends here
