;; basic initialization dispatch

(defconst emacs-load-start-time (current-time))

(defvar version-root
  (expand-file-name (concat "~" init-file-user
                            (format "/.emacs.d/%d-%d" emacs-major-version emacs-minor-version)))
  "The root of elisp directory for the current version of emacs."
  )

(let* ((init-file (expand-file-name "init.el" version-root))
       (init-org-file (expand-file-name "init.org" version-root)))

  (cond ((file-exists-p init-org-file)
         (require 'org)
         (org-babel-load-file init-org-file))

        ((file-exists-p init-file)
         (load init-file))

        ;; the catchall for older version
        ((= emacs-major-version 23)
         (setq version-root (expand-file-name (concat "~" init-file-user "/.emacs.d/init-orig.el")))
         (load version-root))

        (t
         (message "No customization for version %d.%d" emacs-major-version emacs-minor-version))
        ))

; see https://github.com/jwiegley/dot-emacs/blob/master/init.el

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-load-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

