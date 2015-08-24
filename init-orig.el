;; ======================================================================
;;
;; DEBUGGING

;;; measure how long it takes to load initialization

(setq emacs-load-start-time (current-time))

;;(setq message-log-max 100)

;; Leave these here for debugging this file!
;;(setq debug-on-error t)
;;(add-hook 'after-init-hook
;;        '(lambda () (setq debug-on-error t)))
;;(debug-on-entry 'command-line-1)
(setq message-log-max 1000)

;; ======================================================================
;;
;; PATHS

;; add later...
;;
;; ;; Add some directories to load-path.  Subdirectories that don't
;; ;; contain a ".nosearch" file are also added.  These days this is the
;; ;; standard GNU Emacs way of doing things at the site level.  I do the
;; ;; same for my personal stuff for consistency and convenience.
;; (mapcar '(lambda (dir)
;;         (let* ((xdir (expand-file-name dir))
;;                (default-directory xdir))
;;           ;; If this isn't emacs' real site-lisp directory then
;;           ;; we'd better add ourselves, since this code assumes
;;           ;; that we're there.
;;           (add-to-list 'load-path xdir)
;;           ;; Now add subdirectories.
;;           (normal-top-level-add-subdirs-to-load-path)))
;;
;;      '("~/share/emacs/lisp"
;;        "~/share/emacs/mms"))

(mapcar '(lambda (dir)
           (let* ((xdir (expand-file-name dir)))
             (add-to-list 'load-path xdir)))
        (list (concat "~" init-file-user "/.emacs.d/elisp/psm")
              (concat "~" init-file-user "/.emacs.d/elisp/other")))

;;;

(setenv "EMACS" "t")

(require 'desire)

;; what about highlighting desire and autoload like require

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(desire\\|autoload\\)\\>[    ']*\\(\\sw+\\)?"
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))))

(add-to-list 'desire-load-path
             (expand-file-name (concat "~" init-file-user "/.emacs.d/desire")))

(add-to-list 'auto-mode-alist
             (cons (concat (regexp-quote desire-extension) "\\'")
                   'emacs-lisp-mode))

;; here we don't use init-file-user because we don't want to save
;; other peoples customization in our own directory


;; override for EnCana windows based systems

(if (and (eq system-type 'windows-nt)
         (getenv "USERDOMAIN"))
    (defun system-name ()
      (getenv "USERDOMAIN")))

(defvar emacs-persist-directory
  (expand-file-name (concat "~/.emacs.d/persist/" (system-name)))
  "The location of information relating to persistent information:

auto-save-list  - `auto-save-list-file-name'
auto-save       - `my-auto-save-directory'
bash-history    - `shell-history-directory'
bookmarks       - `bookmark-default-file'
customization   - `custom-file'
recentf         - `recentf-save-file'
saveplace       - `save-place-file'
timeclock       - `timeclock-file'
")


;; don't need to set auto-save-list-file-prefix

;; we don't need to create this directory as it will will be created
;; by auto-save

(setq auto-save-list-file-name
      (expand-file-name (format "auto-save-list/%s-%s"
                                (format-time-string "%Y-%m-%dT%H%M%S%z")
                                (emacs-pid))
                        emacs-persist-directory))

;; need to set this for recover-session to work

(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/" emacs-persist-directory))

(desire 'auto-save)

;; we may want to defer the creation of the directory until the end

(when (not (file-exists-p emacs-persist-directory))
  (message "creating persistent directory %s" emacs-persist-directory)
  (make-directory emacs-persist-directory t))

(setq custom-file (expand-file-name "custom.el" emacs-persist-directory))
(if (not (load custom-file t))
    (progn
      (message "creating blank custom file \"%s\"" custom-file)
      (custom-save-all)))

(defmacro set-custom (symbol value)
  "Set a customizable variable to VALUE unless it has been saved.

A macro wrapper around `my-custom-initialize-default'.

Like `defcustom'."
  (my-custom-initialize-default (list 'quote symbol) value))


(defun my-message (&rest args)
  "my version of message"
  (with-temp-buffer
    (insert (format-time-string "%H:%M:%S"))
    (if (< 0 (length args))
        (insert " " (apply 'format args)))
    (insert "\n")

    (append-to-buffer "*Peter*" (point-min) (point-max))
    ))


(defun my-custom-initialize-default (symbol value)
  "Initialize custom SYMBOL with VALUE if it isn't hasn't been saved.

This plays nice with customized variables; it doesn't overwrite them
if they have been set."
  (my-message "my-custom %s" (eval symbol))
  (my-message "symbol-plist %s" (symbol-plist (eval symbol)))
  (my-message "not boundp %s" (not (boundp (eval symbol))))
  (my-message "not get %s" (not (get (eval symbol) 'saved-value)))

  (if (or (not (boundp (eval symbol)))
          (not (get (eval symbol) 'saved-value)))
      (progn
        (set-default (eval symbol) (eval value))
        ;; from http://www.emacswiki.org/emacs/CustomizingAndSaving#CustomizePlus
        (put (eval symbol) 'customized-value (list (eval value)))
        ;; somehow this make this work
        (my-message "setting to %s" (eval value))
        t)
    (my-message "do not set"))
  (my-message)
  )


(require 'cycle-mini)
(require 'psm-basic)
(require 'eol-conversion)
(require 'screen-lines)

(define-key menu-bar-options-menu [screen-lines-mode]
  (menu-bar-make-toggle toggle-screen-lines-mode screen-lines-mode
                        "Screen lines mode"
                        "Screen lines mode %s"
                        "Toggle screen lines mode"
                        (screen-lines-mode)
                        (force-mode-line-update)))

(require 'transpose-windows)

(set-custom uniquify-buffer-name-style 'post-forward-angle-brackets)

;; just be quiet on startup

(set-custom inhibit-startup-echo-area-message (user-login-name))
(put 'inhibit-startup-echo-area-message 'saved-value (user-login-name))
(setq inhibit-startup-screen t)
(setq view-inhibit-help-message t)

(defun my-inhibit-startup-echo-area-message-cleanup ()
  "This clears the customization afterwards so that we don't get any errors when saving
other customizations."
  (put 'inhibit-startup-echo-area-message 'saved-value nil)
  (put 'inhibit-startup-echo-area-message 'customized-value nil))

(add-hook 'after-init-hook 'my-inhibit-startup-echo-area-message-cleanup)

(require 'uniquify)

(desire 'whitespace)
(desire 'saveplace)
(desire 'displaytime)
(desire 'desktop)
(desire 'bookmark)
(desire 'color-theme)
(desire 'dired)
(desire 'mykeys)
(desire 'infer)
(desire 'perl)
(desire 'python)
(desire 'xsl)
(desire 'howm)
(desire 'backup)
(desire 'calc)
;(desire 'minibuffer)
(desire 'folding)
(desire 'ibuffer)
(desire 'ffap)
(desire 'browse-yank)
(desire 'generic)
(desire 'newdabbrev)
(desire 'calendar)
(desire 'ipa)
(desire 'org)
(desire 'filesets)
(desire 'cust-edit)

(desire 'recentf)

;; maybe add file-journal to quantified-self

(desire 'file-journal)
(desire 'quantified-self)
(desire 'css)

;; so we have both functions on one key

(defun my-files ()
  "Show what files have been accessed.
Double key action accessing the commands:
 - `recentf-open-files'
 - `fj-show'"
  (interactive)
  (cond ((string-equal (buffer-name) "*file-journal*")
         (recentf-open-files))
        ((eq last-command 'my-files)
         (fj-show))
        (t
         (recentf-open-files))))

(global-set-key [f3] 'my-files)

;; so we can deal with differences between versions
(desire (make-symbol (format "holidays-%d" emacs-major-version)))

(desire 'auto-template)
(desire 'emacs-lisp)
(desire 'sh)
(desire 'skeletons)

(desire 'indent)
(desire 'init-dist)

(desire 'setnu)
(desire 'buffer-time-stamp)
(desire 'isearch-extra)
(desire 'uptimes)
(desire 'mail)

(autoload 'hide-lines "hide-lines"
  "Hide lines that match the specified regexp." t)
(autoload 'hide-matching-lines "hide-lines"
  "Hide lines that match the specified regexp." t)
(autoload 'hide-non-matching-lines "hide-lines"
  "Hide lines that don't match the specified regexp." t)

(autoload 'edit-env "edit-env"
  "Display, edit, delete and add environment variables." t)

;; these items are meant for UNIX systems

(if (not (eq system-type 'windows-nt))
    (progn
      (desire 'man)
      (desire 'comint)
      (desire 'systeminfo)

      ;; so i can access emacs from other programs (i really only need
      ;; this at certain times)

      (server-start)
      ))

(if (eq system-type 'windows-nt)
    (progn
      (desire 'ntcomint)
      ))

;; allow site-specific stuff override what we have already loaded

(let (filelist
      tmp
      last
      complete
      site-specific)
  (dolist (element (nreverse (split-string (system-name) "\\.")))
    (if last
        (setq tmp (concat element "." last))
      (setq tmp element))
    (setq last tmp
          filelist (cons tmp filelist)))
  (setq filelist (nreverse filelist))

  (message "Loading site specific file")
  (while (and filelist (not complete))
    (setq site-specific
          (expand-file-name
           (concat "~" init-file-user "/.emacs.d/elisp/site-lisp/"
                   (car filelist) ".el")))
    (message "Testing for %s" site-specific)
    (if (and (file-exists-p site-specific)
             (condition-case err
                 (load site-specific t)
               (error
                (message "problem loading file %s:" site-specific)
                (message "%s" err))))
        (setq complete t))
    (setq filelist (cdr filelist)))

  (if (not complete)
      (progn
        (message "Did not find initialization for \"%s\"" site-specific)

        ;; provide a template for insertion

        (save-excursion
          (set-buffer (find-file-noselect site-specific))
          (basic-save-buffer))
        )))

(put 'dired-find-alternate-file 'disabled nil)

(when (require 'time-date nil t)
  (message "Emacs start-up time: %d seconds." (time-to-seconds (time-since emacs-load-start-time))))
