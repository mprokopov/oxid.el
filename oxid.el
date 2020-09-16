(require 'comint)
(require 'f)
(require 'dash)

;;; Customization
(defgroup oxid nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/mprokopov/oxid.el")
  :link '(emacs-commentary-link :tag "Commentary" "oxid"))

(defcustom oxid-use-docker t
  "OXID is dockerized and uses docker-compose"
  :group 'oxid
  :type 'boolean)

(defcustom oxid-current-theme "risch"
  "Current oxid project theme"
  :group 'oxid
  :type 'string)

(defcustom oxid-current-module ""
  "Current oxid project theme"
  :group 'oxid
  :type 'string)

(defun oxid-touch-module (action)
  (helm :sources '(oxid-modules-helm-source))
  (let* ((module-action (concat "oe:module:" action))
         (command (concat (oxid-project-dir) "vendor/bin/oe-console " module-action " " oxid-current-module)))
    (oxid-run-command command "*OXID Command Output*")))

(defun oxid-activate-module ()
  (interactive)
  (oxid-touch-module "activate"))

(defun oxid-deactivate-module ()
  (interactive)
  (oxid-touch-module "deactivate"))

;; TODO
;; execute composer show
(defun oxid-get-composer-modules ()
  (interactive)
  (cd (oxid-project-dir)))
;; TODO
(defun oxid-open-shop-log ()
  (interactive)
  (cd (oxid-project-dir))

  )

(defun oxid-load-env-vars ()
  "load environment vars from the project root"
  (interactive)
  (load-env-vars (concat (projectile-project-root) ".db-env")))

(defun load-env-vars (filename)
  "sets vars like DB-HOST from file"
  (mapcar (lambda (a)
            (let ((kv (split-string a "=")))
              (setenv (car kv) (cadr kv))))
          (with-temp-buffer
            (insert-file-contents filename)
            (split-string (buffer-string) "\n" t)))
  )

(defun oxid-run-command (command buf)
  (set-buffer (get-buffer-create buf))

  (cd (oxid-project-dir))
  (let ((cmd (oxid-cmd2 command)))
    (insert
     (shell-command-to-string cmd)))
  (switch-to-buffer buf))

(defun oxid-list-themes ()
  ;; (interactive)
  (let ((mydir (concat (oxid-project-dir) "source/Application/views/") ))
    (mapcar 'f-filename (f-directories mydir))))

(setq oxid-theme-helm-source
      '((name . "Themes in OXID")
        (candidates . oxid-list-themes)
        (action . (lambda (candidate)
                    (setq oxid-current-theme candidate)))))

(setq oxid-modules-helm-source
      '((name . "Modules in oxid project")
       (candidates . oxid-list-modules)
       (action . (lambda (candidate)
                   (setq oxid-current-module candidate)))))

(defun oxid-list-modules ()
  (split-string 
   ; TODO: use relative location
   (shell-command-to-string (concat "~/.emacs.d/private/local/oxid/module-autocomplete.clj " (oxid-project-dir)))))

(defun oxid-cmd ()
  "if dockerized returns docker-compose command, otherwise blank"
  (when oxid-use-docker "docker-compose exec -T php "))

(defun oxid-cmd2 (command)
  "if dockerized returns docker-compose command, otherwise blank"
  (if oxid-use-docker
      (concat "docker-compose exec -T php " command)
    command))

(defun oxid-apply-modules-configuration ()
  "docker-compose exec php vendor/bin/oe-console oe:module:apply-configuration"
  (interactive)
  (oxid-run-command "vendor/bin/oe-console oe:module:apply-configuration" "*OXID command output*"))

(defun oxid-db-migrate()
  "runs database migrations"
  (interactive)
  (cd (oxid-project-dir))
  (switch-to-buffer
   (set-buffer (get-buffer-create "*OXID Migration Output*")))
  (insert (shell-command-to-string
           (oxid-cmd2 "vendor/bin/oe-eshop-db_migrate migrations:migrate"))))

(defun oxid-project-dir ()
  "if docker - upper lever with docker-compose, otherwise in oxideshop"
  (if oxid-use-docker
      (concat
       (projectile-project-root) "oxideshop")
    (projectile-project-root)))

(defun oxid-run-grunt ()
  "run grunt for the theme"
  (interactive)
  (helm :sources '(oxid-theme-helm-source))
  (cd (concat (oxid-project-dir) "/source/Application/views/" oxid-current-theme))
  (make-comint-in-buffer "Grunt" "*Grunt*" "grunt")
  (message "Grunt is started."))

(defvar oxid-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'oxid-activate-module)
    (define-key map (kbd "L") #'oxid-deactivate-module)
    (define-key map (kbd "g") #'oxid-run-grunt)
    (define-key map (kbd "d") #'oxid-db-migrate)
    map)
  "Keymap for Oxid commands")

(fset 'oxid-command-map oxid-command-map)

(defvar oxid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") 'oxid-command-map)
    map)
  "Keymap for OXID mode.")

;;;###autoload
(define-minor-mode oxid-mode
  "Provides keybindings for working with oxid projects"
  :group 'oxid
  :require 'oxid
  :global t
  ;; (evil-define-key )
  :keymap oxid-mode-map)

(provide 'oxid)
