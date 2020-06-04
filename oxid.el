(require 'comint)
(require 'f)

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

(defun oxid-activate-module()
  (interactive)
  (oxid-run-command "vendor/bin/oe-console oe:module:install-configuration source/modules/oxps/order-consent-fields" "*OXID Command Output*"))

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
  (let ((mydir (concat (projectile-project-root) "oxideshop/source/Application/views/") ))
    (mapcar 'f-filename (f-directories mydir))))

(setq oxid-theme-helm-source
      '((name . "Themes in OXID")
        (candidates . oxid-list-themes)
        (action . (lambda (candidate)
                    (setq oxid-current-theme candidate)))))

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
      (projectile-project-root)
    (concat
     (projectile-project-root) "oxideshop")))

(defun oxid-run-grunt ()
  "run grunt for the theme"
  (interactive)
  (helm :sources '(oxid-theme-helm-source))
  (cd (concat (projectile-project-root) "/oxideshop/source/Application/views/" oxid-current-theme))
  (make-comint-in-buffer "Grunt" "*Grunt*" "grunt")
  (message "Grunt is started."))

(defvar oxid-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'oxid-activate-module)
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
