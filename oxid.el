;;; oxid.el --- Oxid eCommerce framework support

;;; Commentary:

(require 'comint)
(require 'f)
(require 'dash)
(require 'json)

;;; Customization

(defgroup oxid nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/mprokopov/oxid.el")
  :link '(emacs-commentary-link :tag "Commentary" "oxid"))

(defvar oxid-use-docker t
  "OXID is dockerized and uses `docker-compose'.")

(defvar oxid-project-repo-url ""
  "OXID is dockerized and uses `docker-compose'.")

(defvar oxid-project-jira-url ""
  "Oxid project jira url.")

(defvar oxid-project-confluence-url ""
  "Oxid project confluence url.")

(defvar oxid-project-staging-url ""
  "Oxid project staging url.")

(defvar oxid-project-local-server-url ""
  "Oxid project local server url.")

(defvar oxid-current-theme ""
  "Current oxid project theme.")

(defvar oxid-current-module ""
  "Current oxid project theme.")

(defvar oxid-auto-load-env-vars t
  "Current oxid project theme.")

;;; Code:
(defun oxid-touch-module (action)
  (helm :sources '(oxid-modules-helm-source)
        :input oxid-current-module)
  (let ((module-action (concat "oe:module:" action " " oxid-current-module)))
    (when oxid-current-module
      (oxid-oe-console-command module-action))))


;; helm dependent things
(defvar oxid-modules-helm
  (helm-build-sync-source "Modules in OXID project"
    :candidates #'oxid-list-modules
    :action (lambda (candidate)
              (oxid-module-activate candidate))))

(defvar oxid-modules-helm-deactivate
  (helm-build-sync-source "Modules in OXID project"
    :candidates #'oxid-list-modules
    :action (lambda (candidate)
              (oxid-module-deactivate candidate))))

(defun oxid-helm-activate-module ()
  "lists and activates oxid module"
  (interactive)
  (helm :sources '(oxid-modules-helm-activate)
        :input oxid-current-module))

(defun oxid-helm-deactivate-module ()
  "lists and deactivates oxid module"
  (interactive)
  (helm :sources '(oxid-modules-helm-deactivate)
        :input oxid-current-module))

(setq oxid-theme-helm-source
      '((name . "Themes in OXID")
        (candidates . oxid-list-themes)
        (action . (lambda (candidate)
                    (setq oxid-current-theme candidate)))))

(defvar oxid-modules-helm-source
  (helm-build-sync-source "Modules in OXID project"
                          :candidates #'oxid-list-modules
                          :action (lambda (candidate)
                                    (setq oxid-current-module candidate))))

(defun oxid-select-configuration ()
  (interactive)
  (when oxid-auto-load-env-vars
    (oxid-load-env-vars))
  (helm :sources (helm-build-sync-source "Oxid Environments"
                                         :candidates (get-environment-files)
                                         :action (lambda (config)
                                                   (oxid-load-configuration config)))))

(defun oxid-run-grunt ()
  "Run grunt for the theme."
  (interactive)
  (helm :sources '(oxid-theme-helm-source))
  (cd (concat (oxid-project-dir) "/source/Application/views/" oxid-current-theme))
  (make-comint-in-buffer "Grunt" "*Grunt*" "grunt")
  (message "Grunt is started."))
;; ===

(defun oxid-module-activate (name)
  (let ((module-action (concat "oe:module:activate " name)))
    (oxid-oe-console-command module-action)))

(defun oxid-module-deactivate (name)
  (let ((module-action (concat "oe:module:deactivate " name)))
    (oxid-oe-console-command module-action)))

;; ## Browse functions
(defun oxid-project-browse-repo ()
  (interactive)
  (browse-url oxid-project-repo-url))

(defun oxid-project-browse-local-server ()
  (interactive)
  (browse-url oxid-project-local-server-url))

(defun oxid-project-browse-local-server-admin ()
  (interactive)
  (browse-url (concat oxid-project-local-server-url "/admin/")))

(defun oxid-project-browse-jira ()
  (interactive)
  (browse-url oxid-project-jira-url))

(defun oxid-project-browse-confluence ()
  (interactive)
  (browse-url oxid-project-confluence-url))

(defun oxid-project-browse-staging ()
  (interactive)
  (browse-url oxid-project-staging-url))

(defun oxid-open-shop-log ()
  (interactive)
  (switch-to-buffer
   (find-file-noselect (concat (oxid-project-dir) "/source/log/oxideshop.log"))))

(defun oxid-open-var-folder ()
  (interactive)
  (switch-to-buffer
   (dired (concat (oxid-project-dir) "/var"))))

(defun get-environment-files ()
  (let ((config-dir (concat (oxid-project-dir) "/var/configuration/environment/")))
    (mapcar #'(lambda (f) (s-chop-suffix ".1.yaml" (f-filename f)))
            (f-entries config-dir (lambda (file) (s-matches? ".1.yaml" file))))))


(defun oxid-load-configuration (configuration)
  (interactive)
  ;; (oxid-load-env-vars)
  (let* ((env-config-file (concat (oxid-project-dir) "/var/configuration/environment/" configuration ".1.yaml"))
         (target-config-file (concat (oxid-project-dir) "/var/configuration/environment/1.yaml")))
    (f-delete target-config-file t)
    (f-copy env-config-file target-config-file)
    (oxid-oe-console-command "oe:module:apply-configuration")))

(defun oxid-load-env-vars ()
  "load environment vars from the project root"
  (interactive)
  (load-env-vars (concat (projectile-project-root) ".env")))

(defun oxid-load-testing-env-vars ()
  "load environment vars from the project root"
  (interactive)
  (load-env-vars (concat (projectile-project-root) ".env.test")))

(defun load-env-vars (filename)
  "sets vars like DB-HOST from file"
  (mapcar (lambda (a)
            (let ((kv (split-string a "=")))
              (setenv (car kv) (cadr kv))))
          (with-temp-buffer
            (insert-file-contents filename)
            (split-string (buffer-string) "\n" t)))
  (message "environment vars has been loaded from %s" filename))

(defun oxid-run-command (command buf)
  (set-buffer (get-buffer-create buf))

  (cd (oxid-project-dir))
  (let ((cmd (oxid-cmd2 command)))
    (insert
     (shell-command-to-string cmd)))
  (switch-to-buffer buf))


(defun oxid-run-command-async (command buf)
  (cd (oxid-project-dir))
  (let ((cmd (oxid-cmd2 command)))
    (message "oxid: %s"
     (shell-command-to-string cmd))))

(defun oxid-oe-console-command (command)
  ;; (oxid-load-env-vars)
  (oxid-run-command-async
   (concat "vendor/bin/oe-console " command)
   "*OXID Command Output*"))

(defun oxid-oe-console-outdated-command (command)
  (oxid-run-command
   (concat "vendor/bin/oxid " command)
   "*OXID Command Output*"))

(defun oxid-list-themes ()
  ;; (interactive)
  (let ((mydir (concat (oxid-project-dir) "/source/Application/views/") ))
    (mapcar 'f-filename (f-directories mydir))))

(defun oxid-list-modules ()
  (with-helm-current-buffer
    (split-string
                                        ; TODO: use relative location
     (shell-command-to-string (concat "~/.emacs.d/private/oxid/local/oxid/module-autocomplete.clj " (oxid-project-dir))))))

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
  (oxid-oe-console-command "oe:module:apply-configuration"))

(defun oxid-db-migrate()
  "runs database migrations"
  (interactive)
  (cd (oxid-project-dir))
  ;; (oxid-load-env-vars)
  (switch-to-buffer
   (set-buffer (get-buffer-create "*OXID Migration Output*")))
  (insert (shell-command-to-string
           (oxid-cmd2 "vendor/bin/oe-eshop-db_migrate migrations:migrate"))))

(defun oxid-project-dir ()
  "if docker - upper lever with docker-compose, otherwise in oxideshop"
  (if (oxid-has-oxideshop-dir)
      (concat
       (projectile-project-root) "oxideshop")
    (projectile-project-root)))


(defun oxid-clear-cache ()
  "clears oxid cache"
  (interactive)
  (if (oxid-has-legacy-command)
      (oxid-oe-console-outdated-command "cache:clear")
    (oxid-clear-tmp-folder)))

(defun oxid-clear-tmp-folder ()
  "oldschool way to cleanup cache"
  (f-entries (concat (oxid-project-dir) "/source/tmp")
             (lambda (f)
               (f-delete f t)))
  (message "source/tmp has been cleaned."))

(defun oxid-has-oxideshop-dir ()
  (let ((path (concat (projectile-project-root) "/oxideshop")))
    (f-exists? path)))

(defun oxid-has-legacy-command ()
  (let ((path (concat (oxid-project-dir) "/vendor/bin/oxid")))
    (f-exists? path)))

(defun oxid-fix-modules ()
  (interactive)
  (oxid-oe-console-outdated-command "module:fix -a"))

(defvar oxid-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'oxid-helm-activate-module)
    (define-key map (kbd "L") #'oxid-helm-deactivate-module)
    (define-key map (kbd "g") #'oxid-run-grunt)
    (define-key map (kbd "C") #'oxid-clear-cache)
    (define-key map (kbd "d") #'oxid-db-migrate)
    (define-key map (kbd "v") #'oxid-open-var-folder)
    (define-key map (kbd "o") #'oxid-open-shop-log)
    (define-key map (kbd "c") #'oxid-select-configuration)
    (define-key map (kbd "m") #'oxid-modules-list)
    (define-key map (kbd "b c") #'oxid-project-browse-confluence)
    (define-key map (kbd "b r") #'oxid-project-browse-repo)
    (define-key map (kbd "b j") #'oxid-project-browse-jira)
    (define-key map (kbd "b l") #'oxid-project-browse-local-server)
    (define-key map (kbd "b a") #'oxid-project-browse-local-server-admin)
    (define-key map (kbd "b s") #'oxid-project-browse-staging)
    map)
  "Keymap for Oxid commands")

(fset 'oxid-command-map oxid-command-map)

(defvar oxid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") 'oxid-command-map)
    map)
  "Keymap for OXID mode.")

(defun oxid-composer ()
  (interactive)
  (cd (oxid-project-dir))
  (switch-to-buffer
   (set-buffer (get-buffer-create "*OXID Composer*")))
  (composer-list-mode)
  (tablist-revert))

(defun oxid-modules-list ()
  (interactive)
  (when oxid-auto-load-env-vars
    (oxid-load-env-vars))
  (cd (oxid-project-dir))
  (switch-to-buffer
   (set-buffer (get-buffer-create "*OXID Modules*")))
  (oxid-modules-list-mode)
  (tablist-revert))

(defun indexed-modules-vector (f idx)
  "utility function creates list required by oxid modules list mode"
  (list idx (vector
             (cdr (assoc 'id f))
             (cdr (assoc 'version f))
             (if (eq :json-false
                     (cdr (assoc 'configured f)))
                 "No" "Yes")
             (or
              (cdr (assoc 'en (assoc 'title f))) ""))))

(defun modules-json-list ()
  "reads modules list from the json output and returns elisp structures"
  (json-read-from-string
   (shell-command-to-string (concat "~/.emacs.d/private/oxid/local/oxid/ac-oxid.clj " (oxid-project-dir)))))

(define-derived-mode oxid-modules-list-mode tabulated-list-mode "oxid modules list mode"
  (setq tabulated-list-format [("Name" 49 t)
                               ("Version" 10 t)
                               ("Active" 6 t)
                               ("Title" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries
        (seq-map-indexed #'indexed-modules-vector (modules-json-list)))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'oxid-modules-refresh nil t)
  (tablist-minor-mode)
  ;; (tabulated-list-print t)
  )

(defun oxid-modules-refresh ()
  "refreshes modules list"
  (interactive)
  (setq tabulated-list-entries
        (seq-map-indexed #'indexed-modules-vector (modules-json-list))))

(define-derived-mode composer-list-mode tabulated-list-mode "composer dependencies list mode"
  (setq tabulated-list-format [("Name" 49 t)
                               ("Version" 15 nil)
                               ("Comment" 0 nil)
                               ]);; last columnt takes what left

  ;; TEXT output based implementation
  ;; (setq tabulated-list-entries (seq-map-indexed #'(lambda (f idx)
  ;;                                          (list idx
  ;;                                                (vector
  ;;                                                 (s-trim (substring f 0 49))
  ;;                                                 (s-trim (substring f 50 60))
  ;;                                                 (substring f 70))))
  ;;                                               (butlast (s-lines (shell-command-to-string "/usr/local/bin/composer info")))))

  ;; JSON output based implementation
  (setq tabulated-list-entries (seq-map-indexed #'(lambda (f idx)
                                                    (list idx
                                                          (vector
                                                           (cdar f)
                                                           (cdadr f)
                                                           (or (cdaddr f) ""))))
                                                (cdar
                                                 (json-read-from-string
                                                  (shell-command-to-string "/usr/local/bin/composer info -f json")))))

  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun oxid-activate-marked-modules ()
  (interactive)
  (when oxid-auto-load-env-vars
    (oxid-load-env-vars))
  (--map (let ((name (aref (cdr it) 0)))
           (oxid-module-activate name))
         (tablist-get-marked-items))
  (tablist-revert))

(defun oxid-deactivate-marked-modules ()
  (interactive)
  (when oxid-auto-load-env-vars
    (oxid-load-env-vars))
  (--map (let ((name (aref (cdr it) 0)))
           (oxid-module-deactivate name))
         (tablist-get-marked-items))
  (tablist-revert))

(defvar oxid-modules-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "A" #'oxid-activate-marked-modules)
    (define-key map "O" #'oxid-deactivate-marked-modules)
    (define-key map "r" #'tablist-revert)
    (define-key map "F" #'oxid-fix-modules)
    map)
  "Keymap for `oxid-modules-list-mode'.")

;;;###autoload
(define-minor-mode oxid-mode
  "Provides keybindings for working with oxid projects"
  :group 'oxid
  :require 'oxid
  :global t
  ;; (evil-define-key )
  :keymap oxid-mode-map)

(projectile-register-project-type 'oxid '("oxideshop")
                                  :project-file "oxideshop/source/config.inc.php"
				                          :test "vendor/bin/phpunit"
				                          :test-suffix "Test")

(projectile-register-project-type 'oxid-module '("composer.json" "metadata.php")
                                  :project-file "composer.json"
				                          :test "vendor/bin/phpunit"
				                          :test-suffix "Test")

(provide 'oxid)

(provide 'oxid)

;;; oxid.el ends here
