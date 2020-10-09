(require 'comint)
(require 'f)
(require 'dash)
(require 'helm)
(require 'json)

;;; TODO:
;;; browse project staging
;;; browse project dev
;;; browse project repo
;;; project confluence
;;; project jira

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

(defvar oxid-project-repo-url "https://psgit.oxid-esales.com/psprojects/edeka_eccg/-/merge_requests"
  "OXID is dockerized and uses docker-compose"
  :group 'oxid
  :type 'string)

(defvar oxid-project-jira-url "https://oxid-esales.atlassian.net/secure/RapidBoard.jspa?rapidView=232&projectKey=PECG"
  "oxid project jira url"
  :group 'oxid
  :type 'string)

(defvar oxid-project-confluence-url "https://oxid-esales.atlassian.net/secure/RapidBoard.jspa?rapidView=232&projectKey=PECG"
  "oxid project confluence url"
  :group 'oxid
  :type 'string)

(defvar oxid-current-theme "risch"
  "Current oxid project theme"
  :group 'oxid
  :type 'string)

(defvar oxid-current-module ""
  "Current oxid project theme"
  :group 'oxid
  :type 'string)

;; (defun oxid-touch-module (action)
;;   (helm :sources '(oxid-modules-helm-source)
;;         :input oxid-current-module)
;;   (let ((module-action (concat "oe:module:" action " " oxid-current-module)))
;;     (when oxid-current-module
;;       (oxid-oe-console-command module-action))))

(defvar oxid-modules-helm-activate
  (helm-build-sync-source "Modules in OXID project"
    :candidates #'oxid-list-modules
    :action (lambda (candidate)
              (let ((module-action (concat "oe:module:activate " candidate)))
                (setq oxid-current-module candidate)
                (oxid-oe-console-command module-action)))))

(defvar oxid-modules-helm-deactivate
  (helm-build-sync-source "Modules in OXID project"
    :candidates #'oxid-list-modules
    :action (lambda (candidate)
              (let ((module-action (concat "oe:module:deactivate " candidate)))
                (setq oxid-current-module candidate)
                (oxid-oe-console-command module-action)))))

(defun oxid-project-browse-repo ()
  (interactive)
  (browse-url oxid-project-repo-url))

(defun oxid-project-browse-jira ()
  (interactive)
  (browse-url oxid-project-jira-url))

(defun oxid-project-browse-confluence ()
  (interactive)
  (browse-url oxid-project-confluence-url))

(defun oxid-activate-module ()
  (interactive)
  (helm :sources '(oxid-modules-helm-activate)
        :input oxid-current-module))

(defun oxid-deactivate-module ()
  (interactive)
  (helm :sources '(oxid-modules-helm-deactivate)
        :input oxid-current-module))

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

(defun oxid-select-configuration ()
  (interactive)
  (helm :sources (helm-build-sync-source "Oxid Environments"
                   :candidates (get-environment-files)
                   :action (lambda (config)
                             (oxid-load-configuration config)))))

(defun oxid-load-configuration (configuration)
  (interactive)
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

(defun oxid-oe-console-command (command)
  (oxid-run-command
   (concat "vendor/bin/oe-console " command)
   "*OXID Command Output*"))

(defun oxid-oe-console-outdated-command (command)
  (oxid-run-command
   (concat "vendor/bin/oxid " command)
   "*OXID Command Output*"))

(defun oxid-list-themes ()
  ;; (interactive)
  (let ((mydir (concat (oxid-project-dir) "source/Application/views/") ))
    (mapcar 'f-filename (f-directories mydir))))

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

(defun oxid-list-modules ()
  (with-helm-current-buffer
    (split-string
                                        ; TODO: use relative location
     (shell-command-to-string (concat "~/.emacs.d/private/local/oxid/module-autocomplete.clj " (oxid-project-dir))))))

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

(defun oxid-run-grunt ()
  "run grunt for the theme"
  (interactive)
  (helm :sources '(oxid-theme-helm-source))
  (cd (concat (oxid-project-dir) "/source/Application/views/" oxid-current-theme))
  (make-comint-in-buffer "Grunt" "*Grunt*" "grunt")
  (message "Grunt is started."))

(defun oxid-clear-cache ()
  "clears oxid cache"
  (interactive)
  (oxid-oe-console-outdated-command "cache:clear"))

(defun oxid-has-oxideshop-dir ()
  (let ((path (concat (projectile-project-root) "/oxideshop")))
    (f-exists? path)))

(defun oxid-fix-modules ()
  (interactive)
  (oxid-oe-console-outdated-command "module:fix -a"))

(defvar oxid-browse-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'oxid-project-browse-confluence)
    (define-key map (kbd "r") #'oxid-project-browse-repo)
    (define-key map (kbd "j") #'oxid-project-browse-jira)
    map))

(defvar oxid-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'oxid-activate-module)
    (define-key map (kbd "L") #'oxid-deactivate-module)
    (define-key map (kbd "g") #'oxid-run-grunt)
    (define-key map (kbd "C") #'oxid-clear-cache)
    (define-key map (kbd "d") #'oxid-db-migrate)
    (define-key map (kbd "v") #'oxid-open-var-folder)
    (define-key map (kbd "o") #'oxid-open-shop-log)
    (define-key map (kbd "c") #'oxid-select-configuration)
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
  (composer-list-mode))

(define-derived-mode composer-list-mode tabulated-list-mode "list-demo-mode"
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
  (tablist-minor-mode)
  (tabulated-list-print t))

;;;###autoload
(define-minor-mode oxid-mode
  "Provides keybindings for working with oxid projects"
  :group 'oxid
  :require 'oxid
  :global t
  ;; (evil-define-key )
  :keymap oxid-mode-map)

;; (projectile-register-project-type 'oxid '("oxideshop")
;;                                   :project-file "oxideshop/source/config.inc.php"
;; 				                          :test "vendor/bin/phpunit"
;; 				                          :test-suffix "_test")

(provide 'oxid)
