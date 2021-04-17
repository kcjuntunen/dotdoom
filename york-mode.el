;;; $DOOMDIR/york-mode.el -*- lexical-binding: t; -*-

(defvar york-request-looker-upper-path
  "D:/Source/Repos/Viewer.Etc/Experimental/bin/Debug/Experimental.exe"
  ;; "D:/Source/C#-Production/Viewer/Experimental/bin/Debug/Experimental.exe"
  "The program that pulls in Projects/Phases/Tasks in Org format.")

(defvar york-remote-repo-path
  "G:/"
  "The location of remote git repos.")

(defvar york-local-repo-path
  "D:/Source/Repos/"
  "The location of local git repos.")

(defvar york--repo-name-property-name
  "REPO"
  "The property under which we store and retrieve repo names.")

;;

(defun york-store-repo-name (repo-name)
  "Store repo-name under the property name in `repo-name-property-name'."
  (interactive "sRepo Name: ")
  (org-entry-put (point) york--repo-name-property-name repo-name))

(defun york-get-request-data (request-number)
  "Insert Project/Phase/Task data into buffer in Org format."
  (interactive "sRequestNbr: ")
  (insert (shell-command-to-string
           (format "%s %s"
                   york-request-looker-upper-path request-number))))

(defun york--get-local-repo-name ()
  "Get the local repo for the associated code."
  (let ((repo-name (org-entry-get (point) york--repo-name-property-name)))
    (concat york-local-repo-path repo-name "/" repo-name ".sln")))

(defun york--get-remote-repo-name ()
  "Get the remote repo for the associated code."
  (let ((repo-name (org-entry-get (point) york--repo-name-property-name)))
    (concat york-remote-repo-path repo-name ".git")))

(defun york-open-local-repo-name ()
  "Open thing in the property named `york--repo-name-property-name'."
  (interactive)
  (let ((thing-to-open (york--get-local-repo-name)))
    (org-open-file thing-to-open nil)))

(defun york-open-remote-repo-name ()
  "Open thing in the property named `york--repo-name-property-name'."
  (interactive)
   (let ((thing-to-open (york--get-remote-repo-name)))
     (org-open-file thing-to-open)))

;; Bindings

(map! :leader
      (:prefix-map ("y" . "York")
       (:prefix ("r" . "Requests")
        :desc "Insert Request Data at point" "g" #'york-get-request-data)))

(provide 'york-mode)
