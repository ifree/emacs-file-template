;;; filetemplate.el --- Auto insert template when create new file

;;; Commentary:
;; Utils for quick add template when create new file or create project
;; using template 
;;; examples
;; `file-template-load-file` to apply template to current file
;; `file-template-load` to load template
;; 
;;; Code:
(require 'cl)

(defgroup filetemplate nil
  "emacs file template"
  :version "1.1"
  :prefix "file-template"
  :group 'tools
  )

(defcustom file-template-dir
  (concat (file-name-directory
           (or load-file-name
               (expand-file-name
                "~/.emacs.d/site-lisp/file-template/filetemplate.el"
                ))) "templates")  
  "Directory that stores file type template.")

(defcustom file-template-ext "\\.tpl"
  "Template file extension.")

(defcustom file-template-prompt 'y-or-n-p
  "Prompt use when about to insert template.")

(defcustom file-template-enabled nil
  "Enable file template or not.")

(defvar file-template-resolver-func (lambda (c)
                                      (yas-minor-mode-on)
                                      (yas-expand-snippet c))
  "Template resolver, current use `yas-expand-snippet`.")

(defvar file-template-ignore-list '("." ".." ".git" ".svn")
  "You can override it in .dir-locals.el.")

(defun file-template-choose-template (&optional mode-name)
  "Choose template by MODE-NAME."
  (unless mode-name
    (let ((symbol-name (assoc-default (buffer-file-name) auto-mode-alist 'string-match)))
      (setq mode-name
            symbol-name)))
 
  (when (characterp mode-name)
    (let* ((template-dir (concat file-template-dir "/"  mode-name "/"))
        (template 
         (if (file-directory-p template-dir)
             (read-file-name "select template " template-dir nil t nil (lambda (x) (string-match "\.tpl$" x)))
           nil)))
    template)))

(defun file-template-file-to-string (file)
  "Read FILE content to string."
  (with-temp-buffer
    (insert-file file)
    (buffer-string)))


(defun file-template-resolver (content file &optional dir-mode target-dir)
  "Open FILE then parse CONTENT, if in DIR-MODE create file in TARGET-DIR first."
  (when file-template-resolver-func
    (if dir-mode
        (let ((file-template-enabled nil)) ;inhibit promot when dir-mode
          (find-file
           (concat
            target-dir
            (replace-regexp-in-string
             (concat
              (expand-file-name file-template-dir)
              "/[^/]+/[^/]+/"              ;/mode/xxx.tpl
              ) ""
                (expand-file-name file)))))
      (switch-to-buffer (buffer-name)))
    (let ((func (if (string-match (concat file-template-ext "$") file)
                    file-template-resolver-func
                  'insert)))
      (if (and file-template-prompt (not dir-mode)) ;inhibit promot when dir mode
        (when (funcall file-template-prompt "apply this template? ")
          (funcall func content))
      (funcall func content))
      (save-buffer))))


(defun file-template-apply-to (file &optional allow-dir target-dir)
  "Apply template to a FILE. if ALLOW-DIR will apply directory based template."
  (interactive)
  (when file
    (if (and (file-directory-p file) allow-dir)
        (progn
          (dolist (f (directory-files file))
            (when (not (member f file-template-ignore-list))
              (file-template-apply-to (concat file "/" f) allow-dir target-dir)
              )
            )
          )
      (funcall
       'file-template-resolver
       (file-template-file-to-string file)
       file
       allow-dir target-dir))))


(defun file-template-load (&optional template-file-or-dir target-dir)
  "Load a template from TEMPLATE-DIR."
  (interactive)
  (unless template-file-or-dir
    (let* ((tpl-type
            (completing-read
             "select template type "
             (directory-files file-template-dir)
             '(lambda (x) (not (member x file-template-ignore-list)))
             t
             ))
           (tpl
            (completing-read
             "select template "
             (directory-files (concat file-template-dir "/" tpl-type))
             '(lambda (x) (not (member x file-template-ignore-list)))
             t 
             )
            )
           )
      (setq template-file-or-dir
            (concat file-template-dir "/" tpl-type "/" tpl))))
  (when (and (file-directory-p template-file-or-dir) (not target-dir))
    (setq target-dir
          (read-directory-name
           "please set target directory to evaluate the template. ")))
  (file-template-apply-to
   template-file-or-dir
   (file-directory-p template-file-or-dir)
   target-dir))

(defun file-template-load-file (&optional tpl-file)
  "Load a TEMPLATE-FILE."
  (interactive)
  (yas-minor-mode-on)
  (unless tpl-file
    (setq tpl-file (file-template-choose-template))
    )
  (file-template-apply-to  tpl-file))

(defun file-template-new-file-hook ()
  (when file-template-enabled
    (file-template-load-file)))

(defun file-template-setup ()
  "Initial setup"
  (add-to-list 'find-file-not-found-functions 'file-template-new-file-hook t))

(provide 'filetemplate)

;;; filetemplate.el ends here
