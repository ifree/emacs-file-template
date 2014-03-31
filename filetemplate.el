;;; filetemplate.el --- Auto insert template when create new file

;;; Commentary:
;; Utils for quick add template when create new file
;;; examples
;; direct call
;; `file-template-find-template`
;; if you want to auto load template, call
;; file-template-toggle-auto-load

;;; Code:
(defgroup filetemplate nil
  "emacs file template"
  :version "1.0"
  :prefix "file-template"
  :group 'tools
)


(defcustom file-template-dir "~/.emacs.d/filetemplate"
  "Directory that stores file type template.")

(defcustom file-template-ext "\\.tpl"
  "Template file extension.")

(defcustom file-template-auto-insert nil
  "Auto insert template when create new file.")

(defcustom file-template-enabled nil
  "Enable file template or not")


(defun file-template-file-to-string (file)
  "Read FILE content to string."
  (with-temp-buffer
    (insert-file file)
    (buffer-string)))


(defun file-template-load-template (&optional mode-name)
  "Load templte by MODE-NAME."
  (interactive)
  (unless mode-name
      (set-auto-mode)
      (setq mode-name (symbol-name major-mode)))
  (let* (
        (template-dir (concat file-template-dir "/"  mode-name "/"))
        (template 
         (if (file-directory-p template-dir)
             (read-file-name "select template " template-dir nil t nil (lambda (x) (string-match "\.tpl$" x)))
           nil
           )
        ))    
    (if template
        (file-template-process-template template)
      (message "template for current major mode does not exists."
      ))))


(defun file-template-find-template (template-dir)
  (interactive 
   (list 
    (read-directory-name "choose template base directory: " file-template-dir)))
  (let (
        (template 
         ;; TODO user can only choose file
         (yas-completing-prompt
          "choose template"
          (directory-files template-dir t file-template-ext) #'file-name-base)))
    (if template
        (file-template-process-template template)
      (message "template for current major mode does not exists."))))


(defun file-template-process-template (template)
  "Process the TEMPLATE."
  (if (file-directory-p template)
      (file-template-process-templateEx template)
    (yas-expand-snippet (file-template-file-to-string template)))
  )

(defun file-template-process-templateEx (template-dir)
  "Process special template which is a directory contains a set of folder."
  (let ((target-dir (read-directory-name "choose target directory: ")))
    (dolist (file (directory-files-rec template-dir))
      (let* ((target-file 
              (concat 
               target-dir 
               (replace-regexp-in-string (concat "^" (regexp-quote template-dir)) "" file)))
             (target-file-parent (file-name-directory target-file)))
        (unless (file-directory-p target-file-parent)
          (make-directory target-file-parent t))
        (with-temp-file target-file
          (yas-expand-snippet (file-template-file-to-string file))
          )
))))


(defun file-template-new-file-hook ()
  (when file-template-enabled
    (file-template-load-template))
)

(defun file-template-setup ()
  "Initial setup"
  (add-hook 'find-file-not-found-hooks 'file-template-new-file-hook)  
)

(provide 'filetemplate)

;;; filetemplate.el ends here
