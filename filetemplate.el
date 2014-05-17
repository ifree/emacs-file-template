;;; filetemplate.el --- Auto insert template when create new file

;;; Commentary:
;; Utils for quick add template when create new file
;;; examples
;; direct call
;; `file-template-find-template`
;; if you want to auto load template, call
;; file-template-toggle-auto-load

;;; Code:
(require 'cl)
(defgroup filetemplate nil
  "emacs file template"
  :version "1.0"
  :prefix "file-template"
  :group 'tools
)

(defcustom file-template-dir
  (concat (file-name-directory load-file-name) "templates")
  "Directory that stores file type template.")

(defcustom file-template-ext "\\.tpl"
  "Template file extension.")

(defcustom file-template-prompt 'y-or-n-p
  "Prompt use when about to insert template.")

(defcustom file-template-enabled nil
  "Enable file template or not")

;; From http://www.emacswiki.org/emacs/ElispCookbook#toc58
(defun directory-dirs (dir)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".." ".git"))
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file
                                     (directory-dirs file))
                               dirs))))))
    dirs))

(defun directory-files-rec (dir &optional pattern)
  "find all files in dir"
  (let ((dirs (directory-dirs dir))
	(files (remove-if  (lambda (x) 
                             (or 
                              (file-directory-p x)
                              (string-match "\\.$" x)))
                           (directory-files dir t))))
    (dolist (subdir dirs)
      (setq files (append files 
                          (delq nil (mapcar
                            (lambda (f)
                              (unless (or
                                       (file-directory-p (concat subdir "/" f))
                                       (not (string-match (or pattern ".*") f))
                                       (member f '("." "..")))
                                (concat subdir "/" f))
                              )
                            (directory-files subdir))))))
    files))

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
  (when (funcall file-template-prompt "do you want to apply this template?")
   (if (file-directory-p template)
       (file-template-process-templateEx template)
     (yas-expand-snippet (file-template-file-to-string template))))
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
  (add-to-ordered-list 'find-file-not-found-functions 'file-template-new-file-hook)  
)

(provide 'filetemplate)

;;; filetemplate.el ends here
