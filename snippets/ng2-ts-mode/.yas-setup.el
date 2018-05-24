;; Base functions

(defun buffer-path ()
  (or (buffer-file-name)
      (buffer-name)))

(defun angular-relative-path (full-path angular-dir)
  (file-relative-name full-path (concat angular-dir
                                        (file-name-as-directory "src")
                                        (file-name-as-directory "app"))))

(defun angular-component-name-for-buffer ()
  (let* ((dir (file-name-directory (buffer-path)))
         (basename (first (remove
                           ""
                           (split-string
                            (file-name-base
                             (buffer-path)) "\\.")))))
    (angular-relative-path (concat dir basename) (find-ng-project-dir-for-buffer))))

;; Angular base functions

(defun find-ng-project-dir (dir)
  "Find the closest angular project directory, from dir and up"
  (if (string= "/" dir)
      nil
    (if (file-exists-p (expand-file-name "angular.json" dir))
        dir
      (find-ng-project-dir (expand-file-name "../" dir)))))


(defun find-ng-closest-module (dir angular-dir)
  "Find the closest angular module, discarding routing ones, from dir and up"
  (let ((default-directory dir))
    (if (string= "/" dir)
        nil
      (let ((modules (remove-if
                      (lambda (module)
                        (or (string-contains module "-routing")
                            (string-contains module ".#")))
                      (file-expand-wildcards "*.module.ts"))))
        (if
            modules
            (angular-relative-path (concat dir (first modules)) angular-dir)
          (find-ng-closest-module (expand-file-name "../" dir) angular-dir))))))

(defun find-ng-project-dir-for-buffer ()
  (or (find-ng-project-dir (file-name-directory (buffer-path)))
      (file-name-directory (buffer-file-name))))

(defun find-ng-closest-module-for-buffer ()
  (find-ng-closest-module (file-name-directory (buffer-path)) (find-ng-project-dir-for-buffer)))

;; Generators

(defun ng-generate-component-for-buffer ()
  (let ((default-directory (find-ng-project-dir-for-buffer)))
    (shell-command-to-string (format "ng generate component %s -f -m %s" (angular-component-name-for-buffer) (find-ng-closest-module-for-buffer)))))
