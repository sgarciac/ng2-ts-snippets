;; Angular base functions

(defun angular-relative-path (full-path angular-dir)
  "Given a full path for a file and the angular dir, returns
a relative path suitable for use with angular cli"
  (file-relative-name full-path (concat angular-dir
                                        (file-name-as-directory "src")
                                        (file-name-as-directory "app"))))

(defun angular-component-name-for-buffer ()
  "For the current buffer, given that it contains an angular
typescript file, returns its relative path suitable for use
with angula cli"
  (let* ((dir (file-name-directory (buffer-path)))
         (basename (first (remove
                           ""
                           (split-string
                            (file-name-base
                             (buffer-path)) "\\.")))))
    (angular-relative-path (concat dir basename) (find-ng-project-dir-for-buffer))))

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
  "Find the closest angular project directory, from dir and up
for the current buffer"
  (or (find-ng-project-dir (file-name-directory (buffer-path)))
      (file-name-directory (buffer-file-name))))

(defun find-ng-closest-module-for-buffer ()
  "Find the closest angular module, discarding routing ones,
from dir and up, for the current buffer"
  (find-ng-closest-module (file-name-directory (buffer-path)) (find-ng-project-dir-for-buffer)))

;; Generators

(defun ng-generate-for-buffer (type &optional force add-to-module dry-run)
  "Call ng generate"
  (let ((default-directory (find-ng-project-dir-for-buffer))
        (command (format "ng generate %s %s --flat %s %s %s"
                         type
                         (angular-component-name-for-buffer)
                         (if force "-f" "")
                         (if add-to-module
                             (format "-m %s" (find-ng-closest-module-for-buffer))
                           "")
                         (if dry-run
                             "--dry-run"
                           ""
                           )
                         )))
    (print (format "command: %s" command))
    (shell-command-to-string command)
    )
  (unless dry-run (revert-buffer :ignore-auto :noconfirm)))

;; Utilities

(defun string-contains (haystack needle)
  (string-match-p (regexp-quote needle) haystack))

(defun buffer-path ()
  "Name of the current file or buffer"
  (or (buffer-file-name)
      (buffer-name)))
