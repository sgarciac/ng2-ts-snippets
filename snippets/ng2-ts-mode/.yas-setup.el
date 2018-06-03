;; Angular base functions
(require 'projectile)

(defun angular-buffer-type ()
  (let ((parts (split-string
                (file-name-base
                 (buffer-path)) "\\.")))
    (when (= (length parts) 2)
      (intern (cadr parts)))))

(defun angular-relative-path (full-path angular-dir)
  "Given a full path for a file and the angular dir, returns
a relative path suitable for use with angular cli"
  (file-relative-name full-path (concat angular-dir
                                        (file-name-as-directory "src")
                                        (file-name-as-directory "app"))))

(defun angular-relative-name-for-buffer ()
  "For the current buffer, given that it contains an angular
typescript file, returns its relative path suitable for use
with angula cli"
  (let* ((dir (file-name-directory (buffer-path)))
         (basename (car (remove
                         ""
                         (split-string
                          (file-name-base
                           (buffer-path)) "\\.")))))
    (angular-relative-path (concat dir basename) (projectile-project-root))))

(defun find-ng-closest-module (dir angular-dir)
  "Find the closest angular module, discarding routing ones, from dir and up"
  (let ((default-directory dir))
    (if (string= "/" dir)
        nil
      (let ((modules (seq-filter
                      (lambda (module)
                        (not (or (string-contains module "-routing")
                                 (string-contains module ".#"))))
                      (file-expand-wildcards "*.module.ts"))))
        (if
            modules
            (angular-relative-path (concat dir (first modules)) angular-dir)
          (find-ng-closest-module (expand-file-name "../" dir) angular-dir))))))


(defun find-ng-closest-module-for-buffer ()
  "Find the closest angular module, discarding routing ones,
from dir and up, for the current buffer"
  (find-ng-closest-module (file-name-directory (buffer-path)) (projectile-project-root)))

;; Generators

(defun ng-generate-for-buffer (&optional skip-module dry-run)
  "Call ng generate"
  (save-current-buffer)
  (let ((type (angular-buffer-type))
        (closest-module (find-ng-closest-module-for-buffer))
        (ng-project-dir (projectile-project-root)))
    (when (eq type 'module) (setq skip-module t))
    (cond
     ((not ng-project-dir) (warn "You are not inside an angular cli project."))
     ((and (not skip-module) (not closest-module)) (warn "No module found"))
     ((not (member type '(component directive module pipe service guard))) (warn "Only *.[component|directive|module|pipe|service|guard].ts files are supported"))
     (t (let ((default-directory ng-project-dir)
              (command (format
                        "ng generate %s %s --flat -f %s %s"
                        type
                        (angular-relative-name-for-buffer)
                        (if (not skip-module)
                            (format "-m %s" closest-module) "")
                        (if dry-run
                            "--dry-run" ""))))
          (print (format "command: %s" command))
          (let ((result (shell-command-to-string command)))
            (unless dry-run (revert-buffer :ignore-auto :noconfirm))
            (print result)
            result))))))

;; Utilities
(defun string-contains (haystack needle)
  (string-match-p (regexp-quote needle) haystack))

(defun buffer-path ()
  "Name of the current file or buffer"
  (or (buffer-file-name)
      (buffer-name)))
