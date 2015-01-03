(require 'f)

(defvar rake-support-path
  (f-dirname load-file-name))

(defvar rake-features-path
  (f-parent rake-support-path))

(defvar rake-root-path
  (f-parent rake-features-path))

(add-to-list 'load-path rake-root-path)

(require 'rake)
(require 'espuds)
(require 'ert)

(defvar rake-test-app-path
  (f-canonical (concat (make-temp-file "rake-test" t) "/")))

(defvar rake-test-spring-pid-file
  (concat
   temporary-file-directory
   "spring/"
   (md5 rake-test-app-path 0 -1)
   ".pid"))

(defvar rake-test-zeus-pid-file
  (concat rake-test-app-path ".zeus.sock"))

(defvar rake-test-gemfile
  (concat rake-test-app-path "Gemfile"))

(defun rake-test-touch-file (filepath)
  (let ((fullpath (expand-file-name filepath rake-test-app-path)))
    (f-touch fullpath)))

(Setup
 (setq kill-buffer-query-functions nil
       rake-cache-file (concat rake-test-app-path "rake.cache")
       rake-enable-caching nil)
 (make-temp-file rake-test-app-path t)
 (rake-test-touch-file "Rakefile")
 (cd rake-test-app-path))

(Before
 ;; Before each scenario is run
 )

(After
 (when (member "*rake-compilation*" (mapcar 'buffer-name (buffer-list)))
   (kill-buffer "*rake-compilation*"))
 (remhash (rake--root) rake--cache)
 (when (f-file? rake-test-spring-pid-file)
   (f-delete rake-test-spring-pid-file))
 (when (f-file? rake-test-zeus-pid-file)
   (f-delete rake-test-zeus-pid-file))
 (when (f-file? rake-test-gemfile)
   (f-delete rake-test-gemfile)))

(Teardown
 (delete-directory rake-test-app-path t)
 )
