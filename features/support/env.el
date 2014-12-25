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

(defun rake-test-touch-file (filepath)
  (let ((fullpath (expand-file-name filepath rake-test-app-path)))
    (f-touch fullpath)))

(Setup
 (make-temp-file rake-test-app-path t)
 (rake-test-touch-file "Rakefile")
 (cd rake-test-app-path)
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 (delete-directory rake-test-app-path t)
 )
