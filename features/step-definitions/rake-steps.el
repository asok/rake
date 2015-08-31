(defmacro rake-test-running-command-step (cmd arg &optional extra-step)
  `(progn
     (When "I start an action chain")
     (When "I press \"M-x\"")
     (And (concat "I type \"" ,cmd "\""))
     (When "I press \"RET\"")
     (And (concat "I type \"" ,arg "\""))
     (And "I execute the action chain")))

(Given "^I have Rakefile with content:$"
  (lambda (content)
    (find-file (concat rake-test-app-path "/Rakefile"))
    (When "I clear the buffer")
    (When "I insert:" content)
    (save-buffer)))

(Given "^I have Gemfile$"
  (lambda ()
    (f-touch rake-test-gemfile)))

(When "^I run rake selecting \"\\(.+\\)\"$"
  (lambda (arg)
    (When "I start an action chain")
    (And "I press \"M-x\"")
    (And (concat "I type \"rake\""))
    (And "I press \"RET\"")
    (And (concat "I type \"" arg "\""))
    (And "I execute the action chain")))

(When "^I run rake selecting \"\\(.+\\)\" and setting \"\\(.+\\)\" as an argument$"
  (lambda (arg rake-arg)
    (When "I start an action chain")
    (And "I press \"C-u\"")
    (And "I press \"M-x\"")
    (And "I type \"rake\"")
    (And "I press \"RET\"")
    (And (concat "I type \"" arg "\""))
    (And "I press \"RET\"")
    (And (concat "I type \"" rake-arg "\""))
    (And "I execute the action chain")))

(When "^I run rake-find-task selecting \"\\(.+\\)\""
  (lambda (task)
    (When "I start an action chain")
    (And "I press \"M-x\"")
    (And (concat "I type \"rake-find-task\""))
    (And "I press \"RET\"")
    (And (concat "I type \"" task "\""))
    (And "I execute the action chain")))

(Given "^spring is running"
  (lambda ()
    (let ((dir (concat temporary-file-directory "spring/")))
      (when (not (f-exists? dir))
        (make-directory dir))
      (f-touch rake-test-spring-pid-file))))

(Given "^zeus is running"
  (lambda ()
    (f-touch rake-test-zeus-pid-file)))

(And "^the task \"\\(.+\\)\" is in the cache$"
  (lambda (name)
    (let ((tasks (gethash rake-test-app-path (rake--deserialize-cache))))
      (should (equal (list name) tasks)))))

(And "^the task \"\\(.+\\)\" is not in the cache$"
  (lambda (name)
    (let* ((content (rake--deserialize-cache))
           (tasks (and content (gethash rake-test-app-path content))))
      (should (not (equal (list name) tasks))))))

(Given "^I enable the cache$"
  (lambda ()
    (setq rake-enable-caching t)))

(And "^the cursor should be on line \\([0-9]+\\)$"
  (lambda (line-num)
    (should (equal (+ 1 (count-lines 1 (point))) (string-to-number line-num)))))

(And "I debug"
  (lambda ()
    (print (buffer-string))))
