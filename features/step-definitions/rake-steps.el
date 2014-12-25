(Given "^I have Rakefile with content:$"
       (lambda (content)
         (find-file (concat rake-test-app-path "/Rakefile"))
         (When "I clear the buffer")
         (When "I insert:" content)
         (save-buffer)))

(When "^I run command \"\\(.+\\)\" \\(?:selecting\\|inputting\\) \"\\(.+\\)\"$"
      (lambda (command argument)
        (When "I start an action chain")
        (When "I press \"M-x\"")
        (And (s-lex-format "I type \"${command}\""))
        (When "I press \"RET\"")
        (And (s-lex-format "I type \"${argument}\""))
        (And "I execute the action chain")))

(And "I debug"
     (lambda ()
       (print (buffer-string))))
