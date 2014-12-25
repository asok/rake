;;; rake.el --- Command to run rake - make for ruby

;; Copyright (C) 2014 Adam Sokolnicki

;; Author:            Adam Sokolnicki <adam.sokolnicki@gmail.com>
;; URL:               https://github.com/asok/rake.el
;; Version:           0.1.0
;; Keywords:          rake, ruby
;; Package-Requires:  ((f "0.13.0") (dash "1.5.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;; Code:

(defmacro rake--with-root (body-form)
  `(let* ((default-directory (locate-dominating-file default-directory "Rakefile")))
     (if default-directory
         ,body-form
       (user-error "Rakefile not found."))))

(defun rake-tasks ()
  (shell-command-to-string "rake -T -A"))

;; Shamelessly stolen from ruby-starter-kit.el:
;; https://github.com/technomancy/emacs-starter-kit/blob/v2/modules/starter-kit-ruby.el
(defun rake--pcmpl-tasks ()
  "Return a list of all the rake tasks defined in the current projects."
  (--keep it
          (--map (if (string-match "rake \\([^ ]+\\)" it) (match-string 1 it))
                 (split-string (rake-tasks) "[\n]"))))

(define-derived-mode rake-compilation-mode compilation-mode "Rake Compilation"
  "Compilation mode used by `rake-compile'.")

;;;###autoload
(defun rake-compile (task)
  (interactive (list
                (completing-read
                 "Rake (default: default): "
                 (rake--pcmpl-tasks))))
  (rake--with-root
   (compile
    (concat "rake "
            (if (= 0 (length task)) "default" task))
    'rake-compilation-mode)))

(provide 'rake)

;;; rake.el ends here
