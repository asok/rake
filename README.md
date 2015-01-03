## Rake [![Build Status](https://travis-ci.org/asok/rake.png?branch=master)](https://travis-ci.org/asok/rake)

**Rake** is a package for running rake tasks in Emacs.

* runs rake command using zeus, spring or bundler
* caches the tasks
* displays the docstrings of the tasks

![Screenshot](https://github.com/asok/rake/raw/master/screenshots/rake.png)

## Installation

#### Melpa

Once you have setup [Melpa](http://melpa.milkbox.net/#/getting-started) you can use `package-install` command to install Projectile Rails. The package name is `rake`.

## Usage

### `rake` command

* `M-x rake` to run a rake task.
* `C-u M-x rake` to amend the command to run. Useful if you want to add arguments.
* `C-u C-u M-x rake` to bypass the cache (when enabled).

### `rake-find-task` command

* `M-x rake-find-task` to find a rake task.

### Setting up keybinding

By default `rake` command is not bound to any key.
You might want to do something like this:

```el
(define-key ruby-mode-map (kbd "C-!") 'rake)
```

Replace `(kbd "C-!")` with a key of your liking.

## Customization

### Caching

By default the caching is enabled. To disable it:

```el
(setq rake-enable-caching nil)
```

### Completion

By default `ido` is used for completion. You can customize it with:

```el
(setq rake-completion-system 'helm)
```

You can set it to `ido`, `helm`, `grizzl` or `default` for the Emacs' default completion.
Also, you can set it to the symbol of a custom command that accepts "prompt" as the first argument
and "choices" as the second argument.

Also if you are a user of [Projectile](http://batsov.com/projectile/) you might want to just set it
to the value of `projectile-completion-system` like this:

```el
(eval-after-load 'projectile
  '(setq rake-completion-system projectile-completion-system))
```


## Alternatives

* [rake.el](https://github.com/vderyagin/rake.el)

## Contribution

Install [cask](https://github.com/rejeep/cask.el) if you haven't
already, then:

```bash
$ cd /path/to/rake
$ cask
```

Run all tests with:

```bash
$ cask exec ecukes
```
