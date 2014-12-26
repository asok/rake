## Rake.el

**Rake.el** is a package for running rake tasks in Emacs.

* runs rake command using zeus, spring or bundler
* caches the tasks

## Installation

## Usage

Do `M-x rake` to run a rake task.

You can do `C-u C-u M-x rake` in order to bypass the cache (when enabled).

### Customization

By default the caching is enabled. To disable it:

```el
(setq rake-enable-caching nil)
```


## Contribution

Install [cask](https://github.com/rejeep/cask.el) if you haven't
already, then:

```bash
$ cd /path/to/rake.el
$ cask
```

Run all tests with:

```bash
$ cask exec ecukes
```
