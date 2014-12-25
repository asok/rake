## Rake.el

**Rake.el** is a package for running rake tasks in Emacs.
It tries to be smart and uses zeus, spring or bundler to run the command.

## Installation

## Usage

Do `M-x rake-compile` to run a rake task.

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
