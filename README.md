# docker.el

**THIS PACKAGE IS USABLE BUT UNDER HEAVY DEVELOPMENT**

## Installation

The recommended way to install docker.el is through [MELPA](https://github.com/milkypostman/melpa).

## Quickstart

Docker.el follows the philosophy of `dired` (or `ibuffer`), where you
select things to operate on followed by actions like in
[magit](https://github.com/magit/magit).

### Common selection bindings

On all the listings, you can use the following keybindings:

* `m`: mark
* `u`: unmark
* `t`: toggle marks
* `U`: unmark all

### Images

Running <kbd>M-x docker-images</kbd> lists the docker images.
After having selected some images, you can do the following actions:

* `F`: pull
* `P`: push
* `D`: rmi
* `R`: run

### Containers

Running <kbd>M-x docker-containers</kbd> lists the docker containers.
After having selected some containers, you can do the following actions:

* `S`: start
* `O`: stop
* `P`: pause
* `R`: restart
* `D`: rm

### Global minor mode

 Running <kbd>M-x docker-global-mode</kbd> creates keybindings to the various docker utilities:

 * `C-c d b`: builds dockerfile (depends on [dockerfile-mode](https://github.com/spotify/dockerfile-mode)).
 * `C-c d c`: list containers
 * `C-c d i`: list images

## Contributions welcome!

Either as suggestions or as pull requests by opening tickets on the
[issue tracker](https://github.com/Silex/docker.el/issues).
