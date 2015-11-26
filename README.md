# docker.el

**THIS PACKAGE IS USABLE BUT UNDER HEAVY DEVELOPMENT**

## Installation

The recommended way to install docker.el is through [MELPA](https://github.com/milkypostman/melpa).

## Quickstart

Use <kbd>M-x docker-images</kbd> or <kbd>M-x docker-containers</kbd>,
then mark/unmark items using the following keybindings:

- `m`: mark
- `u`: unmark
- `t`: toggle marks
- `U`: unmark all

Press `?` to known about available keybindings in order to run actions on these items.

You can also call the [API](#api) directly.


## Screenshots

### images

![docker.el screenshot](screenshots/images.png)


## Keymaps

### docker-global-mode

Running <kbd>M-x docker-global-mode</kbd> creates keybindings to the
various docker api. The keymap prefix is `C-c d` by default and can be
changed with <kbd>M-x customize-variable docker-keymap-prefix</kbd>.

| command                                             | keymap                                     | description       |
|-----------------------------------------------------|--------------------------------------------|-------------------|
| [docker-images](#docker-images)                     | <kbd>C-c d i i</kbd> or <kbd>C-c d I</kbd> | list images       |
| [docker-rmi](#docker-rmi)                           | <kbd>C-c d i d</kbd>                       | delete image      |
| [docker-pull](#docker-pull)                         | <kbd>C-c d i f</kbd>                       | pull image        |
| [docker-push](#docker-push)                         | <kbd>C-c d i p</kbd>                       | push image        |
| [docker-run](#docker-run)                           | <kbd>C-c d i r</kbd>                       | run image         |
| [docker-containers](#docker-containers)             | <kbd>C-c d c c</kbd> or <kbd>C-c d C</kbd> | list containers   |
| [docker-rm](#docker-rm)                             | <kbd>C-c d c d</kbd>                       | delete container  |
| [docker-stop](#docker-stop)                         | <kbd>C-c d c o</kbd>                       | stop container    |
| [docker-pause](#docker-pause)                       | <kbd>C-c d c p</kbd>                       | pause container   |
| [docker-restart](#docker-restart)                   | <kbd>C-c d c r</kbd>                       | restart container |
| [docker-start](#docker-start)                       | <kbd>C-c d c s</kbd>                       | start container   |
| [docker-unpause](#docker-unpause)                   | <kbd>C-c d c e</kbd>                       | unpause container |
| [dockerfile-build-buffer](#dockerfile-build-buffer) | <kbd>C-c d B</kbd>                         | Build Dockerfile  |

### docker-images

<kbd>M-x docker-images</kbd> lists the docker images.
After having selected some images, you can do the following actions:

- `F`: pull
- `P`: push
- `D`: rmi
- `R`: run

### docker-containers

Running <kbd>M-x docker-containers</kbd> lists the docker containers.
After having selected some containers, you can do the following actions:

* `S`: start
* `O`: stop
* `P`: pause
* `E`: unpause
* `R`: restart
* `D`: rm


## API

### docker-rmi

Deletes an image.

### docker-pull

Pull an image.

### docker-push

Push an image.

### docker-run

Run an image.

### docker-rm

Deletes a container.

### docker-stop

Stops a container.

### docker-pause

Pauses a container.

### docker-restart

Restarts a container.

### docker-start

Starts a container.

### docker-unpause

Unpause a container.

### dockerfile-build-buffer

See [docker-file-mode](https://github.com/spotify/dockerfile-mode) for more information,


## Philosophy

This package is inspired by packages like:

- `dired` / `ibuffer` for selecting images / containers to operate on.
- [magit](https://github.com/magit/magit) for the various actions popups.
- [projectile](https://github.com/bbatsov/projectile) for the keymap.


## Contributions welcome!

Either as suggestions or as pull requests by opening tickets on the
[issue tracker](https://github.com/Silex/docker.el/issues).
