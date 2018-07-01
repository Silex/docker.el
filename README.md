[![MELPA](http://melpa.org/packages/docker-badge.svg)](http://melpa.org/#/docker)
[![MELPA Stable](http://stable.melpa.org/packages/docker-badge.svg)](http://stable.melpa.org/#/docker)

# docker.el

* [Installation](#installation)
* [Quickstart](#quickstart)
* [Screenshots](#screenshots)
  * [images](#images)
* [Commands](#commands)
  * [docker-images](#docker-images)
  * [docker-containers](#docker-containers)
  * [docker-volumes](#docker-volumes)
  * [docker-networks](#docker-networks)
  * [docker-machines](#docker-machines)
* [Customizations](#customizations)
* [FAQ](#faq)
  * [How to use docker-machine under OSX?](#how-to-use-docker-machine-under-osx)
* [Contributions](#contributions)

## Installation

The recommended way to install docker.el is through [MELPA](https://github.com/milkypostman/melpa).

Here is a example [use-package](https://github.com/jwiegley/use-package) configuration:

``` elisp
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
```

## Quickstart

Use <kbd>M-x docker</kbd>, select a resource then then mark/unmark items using the following keybindings:

| Keymap             | Description          |
|--------------------|----------------------|
| <kbd>m</kbd>       | Mark item            |
| <kbd>U</kbd>       | Unmark all           |
| <kbd>u</kbd>       | Unmark item          |
| <kbd>t</kbd>       | Toggle marks         |
| <kbd>s</kbd>       | Sort                 |
| <kbd><</kbd>       | Shrink column        |
| <kbd>></kbd>       | Enlarge column       |
| <kbd>C-c C-e</kbd> | Export to csv        |
| <kbd>* r</kbd>     | Mark items by regexp |

You can press `?` on all the listing to see the available actions. Also check out https://github.com/politza/tablist
to find more about the marking possibilities.

## Screenshots

### images

![docker.el screenshot](screenshots/images.png)

## Commands

### docker-images

<kbd>M-x docker-images</kbd> lists the docker images.
After having selected some images, you can do the following actions:

- `F`: pull
- `P`: push
- `D`: rmi
- `R`: run
- `I`: inspect
- `T`: tag

### docker-containers

Running <kbd>M-x docker-containers</kbd> lists the docker containers.
After having selected some containers, you can do the following actions:

* `S`: start
* `O`: stop
* `R`: restart
* `P`: pause/unpause
* `I`: inspect
* `L`: logs
* `C`: cp
* `D`: rm
* `K`: kill
* `d`: diff
* `f`: find-file
* `b`: shell
* `r`: rename

### docker-volumes

Running <kbd>M-x docker-volumes</kbd> lists the docker volumes.
After having selected some volumes, you can do the following actions:

* `D`: rm

### docker-networks

Running <kbd>M-x docker-networks</kbd> lists the docker networks.
After having selected some networks, you can do the following actions:

* `D`: rm

### docker-machines

Running <kbd>M-x docker-machines</kbd> lists the docker machines.
After having selected some machines, you can do the following actions:

* `C`: create
* `S`: start
* `E`: env
* `O`: stop
* `R`: restart
* `D`: rm

## Customizations

| Variable                           | Description                           | Default          |
|------------------------------------|---------------------------------------|------------------|
| docker-command                     | The docker binary to use              | `docker`         |
| docker-containers-default-sort-key | Sort key for docker containers        | `("Image")`      |
| docker-containers-shell-file-name  | Shell to use when entering containers | `/bin/bash`      |
| docker-containers-show-all         | Show non-running containers           | `t`              |
| docker-images-default-sort-key     | Sort key for docker images            | `("Repository")` |
| docker-keymap-prefix               | Prefix for `docker-mode`              | `C-c d`          |
| docker-run-as-root                 | Run docker as root                    | `nil`            |

## FAQ

### How to use docker-machine under OSX?

The following configuration is required (some of it can probably be simplified by using
https://github.com/purcell/exec-path-from-shell).

``` elisp
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
;; Use "docker-machine env box" command to find out your environment variables
(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
(setenv "DOCKER_CERT_PATH" "/Users/foo/.docker/machine/machines/box")
(setenv "DOCKER_MACHINE_NAME" "box")
```

## Contributions

They are very welcome, either as suggestions or as pull requests by opening tickets
on the [issue tracker](https://github.com/Silex/docker.el/issues).
