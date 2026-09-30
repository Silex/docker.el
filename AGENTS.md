# AGENTS.md

Development notes for agents and contributors working on docker.el.

## What this is

`docker.el` is an Emacs package providing a [transient](https://github.com/magit/transient)
and [tablist](https://github.com/emacsorphanage/tablist) interface to the `docker` command
line. It is distributed through MELPA, so it must stay loadable on every Emacs version in
the CI matrix (28.1 and up) and keep its dependency list small.

## Layout

Flat, one file per docker resource, plus four shared ones:

| File                   | Holds                                                                 |
| ---------------------- | --------------------------------------------------------------------- |
| `docker.el`            | the `docker` entry point transient and the package headers            |
| `docker-core.el`       | generic actions shared by every resource (inspect, rm, logs, …)       |
| `docker-process.el`    | process handling, `docker-with-sudo`, the terminal backend dispatcher |
| `docker-utils.el`      | the transient/tablist macros and the small helpers                    |
| `docker-group.el`      | the `docker` customize group                                          |
| `docker-faces.el`      | faces                                                                 |
| `docker-<resource>.el` | one per resource: container, image, volume, network, context, compose |

Only `docker.el` carries `Version:` and `Package-Requires:`; the other files must not. The
version is also stated in the `(package ...)` form in `Eask`, which eask refuses to run
without, so a release bumps both.

## Running the tests

CI (`.github/workflows/ci.yml`) byte-compiles the package with
[Eask](https://github.com/emacs-eask/cli) inside the `silex/emacs:<version>-ci-eask`
Docker image, with warnings treated as errors, then runs the ERT suite in `test/`.
Reproduce it locally the same way:

```sh
docker run --rm -v "$PWD":/work -w /work silex/emacs:30.2-ci-eask \
  bash -c "eask install-deps --dev && eask uninstall docker && eask compile --strict && eask test ert ./test/*.el"
```

Swap `30.2` for any Emacs version from the CI matrix (28.1 to 30.2). To test against
MELPA stable (the `stable` matrix jobs), prepend
`eask source delete melpa && eask source add melpa-stable &&` to the `bash -c` command.

Delete the `docker*.elc` files between runs of different Emacs versions. The repository
is bind-mounted, so the byte code from the previous version stays behind and the next one
loads it.

Run it on `28.1` as well as `30.2` when touching docstrings: Emacs 30 splits
`docstrings-wide` and `docstrings-control-chars` out of the `docstrings` warning
group, so the two ends of the matrix do not report the same set.

The suite runs in batch, so it reaches the pure helpers, the tramp paths the container
entry points build and the terminal backend dispatch, but not the transient UI, a real
tramp connection or a docker daemon. Changes to those still need a manual check in
`emacs -Q`.

## Running the linters

Linting is not part of the compile step; a separate CI job runs it on one Emacs version:

```sh
docker run --rm -v "$PWD":/work -w /work silex/emacs:30.2-ci-eask \
  bash -c "eask install-deps --dev && eask uninstall docker && eask lint checkdoc --strict && eask lint package --strict && eask lint keywords --strict"
```

`--strict` is required: without it eask prints the issues and still exits 0.
