# AGENTS.md

Development notes for agents and contributors working on docker.el.

## Running the tests

CI (`.github/workflows/ci.yml`) byte-compiles the package with
[Eask](https://github.com/emacs-eask/cli) inside the `silex/emacs:<version>-ci-eask`
Docker image, with warnings treated as errors. Reproduce it locally the same way:

```sh
docker run --rm -v "$PWD":/work -w /work silex/emacs:30.2-ci-eask \
  bash -c "eask install-deps --dev && eask uninstall docker && eask compile --strict"
```

Swap `30.2` for any Emacs version from the CI matrix (28.1–30.2). To test against
MELPA stable (the `stable` matrix jobs), prepend
`eask source delete melpa && eask source add melpa-stable &&` to the `bash -c` command.
