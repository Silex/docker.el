(package "docker"
  "2.5.0"
  "Interface to Docker")

(website-url "https://github.com/Silex/docker.el")
(keywords "files" "convenience")

(package-file "docker.el")

(script "test" "eask test ert ./test/*.el")

(source "gnu")
(source "melpa")

(depends-on "emacs"     "28.1")
(depends-on "aio"       "1.2")
(depends-on "dash"      "2.19.1")
(depends-on "s"         "1.13.0")
(depends-on "tablist"   "1.1")
(depends-on "transient" "0.4.3")
;; hack: this forces the built-in transient to be upgraded to latest.
;; make sure to run "eask uninstall docker" in CI to avoid confusing the versions of docker.el
(development
  (depends-on "docker"))
