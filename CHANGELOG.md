# Changelog

## 1.2.0 (2018-10-24)

- Add docker-container-attach.
- Use better keybinding for docker-compose exec.

## 1.1.1 (2018-10-03)

- Fix docker-compose restart binding.

## 1.1.0 (2018-09-25)

- Add docker-compose exec.
- Add docker-compose down.
- Add docker-compose create.
- Add separate docker-compose action for all services.
- Add tail option to docker-compose logs.

## 1.0.0 (2018-09-14)

- Support docker-compose.
- Configurable listings.
- Add options to the docker popup.
- Browse volumes with dired.
- Add default sort key for machine, networks and volumes.
- Various improvements.

## 0.7.0 (2018-07-02)

- Improve navigation.
- Singularize resources like in docker.
- Refactor code.

## 0.6.0 (2018-05-24)

- Improve documentation.
- Use lexical binding.
- Allow customization of tabulated list sort key.
- Support eshell to containers.
- Add flag to toggle if docker command will run as root.
- Customize which shell to use when entering containers.

## 0.5.3 (2017-11-21)

- Add changelog.
- Handle JSON errors.
- Rename docker-rename-entry to docker-containers-rename.
- TRAMP support for remote containers shells.
- Add docker kill support (#55).
- Make docker command customizable.
- Various bugfixes.
- Update documentation.

## 0.5.2 (2016-10-31)

- Fix docker-images unable to remove "repo:<none>" images.
- Add docker-machine-create.

## 0.5.1 (2016-10-18)

- Improve docker-machine-env parsing.

## 0.5.0 (2016-10-18)

- Show all containers by default.
- Add missing variable customization types.
- Add customization for showing all/only-running containers.
- Add docker inspect support (#45).
- Add docker tag support (#41).
- Add docker rename support (#40).
- Add shell and dired support.
- Add docker inspect support.
- Add docker diff support.
- Add docker cp support.
- Various bugfixes.

## 0.4.0 (2016-10-18)

- Preserve marks when refreshing.
- Replace tabulated-list-extensions by tablist.
- Implement docker-logs.
- Implement docker-inspect.
- Improve documentation.

## 0.3.1 (2016-04-08)

- Bugfixes for docker-rmi.
- Add flag to sync time between host & containers.
- Add "web ports" flag to docker-run.
- Add docker-machine.el.

## 0.3.0 (2016-04-03)

- Select current row if selection is empty.
- Specify command from popup.
- Add lots of new docker-run options.
- Add docker-networks.
- Add volumes switch on docker rm.
- Improve documentation.

## 0.2.0 (2015-11-26)

- Add `docker-volume` support.
- Refactor documentation.

## 0.1.0 (2015-10-01)

- Fix docker-unpause bindings & documentation.
- Add '-d' flag for docker-run-popup.
- Make an error when there's nothing selected.
- Add `docker-ps` alias.
- Add docker-unpause.
- Allow calling M-x docker-pull/docker-rm.
- Implement containers manipulation.
- Add docker-images.
- Initial commit.
