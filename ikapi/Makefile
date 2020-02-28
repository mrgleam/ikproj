IMAGE := mathsermone/ikapi
# Getting the version out of the package.yml file
VERSION := $(shell cat ikapi.cabal | grep version | head -1 | awk -F: '{ print $$2 }' | sed 's/[", ]//g')

# Variable for filename for store running procees id
PID_FILE = /tmp/ikapi.pid

build:
	stack build

start-dev: build
	stack exec ikapi-exe & echo $$! > $(PID_FILE)

test:
	stack test

build-dependencies:
	stack --no-terminal --install-ghc test --only-dependencies

test-ci: build-dependencies test
	@echo "Testing ik-api" && printf '%*s\n' "40" '' | tr ' ' -

release: build-dependencies
	stack --no-terminal test --haddock --no-haddock-deps
	docker build --build-arg BINARY_PATH=".stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/ikapi-exe/ikapi-exe" -t ${IMAGE}:${VERSION} -f Dockerfile .
	docker tag ${IMAGE}:${VERSION} ${IMAGE}:latest
	echo ${DOCKER_PASSWORD} | docker login -u ${DOCKER_USERNAME} --password-stdin
	docker push ${IMAGE}:${VERSION}
	docker push ${IMAGE}:latest

release-osx: build-dependencies
	stack --no-terminal test --haddock --no-haddock-deps
	docker build --build-arg BINARY_PATH=".stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/ikapi-exe/ikapi-exe" -t ${IMAGE}:${VERSION} -f Dockerfile .

# Stop task will kill process by ID stored in PID_FILE (and all child processes by pstree).
stop:
	-kill `pstree -p \`cat $(PID_FILE)\` | tr "\n" " " |sed "s/[^0-9]/ /g" |sed "s/\s\s*/ /g"`

# Before task will only prints message. Actually, it is not necessary. You can remove it, if you want.
before:
	@echo "STOPED ik-api" && printf '%*s\n' "40" '' | tr ' ' -

restart-dev: stop before start-dev
	@echo "STARTED ik-api" && printf '%*s\n' "40" '' | tr ' ' -

restart-test: test
	@echo "RESTARTED live reload testing" && printf '%*s\n' "40" '' | tr ' ' -

serve-dev: start-dev
	fswatch -or --event=Updated ./src ./app | \
	xargs -n1 -I {} make restart-dev

test-watch: test
	fswatch -or --event=Updated ./src ./app ./test | \
	xargs -n1 -I {} make restart-test

# .PHONY is used for reserving tasks words
.PHONY: test-watch test serve-dev test-ci release