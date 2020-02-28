IMAGE := mathsermone/ikweb
# Getting the version out of the package.yml file
VERSION := $(shell cat package.json | grep version | head -1 | awk -F: '{ print $$2 }' | sed 's/[", ]//g')

install:
	npm install

build:
	npm run build

test:
	npm run test:coverage

release:
	echo ${DOCKER_PASSWORD} | docker login -u ${DOCKER_USERNAME} --password-stdin
	docker build -t ${IMAGE}:${VERSION} -f Dockerfile .
	docker tag ${IMAGE}:${VERSION} ${IMAGE}:latest
	docker push ${IMAGE}:${VERSION}
	docker push ${IMAGE}:latest

test-ci: install test
	@echo "Testing ik-web" && printf '%*s\n' "40" '' | tr ' ' -

# .PHONY is used for reserving tasks words
.PHONY: test build test-ci release