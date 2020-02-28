# Getting the version out of the package.yml file
VERSION := $(shell cat package.json | grep version | cut -d ' ' -f 2)

install:
	npm install

build:
	npm run build

test:
	npm run test:coverage

release:
	docker build -t mathsermone/ikweb:${VERSION} -f Dockerfile .
	echo ${DOCKER_PASSWORD} | docker login -u ${DOCKER_USERNAME} --password-stdin
  docker push mathsermone/ikweb:${VERSION}

test-develop: install test
	@echo "Deploying ik-web for develop environment" && printf '%*s\n' "40" '' | tr ' ' -

# .PHONY is used for reserving tasks words
.PHONY: test build deploy-develop release