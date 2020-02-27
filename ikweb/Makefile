install:
	npm install

build:
	npm run build

test:
	npm run test:coverage

test-develop: install test
	@echo "Deploying ik-web for develop environment" && printf '%*s\n' "40" '' | tr ' ' -

# .PHONY is used for reserving tasks words
.PHONY: test build deploy-develop