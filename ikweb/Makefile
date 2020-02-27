install:
	npm install

build:
	npm run build

test:
	npm run test:coverage

deploy-develop: install build test
	@echo "Deploying ik-web for develop environment" && printf '%*s\n' "40" '' | tr ' ' -

# .PHONY is used for reserving tasks words
.PHONY: test build deploy-develop