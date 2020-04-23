IMAGE := mathsermone/ikweb
VERSION := $(shell cat server/server.cabal | grep version | head -1 | awk -F: '{ print $$2 }' | sed 's/[", ]//g')

all: 
	cd client && nix-shell --run "cabal --builddir=dist-client --config-file=cabal.config build"
	mkdir -p server/static
	ln -sf ../../client/dist-client/build/x86_64-linux/ghcjs-8.6.0.1/client-0.1.0.0/x/client/build/client/client.jsexe/all.js server/static/
	cd server && nix-shell --run "cabal run server"

build-client:
	cd client && nix-shell --run "cabal --builddir=dist-client --config-file=cabal.config build"
	mkdir -p server/static
	ln -sf ../../client/dist-client/build/x86_64-linux/ghcjs-8.6.0.1/client-0.1.0.0/x/client/build/client/client.jsexe/all.js server/static/

run-server:
	cd server && nix-shell --run "cabal run server"

build-server:
	cd server && nix-shell --run "cabal --builddir=dist-server build"

test-ci:
	@echo "Testing ikweb-hs" && printf '%*s\n' "40" '' | tr ' ' -

release:
	nix-build docker.nix && docker load < result
	docker tag ikproj:latest registry.heroku.com/ikweb-hs/web
	docker push registry.heroku.com/ikweb-hs/web
	docker tag ikproj:latest ${IMAGE}:${VERSION}
	docker tag ${IMAGE}:${VERSION} ${IMAGE}:latest
	docker push ${IMAGE}:${VERSION}
	docker push ${IMAGE}:latest
