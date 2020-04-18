all: 
	nix-shell -A client --run "cabal --builddir=dist-client --config-file=config/client.config build"
	mkdir -p static
	ln -sf ../dist-client/build/x86_64-linux/ghcjs-8.6.0.1/ikproj-1.0/x/client/build/client/client.jsexe/all.js static/
	nix-shell -A server --run "cabal --builddir=dist-server --config-file=config/server.config run server"

# build client, in a "nix-shell -A client"
client:
	nix-shell -A client --run "cabal --builddir=dist-client --config-file=config/client.config build"
	mkdir -p static
	ln -sf ../dist-client/build/x86_64-linux/ghcjs-8.6.0.1/ikproj-1.0/x/client/build/client/client.jsexe/all.js static/

# run server, in a "nix-shell -A server"
server:
	cabal --builddir=dist-server --config-file=config/server.config run server

clean:
	rm -rf dist-* static/all.js

