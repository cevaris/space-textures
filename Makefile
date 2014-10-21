SHELL := /bin/bash

all: test configure
	cabal build
	cp dist/build/SpaceTexture/SpaceTexture .
	cp dist/build/SpaceTexture/SpaceTexture SpaceTextureAsg7
test:
	if [[ "$$(which cabal)" == "" ]]; then echo cabal does not exist. Install it with \'sudo apt-get install cabal-install\'; exit 1; fi
configure:
	cabal update
	cabal install cabal
	cabal install --only-dependencies
	cabal configure
clean:
	cabal clean
	- rm -fr SpaceTexture SpaceTextureAsg7 dist
	