SHELL := /bin/bash
export PATH := "~/.cabal/bin:$$PATH"

all: test configure
	cabal build
	cp dist/build/SpaceTexture/SpaceTexture .
	cp dist/build/SpaceTexture/SpaceTexture SpaceTextureAsg7
test:
	if [[ "$$(which cabal)" == "" ]]; then echo cabal does not exist. Install it with \'sudo apt-get install cabal-install\'; exit 1; fi
	if [[ "$$(dpkg -s libghc-zlib-dev)" == 1 ]]; then echo libghc-zlib-dev is not installed. Install it with \'sudo apt-get install libghc-zlib-dev\'; exit 1; fi
	if [[ "$$(dpkg -s libghc-zlib-bindings-dev)" == 1 ]]; then echo libghc-zlib-dev is not installed. Install it with \'sudo apt-get install libghc-zlib-dev\'; exit 1; fi
configure:
	# sudo apt-get install libghc-zlib-dev  libghc-zlib-bindings-dev
	cabal update
	cabal install cabal
	
	cabal install --only-dependencies
	cabal configure
clean:
	cabal clean
	- rm -fr SpaceTexture SpaceTextureAsg7 dist
	