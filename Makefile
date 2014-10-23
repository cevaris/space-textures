SHELL := /bin/bash
UNAME_S := $(shell uname -s)
# export PATH := "~/.cabal/bin:$$PATH"


all: test configure
	cabal build
	cp dist/build/SpaceTexture/SpaceTexture .
	cp dist/build/SpaceTexture/SpaceTexture SpaceTextureAsg7
test:

ifeq (${UNAME_S}, Linux)
ifeq ($$(which cabal),"")
    echo cabal does not exist. Install it with \'sudo apt-get install cabal-install\'; exit 1;
endif
ifeq ($$(dpkg -s libghc-zlib-dev),1)
    echo libghc-zlib-dev is not installed. Install it with \'sudo apt-get install libghc-zlib-dev\'; exit 1;
endif
ifeq ($$(dpkg -s libghc-zlib-bindings-dev),1)
    echo libghc-zlib-bindings-dev is not installed. Install it with \'sudo apt-get install libghc-zlib-bindings-dev\'; exit 1;
endif

endif
configure:
	sudo apt-get install libghc-zlib-dev  libghc-zlib-bindings-dev
	cabal update
	cabal install cabal
	
	cabal install --only-dependencies
	cabal configure
clean: test
	cabal clean
	- rm -fr SpaceTexture SpaceTextureAsg7 dist
	