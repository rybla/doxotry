setup:
    bun install
    bun spago install
    bun spago build

clean_javascript:
    rm -rf node_modules

clean_purescript:
    rm -rf .spago output

clean_client:
    rm -rf client

clean_all: clean_client clean_purescript clean_javascript

test: setup
    bun spago test

serve: setup test
    bun spago run -m Server.Main

bundle_client: setup
    bun spago bundle --platform browser --source-maps --minify --module Doxotry.Client.Main --outfile=client_purescript_dist/main.js
    bun run script/bundle_client.ts

build: test bundle_client

dev: build
    bun http-server dist
 