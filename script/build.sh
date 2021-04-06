#!/usr/bin/env bash

clojure -M:build
mkdir -p dist/cljs-out
cp -r resources/public/* dist
cp target/public/cljs-out/joy-main.js dist/cljs-out
