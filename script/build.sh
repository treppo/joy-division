#!/usr/bin/env bash

clojure -M:build
mkdir -p docs/cljs-out
cp -r resources/public/* docs
cp target/public/cljs-out/joy-main.js docs/cljs-out
