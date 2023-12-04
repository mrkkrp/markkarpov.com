#!/usr/bin/env bash

while inotifywait -e close_write post/*.md tutorial/*.md about.md
do
    nix build '.#site-quick'
done
