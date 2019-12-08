#!/usr/bin/env fish

while inotifywait -e close_write post/*.md tutorial/*.md about.md
    nix-build -A site-quick
end
