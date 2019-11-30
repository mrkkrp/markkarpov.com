#!/usr/bin/env fish

while inotifywait -e close_write post/*.md tutorial/*.md translation/*.md about.md
    nix-build -A site-quick
end
