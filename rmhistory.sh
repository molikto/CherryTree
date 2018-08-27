#!/usr/bin/env bash
git filter-branch -f \
    --prune-empty \
    --tag-name-filter cat \
    --tree-filter 'rm -f $FILE' \
    -- --all
git for-each-ref --format="%(refname)" refs/original/ | xargs -n 1 git update-ref -d
git reflog expire --expire=now --all && git gc --prune=now --aggressive
