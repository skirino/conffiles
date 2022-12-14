#!/usr/bin/env bash
set -euo pipefail
shopt -s inherit_errexit

# TODO:
# - rebase branches that have no origin branch onto the current master?

declare -A origin_exists
current_branch=$(git branch --show-current)

readarray -t branches < <(git branch --format='%(refname:short)')
for b in "${branches[@]}"; do
  if [ -n "$(git branch --list --remotes "origin/$b")" ]; then
    origin_exists["$b"]='yes'
  else
    origin_exists["$b"]='no'
  fi
done

echo '---- git fetch --prune'
git fetch --prune

for b in "${!origin_exists[@]}"; do
  if [ "${origin_exists["$b"]}" = 'no' ]; then
    continue
  fi
  if upstream_sha1=$(git rev-parse "origin/$b" 2>/dev/null); then
    if [ "$(git rev-parse "$b")" != "${upstream_sha1}" ]; then
      if [ "$b" = "${current_branch}" ]; then
        if git diff --exit-code --quiet; then
          echo "----- Upstream of $b has been updated; catching up..."
          git merge --ff-only "$b@{u}"
        else
          echo "----- Do nothing on the current branch ($b) as there's diff found."
        fi
      else
        echo "----- Upstream of $b has been updated; catching up..."
        git push . "${upstream_sha1}:$b" || true
      fi
    fi
  else
    if [ "$b" = "${current_branch}" ]; then
      echo "----- Upstream of $b has been deleted. (do nothing as it's the current branch)"
    else
      echo "----- Upstream of $b has been deleted; removing..."
      git branch -D "$b"
    fi
  fi
done
