#!/usr/bin/env bash

mode=inplace
while [[ $# -gt 0 ]]; do
    case "$1" in
        --mode)
            shift
            mode=$1
            shift
            ;;
        *)
            break
            ;;
    esac
done

toplevel=$(git rev-parse --show-toplevel || realpath .)
extensions0=$(yq -r '.default-extensions | map("-o -X" + .) | join(" ")' "$toplevel/package.yaml")
IFS=' ' read -r -a extensions <<<"$extensions0"

fourmolu --no-cabal "${extensions[@]}" --mode "$mode" --config "$toplevel/fourmolu.yaml" "$@"
