#!/usr/bin/env bash
# Points the Nix package at a published GitHub Release: rewrites the version
# and every system's hash in nix/package/release.json. Each tarball's URL is
# the package's own src.url, evaluated after the version is written, so
# nix/package/default.nix stays the one place that knows where they live.
#
# Usage: scripts/bump-nix-release.sh <version>    (e.g. 0.13.0, no leading v)
#
# Each hash is the SRI sha256 of the release tarball itself, which is what
# fetchurl checks. The tarballs are only hashed, never unpacked or run.
# Needs nix, curl, jq, openssl and base64.
set -euo pipefail

VERSION="${1:?usage: $0 <version>}"
if ! [[ "$VERSION" =~ ^[0-9]+(\.[0-9]+)*$ ]]; then
  echo "error: '$VERSION' is not a release version such as 0.13.0" >&2
  exit 1
fi

FLAKE=$(cd "$(dirname "$0")/.." && pwd)
RELEASE_JSON="$FLAKE/nix/package/release.json"
DOWNLOADS=$(mktemp -d)
trap 'rm -rf "$DOWNLOADS"' EXIT

# The version goes in first: the URLs evaluated below are built from it. The
# old hashes stay until each is replaced, and evaluating a URL fetches nothing.
json=$(jq --arg version "$VERSION" '.version = $version' "$RELEASE_JSON")
printf '%s\n' "$json" >"$RELEASE_JSON"
for SYSTEM in $(jq -r '.platforms | keys[]' "$RELEASE_JSON"); do
  URL=$(nix eval --raw "$FLAKE#packages.$SYSTEM.deslop.src.url")
  curl --fail --silent --show-error --location --retry 3 \
    --output "$DOWNLOADS/$SYSTEM.tar.gz" "$URL"
  HASH="sha256-$(openssl dgst -sha256 -binary "$DOWNLOADS/$SYSTEM.tar.gz" | base64)"
  echo "$SYSTEM ($URL): $HASH"
  json=$(jq --arg system "$SYSTEM" --arg hash "$HASH" \
    '.platforms[$system].hash = $hash' <<<"$json")
done
printf '%s\n' "$json" >"$RELEASE_JSON"
