#!/usr/bin/env bash
# Points the Nix package at a published GitHub Release: rewrites the version
# and every system's hash in nix/package/release.json.
#
# Usage: scripts/bump-nix-release.sh <version>    (e.g. 0.13.0, no leading v)
#
# Each hash is the SRI sha256 of the release tarball itself, which is what
# fetchurl checks. The tarballs are only hashed, never unpacked or run.
# Needs curl, jq, openssl and base64.
set -euo pipefail

VERSION="${1:?usage: $0 <version>}"
if ! [[ "$VERSION" =~ ^[0-9]+(\.[0-9]+)*$ ]]; then
  echo "error: '$VERSION' is not a release version such as 0.13.0" >&2
  exit 1
fi

RELEASE_JSON="$(dirname "$0")/../nix/package/release.json"
DOWNLOADS=$(mktemp -d)
trap 'rm -rf "$DOWNLOADS"' EXIT

json=$(jq --arg version "$VERSION" '.version = $version' "$RELEASE_JSON")
for SYSTEM in $(jq -r '.platforms | keys[]' "$RELEASE_JSON"); do
  ASSET=$(jq -r --arg system "$SYSTEM" '.platforms[$system].asset' "$RELEASE_JSON")
  URL="https://github.com/Ivy-Apps/deslop/releases/download/v$VERSION/deslop-$ASSET.tar.gz"
  curl --fail --silent --show-error --location --retry 3 \
    --output "$DOWNLOADS/$ASSET.tar.gz" "$URL"
  HASH="sha256-$(openssl dgst -sha256 -binary "$DOWNLOADS/$ASSET.tar.gz" | base64)"
  echo "$SYSTEM ($ASSET): $HASH"
  json=$(jq --arg system "$SYSTEM" --arg hash "$HASH" \
    '.platforms[$system].hash = $hash' <<<"$json")
done
printf '%s\n' "$json" >"$RELEASE_JSON"
