#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LIB_DIR="$SCRIPT_DIR/lib"
REPAST_VERSION="2.11.0"
REPAST_URL="https://github.com/Repast/repast.simphony/releases/download/v.${REPAST_VERSION}/repast.simphony.updatesite.${REPAST_VERSION}.zip"
CORE_PLUGIN="repast.simphony.updatesite/plugins/repast.simphony.core_${REPAST_VERSION}.jar"
RUNTIME_PLUGIN="repast.simphony.updatesite/plugins/repast.simphony.runtime_${REPAST_VERSION}.jar"

# Skip if JARs already present
if ls "$LIB_DIR"/*.jar 1>/dev/null 2>&1; then
    echo "lib/ already contains JARs, skipping download."
    exit 0
fi

echo "Downloading Repast Simphony ${REPAST_VERSION} update site..."
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

curl -sL -o "$WORK/repast-updatesite.zip" "$REPAST_URL"

echo "Extracting plugins..."
unzip -o -q "$WORK/repast-updatesite.zip" "$CORE_PLUGIN" "$RUNTIME_PLUGIN" -d "$WORK"

echo "Extracting dependency JARs..."

# Extract core plugin (Repast classes under bin/, dependency JARs under lib/)
(cd "$WORK" && jar xf "$CORE_PLUGIN" bin/ lib/)

# Extract runtime plugin (contains saf.core.runtime.jar with MessageCenter)
mkdir -p "$WORK/runtime-tmp"
(cd "$WORK/runtime-tmp" && jar xf "$WORK/$RUNTIME_PLUGIN" lib/)

# Repackage Repast core classes (stored under bin/ in the plugin JAR) as a normal JAR
jar cf "$WORK/repast-simphony-core.jar" -C "$WORK/bin" .

# Copy everything to project lib/
cp "$WORK/repast-simphony-core.jar" "$LIB_DIR/"
cp "$WORK/lib/"*.jar "$LIB_DIR/"
cp -n "$WORK/runtime-tmp/lib/"*.jar "$LIB_DIR/" 2>/dev/null || true

echo "Done. $(ls "$LIB_DIR"/*.jar | wc -l | tr -d ' ') JARs installed in $LIB_DIR/"
