#!/bin/bash
# Script to invoke DMLC using environment variables instead of hardcoded paths
# Uses DMLC_DIR and DMLC environment variables set by the caller

set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Determine host type
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    HOST_TYPE="win64"
else
    HOST_TYPE="linux64"
fi

# If DMLC_DIR is not set, use project's local build
if [ -z "$DMLC_DIR" ]; then
    DMLC_DIR="$PROJECT_DIR/$HOST_TYPE/bin"
    echo "DMLC_DIR not set, using project's local build: $DMLC_DIR"
fi

# If DIAGNOSTIC_JSON is not set, use default
if [ -z "$DIAGNOSTIC_JSON" ]; then
    DIAGNOSTIC_JSON="diagnostic.json"
fi

# Check if DMLC_DIR exists
if [ ! -d "$DMLC_DIR" ]; then
    echo "Error: DMLC_DIR not found at $DMLC_DIR"
    echo "Run 'make dmlc' first to build DMLC"
    exit 1
fi

# Get SIMICS_BASE from environment or extract from project's simics script
if [ -z "$SIMICS_BASE" ]; then
    SIMICS_BASE=$(grep "SIMICS_BASE_PACKAGE=" "$PROJECT_DIR/simics" | cut -d'=' -f2 | tr -d '"' | tr -d "'")
fi

if [ -z "$SIMICS_BASE" ]; then
    echo "Error: Could not determine SIMICS_BASE"
    exit 1
fi

# Get MODULE_DIR from the first argument (DML file path)
if [ -z "$1" ]; then
    echo "Error: DML file path required as argument"
    exit 1
fi

DML_FILE="$1"
MODULE_DIR=$(dirname "$DML_FILE")

# Invoke DMLC with the DML file as target
exec "$SIMICS_BASE/bin/mini-python" \
    "$DMLC_DIR/dml/python" \
    --ai-json="$DIAGNOSTIC_JSON" \
    --simics-api=7 \
    -I"$DMLC_DIR/dml/api/7" \
    -I"$DMLC_DIR/dml" \
    -I. \
    --info \
    -I. \
    -I"$MODULE_DIR" \
    "$DML_FILE"
