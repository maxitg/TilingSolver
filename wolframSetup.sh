#!/usr/bin/env bash
set -eo pipefail

echo "Setting DropboxDataDirectory as /Physics/Data/Tilings/minimal-sets-v3 in ~/.minimal-tiling-sets..."
echo '{"DropboxDataDirectory": "/Physics/Data/Tilings/minimal-sets-v3"}' > ~/.minimal-tiling-sets
echo "Done."