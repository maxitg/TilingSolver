#!/usr/bin/env bash

tilerRoot=$(cd "$(dirname "$0")" && pwd)
cd "$tilerRoot"

entitlement=$1
size1=$2
size2=$3
maskID=$4

echo "Searching minimal sets for "$size1"-"$size2"-"$maskID"..."
wolframscript -entitlement "$1" -code "<<MinimalSetsSearcher.wl; FindMinimalSets[{"$size1", "$size2"}, "$maskID"];"

echo "Done. Committing the results..."
git add .
git commit -m "Complete results for "$size1"-"$size2"-"$maskID"."
git push

echo "Shutting down..."
sudo shutdown
