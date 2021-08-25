#!/usr/bin/env bash

tilerRoot=$(cd "$(dirname "$0")" && pwd)
cd "$tilerRoot"

entitlement=$1
size1=$2
size2=$3
maskID=$4

git pull
echo "Searching minimal sets for "$size1"-"$size2"-"$maskID"..."
wolframscript -entitlement "$1" -code "<<MinimalSetsSearcher.wl; FindMinimalSets[{"$size1", "$size2"}, "$maskID"];"

echo "Done. Committing the results..."
git checkout -b "auto-"$size1"-"$size2"-"$maskID""
git add .
git commit -m "Complete results for "$size1"-"$size2"-"$maskID"."
git push -u origin "auto-"$size1"-"$size2"-"$maskID""

echo "Shutting down..."
sudo shutdown
