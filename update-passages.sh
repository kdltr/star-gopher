#!/bin/sh

set -ex

url="https://data.explore.star.fr/explore/dataset//tco-bus-circulation-passages-tr/download?format=csv;timezone=%2B0200;use_labels_for_header=false;apikey=$STAR_GOPHER_API_KEY"

while true
do
    tempfile="`mktemp`"
    curl -Lo "$tempfile" "$url"
    ./update-table.sh ./star.sqlite "$tempfile" passages
    rm "$tempfile"
    sleep 60
done
