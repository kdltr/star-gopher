#!/bin/sh

url="https://data.explore.star.fr/explore/dataset//tco-bus-circulation-passages-tr/download?format=csv;timezone=%2B0200;use_labels_for_header=false;apikey=$STAR_GOPHER_API_KEY"

last_rows=0

while true
do
    tempfile="`mktemp`"
    curl -sLo "$tempfile" "$url"
    now_rows=`wc -l <"$tempfile"`
    echo "File contains $now_rows lines" 1>&2
    if test "$now_rows" -lt "$last_rows" && test `expr $now_rows % 100` -eq 0; then
        echo "Discarding…"
	sleep 10
    else
	last_rows="$now_rows"
        ./update-table.sh ./star.sqlite "$tempfile" passages
        sleep 60
    fi
    rm "$tempfile"
done
