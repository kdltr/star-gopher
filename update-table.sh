#!/bin/sh

DATABASE_FILE=$1; shift
FILE=$1; shift
TABLE=$1; shift

printf ".mode csv\n.separator ;\n
BEGIN TRANSACTION;
DELETE FROM $TABLE;
.import $FILE $TABLE
COMMIT;
" | chicken-sqlite3 $DATABASE_FILE
