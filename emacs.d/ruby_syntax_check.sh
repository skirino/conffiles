#!/bin/sh
#
# Avoid outputting "Syntax OK" on success
#
RUBY=`which ruby`
COMMAND="$RUBY -c -w -W2 $*"
OUT=`$COMMAND 2>/dev/null`
ERR=`$COMMAND 2>&1 1>/dev/null`

echo $ERR >&2
[ "$OUT" = "Syntax OK" ] && [ -z "$ERR" ]
