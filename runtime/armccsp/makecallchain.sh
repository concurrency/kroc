#! /bin/bash

# script to generate the API call chain file for the EV3 run-time

TMPFILE=/tmp/gencallchain.tmp.$$
TMPFILE2=/tmp/gencallchain.tmp2.$$

rm -f $TMPFILE $TMPFILE2

for file in *.[ch]; do
	cat $file | grep '@APICALLCHAIN:' | sed -e 's/^\/\*[ \t]*@APICALLCHAIN:[ \t]*//' -e 's/\*\/$//' >> $TMPFILE
done

cat $TMPFILE | while [ : ]; do
	read || break

	FNAME=$(printf '%s\n' "$REPLY" | cut -d : -f 1-1)

	# have =? thing?
	printf '%s\n' "$REPLY" | grep -w -- '=?' > /dev/null && INCSIZE=1 || INCSIZE=0

	if [ $INCSIZE -eq 1 ]; then
		STKBYTES=$(cat *.su | grep -w $FNAME | awk '{print($2);}' | head -1)
		STKQUAL=$(cat *.su | grep -w $FNAME | awk '{print($3);}' | head -1)

		if [ "$STKBYTES" == "" ] || [ "$STKQUAL" == "" ]; then
			printf '%s: no .su entry for [%s]\n' "$0" "$FNAME" 1>&2
		elif [ "$STKQUAL" == "static" ] || [ "$STKQUAL" == "dynamic,bounded" ]; then
			printf '%s\n' "$REPLY" | sed -e 's/=?/='$STKBYTES'/' >> $TMPFILE2
		else
			printf '%s: bad qualifier [%s] for [%s]\n' "$0" "$STKQUAL" "$FNAME" 1>&2
		fi
	else
		printf '%s\n' "$REPLY" >> $TMPFILE2
	fi
done

cat $TMPFILE2

rm -f $TMPFILE $TMPFILE2

