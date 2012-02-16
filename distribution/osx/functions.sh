
process()
{
	# Check if we got one or two parameters, if once, we drop the file in place,
	# but without the .in extension. Otherwise we use the given filename as the
	# output file
	infile=$1
	if [ "Z$2" = "Z" ] ; then
		outfile=`echo "$1" | sed -e 's/\.in//'`
	else
		outfile=$2
	fi

	contents=`cat "$infile"`
	echo "Xforming: $infile -> $outfile"
	eval "echo \"$contents\"" > $outfile
}

makedir()
{
	oldifs=$IFS
	IFS="/"

	directory="$1"
	buildupDir=
	slash=

	for dir in $directory ; do
		buildupDir="$buildupDir$slash$dir"
		if ! [ -d "$buildupDir" ] ; then
			echo -n "Make dir: "
			mkdir -v "$buildupDir"
		fi
		slash="/"
	done

	IFS=$oldifs
}

copydir_exclude()
{
	OLDIFS=$IFS

	path_remove="$1"
	excludes="$3"

	# Make the exclude list
	IFS=";"
	exclude_patterns=""
	semicolon=""
	for pattern in $excludes ; do
		exclude_patterns="$exclude_patterns$semicolon/$pattern/d"
		semicolon=";"
	done

	IFS=$'\x0A'$'\x0D' # Line feed, carriage return

	files=`find "$1" | sed -e "$exclude_patterns"`
	#dest_files=`echo "$files" | sed -e "s!$path_remove!$2/! ; s/\/\/*/\//g"`

	for file in $files ; do
		dest=`echo "$file" | sed -e "s!$path_remove!$2/! ; s/\/\/*/\//g"`
		if [ -L "$file" ] ; then
			echo -n "Symlink:  " 
			ln -v -f -h -s `readlink "$file"` "$dest"
		elif [ -d "$file" ] ; then
			if ! [ -d "$dest" ] ; then
				makedir "$dest"
			fi
		else
			hasin=`expr "$file" : '.*\(\.in\)$' || echo""`
			if ! [ "Z$hasin" = "Z" ] ; then
				process "$file" "${dest%%.in}"
			else
				echo -n "Copy:     "
				cp -f -v "$file" "$dest" || ( echo "Error while copying $file" ; exit 1 )
			fi
		fi
	done
	IFS=$OLDIFS
}

copydir_stdexcludes()
{
	excludes="versions\.txt;\.svn;\.DS_Store;\.swp"
	if ! [ "$3" = "" ] ; then
		excludes="$excludes;$3"
	fi
	copydir_exclude "$1" "$2" "$excludes"
}

copydir_addexcludes()
{
  copydir_stdexcludes "$1" "$2" "$3"
}

copydir()
{
	copydir_stdexcludes "$1" "$2" ""
}

copyfile()
{
	echo -n "Copy:       "
	cp -f -v `echo "$1"` "$2" || ( echo "Error while copying $1" ; exit 1 )
}

