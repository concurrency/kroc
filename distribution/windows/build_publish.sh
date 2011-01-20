#!/bin/bash

VERSION=`cat output/VERSION`
OUTPUT=output
ARCHIVE_FILENAME=Transterpreter-win-dev-$VERSION.zip
ARCHIVE_PATH=$OUTPUT/$ARCHIVE_FILENAME

FILES_PATH=/data/www/org/transterpreter-download/files/dev/win/zip
FEED_PATH=/srv/www/org/transterpreter.download/cumulative-feeds/win-dev-zip/feed.xml
CHANGES_PATH=/srv/www/org/transterpreter.download/cumulative-feeds/win-dev-zip/changes/
HOST=clj@concurrency.cc

DOWNLOAD_BASE_URL="http://download.transterpreter.org/files/dev/win/zip"
DOWNLOAD_URL="$DOWNLOAD_BASE_URL/$ARCHIVE_FILENAME"

SIZE=$(ls -l "$ARCHIVE_PATH" | awk '{print $5}')
PUBDATE=$(date +"%a, %d %b %G %T %z")

if ! [ -f $ARCHIVE_PATH ] ; then
   echo "$ARCHIVE_PATH does not exist"
   echo "Perhaps you did not build_app.py with a clean output directory"
   exit 1
fi

cat >build/feed.xml <<EOF
<?xml version="1.0"?>
<rss version="2.0" xmlns:sparkle="http://www.andymatuschak.org/xml-namepsaces/sparkle">
  <channel>
    <title>Transterpreter Downloads (Win; Zip; Development branch)</title>
    <link>http://download.transterpreter.org/appcast/win-dev-zip.xml</link>
    <description>Transterpreter Appcast</description>
    <language>en-uk</language>
    <pubDate>$PUBDATE</pubDate>
    <lastBuildDate>$PUBDATE</lastBuildDate>
    <docs>http://blogs.law.harvard.edu/tech/rss</docs>
    <generator>A three-headed monkey!</generator>
    <webMaster>webmaster@transterpreter.com</webMaster>

                <item>
                        <title>Transterpreter - Development Version $VERSION</title>
                        <description><![CDATA[
			<style>
				\$include(changelog.css)
			</style>

			\$changes

			    <h3>Development version</h3>
			    <p>Please note that you are using a development version of the
			    Transterpreter, which may break, blow up,
			    dissintigrate, or otherwise harm itself at any
			    point. <!--If you meant to run something less prone to
			    breakage, please go to
			    <a href="http://www.transterpreter.org/">http://www.transterpreter.org/</a>
			    and download one of
			    the official releases.--></p>

                        ]]></description>
                        <pubDate>$PUBDATE</pubDate>
                        <enclosure
                                url="$DOWNLOAD_URL"
                                sparkle:version="$VERSION"
                                type="application/octet-stream"
                                length="$SIZE"
                        />
                </item>
  </channel>
</rss>
EOF

# FIXME: There is something wrong with the above XML fragment which does not
# seem to parse with WinSparkle... works fine in Sparkle for the Mac.
# Therefore we use the XML from the WinSparkle example, which seems to work


cat >build/feed.xml <<EOF
<?xml version="1.0" encoding="utf-8"?>
<rss version="2.0" xmlns:sparkle="http://www.andymatuschak.org/xml-namespaces/sparkle">
<channel>
    <title>Transterpreter Downloads (Win; Zip; Development branch)</title>
    <link>http://download.transterpreter.org/appcast/win-dev-zip.xml</link>
    <description>Transterpreter Appcast</description>
    <language>en-uk</language>
    <item>
      <title>Transterpreter - Development Version $VERSION</title>
      <description><![CDATA[
			<style>
				\$include(changelog.css)
			</style>

			\$changes

			    <h3>Development version</h3>
			    <p>Please note that you are using a development version of the
			    Transterpreter, which may break, blow up,
			    dissintigrate, or otherwise harm itself at any
			    point. <!--If you meant to run something less prone to
			    breakage, please go to
			    <a href="http://www.transterpreter.org/">http://www.transterpreter.org/</a>
			    and download one of
			    the official releases.--></p>


          ]]>
      </description>
      <pubDate>$PUBDATE</pubDate>
      <enclosure url="$DOWNLOAD_URL"
                 sparkle:version="$VERSION"
                 type="application/octet-stream"
                 length="$SIZE"/>
    </item>
  </channel>
</rss>
EOF

if ! [ -f "changes/version_$VERSION.markdown" ] ; then
  svn mv changes/version_CURRENT.markdown changes/version_$VERSION.markdown && \
  echo -e "# Changes Since $VERSION\n\n" > changes/version_CURRENT.markdown && \
  svn add changes/version_CURRENT.markdown && \
  svn commit changes/version_$VERSION.markdown \
             changes/version_CURRENT.markdown \
     -m "Win Dist: (automatic commit) activating changelog entry"
fi

scp "changes/version_$VERSION.markdown" $HOST:$CHANGES_PATH
scp "$ARCHIVE_PATH" $HOST:$FILES_PATH
scp "build/feed.xml" $HOST:$FEED_PATH
