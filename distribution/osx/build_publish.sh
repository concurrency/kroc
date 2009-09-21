#!/bin/bash
set -o errexit

VERSION=`defaults read $PWD/output/Transterpreter.app/Contents/Info CFBundleVersion`
DOWNLOAD_BASE_URL="http://download.transterpreter.org/files/mac-dev"
#RELEASENOTES_URL="http://www.absolutepanic.org/pgX/release-notes.html#version-$VERSION"
FILES_PATH=/data/www/org/transterpreter-download/files/mac-dev
FEED_PATH=/data/www/org/transterpreter-download/feeds/mac-dev.xml
HOST=unhosting.org

ARCHIVE_FILENAME="Transterpreter-dev-$VERSION.zip"
DOWNLOAD_URL="$DOWNLOAD_BASE_URL/$ARCHIVE_FILENAME"
KEYCHAIN_PRIVKEY_NAME="Transterpreter.app Sparkle Private Key"

WD=$PWD

SIZE=$(stat -f %z "$ARCHIVE_FILENAME")
PUBDATE=$(date +"%a, %d %b %G %T %z")
SIGNATURE=$(
	openssl dgst -sha1 -binary < "$ARCHIVE_FILENAME" \
	| openssl dgst -dss1 -sign <(security find-generic-password -g -s "$KEYCHAIN_PRIVKEY_NAME" 2>&1 1>/dev/null | perl -pe '($_) = /"(.+)"/; s/\\012/\n/g') \
	| openssl enc -base64
)

[ $SIGNATURE ] || { echo Unable to load signing private key with name "'$KEYCHAIN_PRIVKEY_NAME'" from keychain; false; }

cat >build/feed.xml <<EOF
<?xml version="1.0"?>
<rss version="2.0" xmlns:sparkle="http://www.andymatuschak.org/xml-namepsaces/sparkle">
  <channel>
    <title>Transterpreter Downloads (Mac; Development branch)</title>
    <link>http://download.transterpreter.org/appcast/mac-dev.xml</link>
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
			    <h2>Development version</h2>
			    <p>You are using a development version of the
			    Transterpreter, which may break, blow up,
			    dissintigrate, or otherwise harm itself at any
			    point. If you meant to run something less prone to
			    breakage, please go to
			    <a href="http://www.transterpreter.org/"> and download one of
			    the official releases.</p>
			]]></description>
			<pubDate>$PUBDATE</pubDate>
			<enclosure
				url="$DOWNLOAD_URL"
				sparkle:version="$VERSION"
				type="application/octet-stream"
				length="$SIZE"
				sparkle:dsaSignature="$SIGNATURE"
			/>
		</item>
  </channel>
</rss>
EOF

scp "$ARCHIVE_FILENAME" $HOST:$FILES_PATH
scp "build/feed.xml" $HOST:$FEED_PATH
#echo scp "'$SOURCE_ROOT/release-notes/release-notes.html'" unhosting.org:/data/www/org/absolutepanic/www/pgX/
#echo scp "'$WD/appcast.xml'" www.example.com:web/software/my-cool-app/appcast.xml
