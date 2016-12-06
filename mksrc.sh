#!/bin/bash
set -eu

# $1 must be a version number, like '098-beta4'

VERSION="$1"
shift 1

SRCDIR=src

#if [ -n "$IS_SOUND" ]; then
#  SOUND_SUFFIX=''
#else
#  SOUND_SUFFIX='-nosound'
#fi

SRC_BASE_NAME=valkyrie-"$VERSION"
# Prepare temp dir
DIST_TMP_PATH=pkg/"$SRC_BASE_NAME"/

rm -Rf "$DIST_TMP_PATH"
/bin/mkdir "$DIST_TMP_PATH" "$DIST_TMP_PATH"src

cp -R "$SRCDIR"/*.pas "$DIST_TMP_PATH"src
cp -R "$SRCDIR"/*.inc "$DIST_TMP_PATH"src
cp -R "$SRCDIR"/*.txt "$DIST_TMP_PATH"src
     
# Pack ----------------------------------------

DIST_ARCHIVE_NAME="$SRC_BASE_NAME".zip

cd pkg
rm -f "$DIST_ARCHIVE_NAME"
zip -r "$DIST_ARCHIVE_NAME" "$SRC_BASE_NAME" 
cd .. 

#tar czf "$DIST_ARCHIVE_NAME" "$ARCHIVE_BASE_NAME"
#pushd > /dev/null
cp pkg/"$DIST_ARCHIVE_NAME" .

echo 'Made '"$DIST_ARCHIVE_NAME"