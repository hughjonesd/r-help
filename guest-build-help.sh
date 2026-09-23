#!/bin/bash

# Each evercran image contains one or more R installations. Installed HTML is
# copied unchanged. Versions with compiled help databases then render the
# missing topic pages using their own version of R.

set -e

for RVERSION in /opt/R/*; do
  RBASE=$(basename "$RVERSION")
  VERSION=$RBASE
  case "$RBASE" in
    0.50-a1 | 0.50-a4 ) VERSION="0.50" ;;
    0.60.0 ) VERSION="0.60" ;;
  esac
  VERSIONDIR="site/$VERSION"
  mkdir -p "$VERSIONDIR"

  LIBRARY=""
  for CANDIDATE in "$RVERSION/library" "$RVERSION/lib/R/library" \
      "$RVERSION/share/R/library"; do
    if [ -d "$CANDIDATE" ]; then
      LIBRARY=$CANDIDATE
      break
    fi
  done

  if [ -n "$LIBRARY" ]; then
    for PKGDIR in "$LIBRARY"/*; do
      [ -f "$PKGDIR/help/AnIndex" ] || continue
      PACKAGE=$(basename "$PKGDIR")
      mkdir -p "$VERSIONDIR/$PACKAGE"
      if [ -d "$PKGDIR/html" ]; then
        cp -R "$PKGDIR/html/." "$VERSIONDIR/$PACKAGE/"
      fi
      awk -v package="$PACKAGE" -F '\t' \
        'NF >= 2 { print package "\t" $1 "\t" $2 }' \
        "$PKGDIR/help/AnIndex" >> "$VERSIONDIR/aliases.tsv"
    done

    if find "$LIBRARY" -path "*/help/*.rdb" -type f | grep -q .; then
      "$RVERSION/bin/R" --vanilla < guest-render-help.R
    fi
  elif [ -f "$RVERSION/help/AnIndex" ] && [ -d "$RVERSION/html" ]; then
    # The first releases have one help directory rather than packages.
    mkdir -p "$VERSIONDIR/base"
    cp -R "$RVERSION/html/." "$VERSIONDIR/base/"
    awk -F '\t' 'NF >= 2 { print "base\t" $1 "\t" $2 }' \
      "$RVERSION/help/AnIndex" >> "$VERSIONDIR/aliases.tsv"
  fi

  if [ ! -s "$VERSIONDIR/aliases.tsv" ] && [ -d "$RVERSION/html/funs" ]; then
    # R 0.50 has one HTML file per function and no AnIndex.
    mkdir -p "$VERSIONDIR/base"
    for HTML in "$RVERSION/html/funs/"*.html "$RVERSION/html/funs/".*.html; do
      [ -f "$HTML" ] || continue
      TOPIC=$(basename "$HTML" .html)
      cp "$HTML" "$VERSIONDIR/base/"
      printf 'base\t%s\t%s\n' "$TOPIC" "$TOPIC" >> "$VERSIONDIR/aliases.tsv"
    done
  fi
done
