#!/bin/sh
# $Id: mkconfig.sh,v 1.1.4.4 2007/05/14 12:01:32 xleroy Exp $

cd `dirname $0`/..

sed -e 's/^\(.*\$([0-9]).*\)$/# \1/' \
    -e 's/\$(\([^)]*\))/${\1}/g' \
    -e 's/^\([^#=]*\)=\([^"]*\)$/if [ "x$\1" = "x" ]; then \1="\2"; fi/' \
    config/Makefile > config/config.sh


