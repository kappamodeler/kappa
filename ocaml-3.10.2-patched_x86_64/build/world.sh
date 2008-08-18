#!/bin/sh
# $Id: world.sh,v 1.2.4.1 2007/03/12 11:58:48 pouillar Exp $
cd `dirname $0`
set -ex
./mkconfig.sh
./mkmyocamlbuild_config.sh
. ../config/config.sh
if [ "x$EXE" = "x.exe" ]; then
  ./boot-c-parts-windows.sh
else
  ./boot-c-parts.sh
fi
./boot.sh $@
./world.all.sh $@
