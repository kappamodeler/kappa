#!/bin/sh
# $Id: partial-boot.sh,v 1.2.4.9 2007/05/22 10:54:59 pouillar Exp $
set -ex
cd `dirname $0`/..
OCAMLBUILD_PARTIAL="true"
export OCAMLBUILD_PARTIAL
mkdir -p _build
cp -rf boot _build/
./build/mkconfig.sh
./build/mkmyocamlbuild_config.sh
./build/boot.sh
