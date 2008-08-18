#!/bin/sh
# $Id: ocamlbuild-native-only.sh,v 1.2.4.4 2007/03/12 11:58:48 pouillar Exp $
set -e
OCAMLBUILD_PARTIAL="true"
export OCAMLBUILD_PARTIAL
cd `dirname $0`/..
. build/targets.sh
set -x
$OCAMLBUILD $@ native_stdlib_partial_mode $OCAMLOPT_BYTE $OCAMLLEX_BYTE $OCAMLBUILD_NATIVE
