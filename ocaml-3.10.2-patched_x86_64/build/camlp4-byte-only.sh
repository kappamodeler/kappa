#!/bin/sh
# $Id: camlp4-byte-only.sh,v 1.2.4.3 2007/03/12 11:58:48 pouillar Exp $
set -e
OCAMLBUILD_PARTIAL="true"
export OCAMLBUILD_PARTIAL
cd `dirname $0`/..
. build/targets.sh
set -x
$OCAMLBUILD $@ byte_stdlib_partial_mode $OCAMLC_BYTE $OCAMLLEX_BYTE $CAMLP4_BYTE
