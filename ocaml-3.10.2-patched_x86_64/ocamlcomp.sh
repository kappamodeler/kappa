#!/bin/sh

topdir=`dirname $0`

exec $topdir/ocamlc.opt -nostdlib -I $topdir/stdlib "$@"
