#!/bin/sh

topdir=`dirname $0`

exec $topdir/ocamlopt.opt -nostdlib -I $topdir/stdlib "$@"
