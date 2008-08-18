(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ccomp.mli,v 1.11.6.1 2007/11/10 12:23:37 xleroy Exp $ *)

(* Compiling C files and building C libraries *)

val command: string -> int
val run_command: string -> unit
val compile_file: string -> int
val create_archive: string -> string list -> int
val expand_libname: string -> string
val quote_files: string list -> string
val quote_optfile: string option -> string
val make_link_options: string list -> string
val merge_manifest: string -> int
