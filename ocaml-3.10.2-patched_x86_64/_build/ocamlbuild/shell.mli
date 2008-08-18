(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shell.mli,v 1.1 2007/02/07 08:59:14 ertai Exp $ *)
(* Original author: Nicolas Pouillard *)
val is_simple_filename : string -> bool
val quote_filename_if_needed : string -> string
val chdir : string -> unit
val rm : string -> unit
val rm_f : string -> unit
val rm_rf : string -> unit
val mkdir : string -> unit
val try_mkdir : string -> unit
val mkdir_p : string -> unit
val cp : string -> string -> unit
val mv : string -> string -> unit
val readlink : string -> string
val is_link : string -> bool
