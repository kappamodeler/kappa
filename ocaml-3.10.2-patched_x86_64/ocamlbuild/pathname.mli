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

(* $Id: pathname.mli,v 1.1.4.2 2007/12/18 08:56:50 ertai Exp $ *)
(* Original author: Nicolas Pouillard *)
include Signatures.PATHNAME
val link_to_dir : t -> t -> bool
val normalize : t -> t
