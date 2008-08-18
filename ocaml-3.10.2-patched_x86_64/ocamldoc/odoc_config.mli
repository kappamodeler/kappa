(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_config.mli,v 1.1.20.2 2007/03/07 08:50:05 xleroy Exp $ *)

(** Ocamldoc configuration contants. *)

(** Default path to search for custom generators and to install them. *)
val custom_generators_path : string

(** A flag to indicate whether to print ocamldoc warnings or not. *)
val print_warnings : bool ref
