let bindir = ref "/usr/local/bin";;
let libdir = ref (try Filename.concat (Sys.getenv "OCAMLLIB") "ocamlbuild" with Not_found -> "/usr/local/lib/ocaml/ocamlbuild");;
