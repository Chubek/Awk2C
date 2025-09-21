(lang dune 3.19)

(name awk2c)

(generate_opam_files true)

(source
 (github Chubek/Awk2C))

(authors "Chubak Bidpaa <chubakbidpaa@riseup.net>")

(license LICENSE)

(documentation https://chubek.github.io/awk2c)

(package
 (name awk2c)
 (synopsis "A compiler for Awk targeting self-contained C")
 (depends ocaml)
 (tags
  ("awk" "unix" "programming languages" "compiler" "transpiler" "awk transpiler")))

