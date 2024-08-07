type t = string

val str : char -> t

val length : t -> int
val lengthGe : t -> int -> bool

val append : t -> t -> t

val sub : t -> int -> char
val suffix : t -> int -> string

val index : t -> char -> option int
val sindex : {Haystack : t, Needle : t} -> option int
val atFirst : t -> char -> option string

val mindex : {Haystack : t, Needle : t} -> option int

val substring : t -> {Start : int, Len : int} -> string

val seek : t -> char -> option string
val mseek : {Haystack : t, Needle : t} -> option (char * string)

val split : t -> char -> option (string * string)
val split' : t -> char -> option (string * string) (* The matched character is kept at the beginning of the suffix. *)

(* Needle is a set of characters to look for. msplit returns the first instance
of the first character of the needle that it finds in the haystack, or None if
none are found. The char in the returned tuple is the needle that was found. *)
val msplit : {Haystack : t, Needle : t} -> option (string * char * string)

(* Needle is a possible substring of haystack. If found in the haystack,
return the substrings before and after the needle, otherwise return None. *)
val ssplit : {Haystack : t, Needle : t} -> option (string * string)

val all : (char -> bool) -> string -> bool
val mp : (char -> char) -> string -> string

val newlines : ctx ::: {Unit} -> [[Body] ~ ctx] => string -> xml ([Body] ++ ctx) [] []

val isPrefix : {Full : t, Prefix : t} -> bool

val trim : t -> t

(* The input string must be all hex characters.  The output is the number it represents in hex. *)
val parseHex : string -> int
