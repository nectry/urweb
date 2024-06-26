type t = char

val isAlnum : t -> bool
val isAlpha : t -> bool
val isBlank : t -> bool
val isCntrl : t -> bool
val isDigit : t -> bool
val isGraph : t -> bool
val isLower : t -> bool
val isPrint : t -> bool
val isPunct : t -> bool
val isSpace : t -> bool
val isUpper : t -> bool
val isXdigit : t -> bool
val toLower : t -> t
val toUpper : t -> t

val toInt : t -> int
val fromInt : int -> t

(* toHex and fromHex only work with characters in the hex range, i.e., digits
and letters A-F.  The int piece must/will be between 0 and 15 inclusive. *)
val toHex : t -> int
val fromHex : int -> t