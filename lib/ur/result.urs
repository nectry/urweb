(* Be aware that this doesn't precisely obey the monad laws!  However, if you
treat all failure states as isomporphic (i.e., that the string contained in them
is more of a debug value than something with guaranteed semantics), then the
laws are okay.  Basically, this means that you shouldn't _rely_ on specific
error messages always coming out. *)
val monad : monad result

val show_result : a ::: Type -> show a -> show (result a)
val eq : a ::: Type -> eq a -> eq (result a)

val isFailure : a ::: Type -> result a -> bool
val isSuccess : a ::: Type -> result a -> bool

val mp : a ::: Type -> b ::: Type -> (a -> b) -> result a -> result b
val bind : a ::: Type -> b ::: Type -> (a -> result b) -> result a -> result b

val get : a ::: Type -> a -> result a -> a
val errorGet : a ::: Type -> result a -> a

(* readResult returns a Failure if the string cannot be read *)
val readResult : t ::: Type -> read t -> string -> result t

(* `guard` is generally useful in a monadic context, but only when the monad is
also an `Alternative`.  Ur/Web doesn't have `Alternative` (or `Monoid`) yet, so
we stick it here. The guard takes an error message. *)
val guard : bool -> xbody -> result unit


