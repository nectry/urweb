datatype t = datatype Basis.option

val monad : monad t

val eq : a ::: Type -> eq a -> eq (t a)
val ord : a ::: Type -> ord a -> ord (t a)

val isNone : a ::: Type -> t a -> bool
val isSome : a ::: Type -> t a -> bool

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b
val app : m ::: (Type -> Type) -> a ::: Type -> monad m -> (a -> m {}) -> t a -> m {}
val bind : a ::: Type -> b ::: Type -> (a -> option b) -> t a -> t b

val get : a ::: Type -> a -> option a -> a
val getOrError : a ::: Type -> xbody -> option a -> a
val unsafeGet : a ::: Type -> option a -> a

val mapM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type -> (a -> m b) -> t a -> m (t b)

val unOption : a ::: Type -> b ::: Type -> b -> (a -> b) -> option a -> b

(* `guard` is generally useful in a monadic context, but only when the monad is
also an `Alternative`.  Ur/Web doesn't have `Alternative` (or `Monoid`) yet, so
we stick it here. *)
val guard : bool -> option unit
