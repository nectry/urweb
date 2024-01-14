(* A (concrete) type of nonempty lists, for cases when we want the type system
 * to enforce that a list must have elements. *)
type t a = {
     First : a,
     Rest : list a
}

(* Converting to and from normal lists.  The former may raise an error. *)
val toList : a ::: Type -> t a -> list a
val fromList : a ::: Type -> list a -> t a

(* Causing a side effect for every element of a list *)
val app : m ::: (Type -> Type) -> a ::: Type
          -> monad m -> (a -> m unit) -> t a -> m unit

(** Different forms of mapping *)

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b
(* Garden variety *)

val mapi : a ::: Type -> b ::: Type -> (int -> a -> b) -> t a -> t b
(* Passing numeric index within the list *)

val mapM : m ::: (Type -> Type) -> a ::: Type -> b ::: Type
           -> monad m -> (a -> m b) -> t a -> m (t b)
(* With arbitrary monadic side effects *)

val mapX : a ::: Type -> ctx ::: {Unit} -> (a -> xml ctx [] []) -> t a -> xml ctx [] []
(* Producing XML *)


(** Search for elements matching a predicate *)

val exists : a ::: Type -> (a -> bool) -> t a -> bool
(* Garden variety *)

val existsM : m ::: (Type -> Type) -> monad m -> a ::: Type -> (a -> m bool) -> t a -> m bool
(* With arbitrary monadic side effects.  (Note here that we stop the search
 * early when finding a match! *)

val findM : m ::: (Type -> Type) -> monad m -> a ::: Type -> (a -> m bool) -> t a -> m (option a)
(* Like above but also returns the list element that was found *)

val search : a ::: Type -> b ::: Type -> (a -> option b) -> t a -> option b


(** Miscellaneous *)

val rev : a ::: Type -> t a -> t a
(* Reversal *)
val snoc : a ::: Type -> t a -> a -> t a
(* Adding an element to the end of a list *)

val sort : a ::: Type -> (a -> a -> bool) (* > predicate *) -> t a -> t a

val json_t : a ::: Type -> Json.json a -> Json.json (t a)
(* Support for JSON serialization and deserialization *)
