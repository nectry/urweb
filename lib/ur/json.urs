(** The JSON text-based serialization format (with YAML as a bonus) *)

class json

val toJson : a ::: Type -> json a -> a -> string
val fromJson : a ::: Type -> json a -> string -> a
val fromJson' : a ::: Type -> json a -> string -> a * string

(* Versions of fromJson that return a `result` instead of erroring on failure. *)
val fromJsonR : a ::: Type -> json a -> string -> result a
val fromJsonR' : a ::: Type -> json a -> string -> result (a * string)

val toYaml : a ::: Type -> json a -> a -> string
val fromYaml : a ::: Type -> json a -> string -> a

(* Versions of fromJson that return a `result` instead of erroring on failure. *)
val fromYamlR : a ::: Type -> json a -> string -> result a
val fromYamlR' : a ::: Type -> json a -> string -> result (a * string)

val mkJson : a ::: Type -> {ToJson : a -> string,
                            FromJson : string -> result (a * string)} -> json a

val json_string : json string
(* NOTE: We don't follow the rules exactly on reading numeric values.
Specifically, we are more lenient than the JSON standard as we allow quoted
numbers as numbers.  We can do this because we interpret json based on a schema
(i.e., the type) rather than discovering what the values are based on form
alone. *)
val json_int : json int
val json_float : json float
val json_bool : json bool
val json_time : json time
val json_option : a ::: Type -> json a -> json (option a)
val json_list : a ::: Type -> json a -> json (list a)

(* By the way, time formatting follows RFC 3339, and we expose the more
 * primitive formatting functions here. *)
val rfc3339_out : time -> string
val rfc3339_in : string -> time
val rfc3339_in' : string -> result time

val json_record : ts ::: {Type} -> folder ts -> $(map json ts) -> $(map (fn _ => string) ts) -> json $ts

(* Make a json instance for a record where some fields are allowed to be
optional.  For required fields, simply provide the label, but for optional
fields, additionally provide the default value.  During serializing, if a field
contains its default value, it will be omitted, and during parsing, any field
that is missing will be filled in with its default value. *)
val json_record_withDefaults
  : ts ::: {Type} -> ots ::: {Type} -> [ts ~ ots]
  => folder ts -> $(map json ts) -> $(map (fn _ => string) ts)
  -> folder ots -> $(map json ots) -> $(map (fn t => string * t) ots)
  -> json $(ts ++ ots)

val json_record_withOptional : ts ::: {Type} -> ots ::: {Type} -> [ts ~ ots]
                               => folder ts -> $(map json ts) -> $(map (fn _ => string) ts)
                               -> folder ots -> $(map json ots) -> $(map (fn _ => string) ots)
                               -> json $(ts ++ map option ots)
val json_variant : ts ::: {Type} -> folder ts -> $(map json ts) -> $(map (fn _ => string) ts) -> json (variant ts)

(* A version of json_variant that doesn't use labels.  This is generally a bad
idea, as we can't tell apart two constructors that have the same payload, but
it's used in certain APIs and so is useful to us. NOTE: This plays poorly with
the note above for numeric types, as it means that a string containing a number
may be treated as a number even though it's quoted and designed to be treated as
a string *)
val json_variant_anon : ts ::: {Type} -> folder ts -> $(map json ts) -> json (variant ts)

val json_unit : json unit

val json_dict : a ::: Type -> json a -> json (list (string * a))
(* Simple key-value list, encoded in YAML with keys as labels on lines *)

val json_derived : base ::: Type -> derived ::: Type
                   -> (base -> result derived)
                   -> (derived -> base)
                   -> json base -> json derived

(* A version of json_derived where conversion cannot fail. *)
val json_derived' : base ::: Type -> derived ::: Type
                   -> (base -> derived)
                   -> (derived -> base)
                   -> json base -> json derived

functor Recursive (M : sig
                       con t :: Type -> Type
                       val json_t : a ::: Type -> json a -> json (t a)
                   end) : sig
    datatype r = Rec of M.t r

    val json_r : json r
end

datatype prim = String of string | Int of int | Float of float | Bool of bool
val json_prim : json prim
val show_prim : show prim

(* Make a JSON instance for a datatype.  Ur/Web doesn't have macros/templates,
so we convert to a variant and back, meaning we need the mappings to/from the
variant. *)
val json_datatype :
    t ::: Type ->
    ts ::: {Type} ->
    folder ts ->
    $(map json ts) ->
    $(map (fn _ => string) ts) ->
    $(map (fn t' => t' -> t) ts) ->
    (t -> variant ts) ->
    json t

(* Make a JSON instance for a recursive datatype.  Ur/Web doesn't have
macros/templates, so we convert to a variant and back, meaning we need the
mappings to/from the variant. *)
functor RecursiveDataType
    (M : sig
     con t :: Type
     con ts :: Type -> {Type}
     val fl : a ::: Type -> folder (ts a)
     val js : a ::: Type -> json a -> $(map json (ts a))
     val names : a ::: Type -> $(map (fn _ => string) (ts a))
     val to : a ::: Type -> (a -> t) -> $(map (fn t' => t' -> t) (ts a))
     val from : a ::: Type -> (t -> a) -> t -> variant (ts a)
     end) : sig
    val json_t : json M.t
end

(* Like above, but make the constructors anonymous, i.e., derived based on their
fields. *)
functor RecursiveDataTypeAnon
    (M : sig
     con t :: Type
     con ts :: Type -> {Type}
     val fl : a ::: Type -> folder (ts a)
     val js : a ::: Type -> json a -> $(map json (ts a))
     val to : a ::: Type -> (a -> t) -> $(map (fn t' => t' -> t) (ts a))
     val from : a ::: Type -> (t -> a) -> t -> variant (ts a)
     end) : sig
    val json_t : json M.t
end
