open Json

type a = {
     X : int,
     Y : string
}

type b = {
     Foo : a,
     Bar : a
}

(*type c = {
     Baz : b,
     Qux : b
}*)

val json_a : json a = json_record {X = "X", Y = "Y"}
val json_b : json b = json_record {Foo = "Foo", Bar = "Bar"}
(*val json_c : json c = json_record {Baz = "Baz", Qux = "Qux"}*)

fun main (s : string) : transaction page = return (txt ((fromJson s : b).Foo.X))
