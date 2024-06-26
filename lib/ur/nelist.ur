type t a = {
     First : a,
     Rest : list a
}

fun toList [a] (nel : t a) : list a =
    nel.First :: nel.Rest

fun fromList [a] (ls : list a) : t a =
    case ls of
        [] => error <xml>Nelist.fromList: input empty</xml>
      | x :: ls' => {First = x, Rest = ls'}

fun fromListResult [a] (ls : list a) : result (t a) =
    case ls of
        [] => Failure <xml>Nelist.fromList: input empty</xml>
      | x :: ls' => Success {First = x, Rest = ls'}

fun app [m] [a] (_ : monad m) (f : a -> m unit) (nel : t a) : m unit =
    f nel.First; List.app f nel.Rest

fun mp [a] [b] (f : a -> b) (nel : t a) : t b =
    {First = f nel.First, Rest = List.mp f nel.Rest}

fun mapi [a] [b] (f : int -> a -> b) (nel : t a) : t b =
    {First = f 0 nel.First, Rest = List.mapi (fn i x => f (i + 1) x) nel.Rest}

fun mapM [m] [a] [b] (_ : monad m) (f : a -> m b) (nel : t a) : m (t b) =
    fst <- f nel.First;
    rst <- List.mapM f nel.Rest;
    return {First = fst, Rest = rst}

fun mapX [a] [ctx] (f : a -> xml ctx [] []) (nel : t a) : xml ctx [] [] = <xml>
  {f nel.First}
  {List.mapX f nel.Rest}
</xml>

fun mem [a] (_ : eq a) (a : a) (nelist : t a) : bool =
  nelist.First = a || List.mem a nelist.Rest

fun exists [a] (f : a -> bool) (nel : t a) : bool =
    f nel.First || List.exists f nel.Rest

fun existsM [m] (_ : monad m) [a] (f : a -> m bool) (nel : t a) : m bool =
    b <- f nel.First;
    if b then
        return True
    else
        List.existsM f nel.Rest

fun rev [a] (nel : t a) : t a =
    let
        val rest = List.rev nel.Rest
    in
        case rest of
            [] => nel
          | last :: rest' => {First = last, Rest = List.append rest' (nel.First :: [])}
    end

fun snoc [a] (nel : t a) (x : a) : t a =
    {First = nel.First, Rest = List.append nel.Rest (x :: [])}

fun findM [m] (_ : monad m) [a] (f : a -> m bool) (nel : t a) : m (option a) =
    b <- f nel.First;
    if b then
        return (Some nel.First)
    else
        List.findM f nel.Rest

fun sort [a] (f : a -> a -> bool) (nel : t a) : t a =
    fromList (List.sort f (toList nel))

fun json_t [a] (_ : Json.json a) =
    Json.json_derived fromListResult toList

fun search [a] [b] (f : a -> option b) (nel : t a) =
    case f nel.First of
        None => List.search f nel.Rest
      | v => v
