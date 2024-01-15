fun show_result [a] (s : show a) =
  mkShow (fn r => case r of
      Failure e => "Failure of " ^ show e
    | Success v => "Success of " ^ @@show [a] s v)

fun eq [a] (_ : eq a) =
    mkEq (fn x y =>
             case (x, y) of
                 (Failure x, Failure y) => True
                 (* Is this scary?  Failures are failures... *)
               | (Success x, Success y) => x = y
               | _ => False)

(* fun ord [a] (_ : ord a) =
    mkOrd {Lt = fn x y =>
                   case (x, y) of
                       (Failure x, Failure y) => x < y
                     | (Failure _, Success _) => True
                     | (Success _, Failure _) => False
                     | (Success x, Success y) => x < y,
           Le = fn x y =>
                   case (x, y) of
                       (Failure x, Failure y) => x <= y
                     | (Failure _, Success _) => True
                     | (Success _, Failure _) => False
                     | (Success x, Success y) => x <= y} *)

fun isFailure [a] x =
    case x of
        Failure _ => True
      | Success _ => False

fun isSuccess [a] x =
    case x of
        Failure _ => False
      | Success _ => True

fun mp [a] [b] f x =
    case x of
        Failure e => Failure e
      | Success a => Success (f a)

fun bind [a] [b] f x =
    case x of
        Failure e => Failure e
      | Success a => f a

fun get [a] (x : a) (r : result a) =
    case r of
        Failure _ => x
      | Success v => v

fun errorGet [a] (r : result a) =
    case r of
        Failure e => error e
      | Success v => v

fun readResult [t] (_ : read t) (s : string) : result t =
  case read s of
    None => Failure <xml>Cannot read: {[s]}</xml>
  | Some v => Success v

fun guard (b : bool) (e : xbody) : result unit =
  if b then Success () else Failure e
