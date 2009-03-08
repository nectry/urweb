(** Row folding *)

con folder = K ==> fn r :: {K} =>
                      tf :: ({K} -> Type)
                      -> (nm :: Name -> v :: K -> r :: {K} -> tf r
                          -> [[nm] ~ r] => tf ([nm = v] ++ r))
                      -> tf [] -> tf r

structure Folder = struct
    fun fold K (r ::: {K}) (fl : folder r) = fl

    fun nil K (tf :: {K} -> Type)
            (f : nm :: Name -> v :: K -> r :: {K} -> tf r
                 -> [[nm] ~ r] => tf ([nm = v] ++ r))
            (i : tf []) = i

    fun cons K (r ::: {K}) (nm :: Name) (v :: K) [[nm] ~ r] (fold : folder r)
             (tf :: {K} -> Type)
             (f : nm :: Name -> v :: K -> r :: {K} -> tf r
                  -> [[nm] ~ r] => tf ([nm = v] ++ r))
             (i : tf []) = f [nm] [v] [r] (fold [tf] f i) !

    fun concat K (r1 ::: {K}) (r2 ::: {K}) [r1 ~ r2]
        (f1 : folder r1) (f2 : folder r2)
        (tf :: {K} -> Type)
        (f : nm :: Name -> v :: K -> r :: {K} -> tf r
             -> [[nm] ~ r] => tf ([nm = v] ++ r))
        (i : tf []) =
        f1 [fn r1' => [r1' ~ r2] => tf (r1' ++ r2)]
           (fn (nm :: Name) (v :: K) (r1' :: {K}) (acc : [r1' ~ r2] => tf (r1' ++ r2))
                            [[nm] ~ r1'] [[nm = v] ++ r1' ~ r2] =>
               f [nm] [v] [r1' ++ r2] acc !)
           (fn [[] ~ r2] => f2 [tf] f i) !

    fun mp K1 K2 (f ::: K1 -> K2) (r ::: {K1})
        (fold : folder r)
        (tf :: {K2} -> Type)
        (f : nm :: Name -> v :: K2 -> r :: {K2} -> tf r
             -> [[nm] ~ r] => tf ([nm = v] ++ r))
        (i : tf []) =
        fold [fn r => tf (map f r)]
        (fn (nm :: Name) (v :: K1) (rest :: {K1}) (acc : tf (map f rest)) [[nm] ~ rest] =>
            f [nm] [f v] [map f rest] acc !)
        i
end


fun not b = if b then False else True

con idT (t :: Type) = t
con record (t :: {Type}) = $t
con fst = K1 ==> K2 ==> fn t :: (K1 * K2) => t.1
con snd = K1 ==> K2 ==> fn t :: (K1 * K2) => t.2
con fst3 = K1 ==> K2 ==> K3 ==> fn t :: (K1 * K2 * K3) => t.1
con snd3 = K1 ==> K2 ==> K3 ==> fn t :: (K1 * K2 * K3) => t.2
con thd3 = K1 ==> K2 ==> K3 ==> fn t :: (K1 * K2 * K3) => t.3

con mapUT = fn f :: Type => map (fn _ :: Unit => f)

con ex = fn tf :: (Type -> Type) =>
            res ::: Type -> (choice :: Type -> tf choice -> res) -> res

fun ex (tf :: (Type -> Type)) (choice :: Type) (body : tf choice) : ex tf =
 fn (res ::: Type) (f : choice :: Type -> tf choice -> res) =>
    f [choice] body

fun compose (t1 ::: Type) (t2 ::: Type) (t3 ::: Type)
            (f1 : t2 -> t3) (f2 : t1 -> t2) (x : t1) = f1 (f2 x)

fun txt (t ::: Type) (ctx ::: {Unit}) (use ::: {Type}) (_ : show t) (v : t) =
    cdata (show v)

fun foldUR (tf :: Type) (tr :: {Unit} -> Type)
           (f : nm :: Name -> rest :: {Unit}
                -> [[nm] ~ rest] =>
                      tf -> tr rest -> tr ([nm] ++ rest))
           (i : tr []) (r :: {Unit}) (fold : folder r)=
    fold [fn r :: {Unit} => $(mapUT tf r) -> tr r]
         (fn (nm :: Name) (t :: Unit) (rest :: {Unit}) acc
                          [[nm] ~ rest] r =>
             f [nm] [rest] ! r.nm (acc (r -- nm)))
         (fn _ => i)

fun foldUR2 (tf1 :: Type) (tf2 :: Type) (tr :: {Unit} -> Type)
           (f : nm :: Name -> rest :: {Unit}
                -> [[nm] ~ rest] =>
                      tf1 -> tf2 -> tr rest -> tr ([nm] ++ rest))
           (i : tr []) (r :: {Unit}) (fold : folder r) =
    fold [fn r :: {Unit} => $(mapUT tf1 r) -> $(mapUT tf2 r) -> tr r]
         (fn (nm :: Name) (t :: Unit) (rest :: {Unit}) acc
                          [[nm] ~ rest] r1 r2 =>
             f [nm] [rest] ! r1.nm r2.nm (acc (r1 -- nm) (r2 -- nm)))
         (fn _ _ => i)

fun foldURX2 (tf1 :: Type) (tf2 :: Type) (ctx :: {Unit})
           (f : nm :: Name -> rest :: {Unit}
                -> [[nm] ~ rest] =>
                      tf1 -> tf2 -> xml ctx [] []) =
    foldUR2 [tf1] [tf2] [fn _ => xml ctx [] []]
            (fn (nm :: Name) (rest :: {Unit}) [[nm] ~ rest] v1 v2 acc =>
                <xml>{f [nm] [rest] ! v1 v2}{acc}</xml>)
            <xml/>

fun foldR K (tf :: K -> Type) (tr :: {K} -> Type)
           (f : nm :: Name -> t :: K -> rest :: {K}
                -> [[nm] ~ rest] =>
                      tf t -> tr rest -> tr ([nm = t] ++ rest))
           (i : tr []) (r :: {K}) (fold : folder r) =
    fold [fn r :: {K} => $(map tf r) -> tr r]
             (fn (nm :: Name) (t :: K) (rest :: {K}) (acc : _ -> tr rest)
                              [[nm] ~ rest] r =>
                 f [nm] [t] [rest] ! r.nm (acc (r -- nm)))
             (fn _ => i)

fun foldR2 K (tf1 :: K -> Type) (tf2 :: K -> Type) (tr :: {K} -> Type)
            (f : nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
            (i : tr []) (r :: {K}) (fold : folder r) =
    fold [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> tr r]
         (fn (nm :: Name) (t :: K) (rest :: {K})
                          (acc : _ -> _ -> tr rest) [[nm] ~ rest] r1 r2 =>
             f [nm] [t] [rest] ! r1.nm r2.nm (acc (r1 -- nm) (r2 -- nm)))
         (fn _ _ => i)

fun foldRX K (tf :: K -> Type) (ctx :: {Unit})
            (f : nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf t -> xml ctx [] []) =
    foldR [tf] [fn _ => xml ctx [] []]
          (fn (nm :: Name) (t :: K) (rest :: {K}) [[nm] ~ rest] r acc =>
              <xml>{f [nm] [t] [rest] ! r}{acc}</xml>)
          <xml/>

fun foldRX2 K (tf1 :: K -> Type) (tf2 :: K -> Type) (ctx :: {Unit})
             (f : nm :: Name -> t :: K -> rest :: {K}
                  -> [[nm] ~ rest] =>
                        tf1 t -> tf2 t -> xml ctx [] []) =
    foldR2 [tf1] [tf2] [fn _ => xml ctx [] []]
           (fn (nm :: Name) (t :: K) (rest :: {K}) [[nm] ~ rest]
                            r1 r2 acc =>
               <xml>{f [nm] [t] [rest] ! r1 r2}{acc}</xml>)
           <xml/>

fun queryX (tables ::: {{Type}}) (exps ::: {Type}) (ctx ::: {Unit})
           [tables ~ exps] (q : sql_query tables exps)
           (f : $(exps ++ map (fn fields :: {Type} => $fields) tables)
                -> xml ctx [] []) =
    query q
          (fn fs acc => return <xml>{acc}{f fs}</xml>)
          <xml/>

fun queryX' (tables ::: {{Type}}) (exps ::: {Type}) (ctx ::: {Unit})
            [tables ~ exps] (q : sql_query tables exps)
            (f : $(exps ++ map (fn fields :: {Type} => $fields) tables)
                 -> transaction (xml ctx [] [])) =
    query q
          (fn fs acc =>
              r <- f fs;
              return <xml>{acc}{r}</xml>)
          <xml/>

fun oneOrNoRows (tables ::: {{Type}}) (exps ::: {Type})
                [tables ~ exps]
                (q : sql_query tables exps)  =
    query q
          (fn fs _ => return (Some fs))
          None

fun oneRow (tables ::: {{Type}}) (exps ::: {Type})
                [tables ~ exps] (q : sql_query tables exps) =
    o <- oneOrNoRows q;
    return (case o of
                None => error <xml>Query returned no rows</xml>
              | Some r => r)

fun eqNullable (tables ::: {{Type}}) (agg ::: {{Type}}) (exps ::: {Type})
    (t ::: Type) (_ : sql_injectable (option t))
    (e1 : sql_exp tables agg exps (option t))
    (e2 : sql_exp tables agg exps (option t)) =
    (SQL ({e1} IS NULL AND {e2} IS NULL) OR {e1} = {e2})

fun eqNullable' (tables ::: {{Type}}) (agg ::: {{Type}}) (exps ::: {Type})
    (t ::: Type) (_ : sql_injectable (option t))
    (e1 : sql_exp tables agg exps (option t))
    (e2 : option t) =
    case e2 of
        None => (SQL {e1} IS NULL)
      | Some _ => sql_binary sql_eq e1 (sql_inject e2)