datatype t = datatype Basis.list

val show = fn [a] (_ : show a) =>
              let
                  fun show' (ls : list a) =
                      case ls of
                          [] => "[]"
                        | x :: ls => show x ^ " :: " ^ show' ls
              in
                  mkShow show'
              end

val eq = fn [a] (_ : eq a) =>
            let
                fun eq' (ls1 : list a) ls2 =
                    case (ls1, ls2) of
                        ([], []) => True
                      | (x1 :: ls1, x2 :: ls2) => x1 = x2 && eq' ls1 ls2
                      | _ => False
            in
                mkEq eq'
            end

fun foldl [a] [b] (f : a -> b -> b) =
    let
        fun foldl' acc ls =
            case ls of
                [] => acc
              | x :: ls => foldl' (f x acc) ls
    in
        foldl'
    end

fun foldli [a] [b] (f : int -> a -> b -> b) =
    let
        fun foldli' i acc ls =
            case ls of
                [] => acc
              | x :: ls => foldli' (i + 1) (f i x acc) ls
    in
        foldli' 0
    end

val rev = fn [a] =>
             let
                 fun rev' acc (ls : list a) =
                     case ls of
                         [] => acc
                       | x :: ls => rev' (x :: acc) ls
             in
                 rev' []
             end

fun foldr [a] [b] f (acc : b) (ls : list a) = foldl f acc (rev ls)

fun foldlAbort [a] [b] f =
    let
        fun foldlAbort' acc ls =
            case ls of
                [] => Some acc
              | x :: ls =>
                case f x acc of
                    None => None
                  | Some acc' => foldlAbort' acc' ls
    in
        foldlAbort'
    end

val length = fn [a] =>
                let
                    fun length' acc (ls : list a) =
                        case ls of
                            [] => acc
                          | _ :: ls => length' (acc + 1) ls
                in
                    length' 0
                end

fun snoc [a] (lst : t a) (a : a) : t a = rev (a :: rev lst)

fun foldlMapAbort [a] [b] [c] f =
    let
        fun foldlMapAbort' ls' acc ls =
            case ls of
                [] => Some (rev ls', acc)
              | x :: ls =>
                case f x acc of
                    None => None
                  | Some (x', acc') => foldlMapAbort' (x' :: ls') acc' ls
    in
        foldlMapAbort' []
    end

val revAppend = fn [a] =>
                   let
                       fun ra (ls : list a) acc =
                           case ls of
                               [] => acc
                             | x :: ls => ra ls (x :: acc)
                   in
                       ra
                   end

fun append [a] (ls1 : t a) (ls2 : t a) = revAppend (rev ls1) ls2

fun concat [a] (lst : t (t a)) : t a = foldl append [] lst

fun mp [a] [b] f =
    let
        fun mp' acc ls =
            case ls of
                [] => rev acc
              | x :: ls => mp' (f x :: acc) ls
    in
        mp' []
    end

fun mapConcat [a] [b] f =
    let
        fun mapConcat' acc ls =
            case ls of
                [] => rev acc
              | x :: ls => mapConcat' (revAppend (f x) acc) ls
    in
        mapConcat' []
    end

fun mapi [a] [b] f =
    let
        fun mp' n acc ls =
            case ls of
                [] => rev acc
              | x :: ls => mp' (n + 1) (f n x :: acc) ls
    in
        mp' 0 []
    end

fun mapPartial [a] [b] f =
    let
        fun mp' acc ls =
            case ls of
                [] => rev acc
              | x :: ls => mp' (case f x of
                                    None => acc
                                  | Some y => y :: acc) ls
    in
        mp' []
    end

fun mapX [a] [ctx ::: {Unit}] f =
    let
        fun mapX' ls =
            case ls of
                [] => <xml/>
              | x :: ls => <xml>{f x}{mapX' ls}</xml>
    in
        mapX'
    end

fun mapXi [a] [ctx ::: {Unit}] f =
    let
        fun mapX' i ls =
            case ls of
                [] => <xml/>
              | x :: ls => <xml>{f i x}{mapX' (i + 1) ls}</xml>
    in
        mapX' 0
    end

fun mapM [m ::: (Type -> Type)] (_ : monad m) [a] [b] f =
    let
        fun mapM' acc ls =
            case ls of
                [] => return (rev acc)
              | x :: ls => x' <- f x; mapM' (x' :: acc) ls
    in
        mapM' []
    end

fun mapConcatM [m] (_ : monad m) [a] [b] f =
    let
        fun mapConcatM' acc ls =
            case ls of
                [] => return (rev acc)
              | x :: ls' => ls <- f x; mapConcatM' (revAppend ls acc) ls'
    in
        mapConcatM' []
    end

fun mapMi [m ::: (Type -> Type)] (_ : monad m) [a] [b] f =
    let
        fun mapM' i acc ls =
            case ls of
                [] => return (rev acc)
              | x :: ls => x' <- f i x; mapM' (i + 1) (x' :: acc) ls
    in
        mapM' 0 []
    end

fun mapPartialM [m ::: (Type -> Type)] (_ : monad m) [a] [b] f =
    let
        fun mapPartialM' acc ls =
            case ls of
                [] => return (rev acc)
              | x :: ls =>
                v <- f x;
                mapPartialM' (case v of
                                  None => acc
                                | Some x' => x' :: acc) ls
    in
        mapPartialM' []
    end

fun mapXM [m ::: (Type -> Type)] (_ : monad m) [a] [ctx ::: {Unit}] f =
    let
        fun mapXM' ls =
            case ls of
                [] => return <xml/>
              | x :: ls =>
                this <- f x;
                rest <- mapXM' ls;
                return <xml>{this}{rest}</xml>
    in
        mapXM'
    end

fun filter [a] f =
    let
        fun fil acc ls =
            case ls of
                [] => rev acc
              | x :: ls => fil (if f x then x :: acc else acc) ls
    in
        fil []
    end

fun exists [a] f =
    let
        fun ex ls =
            case ls of
                [] => False
              | x :: ls =>
                if f x then
                    True
                else
                    ex ls
    in
        ex
    end

fun existsM [m] (_ : monad m) [a] f =
    let
        fun ex ls =
            case ls of
                [] => return False
              | x :: ls =>
                b <- f x;
                if b then
                    return True
                else
                    ex ls
    in
        ex
    end

fun foldlMap [a] [b] [c] f =
    let
        fun fold ls' st ls =
            case ls of
                [] => (rev ls', st)
              | x :: ls =>
                case f x st of
                    (y, st) => fold (y :: ls') st ls
    in
        fold []
    end

fun foldlMapM [m] (_ : monad m) [a] [b] [c] f =
    let
        fun fold ls' st ls =
            case ls of
                [] => return (rev ls', st)
              | x :: ls =>
                (y, st) <- f x st;
                fold (y :: ls') st ls
    in
        fold []
    end

fun mem [a] (_ : eq a) (x : a) =
    let
        fun mm ls =
            case ls of
                [] => False
              | y :: ls => y = x || mm ls
    in
        mm
    end

fun find [a] f =
    let
        fun find' ls =
            case ls of
                [] => None
              | x :: ls =>
                if f x then
                    Some x
                else
                    find' ls
    in
        find'
    end

fun findM [m] (_ : monad m) [a] f =
    let
        fun find' ls =
            case ls of
                [] => return None
              | x :: ls =>
                b <- f x;
                if b then
                    return (Some x)
                else
                    find' ls
    in
        find'
    end

fun search [a] [b] f =
    let
        fun search' ls =
            case ls of
                [] => None
              | x :: ls =>
                case f x of
                    None => search' ls
                  | v => v
    in
        search'
    end

fun searchM [m] (_ : monad m) [a] [b] f =
    let
        fun search' ls =
            case ls of
                [] => return None
              | x :: ls =>
                o <- f x;
                case o of
                    None => search' ls
                  | v => return v
    in
        search'
    end

fun foldlM [m] (_ : monad m) [a] [b] f =
    let
        fun foldlM' acc ls =
            case ls of
                [] => return acc
              | x :: ls =>
                acc <- f x acc;
                foldlM' acc ls
    in
        foldlM'
    end

fun foldlMi [m] (_ : monad m) [a] [b] f =
    let
        fun foldlMi' i acc ls =
            case ls of
                [] => return acc
              | x :: ls =>
                acc <- f i x acc;
                foldlMi' (i + 1) acc ls
    in
        foldlMi' 0
    end

fun filterM [m] (_ : monad m) [a] (p : a -> m bool) =
    let
        fun filterM' (acc : list a) (xs : list a) : m (list a) =
            case xs of
                [] => return (rev acc)
              | x :: xs =>
                c <- p x;
                filterM' (if c then x :: acc else acc) xs
    in
        filterM' []
    end

fun all [a] f =
    let
        fun all' ls =
            case ls of
                [] => True
              | x :: ls => f x && all' ls
    in
        all'
    end

fun any [a] f =
    let
        fun any' ls =
            case ls of
                [] => False
              | x :: ls => f x || any' ls
    in
        any'
    end

fun allM [m] (_ : monad m) [a] f =
    let
        fun all' ls =
            case ls of
                [] => return True
              | x :: ls =>
                b <- f x;
                if b then
                    all' ls
                else
                    return False
    in
        all'
    end

fun app [m] (_ : monad m) [a] f =
    let
        fun app' ls =
            case ls of
                [] => return ()
              | x :: ls =>
                f x;
                app' ls
    in
        app'
    end

fun appi [m] (_ : monad m) [a] f =
    let
        fun app' i ls =
            case ls of
                [] => return ()
              | x :: ls =>
                f i x;
                app' (i + 1) ls
    in
        app' 0
    end

fun mapQuery [tables ::: {{Type}}] [exps ::: {Type}] [t ::: Type]
             [tables ~ exps] (q : sql_query [] [] tables exps)
             (f : $(exps ++ map (fn fields :: {Type} => $fields) tables) -> t) =
    ls <- query q
                (fn fs acc => return (f fs :: acc))
                [];
    return (rev ls)

fun mapQueryM [tables ::: {{Type}}] [exps ::: {Type}] [t ::: Type]
             [tables ~ exps] (q : sql_query [] [] tables exps)
             (f : $(exps ++ map (fn fields :: {Type} => $fields) tables) -> transaction t) =
    ls <- query q
                (fn fs acc => v <- f fs; return (v :: acc))
                [];
    return (rev ls)

fun mapQueryPartialM [tables ::: {{Type}}] [exps ::: {Type}] [t ::: Type]
             [tables ~ exps] (q : sql_query [] [] tables exps)
             (f : $(exps ++ map (fn fields :: {Type} => $fields) tables) -> transaction (option t)) =
    ls <- query q
                (fn fs acc => v <- f fs;
                    return (case v of
                                None => acc
                              | Some v => v :: acc))
                [];
    return (rev ls)

fun sort [a] (gt : a -> a -> bool) (ls : t a) : t a =
    let
        fun split ls acc1 acc2 =
            case ls of
                [] => (rev acc1, rev acc2)
              | x :: [] => (rev (x :: acc1), rev acc2)
              | x1 :: x2 :: ls' => split ls' (x1 :: acc1) (x2 :: acc2)

        fun merge ls1 ls2 acc =
            case (ls1, ls2) of
                ([], _) => revAppend acc ls2
              | (_, []) => revAppend acc ls1
              | (x1 :: ls1', x2 :: ls2') => if gt x1 x2 then merge ls1 ls2' (x2 :: acc) else merge ls1' ls2 (x1 :: acc)

        fun sort' ls =
            case ls of
                [] => ls
              | _ :: [] => ls
              | _ =>
                let
                    val (ls1, ls2) = split ls [] []
                in
                    merge (sort' ls1) (sort' ls2) []
                end
    in
        sort' ls
    end

val nth [a] =
    let
        fun nth (ls : list a) (n : int) : option a =
            case ls of
                [] => None
              | x :: ls' =>
                if n <= 0 then
                    Some x
                else
                    nth ls' (n-1)
    in
        nth
    end

fun replaceNth [a] (ls : list a) (n : int) (v : a) : list a =
    let
        fun repNth (ls : list a) (n : int) (acc : list a) =
            case ls of
                [] => rev acc
              | x :: ls' => if n <= 0 then
                                revAppend acc (v :: ls')
                            else
                                repNth ls' (n-1) (x :: acc)
    in
        repNth ls n []
    end

fun findIndex [a] (f : a -> bool) : list a -> option int =
    let
        fun findIndex' (i : int) (ls : list a) =
            case ls of
                [] => None
              | a :: ls => if f a then Some i else findIndex' (i + 1) ls
    in
        findIndex' 0
    end

fun assoc [a] [b] (_ : eq a) (x : a) =
    let
        fun assoc' (ls : list (a * b)) =
            case ls of
                [] => None
              | (y, z) :: ls =>
                if x = y then
                    Some z
                else
                    assoc' ls
    in
        assoc'
    end

fun assocAdd [a] [b] (_ : eq a) (x : a) (y : b) (ls : t (a * b)) =
    case assoc x ls of
        None => (x, y) :: ls
      | Some _ => ls

fun assocAddSorted [a] [b] (_ : eq a) (_ : ord a) (x : a) (y : b) (ls : t (a * b)) =
    let
        fun aas (ls : t (a * b)) (acc : t (a * b)) =
            case ls of
                [] => rev ((x, y) :: acc)
              | (x', y') :: ls' =>
                if x' = x then
                    revAppend ((x, y) :: acc) ls'
                else if x < x' then
                    revAppend ((x, y) :: acc) ls
                else
                    aas ls' ((x', y') :: acc)
    in
        aas ls []
    end

(* Updates an element in the association list, potentially removing it altogether. *)
fun assocUpdate [a] [b] (_ : eq a) (x : a) (f : b -> option b) : list (a * b) -> list (a * b) =
    (* mapPartial (fn (a,b) => if x = a then Option.mp (fn b => (a,b)) (f b) else Some (a,b)) *)
    (* The above one-liner fails with a "Substitution in constructor is blocked by a too-deep unification variable" that I don't know how to fix. *)
    let
        fun mp' acc ls =
            case ls of
                [] => rev acc
              | (a,b) :: ls =>
                mp' (if x = a then
                    (case f b of
                        None => acc
                      | Some y => (a,y) :: acc)
                    else (a,b) :: acc) ls
    in
        mp' []
    end

fun recToList [a ::: Type] [r ::: {Unit}] (fl : folder r)
  = @foldUR [a] [fn _ => list a] (fn [nm ::_] [rest ::_] [[nm] ~ rest] x xs =>
				      x :: xs) [] fl

fun take [a] (n : int) (xs : list a) : list a =
    if n <= 0 then
        []
    else
        case xs of
            [] => []
          | x :: xs => x :: take (n-1) xs

fun drop [a] (n : int) (xs : list a) : list a =
    if n <= 0 then
        xs
    else
        case xs of
            [] => []
          | x :: xs => drop (n-1) xs

fun splitAt [a] (n : int) (xs : list a) : list a * list a =
    (take n xs, drop n xs)

fun span [a] (f : a -> bool) (ls : list a) : list a * list a  =
    let
        fun span' ls acc =
            case ls of
                []      =>  (rev acc, [])
              | x :: xs =>  if f x then span' xs (x :: acc) else (rev acc, ls)
    in
        span' ls []
    end

fun groupBy [a] (f : a -> a -> bool) (ls : list a) : list (list a) =
    let
        fun groupBy' ls acc =
            case ls of
                [] => rev ([] :: acc)
              | x :: xs =>
                let
                    val (ys, zs) = span (f x) xs
                in
                    groupBy' zs ((x :: ys) :: acc)
                end
    in
        groupBy' ls []
    end

fun mapXiM [m ::: Type -> Type] (_ : monad m) [a] [ctx ::: {Unit}] (f : int -> a -> m (xml ctx [] [])) : t a -> m (xml ctx [] []) =
    let
        fun mapXiM' i ls =
            case ls of
                [] => return <xml/>
              | x :: ls =>
                this <- f i x;
                rest <- mapXiM' (i+1) ls;
                return <xml>{this}{rest}</xml>
    in
        mapXiM' 0
    end

fun tabulateM [m] (_ : monad m) [a] (f : int -> m a) n =
    let
        fun tabulate' n acc =
            if n <= 0 then
                return acc
            else
                (v <- f (n-1);
                 tabulate' (n-1) (v :: acc))
    in
        tabulate' n []
    end

fun intercalate (sep : string) (strs : list string) : string = case strs of
    [] => ""
  | fst :: rst => foldl (fn w s => s ^ sep ^ w) fst rst

