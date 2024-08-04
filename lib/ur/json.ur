(* TODO: Change Yaml to use result too *)
con json a = {ToJson : a -> string,
              FromJson : string -> result (a * string),
              ToYaml : bool (* comes immediately after list bullet? *)
                       -> int (* starting indent level *)
                       -> a -> string,
              FromYaml : bool (* comes immediately after list bullet? *)
                      -> int (* starting indent level *)
                      -> string -> result (a * string)}

fun mkJson [a] (x : {ToJson : a -> string,
                     FromJson : string -> result (a * string)}) =
    x ++ {ToYaml = fn _ _ _ => error <xml>No YAML support</xml>,
          FromYaml = fn _ _ _ => error <xml>No YAML support</xml>}

(* These urweb-repo files can't seem to see each other, which means we need to copy some code into them so that things work.  This seems wrong, but I don't know what the right fix is. *)

fun foldl [a] [b] (f : a -> b -> b) =
    let
        fun foldl' acc ls =
            case ls of
                [] => acc
              | x :: ls => foldl' (f x acc) ls
    in
        foldl'
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

fun monadMapR2 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type] [tr :: K -> Type]
         (f : nm :: Name -> t :: K -> tf1 t -> tf2 t -> m (tr t)) =
    let fun foldR2 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type] [tr :: {K} -> Type]
           (f : nm :: Name -> t :: K -> rest :: {K}
                -> [[nm] ~ rest] =>
            tf1 t -> tf2 t -> tr rest -> m (tr ([nm = t] ++ rest)))
           (i : tr []) [r ::: {K}] (fl : folder r) =
    @Top.fold [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> m (tr r)]
     (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest]
                      (acc : _ -> _ -> m (tr rest)) r1 r2 =>
         acc' <- acc (r1 -- nm) (r2 -- nm);
         f [nm] [t] [rest] r1.nm r2.nm acc')
     (fn _ _ => return i)
     fl
    in
    @@foldR2 [m] _ [tf1] [tf2] [fn r => $(map tr r)]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] (v1 : tf1 t) (v2 : tf2 t)
                     (acc : $(map tr rest)) =>
        v' <- f [nm] [t] v1 v2;
        return (acc ++ {nm = v'}))
    {}
    end

fun resultErrorGet [a] (r : result a) : a =
    case r of
        Failure e => error e
      | Success v => v

fun readResult [t] (_ : read t) (s : string) : result t =
  case read s of
    None => Failure <xml>Cannot read: {[s]}</xml>
  | Some v => Success v

fun resultGuard (b : bool) (e : xbody) : result unit =
  if b then Success () else Failure e





fun skipSpaces (s : string) : string =
    if s = "" then
        ""
    else if Char.isSpace (String.sub s 0) then
        skipSpaces (String.suffix s 1)
    else
        s

fun skipRealSpaces (s : string) : string =
    if s = "" then
        ""
    else if String.sub s 0 = #" " then
        skipRealSpaces (String.suffix s 1)
    else
        s

fun skipNewlines (s : string) : string =
    if s = "" then
        ""
    else if String.sub s 0 = #"\n" || String.sub s 0 = #"\r" then
        skipNewlines (String.suffix s 1)
    else
        s

fun toJson [a] (j : json a) : a -> string = j.ToJson
fun fromJsonR' [a] (j : json a) : string -> result (a * string) = j.FromJson

fun fromJsonR [a] (j : json a) (s : string) : result a =
    (v, s') <- j.FromJson (skipSpaces s);
    resultGuard (String.all Char.isSpace s')
        <xml>Extra content at end of JSON record: {[s']}</xml>;
    return v

fun fromJson' [a] (j : json a) : string -> a * string = j.FromJson >>> resultErrorGet

fun fromJson [a] (j : json a) : string -> a = @@fromJsonR [a] j >>> resultErrorGet

fun toYaml [a] (j : json a) : a -> string = j.ToYaml False 0

fun fromYamlR' [a] (j : json a) : string -> result (a * string) = j.FromYaml False 0

fun fromYamlR [a] (j : json a) (s : string) : result a =
    (v, s') <- j.FromYaml False 0 (skipSpaces s);
    resultGuard (String.all Char.isSpace s')
        <xml>Extra content at end of YAML record: {[s']}</xml>;
    return v

fun fromYaml' [a] (j : json a) : string -> a * string = @@fromYamlR' [a] j >>> resultErrorGet

fun fromYaml [a] (j : json a) : string -> a = @@fromYamlR [a] j >>> resultErrorGet

fun escape (s : string) : string =
    let
        fun esc s =
            case s of
                "" => "\""
              | _ =>
                let
                    val ch = String.sub s 0
                in
		    (case ch of
			 #"\n" => "\\n"
		       | #"\r" => "\\r"
		       | #"\t" => "\\t"
		       | #"\"" => "\\\""
		       | #"\\" => "\\\\"
		       | x => String.str ch
		    ) ^ esc (String.suffix s 1)
                end
    in
        "\"" ^ esc s
    end

fun unhex (ch : char) : result int =
    if Char.isDigit ch then
        Success (Char.toInt ch - Char.toInt #"0")
    else if Char.isXdigit ch then
        if Char.isUpper ch then
            Success (10 + (Char.toInt ch - Char.toInt #"A"))
        else
            Success (10 + (Char.toInt ch - Char.toInt #"a"))
    else
        Failure <xml>Invalid hexadecimal digit "{[ch]}"</xml>

fun unescape (s : string) : result (string * string) =
  let
    fun findEnd endChar i s =
        if s = "" then
            Failure <xml>JSON unescape: string ends before quote: {[s]}</xml>
        else
            let
                val ch = String.sub s 0
            in
                case ch of
                    #"\\" =>
                    if not (strlenGe s 2) then
                        Failure <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                    else if String.sub s 1 = #"u" then
                        if not (strlenGe s 6) then
                            Failure <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                        else
                            findEnd endChar (i+6) (String.suffix s 6)
                    else
                        findEnd endChar (i+2) (String.suffix s 2)
                  | _ =>
                    if ch = endChar then
                        Success i
                    else
                        findEnd endChar (i+1) (String.suffix s 1)
            end

    fun unesc last i s =
      if i >= last then
        Success ""
      else
        let
          val ch = String.sub s 0
        in
          case ch of
              #"\\" =>
                if not (strlenGe s 2) then
                    Failure <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                else if String.sub s 1 = #"u" then
                    if not (strlenGe s 6) then
                        Failure <xml>JSON unescape: Unicode ends early</xml>
                    else
                        h1 <- unhex (String.sub s 2);
                        h2 <- unhex (String.sub s 3);
                        h3 <- unhex (String.sub s 4);
                        h4 <- unhex (String.sub s 5);
                        rest <- unesc last (i+6) (String.suffix s 6);
                        return <| (ofUnicode (h1 * (256*16) + h2 * 256 + h3 * 16 + h4)) ^ rest
                else
                  c <- (case String.sub s 1 of
                      #"n" => Success "\n"
                    | #"r" => Success "\r"
                    | #"t" => Success "\t"
                    | #"\"" => Success "\""
                    | #"'" => Success "'"
                    | #"\\" => Success "\\"
                    | #"/" => Success "/"
                    | #" " => Success " "
                    | x => Failure <xml>JSON unescape: Bad escape char: {[x]}</xml>);
                  rest <- unesc last (i+2) (String.suffix s 2);
                  return <| c ^ rest
            | _ => rest <- unesc last (i+1) (String.suffix s 1);
                   return (String.str ch ^ rest)
        end
  in
    if s = "" || (String.sub s 0 <> #"\"" && String.sub s 0 <> #"'") then
        Failure <xml>JSON unescape: String doesn't start with quote: {[s]}</xml>
    else
      last <- findEnd (String.sub s 0) 1 (String.suffix s 1);
      res <- unesc last 1 (String.suffix s 1);
      return (res, String.suffix s (last+1))
  end

fun readYamlLine
        (minIndent : option int)
        (* set if we come right after a list bullet and should fake some indent *)
        (s : string) : int (* indent level *) * string (* remaining code *) =
    let
        fun read s acc =
            if s = "" then
                (acc, "")
            else
                let
                    val ch = String.sub s 0
                in
                    if ch = #"\n" || ch = #"\r" then
                        read (String.suffix s 1) 0
                    else if ch = #"#" then
                        (* Comment *)
                        case String.seek s #"\n" of
                            None => (acc, s)
                          | Some rest => read rest 0
                    else if ch = #" " then
                        read (String.suffix s 1) (acc + 1)
                    else
                        (acc, s)
                end

        val (i, s) = read s 0
    in
        (case minIndent of
             None => i
           | Some i' => max i' i, s)
    end

datatype block_style = ToSpaces | KeepNewlines
datatype chomp_style = SingleNewline | NoNewline | AllNewlines

(* A string consisting of `i` newlines *)
fun newlines (i : int) : string =
    if i <= 0 then
        ""
    else
        newlines (i-1) ^ "\n"

(* YAMl strings can be multiline.  This reads them in.  The first string output
is the string that's been read and the second is the rest to continue parsing. *)
fun readMultilineYaml (i : int) (s : string) : result (string * string) =
  resultGuard (String.lengthGe s 2)
    <xml>Multiline YAML string ends too early.</xml>;
  (* NOTE: If we try to do `Success ToSpaces` on the next line, we hit a
  compiler error: `unhandled exception: UnboundNamed` in monoize.  Until the
  compiler is fixed, we perform this workaround of returning a boolean and
  turning it into a block_style on the following line. *)
  block <- (case String.sub s 0 of
      #">" => Success True
    | #"|" => Success False
    | _ => Failure <xml>Multiline YAML string starts with unknown character: .</xml>);
  block <- return (if block then ToSpaces else KeepNewlines);
  chomp <- return (case String.sub s 1 of
      #"-" => NoNewline
    | #"+" => AllNewlines
    | _ => SingleNewline);
  (* Here we could check for a number for indentation purposes (https://yaml-multiline.info) *)
  let
        fun read s acc onl (* length of longest suffix of acc that is only newline characters *) =
            let
                val (i', s') = readYamlLine None s
            in
                if i' < i then
                    Success (case chomp of
                         SingleNewline => String.substring acc {Start = 0, Len = String.length acc - onl} ^ "\n"
                       | NoNewline => String.substring acc {Start = 0, Len = String.length acc - onl}
                       | AllNewlines =>
                         case block of
                             KeepNewlines => acc
                           | ToSpaces => String.substring acc {Start = 0, Len = String.length acc - onl} ^ newlines onl, s)
                else
                    case String.split s' #"\n" of
                        None => Failure <xml>Multiline YAML string ends without newline.</xml>
                      | Some (line, s') => read s'
                          (acc ^ line ^ (case block of ToSpaces => " " | KeepNewlines => "\n"))
                          (if String.all (fn ch => ch = #"\r") line then
                             onl + String.length line + 1
                           else if line <> "" && String.sub line (String.length line - 1) = #"\r" then
                             2
                           else
                             1)
            end
  in
      case String.seek s #"\n" of
          None => Failure <xml>Multiline YAML string ends too early.</xml>
        | Some s' => read s' "" 0
  end

fun yamlStringIn i s =
    if s = "" then
        Success ("", "")
    else if String.sub s 0 = #"\"" || String.sub s 0 = #"'" then
        unescape s
    else if String.sub s 0 = #">" || String.sub s 0 = #"|" then
        readMultilineYaml i s
    else case String.msplit {Haystack = s, Needle = "\r\n"} of
             None => Success (s, "")
           | Some (v, _, rest) => Success (v, rest)

val json_string = {ToJson = escape,
                   FromJson = unescape,
                   ToYaml = fn _ _ => escape,
                   FromYaml = fn _ => yamlStringIn}

fun rfc3339_out s =
    let
        val out1 = timef "%Y-%m-%dT%H:%M:%S%z" s
        val len = String.length out1
    in
        if len < 2 then
            error <xml>timef output too short</xml>
        else
            String.substring out1 {Start = 0, Len = len - 2} ^ ":"
            ^ String.suffix out1 (len - 2)
    end

fun rfc3339_in' (s : string) : result time =
    case String.split s #"T" of
        None => Failure <xml>Invalid RFC 3339 string "{[s]}"</xml>
      | Some (date, time) =>
        case String.msplit {Haystack = time, Needle = "Z+-"} of
            None => Failure <xml>Invalid RFC 3339 string "{[s]}"</xml>
          | Some (time, sep, rest) =>
              time <- return (case String.split time #"." of
                                None => time
                              | Some (time, _) => time);
              t <- (case readUtc (date ^ " " ^ time) of
                            None => Failure <xml>Invalid RFC 3339 string "{[s]}"</xml>
                          | Some t => Success t);
              let
                fun withOffset multiplier =
                    case String.split rest #":" of
                        None => Failure <xml>Invalid RFC 3339 string "{[s]}"</xml>
                      | Some (h, m) =>
                        case (read h, read m) of
                            (Some h, Some m) => Success (addSeconds t (multiplier * 60 * (60 * h + m)))
                          | _ => Failure <xml>Invalid RFC 3339 string "{[s]}"</xml>
              in
                case sep of
                    #"Z" => Success t
                  | #"+" => withOffset (-1)
                  | #"-" => withOffset 1
                  | _ => Failure <xml>msplit returns impossible separator</xml>
              end

val rfc3339_in : string -> time = rfc3339_in' >>> resultErrorGet

fun timeOut (s : string) : result (time * string) =
  (v, s') <- unescape s;
  t <- rfc3339_in' v;
  return (t, s')

val json_time = {ToJson = fn tm => escape (rfc3339_out tm),
                 FromJson = timeOut,
                 ToYaml = fn _ _ tm => escape (rfc3339_out tm),
                 FromYaml = fn _ _ => timeOut}

fun numIn [a] (_ : read a) (s : string) : result (a * string) =
    let
        val len = String.length s

        fun findEnd i s =
            if s = "" then
                i
            else
                let
                    val ch = String.sub s 0
                in
                    if Char.isDigit ch || ch = #"-" || ch = #"." || ch = #"E" || ch = #"e" then
                        findEnd (i+1) (String.suffix s 1)
                    else
                        i
                end
    in
        if s <> "" && String.sub s 0 = #"\"" then
            let
                val last = findEnd 1 (String.suffix s 1)
                val rest = String.suffix s last
            in
                if rest <> "" && String.sub rest 0 = #"\"" then
                  res <- readResult (String.substring s {Start = 1, Len = last-1});
                  return (res, String.suffix rest 1)
                else
                  Failure <xml>Unbalanced quotes for JSON number {[s]}</xml>
            end
        else if s <> "" && String.sub s 0 = #"'" then
            let
                val last = findEnd 1 (String.suffix s 1)
                val rest = String.suffix s last
            in
                if rest <> "" && String.sub rest 0 = #"'" then
                  res <- readResult (String.substring s {Start = 1, Len = last-1});
                  return (res, String.suffix rest 1)
                else
                    Failure <xml>Unbalanced quotes for JSON number {[s]}</xml>
            end
        else
            let
                val last = findEnd 0 s
            in
              res <- readResult (String.substring s {Start = 0, Len = last});
              return (res, String.suffix s last)
            end
    end

fun json_num [a] (_ : show a) (_ : read a) : json a =
    {ToJson = show,
    FromJson = numIn,
    ToYaml = fn _ _ => show,
    FromYaml = fn _ _ => numIn}

val json_int = json_num
val json_float = json_num

val json_bool = {
    ToJson = fn b => if b then "true" else "false",
    FromJson = fn s =>
        if String.isPrefix {Full = s, Prefix = "true"} then
            Success (True, String.suffix s 4)
        else if String.isPrefix {Full = s, Prefix = "false"} then
            Success (False, String.suffix s 5)
        else if String.isPrefix {Full = s, Prefix = "\"true\""} then
            Success (False, String.suffix s 6)
        else if String.isPrefix {Full = s, Prefix = "\"false\""} then
            Success (False, String.suffix s 7)
        else
            Failure <xml>JSON: bad boolean string: {[s]}</xml>,
    ToYaml = fn _ _ b => if b then "True" else "False",
    FromYaml = fn _ _ s =>
      let val s' =
        case String.msplit {Haystack = s, Needle = " \r\n"} of
            None => s
          | Some (s', _, _) => s'
      in
        s' <- return (String.mp Char.toLower s');
        v <- (if s' = "true" || s' = "on" || s' = "yes" then
              Success True
          else if s' = "false" || s' = "off" || s' = "no" then
              Success False
          else
              Failure <xml>Invalid YAML Boolean: {[s']}</xml>);
        Success (v, String.suffix s (String.length s'))
      end
    }

fun json_option [a] (j : json a) : json (option a) = {
    ToJson = fn v => case v of
        None => "null"
      | Some v => j.ToJson v,
    FromJson = fn s =>
      if String.isPrefix {Full = s, Prefix = "null"} then
        Success (None, String.suffix s 4)
      else
        (v, s') <- j.FromJson s;
        return (Some v, s'),
    ToYaml = fn b i v => case v of
        None => "null"
      | Some v => j.ToYaml b i v,
    FromYaml = fn b i s =>
      if String.isPrefix {Full = s, Prefix = "null"} then
        Success (None, String.suffix s 4)
      else
        (v, s') <- j.FromYaml b i s;
        return (Some v, s')
    }

fun indent i =
    if i <= 0 then
        ""
    else
        " " ^ indent (i - 1)

fun truncAtNewline s =
    case String.split s #"\n" of
        None => s
      | Some (s', _) => s'

(* Remove leading spaces *)
fun triml s =
    if s <> "" && Char.isSpace (String.sub s 0) then
        triml (String.suffix s 1)
    else
        s

fun removeNewlineIfAfterBullet (afterBullet : bool) (s : string) =
    if afterBullet && s <> "" && String.sub s 0 = #"\n" then
        triml (String.suffix s 1)
    else
        s

fun json_list [a] (j : json a) : json (list a) =
    let
        fun toJ' (ls : list a) : string =
            case ls of
                [] => ""
              | x :: ls => "," ^ toJson j x ^ toJ' ls

        fun toJ (x : list a) : string =
            case x of
                [] => "[]"
              | x :: [] => "[" ^ toJson j x ^ "]"
              | x :: ls => "[" ^ toJson j x ^ toJ' ls ^ "]"

        fun fromJ (s : string) : result (list a * string) =
            let
                fun fromJ' (s : string) : result (list a * string) =
                    if s = "" then
                        Failure <xml>JSON list doesn't end with ']'</xml>
                    else
                        case String.sub s 0 of
                            #"]" => Success ([], String.suffix s 1)
                          | _ =>
                            (x, s') <- j.FromJson s;
                            s' <- return (skipSpaces s');
                            s' <- (if s' = ""
                                then Failure <xml>JSON list doesn't end with ']'</xml>
                                else if String.sub s' 0 = #","
                                then Success (skipSpaces (String.suffix s' 1))
                                else Success s');
                            (ls, s'') <- fromJ' s';
                            return (x :: ls, s'')
            in
                if String.length s = 0 || String.sub s 0 <> #"[" then
                    Failure <xml>JSON list doesn't start with '[': {[s]}</xml>
                else
                    fromJ' (skipSpaces (String.suffix s 1))
            end

        fun toY (i : int) (ls : list a) : string =
            case ls of
                [] => ""
              | x :: ls' => "\n" ^ indent (i + 1) ^ "- " ^ j.ToYaml True (i + 3) x ^ toY i ls'

        fun shortenString s =
            if strlenGe s 30 then
                String.substring s {Start = 0, Len = 30}
            else
                s

        fun fromY (b : bool) (i : int) (s : string) : result (list a * string) =
            let
                val (i', s') = readYamlLine (if b then Some i else None) s
            in
                if i' < i || s' = "" then
                    Success ([], s)
                else if String.sub s' 0 = #"-" then
                    let
                        val s' = String.suffix s' 1
                        val (s', i') = if s' <> "" && String.sub s' 0 = #" " then
                                           (String.suffix s' 1, i' + 2)
                                       else
                                           (s', i' + 1)
                    in
                      (v, s) <- j.FromYaml True i' s';
                      (ls, s) <- fromY False i s;
                      return (v :: ls, s)
                    end
                else
                    Failure <xml>YAML list contains weird delimiter: {[String.sub s' 0]}.</xml>
            end
    in
        {ToJson = toJ,
         FromJson = fromJ,
         ToYaml = fn b i ls =>
                     case ls of
                         [] => "[]"
                       | _ => removeNewlineIfAfterBullet b (toY i ls),
         FromYaml = fn b i s =>
                       let
                           val s = skipRealSpaces s
                       in
                           if String.isPrefix {Full = s, Prefix = "[]"} then
                               Success ([], skipNewlines (String.suffix s 2))
                           else
                               fromY b (i+1) s
                       end}
    end

(* Used to skip over a chunk of json, returning the suffix after the next comma
not enclosed in a string or braces/brackets. *)
fun skipOne (s : string) : string =
    let
        fun skipStringLiteral s delimiter =
            if s = "" then
                s
            else
                let
                    val ch = String.sub s 0
                    val rest = String.suffix s 1
                in
                    if ch = delimiter then
                        rest
                    else if ch = #"\\" then
                        if rest <> "" then
                            skipStringLiteral (String.suffix s 2) delimiter
                        else
                            ""
                    else
                        skipStringLiteral rest delimiter
                end

        fun skipOne s brace bracket =
            if s = "" then
                s
            else
                let
                    val ch = String.sub s 0
                    val rest = String.suffix s 1
                in
                    case ch of
                        #"\"" => skipOne (skipStringLiteral rest #"\"") brace bracket
                      | #"'" => skipOne (skipStringLiteral rest #"'") brace bracket
                      | #"{" => skipOne rest (brace + 1) bracket
                      | #"}" => if brace = 0 then
                                    s
                                else
                                    skipOne rest (brace - 1) bracket

                      | #"[" => skipOne rest brace (bracket + 1)
                      | #"]" =>
                        if bracket = 0 then
                            s
                        else
                            skipOne rest brace (bracket - 1)

                      | #"," =>
                        if brace = 0 && bracket = 0 then
                            s
                        else
                            skipOne rest brace bracket

                      | _ => skipOne rest brace bracket
                end
    in
        skipOne s 0 0
    end

fun firstTen s =
    if String.lengthGe s 10 then String.substring s {Start = 0, Len = 10} else s

(* Like skipOne, but for yaml, skipping over any yaml until we reach an indent
level that's low enough. *)
fun skipUntilIndentLowEnough (target : int) (s : string) : string =
    let
        fun skip s =
            let
                val (i, s') = readYamlLine None s
            in
                if i <= target then
                    s
                else
                    case String.seek s' #"\n" of
                        None => s'
                      | Some s' => skip s'
            end

        (* Don't look for indentation in the first line,
         * as indeed it may not come at an actual line beginning. *)
        val s' = case String.seek s #"\n" of
                     None => s
                   | Some s => skip s
    in
        s'
    end

fun json_record_withDefaults
    [ts ::: {Type}] [ots ::: {Type}] [ts ~ ots]
    (fl : folder ts)
    (jss : $(map json ts))
    (names : $(map (fn _ => string) ts))
    (ofl : folder ots)
    (ojss : $(map json ots))
    (onamesAndDefaults : $(map (fn t => string * t) ots))
      : json $(ts ++ ots) = {
  ToJson = fn r =>
    let
        val withRequired =
            @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
              escape name ^ ":" ^ j.ToJson v ^
                (case acc of "" => "" | acc => "," ^ acc))
            "" fl jss names (r --- _)

        val withOptional =
            @foldR3 [json] [fn t => string * t] [ident] [fn _ => string]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) (name, def) v acc =>
              (* Really, we'd like to check v and def for equality, but I don't
              want to add an eq constraint, so we do this slight hack of
              checking whether their json representations are the same, which
              should work fine. *)
              let val jv = j.ToJson v in
                if jv = j.ToJson def
                then acc
                else escape name ^ ":" ^ jv ^
                  (case acc of "" => "" | acc => "," ^ acc)
              end)
            withRequired ofl ojss onamesAndDefaults (r --- _)
    in
        "{" ^ withOptional ^ "}"
    end,
  FromJson = fn s =>
    let
      fun fromJ s (r : $(map option (ts ++ ots))) : result ($(map option (ts ++ ots)) * string) =
        if s = "" then
          Failure <xml>JSON object doesn't end in brace</xml>
        else if String.sub s 0 = #"}" then
          Success (r, String.suffix s 1)
        else
          (name, s') <- unescape s;
          s' <- return (skipSpaces s');
          s' <- (if s' = "" || String.sub s' 0 <> #":"
              then Failure <xml>No colon after JSON object field name</xml>
              else Success (skipSpaces (String.suffix s' 1)));
          (r, s') <- @foldR2 [json] [fn _ => string] [fn ts => $(map option ts) -> result ($(map option ts) * string)]
              (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name' acc r =>
                  if name = name' then
                    (v, s') <- j.FromJson s';
                    return (r -- nm ++ {nm = Some v}, s')
                  else
                    (r', s') <- acc (r -- nm);
                    return (r' ++ {nm = r.nm}, s'))
              (fn r => Success (r, skipOne s'))
              (@Folder.concat ! fl ofl)
              (jss ++ ojss) (names ++ @Top.mp [fn t => string * t] [fn _ => string] (fn [t] (n,_) => n) ofl onamesAndDefaults) r;
          s' <- return (skipSpaces s');
          s' <- return
              (if s' <> "" && String.sub s' 0 = #","
                  then skipSpaces (String.suffix s' 1)
                  else s');
          fromJ s' r
    in
      if s = "" || String.sub s 0 <> #"{" then
        Failure <xml>JSON record doesn't begin with brace: {[firstTen s]}</xml>
      else
        (r, s') <- fromJ (skipSpaces (String.suffix s 1))
                      (@map0 [option] (fn [t ::_] => None) (@Folder.concat ! fl ofl));
        mandatories <- (@monadMapR2 _ [option] [fn _ => string] [ident]
          (fn [nm ::_] [t ::_] (v : option t) name =>
              case v of
                  None => Failure <xml>Missing JSON object field {[name]}</xml>
                | Some v => Success v) fl (r --- _) names);
        defaults <- (@monadMapR2 _ [option] [fn t => string * t] [ident]
          (fn [nm ::_] [t ::_] (v : option t) (name, def) =>
              case v of
                  None => Success def
                | Some v => Success v) ofl (r --- _) onamesAndDefaults);
        return (mandatories ++ defaults, s')
      end,
    ToYaml = fn b i r =>
      let
        val withOptional =
          @foldR3 [json] [fn t => string * t] [ident] [fn _ => string]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) (name, def) v acc =>
              (* Really, we'd like to check v and def for equality, but I don't
              want to add an eq constraint, so we do this slight hack of
              checking whether their yaml representations are the same, which
              should work fine. *)
              let val yv = j.ToYaml False (i+2) v in
                if yv = j.ToYaml False (i+2) def
                then acc
                else "\n" ^ indent i ^ name ^ ": " ^ yv ^ acc
              end)
            "" ofl ojss onamesAndDefaults (r --- _)

        val withRequired =
          @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
              "\n" ^ indent i ^ name ^ ": " ^ j.ToYaml False (i+2) v ^ acc)
            withOptional fl jss names (r --- _)
      in
          removeNewlineIfAfterBullet b withRequired
      end,
    FromYaml = fn b i s =>
      let
        fun fromY b s (r : $(map option (ts ++ ots))) : result ($(map option (ts ++ ots)) * string) =
          if s = "" then
            return (r, s)
          else
            let
              val (i', s') = readYamlLine (if b then Some i else None) s
            in
              if i' < i then
                return (r, s)
              else
                case String.split s' #":" of
                    None =>
                      if String.all Char.isSpace s' then
                        return (r, "")
                      else
                        Failure <xml>Bad label in YAML record: {[firstTen s']}</xml>
                  | Some (name, s') =>
                    let
                      val s' = skipRealSpaces s'
                      val onames = @Top.mp [fn t => string * t] [fn _ => string] (fn [t] (n,_) => n) ofl onamesAndDefaults
                    in
                      (r, s') <- @foldR2 [json] [fn _ => string] [fn ts => $(map option ts) -> result ($(map option ts) * string)]
                        (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name' acc r =>
                            if name = name' then
                              (v, s') <- j.FromYaml False (i'+1) s';
                              return (r -- nm ++ {nm = Some v}, s')
                            else
                              (r', s') <- acc (r -- nm);
                              return (r' ++ {nm = r.nm}, s'))
                        (fn r => Success (r, skipUntilIndentLowEnough i' s'))
                        (@Folder.concat ! fl ofl) (jss ++ ojss) (names ++ onames) r;
                      fromY False s' r
                    end
              end

        val r = @map0 [option] (fn [t ::_] => None) (@Folder.concat ! fl ofl)
      in
        (r, s') <-
          (if String.isPrefix {Full = s, Prefix = "{}"} then
            return (r, String.suffix s 2)
          else
            fromY b s r);
        mandatories <- (@monadMapR2 _ [option] [fn _ => string] [ident]
          (fn [nm ::_] [t ::_] (v : option t) name =>
            case v of
                None => Failure <xml>Missing YAML object field {[name]}</xml>
              | Some v => Success v) fl (r --- _) names);
        defaults <- (@monadMapR2 _ [option] [fn t => string * t] [ident]
          (fn [nm ::_] [t ::_] (v : option t) (name, def) =>
              case v of
                  None => Success def
                | Some v => Success v) ofl (r --- _) onamesAndDefaults);
        return (mandatories ++ defaults, s')
      end}

fun json_record_withOptional [ts ::: {Type}] [ots ::: {Type}] [ts ~ ots]
                             (fl : folder ts) (jss : $(map json ts)) (names : $(map (fn _ => string) ts))
                             (ofl : folder ots) (ojss : $(map json ots)) (onames : $(map (fn _ => string) ots)): json $(ts ++ map option ots) =
    {ToJson = fn r =>
                 let
                     val withRequired =
                         @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                              escape name ^ ":" ^ j.ToJson v ^ (case acc of
                                                                    "" => ""
                                                                  | acc => "," ^ acc))
                          "" fl jss names (r --- _)

                     val withOptional =
                         @foldR3 [json] [fn _ => string] [option] [fn _ => string]
                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                              case v of
                                  None => acc
                                | Some v =>
                                  escape name ^ ":" ^ j.ToJson v ^ (case acc of
                                                                        "" => ""
                                                                      | acc => "," ^ acc))
                          withRequired ofl ojss onames (r --- _)
                 in
                     "{" ^ withOptional ^ "}"
                 end,
     FromJson = fn s =>
      let
        fun fromJ s (r : $(map option (ts ++ ots))) : result ($(map option (ts ++ ots)) * string) =
          if s = "" then
            Failure <xml>JSON object doesn't end in brace</xml>
          else if String.sub s 0 = #"}" then
            Success (r, String.suffix s 1)
          else
            (name, s') <- unescape s;
            s' <- return (skipSpaces s');
            s' <- (if s' = "" || String.sub s' 0 <> #":"
                then Failure <xml>No colon after JSON object field name</xml>
                else Success (skipSpaces (String.suffix s' 1)));
            (r, s') <- @foldR3 [fn _ => bool] [json] [fn _ => string] [fn ts => $(map option ts) -> result ($(map option ts) * string)]
                (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] skipNull (j : json t) name' acc r =>
                    if name = name' then
                        if skipNull
                            && String.lengthGe s' 5
                            && String.isPrefix {Prefix = "null", Full = s'}
                            && (Char.isSpace (String.sub s' 4)
                                || String.sub s' 4 = #","
                                || String.sub s' 4 = #"}") then
                            Success (r, String.suffix s' 4)
                        else
                            (v, s') <- j.FromJson s';
                            return (r -- nm ++ {nm = Some v}, s')
                    else
                        (r', s') <- acc (r -- nm);
                        return (r' ++ {nm = r.nm}, s'))
                (fn r => Success (r, skipOne s'))
                (@Folder.concat ! fl ofl)
                (@map0 [fn _ => bool] (fn [t ::_] => False) fl
                  ++ @map0 [fn _ => bool] (fn [t ::_] => True) ofl)
                (jss ++ ojss) (names ++ onames) r;
            s' <- return (skipSpaces s');
            s' <- return
                (if s' <> "" && String.sub s' 0 = #","
                    then skipSpaces (String.suffix s' 1)
                    else s');
            fromJ s' r
      in
        if s = "" || String.sub s 0 <> #"{" then
          Failure <xml>JSON record doesn't begin with brace: {[firstTen s]}</xml>
        else
          (r, s') <- fromJ (skipSpaces (String.suffix s 1))
                        (@map0 [option] (fn [t ::_] => None) (@Folder.concat ! fl ofl));
          mandatories <- (@monadMapR2 _ [option] [fn _ => string] [ident]
                (fn [nm ::_] [t ::_] (v : option t) name =>
                    case v of
                        None => Failure <xml>Missing JSON object field {[name]}</xml>
                      | Some v => Success v) fl (r --- _) names);
          return (mandatories ++ (r --- _), s')
        end,
     ToYaml = fn b i r =>
                 let
                     val withOptional =
                         @foldR3 [json] [fn _ => string] [option] [fn _ => string]
                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                              case v of
                                  None => acc
                                | Some v =>
                                  "\n" ^ indent i ^ name ^ ": " ^ j.ToYaml False (i+2) v ^ acc)
                          "" ofl ojss onames (r --- _)

                     val withRequired =
                         @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                              "\n" ^ indent i ^ name ^ ": " ^ j.ToYaml False (i+2) v ^ acc)
                          withOptional fl jss names (r --- _)
                 in
                     removeNewlineIfAfterBullet b withRequired
                 end,
     FromYaml = fn b i s =>
      let
        fun fromY b s (r : $(map option (ts ++ ots))) : result ($(map option (ts ++ ots)) * string) =
          if s = "" then
            return (r, s)
          else
            let
              val (i', s') = readYamlLine (if b then Some i else None) s
            in
              if i' < i then
                return (r, s)
              else
                case String.split s' #":" of
                    None =>
                      if String.all Char.isSpace s' then
                        return (r, "")
                      else
                        Failure <xml>Bad label in YAML record: {[firstTen s']}</xml>
                  | Some (name, s') =>
                    let
                      val s' = skipRealSpaces s'
                    in
                      (r, s') <- @foldR2 [json] [fn _ => string] [fn ts => $(map option ts) -> result ($(map option ts) * string)]
                        (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name' acc r =>
                            if name = name' then
                              (v, s') <- j.FromYaml False (i'+1) s';
                              return (r -- nm ++ {nm = Some v}, s')
                            else
                              (r', s') <- acc (r -- nm);
                              return (r' ++ {nm = r.nm}, s'))
                        (fn r => Success (r, skipUntilIndentLowEnough i' s'))
                        (@Folder.concat ! fl ofl) (jss ++ ojss) (names ++ onames) r;
                      fromY False s' r
                    end
              end

        val r = @map0 [option] (fn [t ::_] => None) (@Folder.concat ! fl ofl)
      in
        (r, s') <-
          (if String.isPrefix {Full = s, Prefix = "{}"} then
            return (r, String.suffix s 2)
          else
            fromY b s r);
        mandatories <- (@monadMapR2 _ [option] [fn _ => string] [ident]
          (fn [nm ::_] [t ::_] (v : option t) name =>
            case v of
                None => Failure <xml>Missing YAML object field {[name]}</xml>
              | Some v => Success v) fl (r --- _) names);
        defaults <- return (r --- _);
        return (mandatories ++ defaults, s')
      end}

(* At the moment, the below code is largely copied and pasted from the last
 * definition, because otherwise the compiler fails to inline enough for
 * compilation to succeed. *)
fun json_record [ts ::: {Type}] (fl : folder ts) (jss : $(map json ts)) (names : $(map (fn _ => string) ts)) : json $ts =
    {ToJson = fn r => "{" ^ @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                             (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                                 escape name ^ ": " ^ j.ToJson v ^ (case acc of
                                                                       "" => ""
                                                                     | acc => "," ^ acc))
                             "" fl jss names r ^ "}",
     FromJson = fn s =>
      let
        fun fromJ s (r : $(map option ts)) : result ($(map option ts) * string) =
          if s = "" then
            Failure <xml>JSON object doesn't end in brace</xml>
          else if String.sub s 0 = #"}" then
            Success (r, String.suffix s 1)
          else
            (name, s') <- unescape s;
            s' <- return (skipSpaces s');
            s' <- (if s' = "" || String.sub s' 0 <> #":"
                then Failure <xml>No colon after JSON object field name</xml>
                else Success (skipSpaces (String.suffix s' 1)));
            (r, s') <- @foldR2 [json] [fn _ => string] [fn ts => $(map option ts) -> result ($(map option ts) * string)]
                (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name' acc r =>
                    if name = name' then
                        (v, s') <- j.FromJson s';
                        return (r -- nm ++ {nm = Some v}, s')
                    else
                        (r', s') <- acc (r -- nm);
                        return (r' ++ {nm = r.nm}, s'))
                (fn r => Success (r, skipOne s'))
                fl jss names r;
            s' <- return (skipSpaces s');
            s' <- return
                (if s' <> "" && String.sub s' 0 = #","
                    then skipSpaces (String.suffix s' 1)
                    else s');
            fromJ s' r
      in
        if s = "" || String.sub s 0 <> #"{" then
          Failure <xml>JSON record doesn't begin with brace: {[firstTen s]}</xml>
        else
          (r, s') <- fromJ (skipSpaces (String.suffix s 1))
                        (@map0 [option] (fn [t ::_] => None) fl);
          r <- (@monadMapR2 _ [option] [fn _ => string] [ident]
                (fn [nm ::_] [t ::_] (v : option t) name =>
                    case v of
                        None => Failure <xml>Missing JSON object field {[name]}</xml>
                      | Some v => Success v) fl r names);
          return (r, s')
        end,
     ToYaml = fn b i r =>
                 removeNewlineIfAfterBullet b
                   (@foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                         "\n" ^ indent i ^ name ^ ": " ^ j.ToYaml False (i+2) v ^ acc)
                     "" fl jss names (r --- _)),
     FromYaml = fn b i s =>
      let
        fun fromY b s (r : $(map option ts)) : result ($(map option ts) * string) =
          if s = "" then
            return (r, s)
          else
            let
              val (i', s') = readYamlLine (if b then Some i else None) s
            in
              if i' < i then
                return (r, s)
              else
                case String.split s' #":" of
                    None =>
                      if String.all Char.isSpace s' then
                        return (r, "")
                      else
                        Failure <xml>Bad label in YAML record: {[firstTen s']}</xml>
                  | Some (name, s') =>
                    let
                      val s' = skipRealSpaces s'
                    in
                      (r, s') <- @foldR2 [json] [fn _ => string] [fn ts => $(map option ts) -> result ($(map option ts) * string)]
                        (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name' acc r =>
                            if name = name' then
                              (v, s') <- j.FromYaml False (i'+1) s';
                              return (r -- nm ++ {nm = Some v}, s')
                            else
                              (r', s') <- acc (r -- nm);
                              return (r' ++ {nm = r.nm}, s'))
                        (fn r => Success (r, skipUntilIndentLowEnough i' s'))
                        fl jss names r;
                      fromY False s' r
                    end
            end

        val r = @map0 [option] (fn [t ::_] => None) fl
      in
        (r, s') <-
          (if String.isPrefix {Full = s, Prefix = "{}"} then
            return (r, String.suffix s 2)
          else
            fromY b s r);
        r <- @monadMapR2 _ [option] [fn _ => string] [ident]
          (fn [nm ::_] [t ::_] (v : option t) name =>
            case v of
                None => Failure <xml>Missing YAML object field {[name]}</xml>
              | Some v => Success v)
          fl r names;
        return (r, s')
      end}

fun destrR [K] [f :: K -> Type] [fr :: K -> Type] [t ::: Type]
    (f : p :: K -> f p -> fr p -> t)
    [r ::: {K}] (fl : folder r) (v : variant (map f r)) (r : $(map fr r)) : t =
    match v
    (@Top.mp [fr] [fn p => f p -> t]
     (fn [p] (m : fr p) (v : f p) => f [p] v m)
     fl r)

fun json_variant [ts ::: {Type}] (fl : folder ts) (jss : $(map json ts)) (names : $(map (fn _ => string) ts)) : json (variant ts) = {
    ToJson = fn r => let val jnames = @map2 [json] [fn _ => string] [fn x => json x * string]
                                     (fn [t] (j : json t) (name : string) => (j, name)) fl jss names
                      in @destrR [ident] [fn x => json x * string]
                          (fn [p ::_] (v : p) (j : json p, name : string) =>
                            "{" ^ escape name ^ ": " ^ j.ToJson v ^ "}") fl r jnames
                      end,
    FromJson = fn s =>
      if s = "" || String.sub s 0 <> #"{" then
        Failure <xml>JSON variant doesn't begin with brace</xml>
      else
        (name : string, s') <- unescape (skipSpaces (String.suffix s 1));
        s' <- return (skipSpaces s');
        s' <- (if s' = "" || String.sub s' 0 <> #":"
            then Failure <xml>No colon after JSON object field name</xml>
            else Success (skipSpaces (String.suffix s' 1)));
        (r, s') <- (@foldR2 [json] [fn _ => string]
            [fn ts => ts' :: {Type} -> [ts ~ ts'] => result (variant (ts ++ ts') * string)]
            (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest] (j : json t) name'
              (acc : ts' :: {Type} -> [rest ~ ts'] => result (variant (rest ++ ts') * string)) [fwd ::_] [[nm = t] ++ rest ~ fwd] =>
                if name = name' then
                  (v, s') <- j.FromJson s';
                  return (make [nm] v, s')
                else acc [fwd ++ [nm = t]])
            (fn [fwd ::_] [[] ~ fwd] => Failure <xml>Unknown JSON object variant name {[name]}</xml>)
            fl jss names) [[]] !;
        s' <- return (skipSpaces s');
        s' <- return
            (if s' <> "" && String.sub s' 0 = #","
                then skipSpaces (String.suffix s' 1)
                else s');
        if s' = "" then
          Failure <xml>JSON object doesn't end in brace</xml>
        else if String.sub s' 0 = #"}" then
          Success (r, String.suffix s' 1)
        else
          Failure <xml>Junk after JSON value in object</xml>,
    ToYaml = fn b i v =>
      (if b then "" else "\n" ^ indent i)
      ^ match v
        (@map2 [json] [fn _ => string] [fn t => t -> string]
          (fn [t] (j : json t) (name : string) (v : t) =>
            name ^ ": " ^ j.ToYaml False (i+2) v) fl jss names),
    FromYaml = fn b i s =>
                  if s = "" then
                      Failure <xml>No YAML variant tag found [1]</xml>
                  else
                      let
                          val (i', s') = readYamlLine (if b then Some i else None) s
                      in
                          if i' < i then
                              Failure <xml>No YAML variant tag found [2]</xml>
                          else
                              case String.split s' #":" of
                                  None => Failure <xml>No YAML variant tag found [3]: {[firstTen s]}</xml>
                                | Some (name, s') =>
                                  let
                                      val s' = skipRealSpaces s'
                                  in
                                      (@foldR2 [json] [fn _ => string]
                                              [fn ts => ts' :: {Type} -> [ts ~ ts'] => result (variant (ts ++ ts') * string)]
                                        (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest] (j : json t) (name' : string)
                                          (acc : ts' :: {Type} -> [rest ~ ts'] => result (variant (rest ++ ts') * string)) [fwd ::_] [[nm = t] ++ rest ~ fwd] =>
                                            if name = name' then
                                              (v, s') <- j.FromYaml False (i'+1) (skipRealSpaces s');
                                              return (make [nm] v, s')
                                            else acc [fwd ++ [nm = t]])
                                        (fn [fwd ::_] [[] ~ fwd] => Failure <xml>Unknown YAML object variant name: {[name]}</xml>)
                                        fl jss names) [[]] !
                                  end
                      end}

fun json_variant_anon [ts ::: {Type}] (fl : folder ts) (jss : $(map json ts)) : json (variant ts) = {
    ToJson = fn v => match v
      (@Top.mp [json] [fn t => t -> string]
        (fn [t] (j : json t) (v : t) => j.ToJson v) fl jss),
    FromJson = fn s =>
      (@foldR [json]
        [fn ts => ts' :: {Type} -> [ts ~ ts'] => result (variant (ts ++ ts') * string)]
        (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest] (j : json t)
          (acc : ts' :: {Type} -> [rest ~ ts'] => result (variant (rest ++ ts') * string)) [fwd ::_] [[nm = t] ++ rest ~ fwd] =>
            case acc [fwd] of
              Success x => acc [fwd ++ [nm = t]]
            | Failure x => (case j.FromJson s of
                Success (v, s') => Success (make [nm] v, s')
              | Failure _ => Failure x))
        (fn [fwd ::_] [[] ~ fwd] => Failure <xml>Unknown anonymous JSON variant</xml>)
        fl jss) [[]] !,
     ToYaml = fn b i v => match v
      (@Top.mp [json] [fn t => t -> string]
        (fn [t] (j : json t) (v : t) => j.ToYaml b i v) fl jss),
     FromYaml = fn b i s =>
      let
        val (i', s') = readYamlLine (if b then Some i else None) s
      in
        (@foldR [json]
          [fn ts => ts' :: {Type} -> [ts ~ ts'] => result (variant (ts ++ ts') * string)]
          (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest] (j : json t)
            (acc : ts' :: {Type} -> [rest ~ ts'] => result (variant (rest ++ ts') * string)) [fwd ::_] [[nm = t] ++ rest ~ fwd] =>
              case acc [fwd] of
                Success x => acc [fwd ++ [nm = t]]
              | Failure x => (case j.FromYaml False i' (skipRealSpaces s') of
                  Success (v, s') => Success (make [nm] v, s')
                | Failure _ => Failure x))
          (fn [fwd ::_] [[] ~ fwd] => Failure <xml>Unknown anonymous YAML variant</xml>)
          fl jss) [[]] !
      end}

val json_unit : json unit = json_record {} {}

fun json_dict [a] (j : json a) : json (list (string * a)) = {
    ToJson = fn ls =>
                 foldl (fn (name, v) acc =>
                           let
                               val this = escape name ^ ":" ^ j.ToJson v
                           in
                               case acc of
                                   "{" => "{" ^ this
                                 | _ => acc ^ "," ^ this
                           end) "{" ls ^ "}",
    FromJson = fn s =>
      let
        fun fromJ (s : string) (acc : list (string * a)) : result (list (string * a) * string) =
          if s = "" then
            Failure <xml>JSON object doesn't end in brace</xml>
          else if String.sub s 0 = #"}" then
            Success (rev acc, String.suffix s 1)
          else
            (name, s') <- unescape s;
            s' <- return (skipSpaces s');
            s' <- (if s' = "" || String.sub s' 0 <> #":"
                then Failure <xml>No colon after JSON object field name</xml>
                else Success (skipSpaces (String.suffix s' 1)));
            (v, s') <- j.FromJson s';
            s' <- return (skipSpaces s');
            s' <- return
                (if s' <> "" && String.sub s' 0 = #","
                    then skipSpaces (String.suffix s' 1)
                    else s');
            fromJ s' ((name, v) :: acc)
      in
        if s = "" || String.sub s 0 <> #"{" then
          Failure <xml>JSON dictionary doesn't begin with brace: {[firstTen s]}</xml>
        else
          fromJ (skipSpaces (String.suffix s 1)) []
      end,
     ToYaml = fn b i ls =>
                 removeNewlineIfAfterBullet b
                   (foldl (fn (k, v) acc => "\n" ^ indent i ^ k ^ ": " ^ j.ToYaml False (i+1) v ^ acc) "" ls),
     FromYaml = fn b i s =>
      let
        fun fromY b s acc =
          let
            val (i', s') = readYamlLine (if b then Some i else None) s
          in
            if i' < i then
              return (rev acc, s)
            else
              case String.split s' #":" of
                  None => Failure <xml>Couldn't find colon reading key-value list from YAML.</xml>
                | Some (name, s') =>
                  (name', rest) <- yamlStringIn 0 name;
                  (v, s') <- j.FromYaml False (i'+1) (skipRealSpaces s');
                  if String.all Char.isSpace rest then
                    fromY False s' ((name', v) :: acc)
                  else
                    Failure <xml>Malformed YAML key in dictionary: {[name]}</xml>
          end
      in
        if String.isPrefix {Full = s, Prefix = "{}"} then
          return ([], String.suffix s 2)
        else
          fromY b s []
      end}

fun json_derived [base] [derived]
        (f1 : base -> result derived) (f2 : derived -> base) (j : json base) : json derived = {
    ToJson = fn x => j.ToJson (f2 x),
    FromJson = fn s =>
      (x, s') <- j.FromJson s;
      x <- f1 x;
      return (x, s'),
    ToYaml = fn b i x => j.ToYaml b i (f2 x),
    FromYaml = fn b i s =>
      (x, s') <- j.FromYaml b i s;
      x <- f1 x;
      return (x, s')}

fun json_derived' [base] [derived] (f : base -> derived) :
        (derived -> base) -> json base -> json derived =
    json_derived (f >>> Success)


functor Recursive (M : sig
                       con t :: Type -> Type
                       val json_t : a ::: Type -> json a -> json (t a)
                   end) = struct
    open M

    datatype r = Rec of t r

    fun rTo (Rec x) = (json_t {ToJson = rTo,
                               FromJson = fn _ => error <xml>Tried to FromJson in ToJson!</xml>,
                               ToYaml = fn _ _ _ => error <xml>Tried to ToYaml in ToJson!</xml>,
                               FromYaml = fn _ _ _ => error <xml>Tried to FromYaml in ToJson!</xml>}).ToJson x

    fun rFrom s =
      (x, s') <- (json_t {
        ToJson = fn _ => error <xml>Tried to ToJson in FromJson!</xml>,
        FromJson = rFrom,
        ToYaml = fn _ _ _ => error <xml>Tried to ToYaml in FromJson!</xml>,
        FromYaml = fn _ _ _ => error <xml>Tried to FromYaml in FromJson!</xml>}).FromJson s;
      return (Rec x, s')

    fun yTo b i (Rec x) = (json_t {ToYaml = yTo,
                                   FromYaml = fn _ _ _ => error <xml>Tried to FromYaml in ToYaml!</xml>,
                                   ToJson = fn _ => error <xml>Tried to ToJson in ToYaml!</xml>,
                                   FromJson = fn _ => error <xml>Tried to FromJson in ToYaml!</xml>}).ToYaml b i x

    fun yFrom b i s =
      (x, s') <- (json_t {
        ToYaml = fn _ _ => error <xml>Tried to ToYaml in FromYaml!</xml>,
        FromYaml = yFrom,
        ToJson = fn _ => error <xml>Tried to ToJson in FromYaml!</xml>,
        FromJson = fn _ => error <xml>Tried to FromJson in FromYaml!</xml>}).FromYaml b i s;
      return (Rec x, s')

    val json_r = {ToJson = rTo, FromJson = rFrom, ToYaml = yTo, FromYaml = yFrom}
end

datatype prim = String of string | Int of int | Float of float | Bool of bool

fun primOut x =
    case x of
        String s => escape s
      | Int n => show n
      | Float n => show n
      | Bool True => "true"
      | Bool False => "false"

fun primIn (s : string) : result (prim * string) =
  if s = "" then
    Failure <xml>Reading primitive from empty JSON string</xml>
  else
    let val ch = String.sub s 0 in
      if ch = #"\"" || ch = #"'" then
        (r, s') <- unescape s;
        return (String r, s')
      else if String.isPrefix {Full = s, Prefix = "true"} then
        return (Bool True, String.suffix s 4)
      else if String.isPrefix {Full = s, Prefix = "false"} then
        return (Bool False, String.suffix s 5)
      else if Char.isDigit ch || ch = #"-" || ch = #"." then
        (r, s') <- numIn s;
        case read r of
            Some n => return (Int n, s')
          | None =>
            case read r of
                Some n => return (Float n, s')
              | None => Failure <xml>Invalid number in JSON</xml>
      else
        Failure <xml>Didn't find primitive where expected in JSON</xml>
    end

val json_prim =
    {ToJson = primOut,
     ToYaml = fn _ _ => primOut,
     FromJson = primIn,
     FromYaml = fn _ _ => primIn}

val show_prim = mkShow (fn x =>
                           case x of
                               String s => s
                             | Int n => show n
                             | Float n => show n
                             | Bool b => show b)

val json_datatype [t] [ts] (fl : folder ts)
        (js : $(map json ts))
        (names : $(map (fn _ => string) ts))
        (fromVariant : $(map (fn t' => t' -> t) ts))
        (toVariant : t -> variant ts) : json t =
  let val jv : json (variant ts) = @@json_variant [ts] fl js names
  in json_derived' (fn x => (match x fromVariant)) toVariant jv
  end

functor RecursiveDataType
    (M : sig
     con t :: Type
     con ts :: Type -> {Type}
     val fl : a ::: Type -> folder (ts a)
     val js : a ::: Type -> json a -> $(map json (ts a))
     val names : a ::: Type -> $(map (fn _ => string) (ts a))
     val to : a ::: Type -> (a -> t) -> $(map (fn t' => t' -> t) (ts a))
     val from : a ::: Type -> (t -> a) -> t -> variant (ts a)
     end) = struct
    open M

    structure RecT = Recursive(struct
        con t self = variant (ts self)
        val json_t [a] (ja : json a) : json (t a) =
            @@json_variant [ts a] fl (js ja) names
    end)

    fun to' (RecT.Rec x) : t = match x (to to')
    fun from' (x : t) : RecT.r = RecT.Rec (from from' x)

    val json_t : json t = json_derived' to' from' RecT.json_r

end

functor RecursiveDataTypeAnon
    (M : sig
     con t :: Type
     con ts :: Type -> {Type}
     val fl : a ::: Type -> folder (ts a)
     val js : a ::: Type -> json a -> $(map json (ts a))
     val to : a ::: Type -> (a -> t) -> $(map (fn t' => t' -> t) (ts a))
     val from : a ::: Type -> (t -> a) -> t -> variant (ts a)
     end) = struct
    open M

    structure RecT = Recursive(struct
        con t self = variant (ts self)
        val json_t [a] (ja : json a) : json (t a) =
            @@json_variant_anon [ts a] fl (js ja)
    end)

    fun to' (RecT.Rec x) : t = match x (to to')
    fun from' (x : t) : RecT.r = RecT.Rec (from from' x)

    val json_t : json t = json_derived' to' from' RecT.json_r

end
