type t = Basis.string

val str = Basis.str1

val length = Basis.strlen
val lengthGe = Basis.strlenGe
val append = Basis.strcat

val sub = Basis.strsub
val suffix = Basis.strsuffix

val index = Basis.strindex
fun sindex r = Basis.strsindex r.Haystack r.Needle
val atFirst = Basis.strchr

fun mindex {Haystack = s, Needle = chs} =
    let
        val n = Basis.strcspn s chs
    in
        if n >= length s then
            None
        else
            Some n
    end

fun substring s {Start = start, Len = len} = Basis.substring s start len

fun seek s ch =
    case index s ch of
        None => None
      | Some i => Some (suffix s (i + 1))
fun mseek {Haystack = s, Needle = chs} =
    case mindex {Haystack = s, Needle = chs} of
        None => None
      | Some i => Some (sub s i, suffix s (i + 1))

fun split s ch =
    case index s ch of
        None => None
      | Some i => Some (substring s {Start = 0, Len = i},
                        suffix s (i + 1))
fun split' s ch =
    case index s ch of
        None => None
      | Some i => Some (substring s {Start = 0, Len = i},
                        suffix s i)
fun msplit {Haystack = s, Needle = chs} =
    case mindex {Haystack = s, Needle = chs} of
        None => None
      | Some i => Some (substring s {Start = 0, Len = i},
                        sub s i,
                        suffix s (i + 1))

fun ssplit r =
    case sindex r of
        None => None
      | Some i => Some (substring r.Haystack {Start = 0, Len = i},
                        suffix r.Haystack (i + length r.Needle))

fun all f s =
    let
        val len = length s

        fun al i =
            i >= len
            || (f (sub s i) && al (i + 1))
    in
        al 0
    end

fun mp f s =
    let
        fun mp' i acc =
            if i < 0 then
                acc
            else
                mp' (i - 1) (str (f (sub s i)) ^ acc)
    in
        mp' (length s - 1) ""
    end

fun newlines [ctx] [[Body] ~ ctx] (s : string) : xml ([Body] ++ ctx) [] [] =
    case split s #"\n" of
        None => cdata s
      | Some (s1, s2) => <xml>{[s1]}<br/>{newlines s2}</xml>

fun isPrefix {Full = f, Prefix = p} =
    length f >= length p && substring f {Start = 0, Len = length p} = p

fun trim s =
    let
        val len = length s

        fun findStart i =
            if i < len && isspace (sub s i) then
                findStart (i+1)
            else
                i

        fun findFinish i =
            if i >= 0 && isspace (sub s i) then
                findFinish (i-1)
            else
                i

        val start = findStart 0
        val finish = findFinish (len - 1)
    in
        if finish >= start then
            substring s {Start = start, Len = finish - start + 1}
        else
            ""
    end


(* Copied from Char because urweb/lib modules don't know about each other. *)
fun toHex ch =
    if Basis.isdigit ch then
        Basis.ord ch - Basis.ord #"0"
    else if Basis.isxdigit ch then
        Basis.ord ch - Basis.ord (if Basis.isupper ch then #"A" else #"a") + 10
    else
        Basis.error <xml>String's version of Char.toHex: Invalid hexadecimal digit "{[ch]}"</xml>

fun parseHex (s : string) : int =
  let fun byDigit x acc =
        if x = length s then acc
        else byDigit (x+1) (16 * acc + toHex (sub s x))
  in byDigit 0 0 end
