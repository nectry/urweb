type t = char

val isAlnum = Basis.isalnum
val isAlpha = Basis.isalpha
val isBlank = Basis.isblank
val isCntrl = Basis.iscntrl
val isDigit = Basis.isdigit
val isGraph = Basis.isgraph
val isLower = Basis.islower
val isPrint = Basis.isprint
val isPunct = Basis.ispunct
val isSpace = Basis.isspace
val isUpper = Basis.isupper
val isXdigit = Basis.isxdigit
val toLower = Basis.tolower
val toUpper = Basis.toupper

val toInt = Basis.ord
val fromInt = Basis.chr

fun fromHex n =
    if n < 0 then
        Basis.error <xml>Char.fromHex: negative</xml>
    else if n < 10 then
        fromInt (toInt #"0" + n)
    else if n < 16 then
        fromInt (toInt #"A" + n - 10)
    else
        Basis.error <xml>Char.fromHex: too big</xml>

fun toHex ch =
    if isDigit ch then
        toInt ch - toInt #"0"
    else if isXdigit ch then
        toInt ch - toInt (if isUpper ch then #"A" else #"a") + 10
    else
        Basis.error <xml>Char.toHex: Invalid hexadecimal digit "{[ch]}"</xml>

