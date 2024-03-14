open Json


fun fromJsonTest [t :: Type] (j : json t) (_ : show t) (i : int) (s : string) : xtable = case @@fromJsonR [t] j s of
    Success t => <xml><tr><td style="border-style: solid; padding: 10px">{[i]}</td><td><pre>{[t]}</pre></td></tr></xml>
  | Failure x => <xml><tr><td style="border-style: solid; padding: 10px">{[i]}</td><td>{x}</td></tr></xml>

fun toJsonTest [t ::: Type] (j : json t) (i : int) (t : t) : xtable =
    <xml><tr><td style="border-style: solid; padding: 10px">{[i]}</td><td><pre>{[toJson t]}</pre></td></tr></xml>

fun fromYamlTest [t :: Type] (j : json t) (_ : show t) (i : int) (s : string) : xtable = case @@fromYamlR [t] j s of
    Success t => <xml><tr><td style="border-style: solid; padding: 10px">{[i]}</td><td><pre>{[t]}</pre></td></tr></xml>
  | Failure x => <xml><tr><td style="border-style: solid; padding: 10px">{[i]}</td><td>{x}</td></tr></xml>

fun toYamlTest [t ::: Type] (j : json t) (i : int) (t : t) : xtable =
    <xml><tr><td style="border-style: solid; padding: 10px">{[i]}</td><td><pre>{[toYaml t]}</pre></td></tr></xml>



type myRow = [A = bool, B = string, C = list (option int), D = int, E = option string]

val _ : show (variant myRow) = mkShow <| fn x => match x {
  A = fn x => "A=" ^ show x,
  B = fn x => "B=\"" ^ show x ^ "\"",
  C = fn x => "C=" ^ show x,
  D = fn x => "D=" ^ show x,
  E = fn x => "E=" ^ (case x of Some x => show x | None => "null")}
val _ : show $myRow = mkShow (fn r => "{A=" ^ show r.A ^ ", B=" ^ show r.B ^ ", C="^ show r.C ^ ", D="^ show r.D ^ ", E="^ (case r.E of Some x => show x | None => "null") ^ "}")
val _ : json $myRow = json_record_withDefaults {A = "A", B = "B", D = "D"} {C = ("C", []), E = ("E", None)}


fun main () : transaction page = return <xml><body><table>
  {fromJsonTest [string] 1 "\"\\\\line \/ 1\\nline 2\""}
  {fromJsonTest [list int] 2 "[1, 2, 3]"}
  {toJsonTest 3 ("hi" :: "bye\"" :: "hehe" :: [])}
  {@@toJsonTest [$myRow] _  4 {A = False, B = "35", C = Some 3 :: None :: [], D = 42, E = None}}
  {fromJsonTest [$myRow] 5 "{\"A\": false,\"D\": 42,\"B\": \"foo\", \"C\": [3, null]}"}
  {let val _ : json (variant myRow) = json_variant {A = "A", B = "B", C = "C", D = "D", E = "E"}
    in <xml>
    {toJsonTest 6 (make [#B] "foo" : variant myRow)}
    {fromJsonTest [variant myRow] 7 "{\"A\": true}"}
    {fromJsonTest [variant myRow] 8 "{\"D\": 35}"}
    {fromJsonTest [variant myRow] 9 "{\"B\": \"35\"}"}
    </xml>end}
  {let val _ : json (variant myRow) = json_variant_anon
    in <xml>
    {toJsonTest 10 (make [#B] "foo" : variant myRow)}
    {fromJsonTest [variant myRow] 11 "true"}
    {fromJsonTest [variant myRow] 12 "35"}
    {fromJsonTest [variant myRow] 13 "\"35\""}
    </xml>end}
  {fromJsonTest [$myRow] 14 "{\"A\": false,\"D\": 42,\"B\": \"foo\"}"}
  {@@toJsonTest [$myRow] _ 15 {A = False, B = "35", C = [], D = 42, E = None}}

  {fromYamlTest [string] 21 "\"\\\\line \/ 1\\nline 2\""}
  {fromYamlTest [list int] 22 "-1\n-2\n-3"}
  {toYamlTest 23 ("hi" :: "bye\"" :: "hehe" :: [])}
  {@@toYamlTest [$myRow] _  24 {A = False, B = "35", C = Some 3 :: None :: [], D = 42, E = None}}
  {fromYamlTest [$myRow] 25 "A: false\nD: 42\nC:\n  - 3\n  - null\nB: \"foo\""}
  {let val _ : json (variant myRow) = json_variant {A = "A", B = "B", C = "C", D = "D", E = "E"}
    in <xml>
    {toYamlTest 26 (make [#B] "foo" : variant myRow)}
    {fromYamlTest [variant myRow] 27 "A: true"}
    {fromYamlTest [variant myRow] 28 "D: 35"}
    {fromYamlTest [variant myRow] 29 "B: \"35\""}
    </xml>end}
  {let val _ : json (variant myRow) = json_variant_anon
    in <xml>
    {toYamlTest 30 (make [#B] "foo" : variant myRow)}
    {fromYamlTest [variant myRow] 31 "true"}
    {fromYamlTest [variant myRow] 32 "35"}
    {fromYamlTest [variant myRow] 33 "\"35\""}
    </xml>end}
  {@@toYamlTest [$myRow] _ 34 {A = False, B = "35", C = [], D = 42, E = None}}
  {fromYamlTest [$myRow] 35 "A: false\nD: 42\nB: \"foo\""}
  </table>
  </body></xml>
