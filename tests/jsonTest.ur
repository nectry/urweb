open Json

type myRow = [A = bool, B = string, C = int]

val _ : show (variant myRow) = mkShow <| fn x => match x {
  A = fn x => "A=" ^ show x,
  B = fn x => "B=\"" ^ show x ^ "\"",
  C = fn x => "C=" ^ show x}
val _ : show $myRow = mkShow (fn r => "{A=" ^ show r.A ^ ", B=" ^ show r.B ^ ", C="^ show r.C ^ "}")
val _ : json $myRow = json_record {A = "A", B = "B", C = "C"}

val (a1,a2,a3,a4) =
  let val _ : json (variant myRow) = json_variant_anon
  in
    ( toJson (make [#B] "foo" : variant myRow)
    , fromJson "true" : variant myRow
    , fromJson "35" : variant myRow
    , fromJson "\"35\"" : variant myRow) end

val (v1,v2,v3,v4) =
  let val _ : json (variant myRow) = json_variant {A = "A", B = "B", C = "C"}
  in
    ( toJson (make [#B] "foo" : variant myRow)
    , fromJson "{\"A\": true}" : variant myRow
    , fromJson "{\"C\": 35}" : variant myRow
    , fromJson "{\"B\": \"35\"}" : variant myRow) end



fun main () : transaction page = return <xml><body>
  <pre>{[ fromJson "\"\\\\line \/ 1\\nline 2\"" : string ]}</pre><br/>
  <pre>{[fromJson "[1, 2, 3]" : list int]}</pre><br/>
  <pre>{[toJson ("hi" :: "bye\"" :: "hehe" :: [])]}</pre><br/>
  <pre>{[toJson {A = False, B = "35", C = 42}]}</pre><br/>
  <pre>{[fromJson "{\"A\": false,\"C\": 42,\"B\": \"foo\"}" : $myRow]}</pre><br/>
  <pre>{[a1]}</pre><br/>
  <pre>{[a2]}</pre><br/>
  <pre>{[a3]}</pre><br/>
  <pre>{[a4]}</pre><br/>
  <pre>{[v1]}</pre><br/>
  <pre>{[v2]}</pre><br/>
  <pre>{[v3]}</pre><br/>
  <pre>{[v4]}</pre><br/>
  </body></xml>
