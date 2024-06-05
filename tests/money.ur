table products : { Product : string, Price : money }

fun compare r =
    case (read r.P1, read r.P2) of
        (Some (p1 : money), Some p2) =>
        return <xml><body>
          Equal? {[p1 = p2]}<br/>
          Less-than? {[p1 < p2]}<br/>
          Less-than-or-equal? {[p1 <= p2]}
        </body></xml>
      | _ => error <xml>Malformed prices</xml>

fun main () =
    prods <- queryX (SELECT *
                     FROM products
                     ORDER BY products.Product)
                    (fn {Products = r} => <xml><tr>
                      <td>{[r.Product]}</td> <td>{[r.Price]}</td>
                    </tr></xml>);
    return <xml><body>
      <table>
        <tr> <th>Product</th> <th>Price</th> </tr>
        {prods}
      </table>

      <h1>Add new</h1>

      <form>
        Name: <textbox{#Product}/><br/>
        Price: <textbox{#Price}/><br/>
        <submit action={add}/>
      </form>

      <h1>Add new (exceptions version)</h1>

      <form>
        Name: <textbox{#Product}/><br/>
        Price: <textbox{#Price}/><br/>
        <submit action={addEx}/>
      </form>

      <h1>Comparison</h1>

      <form>
        Price #1: <textbox{#P1}/><br/>
        Price #2: <textbox{#P2}/><br/>
        <submit action={compare}/>
      </form>
    </body></xml>

and add r =
    case read r.Price of
        None => error <xml>Poorly formatted price: "{[r.Price]}"</xml>
      | Some pr =>
        dml (INSERT INTO products(Product, Price) VALUES ({[r.Product]}, {[pr]}));
        main ()

and addEx r =
    dml (INSERT INTO products(Product, Price) VALUES ({[r.Product]}, {[readError r.Price]}));
    main ()
