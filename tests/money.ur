table products : { Product : string, Price : money }

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
