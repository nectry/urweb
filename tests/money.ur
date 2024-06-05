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

fun addClient r =
    dml (INSERT INTO products(Product, Price)
         VALUES ({[r.Product]}, {[r.Price]}))

fun mainClient () =
    prods <- queryL1 (SELECT *
                      FROM products
                      ORDER BY products.Product);
    prods <- source prods;
    addProd <- return (fn r =>
                          prodsV <- get prods;
                          set prods (List.sort (fn a b => a.Product > b.Product) (r :: prodsV)));
    prod <- source "";
    price <- source "";
    pr1 <- source "";
    pr2 <- source "";
    cmp <- source None;
    return <xml><body>
      <table>
        <tr> <th>Product</th> <th>Price</th> </tr>
        <dyn signal={prods <- signal prods;
                     return (List.mapX (fn r => <xml><tr>
                       <td>{[r.Product]}</td> <td>{[r.Price]}</td>
                     </tr></xml>) prods)}/>
      </table>

      <h1>Add new</h1>

      Name: <ctextbox source={prod}/><br/>
      Price: <ctextbox source={price}/><br/>
      <button onclick={fn _ =>
                          prod <- get prod;
                          price <- get price;
                          case read price of
                              None => error <xml>Invalid price</xml>
                            | Some price =>
                              pr <- return {Product = prod, Price = price};
                              rpc (addClient pr);
                              addProd pr}>Submit</button>
      <button onclick={fn _ =>
                          prod <- get prod;
                          price <- get price;
                          pr <- return {Product = prod, Price = readError price};
                          rpc (addClient pr);
                          addProd pr}>SubmitEx</button>

      <h1>Comparison</h1>

      Price #1: <ctextbox source={pr1}/><br/>
      Price #2: <ctextbox source={pr2}/><br/>
      <button onclick={fn _ =>
                          pr1 <- get pr1;
                          pr2 <- get pr2;
                          case (read pr1, read pr2) of
                              (Some (pr1 : money), Some pr2) => set cmp (Some (pr1, pr2))
                            | _ => error <xml>Malformed price(s)</xml>}>Compare</button><br/>
      <dyn signal={cmp <- signal cmp;
                   return (case cmp of
                               None => <xml></xml>
                             | Some (pr1, pr2) => <xml>
                               Equal? {[pr1 = pr2]}<br/>
                               Less-than? {[pr1 < pr2]}<br/>
                               Less-than-or-equal? {[pr1 <= pr2]}
                             </xml>)}/>
    </body></xml>
