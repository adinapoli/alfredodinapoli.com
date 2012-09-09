---
title: Humbly simple F# Maybe monad application scenario
description: Here I describe a simple scenario where monads are useful.
tags: fp, fsharp
---

##Humbly simple F# Maybe monad application scenario
I won't turn this article in another spammish claim about monads and functional
programming. If you are interested in digging inside it, from a theoretical
point of view, I suggest you start <a
href="http://en.wikipedia.org/wiki/Monad_(functional_programming)">here</a> to
get the gist of what I'm talking about. Let's say briefly that monads are a
useful and clever way to write safe and more beautiful code. For once let's
focus to the practical side of the question, without further ado.

###A simple scenario
Suppose you are working on a real world application for a supermarket, and you need to create a simple data structure to store products, with their IDs and prices. The pair (key,value) immediately make the word dictionary to spring up in mind. So you want to create a data structures that embeds a dictionary and you want also to expose a bunch of APIs for inserting a new product and to get the price of a product given his ID. In a standard Object Oriented language you would end up writing a thing like this:

~~~~~{.cpp}
class Inventory
{
public:
    Inventory();
    void Stock(const std::string& id, float price){ inventory_[id] = price; }
    float Price(const std::string&; id){ ... }
private:
    std::map < std::string, float> inventory_;
};
~~~~~

Let's focus our attention to the Price method. You are asking the price for a certain product, given his id. This translates in a lookup inside our inventory map. But what about if the product doesn't exists? Good programmers would throw an exception, that will be captured at run-time somewhere, leading to a code that we know very well, and that's not so beautiful to look at:

~~~~~{.cpp}
float sumPrices(const Inventory&; i, const std::string&; id1, const std::string id2)
{
    try
    {
      float p1 = i.Price(id1);
      float p2 = i.Price(id2);
      return p1 + p2;
    } catch (ProductNotFoundException e)
    { //handle somehow }

}
~~~~~

Monads and F# comes to the rescue, allowing us to write a great deal more beautiful code than this. Let's see how.

###Separating the impure from the pure
Now let's move our problem in the F# and .NET world. We need to create a type to model an Inventory, expose the above mentioned APIs and also provide a mechanism to sum prices, printing the total whether all the id were found, an error otherwise. The simplest thing to do is to reuse one of the existing .NET classes, to be precise a <em>System.Collections.Generic.Dictionary:</em>

~~~~~{.fsharp}
type ProductId = string
type Price = float

type Inventory() =
    let inv_ = new System.Collections.Generic.Dictionary<ProductId, Price>()

    member this.Stock (id : ProductId) (price : Price) =
        inv_.Add(id, price)
~~~~~

So, we have defined two type synonyms for a better understanding of the problem, this new type as well as a member to stock a new product, given its id and its price. Now we must be wary about writing the Price member, because in case of unsuccessful lookup .NET throws a
<em>System.Collections.Generic.KeyNotFoundException</em>, which is not good in our pure functional world. We elegantly solve this using the <em>Option</em> type. We can see <em>Option</em> as a type (to be precise it's a discriminated union) that represent only two possibles values: A success value where it wraps up the result in the type constructor <em>Some</em>, and a failure which translates to the value <em>None</em>:

~~~~~{.fsharp}
type Option<'a> =
    | Some of 'a
    | None
~~~~~

Now we can separate the "impure", null-mined .NET world from our domain:

~~~~~{.fsharp}
member this.Price (id : ProductId) =
    try
        Some(inv_.[id])
    with
        | : ? System.Collections.Generic.KeyNotFoundException -> None
~~~~~

Now every time we ask for a product, we don't take the risk of raising an exception, while having an <em>Option<Price></em> back give us a great power, because we can pattern match against an Option type to discriminate a function behavior according to the value. Little example:

~~~~~{.fsharp}
>let inv = new Inventory();;
val inv : Inventory
> inv.Stock "foo" 10.0;;
val it : unit = ()
> inv.Price "foo";;
val it : Price option = Some 10.0
> inv.Price "doesnotexists";;
val it : Price option = None
~~~~~

Pretty neat isn't it? Now that we have this nice type to work with, let's solve the second part of the problem: how to sum two or more product's prices, displaying the sum or an error messages if some product doesn't exist. To achieve our goal, we'll use the Maybe monad, implemented inside the <a href="https://github.com/fsharp/fsharpx">FSharpx </a>library.

###Concatenating computations
I won't bother you with jabbering about computational expressions and so on, but I'll show you the code. Let's first create a function that takes the computation final result (under the form of  an <em>Option<Price></em>) and print a success or a failure message:

~~~~~{.fsharp}
let reporter (priceSum : Price option) : unit =
    match priceSum with
    | Some(p) -> printfn "Total price: %g." p
    | None    -> printfn "One or more id(s) not found."
~~~~~

Nothing particularly amazing here. Now let's create a new infix function (for the lay people: a function that can be invoked between two parameters, in an infix way). Bear in mind that this is not always the way to go, because creating too much weird operators can lead to a less readable code, but here we won't care and proceed anyway:

~~~~~{.fsharp}
let inline (|@|) (p1 : Price option) (p2 : Price option) =
    maybe{
        let! v1 = p1
        let! v2 = p2
        return! Some(v1 + v2)
    }
~~~~~

Ok, so let's clarify things a bit. The easiest way to think about monads, in my opinion, is like a sort of Lego pieces, each piece may contain a value (or not, it depends).
<a href="http://alfredodinapoli.files.wordpress.com/2012/04/drawing-18.png"><img class="aligncenter size-full wp-image-467" title="Drawing 18" src="http://alfredodinapoli.files.wordpress.com/2012/04/drawing-18.png" alt="" width="630" height="391" /></a>

In this case in order to concatenate our computations we need to concatenate only the same Lego pieces together. Do you remember in the primary school when teachers keep you telling that you can't sum pears with apples? The concept is more or less the same. So we can sum and combine only monads of the same type. But how? The answer lies in the lines of code above. With the operator <em>let!</em> we performs a sort of <em>destructuring</em>  (to be precise a <em>monadic binding</em>) which means that we peek inside our Lego piece and we take out the content, whether it exists or not. <em>Yes, whether it exists or not.</em> In this specific case, the Maybe monad ensures us that failure is propagated. In other terms, if you "sum" a <em>Some(value)</em> with <em>None</em>, we will obtain <em>None</em>, because <em>None</em> is the <em>Zero</em> value for that monad. I don't want to dig too much inside this theoretical stuff, but every monad has it's zero, a value you can sum against without having an impact on the overall computation (think about the (+) operator, 0 (zero) is the numeric value you can sum indefinitely without affecting the result). All this babbling means that if <em>v1</em> and <em>v2</em> binds to a "real" float, the result will be wrapped inside a <em>Some(v1+v2)</em>, which is the real sum. Otherwise a <em>None</em> will be returned. Gosh, I hope is clear enough. To clarify, here is an example of our new operator in action:

~~~~~{.fsharp}
> let price1 = Some(10.0);;
val price1 : float option = Some 10.0
> let price2 = Some(4.90);;
val price2 : float option = Some 4.9
> price1 |@| price2;;
val it : Price option = Some 14.9
> price1 |@| None;;
val it : Price option = None
~~~~~

Seen? Pretty cool, isn't it? No meaningless exceptions, no null spreading everywhere. Just simple type who are concatenated to form a result.

###The final touch: making it more general
As final step we'll write a simple function that takes a list of ids, an inventory and prints our total whether it "exists" (i.e if every product is inside the inventory) or failure message if something goes wrong. Here we go:

~~~~~{.fsharp}
let sumAndReport (inventory : Inventory) (ids : ProductId list) : unit =
    let basket = List.map (fun pid -> inventory.Price(pid)) ids
    in List.reduce (fun p1 p2 -> p1 |@| p2) basket
    |> reporter
~~~~~

It should be clear enough. If you are an F# newcomer, the |> is the <em>pipeline </em>operator, is passes the result on the right side to a function on the left. In worth noting that when we get the list of products with the basket let binding, we don't care whether those products exist! In fact, monads (in this case the Maybe monad) abstract the computation incapsulating a success or a failure in a way that we can control, combine and mix without worrying about the rest. And this is only the tip of the iceberg!

I redirect you to this <a href="https://gist.github.com/2274497">gist</a>, where you'll find the full source code, as well as a couple of data for testing purpose.

###Conclusions
Monads are a clever idea to encapsulate a lot of things: failure, state, IO, etc.. and having monads support in F# is simply awesome. The real world can benefit from this strange creatures, I believe. In an upcoming article we'll see how make our code even more simple, beautiful and short thanks to some feedback provided on the gist from the user <a href="https://github.com/mausch">mausch</a>.

Stay tuned!

