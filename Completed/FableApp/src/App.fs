module FableApp

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Import
open Fable.AST.Babel
open Fable.PowerPack
open Fable.PowerPack.Fetch

type Person = { lastname: string; firstname:string}

//region code 2
type ProductType =
    | Apple
    | Apricot
    | Banana
    | Cherry

type Quantity =
    | UnitQuantity of  int 
    | KilogramQuantity of decimal 

type ProductLabel = ProductLabel of string

type Product = ProductType * ProductLabel * Quantity

// mesure type 
[<Measure>] type eur
// Function type
type FruitPricer = Product -> float<eur> option

let productCatalog = 
    [ 
        Apple, ProductLabel("GrannySmith"), UnitQuantity(3) 
        Apple, ProductLabel("GoldenDelicious") , UnitQuantity(2) 
        Banana, ProductLabel ("Cavendish"), UnitQuantity(5) 
        Banana, ProductLabel ("GrosMichel"), UnitQuantity(3) 
        Cherry, ProductLabel ("Bing"), KilogramQuantity(0.250M) 
        Apricot, ProductLabel("Harglow"), UnitQuantity(6) 
    ]


let pricer:FruitPricer = 
    function
    | Apple,_ , UnitQuantity (q) -> Some (1.00<eur> * float(q))
    | Apricot,_ ,  UnitQuantity (q) -> Some (1.60<eur> *  float(q))
    | Banana,_ ,  UnitQuantity (q) -> Some (2.00<eur> * float(q))
    | Cherry,_ ,  KilogramQuantity (q) -> Some (1.45<eur>  * float(q))
    | _ ,_ , _  -> None

// endregion

//=======================
// Js components
//=======================

let createForm() =
    Browser.document.createElement_form()

let createInput id placeHolder = 
    let input = Browser.document.createElement_input()
    input.id <- id
    input.placeholder <- placeHolder
    input.className <- "form-control"
    input

let createRow() =
    let row = Browser.document.createElement_div()
    row.className <- "row m-3"
    row

let createCol() =
    let col = Browser.document.createElement_div()
    col.className <- "col"
    col
 
let createBtn text onclickHandler = 
    let button = Browser.document.createElement_button()
    button.onclick <- onclickHandler
    button.textContent <- text
    button.className <- "btn btn-primary m-3"
    button

let createDivWithText text =
    let elm = Browser.document.createElement_div()
    elm.innerText <- text
    elm


let createListItem innerHTML =
    let item = Browser.document.createElement_li()
    item.className <- "list-group-item"
    item.innerHTML <- innerHTML
    item

let createList items =
    let list = Browser.document.createElement_ul()
    list.className <- "list-group"
    
    items |> Seq.iter (list.appendChild >> ignore)
    list


let appendChild (child:HTMLElement) (parent:HTMLElement) =
    parent.appendChild child :?> HTMLElement

let appendChild' child parent = appendChild child parent |> ignore

let createPersonForm() = 
    let form = createForm()
    
    let row = form |> appendChild (createRow())

    row |> appendChild (createCol()) 
        |> appendChild' (createInput "firstname" "Firstname")

    row |> appendChild (createCol()) 
        |> appendChild' (createInput "lastname" "Lastname")

    form


let getPerson() = 
    // downcast to HTMLInputElement
    let firstInput = Browser.document.getElementById("firstname") :?> HTMLInputElement
    let lastInput = Browser.document.getElementById("lastname") :?> HTMLInputElement

    {firstname= firstInput.value; lastname=lastInput.value}

let showPerson = getPerson >> sprintf "%A" >> createDivWithText


let createProductItem pricer product = 
    let textformat = sprintf "<b>%s : %s</b> <i>%A</i>"

    let innerHtml = 
        match product with
        | Apple, ProductLabel(label), _ -> textformat "Apple" label (pricer product)
        | Apricot, ProductLabel(label), _ -> textformat "Apricot" label (pricer product)
        | Banana, ProductLabel(label), _ -> textformat "Banana" label (pricer product)
        | Cherry, ProductLabel(label), _ -> textformat "Cherry" label (pricer product)
        
    createListItem innerHtml

let showProducts pricer (catalog:Product list) = 
    catalog
    |> Seq.map (createProductItem pricer)
    |> createList


[<Pojo>]
type User = {
    id: int
    name: string
    username: string
    email: string
}

let fetchUser() =
    promise {
        let! res = fetch "https://jsonplaceholder.typicode.com/users" []
        let! users = res.json<User list>()

        return users
    }
let init() =
    let app = document.getElementById("app")
    
    app |> appendChild' (createPersonForm())

    let onclickHandler = fun (_:MouseEvent) ->
        app |> appendChild (showPerson()) :> obj

    app |> appendChild' (createBtn "valid" onclickHandler)

    app |> appendChild' (showProducts pricer productCatalog)


    let fetchOnclick e =
        promise {
                try
                    let! users = fetchUser()

                    let listElm = 
                        users
                        |> Seq.map (fun u -> createListItem u.name)
                        |> createList 
                    
                    app |> appendChild' listElm 
                with
                | ex -> Browser.window.alert (sprintf "Can't featch users: %s" ex.Message) 
            } |> Promise.start
            
        obj()
           
    app |> appendChild' (createBtn "Load users" fetchOnclick)


init()