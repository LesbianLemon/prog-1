(* ========== Vaja 2: Uvod v funkcijsko programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Vektorje predstavimo kot seznam števil s plavajočo vejico.
[*----------------------------------------------------------------------------*)

type vector = float list

(*----------------------------------------------------------------------------*]
Definirajte enotske vektorje `i`, `j` in `k` v treh dimenzijah.
[*----------------------------------------------------------------------------*)

let i : vector = [1.; 0.; 0.]
let j : vector = [0.; 1.; 0.]
let k : vector = [0.; 0.; 1.]

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razteg : float -> vector -> vector`, ki vektor, 
predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let razteg (k : float) (vector : vector) : vector = List.map (( *. ) k) vector

(*let rec razteg (k : float) (vector : vector) : vector = match vector with*)
(*    | [] -> []*)
(*    | (x : float) :: (tail : vector) -> (k *. x) :: razteg k tail*)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `sestej : vector -> vector -> vector`, ki vrne vsoto dveh 
vektorjev.
[*----------------------------------------------------------------------------*)

let sestej (vector1 : vector) (vector2 : vector) : vector = List.map2 ( +. ) vector1 vector2

(*let rec sestej (vector1 : vector) (vector2 : vector) : vector = match vector1, vector2 with*)
(*    | [], [] -> []*)
(*    | (_, []) | ([], _) -> raise (Invalid_argument "Vectors are not the same length")*)
(*    | (x1 : float) :: (tail1 : vector), (x2 : float) :: (tail2 : vector) -> (x1 +. x2) :: sestej tail1 tail2*)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `skalarni_produkt : vector -> vector -> float`, ki izračuna 
skalarni produkt dveh vektorjev
[*----------------------------------------------------------------------------*)

let skalarni_produkt (vector1 : vector) (vector2 : vector) : float = List.fold_left2 (fun acc x y -> x *. y +. acc) 0. vector1 vector2

(*let rec skalarni_produkt (vector1 : vector) (vector2 : vector) : float = match vector1, vector2 with*)
(*    | [], [] -> *)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `norma : vector -> float`, ki vrne evklidsko normo vektorja.
[*----------------------------------------------------------------------------*)

let norma (vector : vector) : float = skalarni_produkt vector vector |> Float.sqrt

(*----------------------------------------------------------------------------*]
Napišite funkcijo `projeciraj : vector -> vector -> vector`, ki izračuna 
projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projeciraj (vector1 : vector) (vector2 : vector) : vector = razteg ((skalarni_produkt vector1 vector2) /. (norma vector2)) vector2

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML 
oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.

Primer:
`ovij "h1" "Hello, world!"`

[*----------------------------------------------------------------------------*)

let ovij (tag : string) (content : string) : string = "<" ^ tag ^ ">" ^ content ^ "</" ^ tag ^ ">"

(*----------------------------------------------------------------------------*]
Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število 
presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za ustrezno število presledkov.

Primer:
`zamakni 4 "Hello, world!"`

[*----------------------------------------------------------------------------*)

let zamakni (n : int) (string : string) : string =
    string
    |> String.split_on_char '\n'
    |> List.map (( ^ ) (String.make n ' '))
    |> String.concat "\n"

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne 
niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:

Primer:
`ul ["ananas"; "banana"; "čokolada"]`

[*----------------------------------------------------------------------------*)

let ul (string_list : string list) : string =
    string_list
    |> List.map (ovij "li")
    |> String.concat "\n"
    |> zamakni 4
    |> (fun (string : string) : string -> ovij "ul" ("\n" ^ string ^ "\n"))

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme niz, 
ki vsebuje vejico, loči na del pred in del za njo.

Primer:
`razdeli_vrstico "mleko, 2"`

[*----------------------------------------------------------------------------*)

let razdeli_vrstico (string : string) : string * string =
    string
    (*|> (fun (string : string) : (string * string) -> let s1 :: s2 :: _ = String.split_on_char ',' string in (s1, s2))*)
    |> (fun (string : string) : (string * string) -> match (String.split_on_char ',' string) with
        | s1 :: s2 :: _ -> (s1, s2)
        | _ -> raise (Invalid_argument "Given string does not contain a comma")
    )

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`, 
ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike 
"izdelek, vrednost", in vrne seznam ustreznih parov.

Primer:
`pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"`

[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov (string : string) : (string * string) list =
    string
    |> String.split_on_char '\n'
    |> List.map razdeli_vrstico

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list`,
ki dano funkcijo uporabi na vseh drugih komponentah elementov seznama.

Primer:
```ml
let seznam = [("ata", "mama"); ("teta", "stric")] in 
pretvori_druge_komponente String.length seznam
```

[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente (f : 'a -> 'b) (pair_list : (string * 'a) list) : (string * 'b) list =
    pair_list
    |> List.map (fun ((string, other) : string * 'a) : (string * 'b) -> (string, f other))

(*----------------------------------------------------------------------------*]
Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki 
sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni 
znesek nakupa.

Primer:
```ml
let nakupovalni_seznam = "mleko, 2\njabolka, 5"
and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
izracunaj_skupni_znesek cenik nakupovalni_seznam
```

[*----------------------------------------------------------------------------*)

(*let rec izracunaj_skupni_znesek (string1 : string) (string2 : string) : float =*)
(*    let split_newline (string : string) : string list = String.split_on_char '\n' in*)
(*    split_newline string1 @ split_newline string2*)
(*    |> List.map pretvori_v_seznam_parov*)
(*    |> pretvori_druge_komponente (fun )*)
