(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
 Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let reverse (list : 'a list) : 'a list =
    let rec reverse_aux (acc : 'a list) (list : 'a list) : 'a list =
        match list with
        | [] -> acc
        | head :: tail -> reverse_aux (head :: acc) tail
    in
    reverse_aux [] list

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let repeat (x : 'a) (n : int) : 'a list =
    let rec repeat_aux (acc : 'a list) (n : int) : 'a list =
        match n with
        | n when n <= 0 -> acc
        | n -> repeat_aux (x :: acc) (n - 1)
    in
    repeat_aux [] n

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
Pri tem ne smete uporabbiti vgrajene funkcije [List.init].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let range (n : int) : int list =
    let rec range_aux (acc : int list) (i : int) : int list =
        if i > n then acc
        else range_aux (i :: acc) (i + 1)
    in
    range_aux [] 0
    |> List.rev

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f x0; f x1; f x2; ...].
 Pri tem ne smete uporabiti vgrajene funkcije [List.map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+) 2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let map (f : 'a -> 'b) (list : 'a list) : 'b list =
    let rec map_aux (acc : 'b list) (list : 'a list) : 'b list =
        match list with
        | [] -> acc
        | head :: tail -> map_aux (f head :: acc) tail
    in
    map_aux [] list
    |> List.rev

(*----------------------------------------------------------------------------*]
 Časovna zahtevnost operatorja [@] je linearna v prvem argumentu, poskušajte 
 napisati reverse tako, da bo bolj učinkovit in hkrati repno rekurziven.
 Pri tem ne smete uporabiti vgrajene funkcije [List.rev] ali [List.rev_append].
[*----------------------------------------------------------------------------*)

let reverse = reverse

(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x + 2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map_tlrec = map

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      index = 0
      for x in list:
          mapi_list += [f(x, index)]
          index += 1
      return mapi_list

 Pri tem ne smete uporabiti vgrajene funkcije [List.mapi].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let mapi (f : int -> 'a -> 'b) (list : 'a list) : 'b list =
    let rec mapi_aux (acc : 'b list) (i : int) (list : 'a list) =
        match list with
        | [] -> acc
        | head :: tail -> mapi_aux (f i head :: acc) (i + 1) tail
    in
    mapi_aux [] 0 list
    |> List.rev

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 Pri tem ne smete uporabiti vgrajene funkcije [List.combine].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

let zip (list1 : 'a list) (list2 : 'b list) : ('a * 'b) list =
    if List.length list1 <> List.length list2 then raise (Failure "Different lengths of input lists.")
    else
        let rec zip_aux (acc : ('a * 'b) list) (list1 : 'a list) (list2 : 'b list) : ('a * 'b) list =
            match list1, list2 with
            | ([], _ | _, []) -> acc
            | head1 :: tail1, head2 :: tail2 -> zip_aux ((head1, head2) :: acc) tail1 tail2
        in
        zip_aux [] list1 list2
        |> List.rev

(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 Pri tem ne smete uporabiti vgrajene funkcije [List.split].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let unzip (pair_list : ('a * 'b) list) : 'a list * 'b list =
    let rec unzip_aux (acc : 'a list * 'b list) (pair_list : ('a * 'b) list) : 'a list * 'b list =
        match pair_list with
        | [] -> acc
        | (h1, h2) :: tail -> unzip_aux (h1 :: fst acc, h2 :: snd acc) tail
    in
    unzip_aux ([], []) pair_list
    |> (fun (list_pair : 'a list * 'b list) : ('a list * 'b list) -> (List.rev (fst list_pair), List.rev (snd list_pair)))

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec = unzip

(*----------------------------------------------------------------------------*]
 Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # loop (fun x -> x < 10) ((+) 4) 4;;
 - : int = 12
[*----------------------------------------------------------------------------*)

let rec loop (cond : 'a -> bool) (f : 'a -> 'a) (x : 'a) : 'a =
    if cond x then loop cond f (f x)
    else x

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc (f : 'a -> 'a -> 'a) (list : 'a list) : 'a =
    match list with
    | ([] | _ :: []) -> raise (Failure "Input list has less than two elements.")
    | head1 :: head2 :: [] -> f head1 head2
    | head1 :: head2 :: tail -> fold_left_no_acc f ((f head1 head2) :: tail)

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let apply_sequence (f : 'a -> 'a) (x : 'a) (n : int) : 'a list =
    let rec apply_sequence_aux (acc : 'a list) (n : int) : 'a list =
        if n < 0 then acc
        else apply_sequence_aux (x :: map f acc) (n - 1)
    in
    apply_sequence_aux [] n

(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 Pri tem ne smete uporabiti vgrajene funkcije [List.filter].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let filter (f : 'a -> bool) (list : 'a list) : 'a list =
    let rec filter_aux (acc : 'a list) (list : 'a list) : 'a list =
        match list with
        | [] -> acc
        | head :: tail -> filter_aux
            (
                if f head then head :: acc
                else acc
            )
            tail
    in
    filter_aux [] list
    |> List.rev

(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists (f : 'a -> bool) (list : 'a list) : bool =
    match list with
    | [] -> false
    | head :: tail ->
        if f head then true
        else exists f tail

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first (f : 'a -> bool) (default : 'a) (list : 'a list) : 'a =
    match list with
    | [] -> default
    | head :: tail ->
        if f head then head
        else first f default tail
