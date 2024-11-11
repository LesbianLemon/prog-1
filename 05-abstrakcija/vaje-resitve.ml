(*----------------------------------------------------------------------------*
 # Abstrakcija
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Naravna števila
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte signaturo `NAT`, ki določa strukturo naravnih števil. Ima osnovni
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov `int` tip. Opomba: Funkcije za
 pretvarjanje ponavadi poimenujemo `to_int` and `of_int`, tako da skupaj z
 imenom modula dobimo ime `NAT.of_int`, ki nam pove, da pridobivamo naravno
 število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT =
    sig
        type t

        val eq  : t -> t -> bool
        val zero : t
        val one : t
        val ( ++ ) : t -> t -> t
        val ( -- ) : t -> t -> t option
        val ( ** ) : t -> t -> t
        val to_int : t -> int 
        val of_int : int -> t option
    end

(*----------------------------------------------------------------------------*
 Napišite implementacijo modula `Nat_int`, ki zgradi modul s signaturo `NAT`,
 kjer kot osnovni tip uporablja OCamlov tip `int`. Namig: dokler ne
 implementirate vse funkcij v `Nat_int`, se bo OCaml pritoževal. Temu se lahko
 izognete tako, da funkcije, ki še niso napisane nadomestite z `failwith
 "later"`, vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT =
    struct
        type t = int

        let eq (x : t) (y : t) : bool = (x = y)

        let zero : t = 0
        let one : t = 1

        let ( ++ ) (x : t) (y : t) : t = x + y

        let ( -- ) (x : t) (y : t) : t option =
            if x < y then None
            else Some (x - y)

        let ( ** ) (x : t) (y : t) : t = x * y

        let to_int (x : t) : int = x

        let of_int (x : int) : t option =
            if x < 0 then None
            else Some x
    end

(*----------------------------------------------------------------------------*
 Napišite implementacijo `NAT`, ki temelji na [Peanovih
 aksiomih](https://en.wikipedia.org/wiki/Peano_axioms). Osnovni tip modula
 definirajte kot naštevni tip, ki vsebuje konstruktor za ničlo in konstruktor za
 naslednika nekega naravnega števila. Večino funkcij lahko implementirate s
 pomočjo rekurzije. Naprimer, enakost števil `k` in `l` določimo s hkratno
 rekurzijo na `k` in `l`, kjer je osnoven primer `Zero = Zero`.
[*----------------------------------------------------------------------------*)

module Nat_peano : NAT =
    struct
        type t =
            | Zero
            | Succ of t

        let rec eq (x : t) (y : t) : bool =
            match x, y with
            | Zero, Zero -> true
            | (_, Zero | Zero, _) -> false
            | Succ a, Succ b -> eq a b

        let zero : t = Zero
        let one : t = Succ Zero

        let rec ( ++ ) (x : t) (y : t) : t =
            match x with
            | Zero -> y
            | Succ a -> ( ++ ) a (Succ y)

        let rec ( -- ) (x : t) (y : t) : t option =
            match x, y with
            | _, Zero -> Some x
            | Zero, _ -> None
            | Succ a, Succ b -> ( -- ) a b

        let rec ( ** ) (x : t) (y : t) : t =
            let rec mul_aux (acc : t) (x : t) (y : t) : t =
                match x with
                | Zero -> acc
                | Succ a -> mul_aux (acc ++ y) a y
            in
            mul_aux Zero x y

        let to_int (x : t) : int =
            let rec to_int_aux (acc : int) (x : t) : int =
                match x with
                | Zero -> acc
                | Succ a -> to_int_aux (acc + 1) a
            in
            to_int_aux 0 x

        let of_int (x : int) : t option =
            if x < 0 then None
            else
                let rec of_int_aux (acc : t) (x : int) : t =
                    if x = 0 then acc
                    else of_int_aux (Succ acc) (x - 1)
                in
                Some (of_int_aux Zero x)
    end

(*----------------------------------------------------------------------------*
 Z ukazom `let module ImeModula = ... in ...` lahko modul definiramo samo
 lokalno. To bomo uporabili za to, da bomo lahko enostavno preklapljali med
 moduloma `Nat_int` in `Nat_peano`, saj bomo enega ali drugega shranili pod ime
 `Nat`. OCaml sicer pozna tudi ustrezne abstrakcije, ki omogočijo preklapljanje
 med moduli, na primer [funktorje](https://ocaml.org/docs/functors) ali
 [prvorazredne module](https://ocaml.org/manual/5.2/firstclassmodules.html), a
 bomo uporabili preprostejšo rešitev.

 Spodnji izračun dopolnite tako, da sešteje prvih 100 naravnih števil. Ker bo
 taka vsota tipa `NAT.t`, ki je abstrakten, končni rezultat pretvorite v tip
 `int` z uporabo funkcije `Nat.to_int`. Če ste oba modula implementirali
 pravilno, bi morali dobiti enak rezultat ne glede na to, katerega poimenujete
 `Nat`.
[*----------------------------------------------------------------------------*)

let sum_nat_100 : int = 
    (*let module Nat = Nat_int in *)
    let module Nat = Nat_peano in
    List.init 100 (( + ) 1)
    |> List.map
        (
            fun (x : int) : Nat.t ->
                match Nat.of_int x with
                | None -> Nat.zero
                | Some a -> a
        )
    |> List.fold_left Nat.( ++ ) Nat.zero
    |> Nat.to_int 
(* val sum_nat_100 : int = 5050 *)

(*----------------------------------------------------------------------------*
 ## Kompleksna števila
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 > Once upon a time, there was a university with a peculiar tenure
 > policy. All faculty were tenured, and could only be dismissed for
 > moral turpitude. What was peculiar was the definition of moral
 > turpitude: making a false statement in class. Needless to say, the
 > university did not teach computer science. However, it had a renowned
 > department of mathematics.
 >
 > One Semester, there was such a large enrollment in complex variables
 > that two sections were scheduled. In one section, Professor Descartes
 > announced that a complex number was an ordered pair of reals, and that
 > two complex numbers were equal when their corresponding components
 > were equal. He went on to explain how to convert reals into complex
 > numbers, what "i" was, how to add, multiply, and conjugate complex
 > numbers, and how to find their magnitude.
 >
 > In the other section, Professor Bessel announced that a complex number
 > was an ordered pair of reals the first of which was nonnegative, and
 > that two complex numbers were equal if their first components were
 > equal and either the first components were zero or the second
 > components differed by a multiple of 2π. He then told an entirely
 > different story about converting reals, "i", addition, multiplication,
 > conjugation, and magnitude.
 >
 > Then, after their first classes, an unfortunate mistake in the
 > registrar's office caused the two sections to be interchanged. Despite
 > this, neither Descartes nor Bessel ever committed moral turpitude,
 > even though each was judged by the other's definitions. The reason was
 > that they both had an intuitive understanding of type. Having defined
 > complex numbers and the primitive operations upon them, thereafter
 > they spoke at a level of abstraction that encompassed both of their
 > definitions.
 >
 > The moral of this fable is that: Type structure is a syntactic
 > discipline for enforcing levels of abstraction.
 >
 > John C. Reynolds, _Types, Abstraction, and Parametric Polymorphism_, IFIP83
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte signaturo modula kompleksnih števil. Potrebujemo osnovni tip, test
 enakosti, ničlo, enko, imaginarno konstanto i, negacijo, konjugacijo,
 seštevanje in množenje.
[*----------------------------------------------------------------------------*)

module type COMPLEX =
    sig
        type t

        val eq : t -> t -> bool
        val zero : t
        val one : t
        val i : t
        val negate : t -> t
        val conjugate : t -> t
        val ( ++ ) : t -> t -> t
        val ( ** ) : t -> t -> t
    end

(*----------------------------------------------------------------------------*
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX =
    struct
        type t = {re : float; im : float}

        let eq (x : t) (y : t) : bool = x.re = y.re && x.im = y.im

        let zero : t = {re = 0.; im = 0.}
        let one : t = {re = 1.; im = 0.}
        let i : t = {re = 0.; im = 1.}

        let negate (x : t) : t =
            {
                re = -. x.re;
                im = -. x.im;
            }

        let conjugate (x : t) : t = {x with im = -. x.im}

        let ( ++ ) (x : t) (y : t) : t =
            {
                re = x.re +. y.re;
                im = x.im +. y.im;
            }

        let ( ** ) (x : t) (y : t) : t =
            {
                re = (x.re *. y.re) -. (x.im *. y.im);
                im = (x.re *. y.im) +. (x.im *. y.re);
            }
    end

(*----------------------------------------------------------------------------*
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument). Priporočilo:
 Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga pustite za konec
 (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX =
    struct
        type t = {magn : float; arg : float}

        (* Pomožne funkcije za lažje življenje. *)
        let pi : float = 2. *. acos 0.
        let rad_of_deg (deg : float) : float = (deg /. 180.) *. pi
        let deg_of_rad (rad : float) : float = (rad /. pi) *. 180.

        let eq (x : t) (y : t) : bool = x.magn = y.magn && x.arg = y.arg

        let zero : t = {magn = 0.; arg = 0.}
        let one : t = {magn = 1.; arg = 0.}
        let i : t = {magn = 0.; arg = pi /. 2.}

        let negate (x : t) : t =
            let neg_arg =
                if 0. <= x.arg && x.arg < pi then pi +. x.arg
                else x.arg -. pi
            in
            {x with arg = neg_arg}

        let conjugate (x : t) : t =
            let conj_arg =
                if x.arg = 0. then 0.
                else 2. *. pi -. x.arg
            in
            {x with arg = conj_arg}

        let ( ++ ) (x : t) (y : t) : t = zero

        let ( ** ) (x : t) (y : t) : t =
            {
                magn = x.magn *. y.magn;
                arg = x.arg +. y.arg;
            }
    end
