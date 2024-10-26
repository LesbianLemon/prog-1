# Zbirnik

Pri vseh nalogah se vam pod razdelkom **RAM** splača nastaviti _View_ na _Decimal_, da bodo vrednosti v pomnilniku predstavljene z običajnim desetiškim zapisom. Prav tako lahko pod možnostjo _Register addressing_ obkljukajte tiste registre, v katerih bodo shranjeni naslovi, saj bodo tisti naslovi ustrezno obarvani.

## Računanje ostanka

Zapišite program, ki v register `A` zapiše ostanek pri deljenju registra `A` z registrom `B`.

```
MOV A, 25
MOV B, 4

CALL remainder
HLT

remainder:		; Calculate remainder of A/B and saves to C
	PUSH A
	MOV C, A
	DIV B
	MUL B
	SUB C, A
	POP A
	RET
```

## Zaporedna števila

Zapišite program, ki na sklad zaporedno postavlja števila od 13 do 42.

```
MOV A, 13
MOV B, 43

sequence:       ; Writes sequence from A to B to stack
	CMP A, B
	JB .loop
.loop:
	PUSH A
	INC A
	CMP A, B
	JB .loop
```

## Menjava pomnilniških celic

Poleg registrov in sklada lahko podatke shranjujemo tudi v pomnilniške celice. To storimo prek naslovov oblike `[N]`, kjer je `N` zaporedni indeks mesta v pomnilniku, ali `[R]`, kjer je `R` ime registra, v katerem je shranjen indeks. Zapišite program, ki zamenja vsebini pomnilniških celic na naslovih `[A]` in `[B]`.

```
MOV A, 210
MOV B, 240

MOV [A], 210
MOV [B], 11

CALL switch
HLT

switch:			; Switches values [A] and [B]
	PUSH C
	PUSH D
	MOV C, [A]
	MOV D, [B]
	MOV [A], D
	MOV [B], C
	POP D
	POP C
	RET
```

## Iskanje najmanjšega števila v seznamu

Zapišite program, ki poišče najmanjše število v danem seznamu. Seznam naj bo podan na začetku pomnilnika in sicer tako, da je v prvih dveh celicah ukaz za skok na začetek programa, v tretji celici dolžina seznama, v naslednjih celicah zaporedoma elementi seznama, takoj za njimi pa celica, v katerega naj se zapiše najmanjše število. Podatke lahko v pomnilnik zapišemo z ukazom `DB`, ki se ne prevede v noben strojni ukaz, ampak samo shrani vrednost v naslednji prostor v pomnilniku. Zgornje podatke bi z njim zapisali takole:

    JMP main
    dolzina:
        DB 10    ; število elementov v seznamu
    seznam:
        DB 50    ; seznam
        DB 56
        DB 60
        DB 46
        DB 44
        DB 58
        DB 42
        DB 52
        DB 48
        DB 54
    minimum:
        DB 0    ; na koncu bo tu minimum

    main:
        ...

```
JMP main
length:
	DB 10	; Number of elements in list
list:
	DB 50	; List elements
	DB 56
	DB 60
	DB 46
	DB 44
	DB 58
	DB 42
	DB 52
	DB 48
	DB 54
minimum:
	DB 0	; Placeholder for minimum

main:
CALL minimum_element
HLT

minimum_element:
	PUSH A
	PUSH B
	PUSH C
	MOV A, list
	MOV B, minimum
	MOV C, [list]
.loop:
	INC A
	CMP A, B
	JAE .stop
	CMP C, [A]
	JBE .loop
	MOV C, [A]
	JMP .loop
.stop:
	MOV A, minimum
	MOV [A], C
	POP C
	POP B
	POP A
	RET
```

## Indeks najmanjšega števila v seznamu

Zapišite funkcijo `poisci_minimum`, ki v register `B` shrani indeks najmanjšega števila v rezini `[A:C]` torej med števili, ki se nahajajo od vključno naslova, shranjenega v `A`, do naslova pred tistim, shranjenim v `C`. Funkcija naj z izjemo registra `B` vrednosti ostalih registrov pusti nespremenjene. Če funkcija deluje pravilno, bi moral spodnji program delovati kot tisti iz prejšnje naloge:

    main:
        ; pripravimo parametre funkcije
        MOV A, seznam
        MOV C, seznam
        ADD C, [dolzina]
        ; pokličemo funkcijo
        CALL poisci_minimum
        ; v mesto, na katerega kaže minimum, shranimo vrednost, na katero kaže B
        ; ker tega ne moremo narediti direktno, si pomagamo z registrom C
        PUSH C 
        MOV C, [B]
        MOV [minimum], C
        POP C
        HLT

```
JMP main
length:
	DB 10	; Number of elements in list
list:
	DB 50	; List elements
	DB 56
	DB 60
	DB 46
	DB 44
	DB 58
	DB 42
	DB 52
	DB 48
	DB 54
minimum:
	DB 0	; Placeholder for minimum

main:
MOV A, list
MOV C, list
ADD C, [length]
CALL find_minimum
PUSH C
MOV C, [B]
MOV [minimum], C
POP C
HLT

find_minimum:
	PUSH A
	PUSH D
	MOV B, A
	MOV D, [A]
.loop:
	INC A
	CMP A, C
	JAE .stop
	CMP D, [A]
	JBE .loop
	MOV D, [A]
	MOV B, A
	JMP .loop
.stop:
	POP D
	POP A
	RET
```

## Urejanje seznama

Zapišite funkcijo `uredi`, ki elemente v rezini [A:C] uredi od najmanjšega do največjega. Pri tem naj vrednosti vseh registrov pusti pri miru. Eden najenostavnejših algoritmov za urejanje je urejanje z izbiranjem. V njem se zapeljete čez seznam, poiščete indeks najmanjšega elementa, nato pa ta element zamenjate s tistim na prvem mestu. Postopek nadaljujete s preostankom seznama, dokler ne pridete do konca.

Delovanje lahko preverite s sledečim programom:

    main:
                            ; pripravimo argumente za funkcijo uredi
        MOV A, seznam       ; register A mora kazati na prvi element
        MOV C, seznam       ; register C mora kazati na zadnji element + 1
        ADD C, [dolzina]
        CALL uredi          ; pokličemo funkcijo za urejanje
        HLT                 ; prekinemo izvajanje

```
JMP main
length:
	DB 10	; Number of elements in list
list:
	DB 50	; List elements
	DB 56
	DB 60
	DB 46
	DB 44
	DB 58
	DB 42
	DB 52
	DB 48
	DB 54
minimum:
	DB 0	; Placeholder for minimum

main:
MOV A, list
MOV C, list
ADD C, [length]
CALL sort
HLT

sort:
	PUSH A
	PUSH B
	PUSH D
.loop_1:
	CALL find_minimum
	MOV D, [A]
	PUSH [B]
	MOV [B], D
	POP D
	MOV [A], D
	INC A
	CMP A, C
	JB .loop_1
.stop_1:
	POP D
	POP B
	POP A
	RET

find_minimum:
	PUSH A
	PUSH D
	MOV B, A
	MOV D, [A]
.loop_2:
	INC A
	CMP A, C
	JAE .stop_2
	CMP D, [A]
	JBE .loop_2
	MOV D, [A]
	MOV B, A
	JMP .loop_2
.stop_2:
	POP D
	POP A
	RET
```

## Izračun praštevil

Če vam ostaja čas, poskusite napisati program, ki čim bolj učinkovito računa zaporedna praštevila in jih dodaja na sklad. Programe bomo primerjali po hitrosti in porabljenem prostoru, preizkusili pa jih bomo na simulatorju, ki podpira neomejenim skladom ter števila, večja od 255.

```
primes:
	MOV D, SP
	MOV B, 2
	PUSH B
.loop:
	MOV C, D
	INC B
.check_prime:
	CMP SP, C
	JB .remainder
	PUSH B
.remainder:
	MOV A, B
	DIV [C]
	MUL [C]
	CMP A, B
	JE .loop
	DEC C
	JMP .check_prime
```
