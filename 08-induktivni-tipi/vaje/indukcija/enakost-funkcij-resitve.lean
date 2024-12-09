def reverse {A : Type} : List A -> List A :=
    fun xs =>
        match xs with
        | [] => []
        | y :: ys => reverse (ys) ++ [y]


def reverseAux {A : Type} : List A -> List A -> List A :=
    fun xs acc =>
        match xs with
        | [] => acc
        | y :: ys => reverseAux ys (y :: acc)


def reverse' {A : Type} : List A -> List A :=
    fun xs =>
        reverseAux xs []


theorem reverse_eq_aux {A : Type} : ∀ {lst : List A}, ∀ {acc : List A}, reverseAux lst acc = (reverse lst) ++ acc :=
    by
        intro lst
        induction lst with
        | nil =>
            intro acc
            simp [reverse, reverseAux]
        | cons x xs ih =>
            intro acc
            simp [reverseAux]
            rw [ih]
            simp [reverse]


theorem reverse_eq {A : Type} {xs : List A} : reverse xs = reverse' xs :=
    by
        simp [reverse']
        rw [reverse_eq_aux]
        simp [List.concat]
