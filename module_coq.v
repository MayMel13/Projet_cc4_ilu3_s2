Require Import Coq.Arith.Arith.
Require Import Coq.Init.Nat.
Require Import Coq.Bool.Bool.

Module Budget : BudgetSig.

(* Type pour représenter un budget *)
Record budget := mkBudget {
  montant : nat
}.

(* Opération pour ajouter un montant au budget *)
Definition ajouter (b : budget) (m : nat) : budget :=
  mkBudget (montant b + m).

(* Opération pour supprimer un montant du budget, avec une vérification pour éviter les valeurs négatives *)
Definition supprimer (b : budget) (m : nat) : option budget :=
  if m <=? montant b then Some (mkBudget (montant b - m))
  else None.

(* Opération pour vérifier si un montant rentre dans le budget *)
Definition verifier (b : budget) (m : nat) : bool :=
  m <=? montant b.

(* Preuve que ajouter puis supprimer un montant revient au budget initial, si la suppression est valide *)
Theorem ajouter_supprimer : forall (b : budget) (m : nat),
  supprimer (ajouter b m) m = Some b.
Proof.
  intros b m.
  unfold ajouter.
  unfold supprimer.
  simpl.
  rewrite <- Nat.add_sub_assoc.
  - rewrite Nat.sub_diag. simpl. reflexivity.
  - apply Nat.le_add_r.
Qed.

(* Preuve que supprimer puis ajouter un montant revient au budget initial, si la suppression est valide *)
Theorem supprimer_ajouter : forall (b : budget) (m : nat),
  verifier b m = true ->
  ajouter (match supprimer b m with Some b' => b' | None => b end) m = b.
Proof.
  intros b m H.
  unfold verifier in H.
  unfold supprimer.
  unfold ajouter.
  destruct (m <=? montant b) eqn:Hle.
  - simpl. rewrite Nat.sub_add. reflexivity. apply Nat.leb_le. assumption.
  - discriminate H.
Qed.

End Budget.
