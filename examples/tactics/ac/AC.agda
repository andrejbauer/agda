
module AC where

import Nat
import Bool
import List
import Fin
import Logic
import Vec

open Nat, hiding (_<_), renaming (_==_ to _=Nat=_)
open Bool
open List, hiding (module Eq) -- (
open Fin, renaming (_==_ to _=Fin=_)
open Logic
open Vec

infix 20 _○_
infix 10 _≡_

ForAll : {A : Set}(n : Nat) -> (Vec n A -> Set) -> Set
ForAll	    zero   F = F ε
ForAll {A} (suc n) F = (x : A) -> ForAll _ \xs -> F (x ∷ xs)

apply : {n : Nat}{A : Set}(F : Vec n A -> Set) -> ForAll n F -> (xs : Vec n A) -> F xs
apply {zero}  F t (vec vnil)	     = t
apply {suc n} F f (vec (vcons x xs)) = apply _ (f x) xs

lambda : {n : Nat}{A : Set}(F : Vec n A -> Set) -> ((xs : Vec n A) -> F xs) -> ForAll n F
lambda {zero } F f = f ε
lambda {suc n} F f = \x -> lambda _ (\xs -> f (x ∷ xs))

data Expr (n : Nat) : Set where
  zro : Expr n
  var : Fin n -> Expr n
  _○_ : Expr n -> Expr n -> Expr n

data Theorem (n : Nat) : Set where
  _≡_ : Expr n -> Expr n -> Theorem n

theorem : (n : Nat) -> ({m : Nat} -> ForAll {Expr m} n \_ -> Theorem m) -> Theorem n
theorem n thm = apply _ thm vars
  where
    vars : Vec n (Expr n)
    vars = map var (fzeroToN-1 n)

module Provable where

  NF : Nat -> Set
  NF n = List (Fin n)

  infix 12 _⊕_

  _⊕_ : {n : Nat} -> NF n -> NF n -> NF n
  []	  ⊕ ys	      = ys
  x :: xs ⊕ []	      = x :: xs
  x :: xs ⊕ (y :: ys) = if x < y
			then x :: (xs ⊕ y :: ys)
			else y :: (x :: xs ⊕ ys)

  normalise : {n : Nat} -> Expr n -> NF n
  normalise  zro    = []
  normalise (var n) = n :: []
  normalise (a ○ b) = normalise a ⊕ normalise b

  infix 30 ↓

  _↓ : {n : Nat} -> NF n -> Expr n
  (i :: is) ↓ = var i ○ is ↓
  []	    ↓ = zro

  infix 10 _=Expr=_ _=NF=_

  _=NF=_ : {n : Nat} -> NF n -> NF n -> Bool
  _=NF=_ = ListEq._==_
    where
      module ListEq = List.Eq _=Fin=_

  substNF : {n : Nat}{xs, ys : NF n}(P : NF n -> Set) -> IsTrue (xs =NF= ys) -> P xs -> P ys
  substNF = ListSubst.subst
    where
      module ListSubst = List.Subst _=Fin=_ subst

  _=Expr=_ : {n : Nat} -> Expr n -> Expr n -> Bool
  a =Expr= b = normalise a =NF= normalise b

  provable : {n : Nat} -> Theorem n -> Bool
  provable (a ≡ b) = a =Expr= b

module Semantics
  {A	 : Set}
  (_==_  : A -> A -> Set)
  (_*_	 : A -> A -> A)
  (one	 : A)
  (refl  : {x : A} -> x == x)
  (sym   : {x, y : A} -> x == y -> y == x)
  (trans : {x, y, z : A} -> x == y -> y == z -> x == z)
  (idL	 : {x : A} -> (one * x) == x)
  (idR	 : {x : A} -> (x * one) == x)
  (comm  : {x, y : A} -> (x * y) == (y * x))
  (assoc : {x, y, z : A} -> (x * (y * z)) == ((x * y) * z))
  (congL : {x, y, z : A} -> y == z -> (x * y) == (x * z))
  (congR : {x, y, z : A} -> x == y -> (x * z) == (y * z))
  where

  open Provable

  expr[_] : {n:Nat} -> Expr n -> Vec n A -> A
  expr[ zro   ] ρ = one
  expr[ var i ] ρ = ρ ! i
  expr[ a ○ b ] ρ = expr[ a ] ρ * expr[ b ] ρ

  eq[_] : {n:Nat} -> Theorem n -> Vec n A -> Set
  eq[ a ≡ b ] ρ = expr[ a ] ρ == expr[ b ] ρ

  data CantProve (A:Set) : Set where
    no-proof : CantProve A

  Prf : {n : Nat} -> Theorem n -> Bool -> Set
  Prf thm true  = ForAll _ \ρ -> eq[ thm ] ρ
  Prf thm false = CantProve (Prf thm true)

  Proof : {n : Nat} -> Theorem n -> Set
  Proof thm = Prf thm (provable thm)

  lem0 : {n : Nat} -> (xs, ys : NF n) -> (ρ : Vec n A) ->
	 eq[ xs ↓ ○ ys ↓ ≡ (xs ⊕ ys) ↓ ] ρ
  lem0 []	 ys	   ρ = idL
  lem0 (x :: xs) []	   ρ = idR
  lem0 (x :: xs) (y :: ys) ρ = if' x < y then less else more
    where
      lhs     = (var x ○ xs ↓) ○ (var y ○ ys ↓)
      lbranch = x :: (xs ⊕ y :: ys)
      rbranch = y :: (x :: xs ⊕ ys)

      less : IsTrue (x < y) -> _
      less x<y = BoolEq.subst {true}{x < y}
		 (\z -> eq[ lhs ≡ (if z then lbranch else rbranch) ↓ ] ρ )
		 x<y (trans (sym assoc)
			    (congL (lem0 xs (y :: ys) ρ))
		     )

      more : IsFalse (x < y) -> _
      more x>=y = BoolEq.subst {false}{x < y}
		 (\z -> eq[ lhs ≡ (if z then lbranch else rbranch) ↓ ] ρ )
		 x>=y ( comm ==> sym assoc
		    ==> congL ( comm ==> lem0 (x :: xs) ys ρ )
		      )
	where
	  infixr 5 _==>_
	  _==>_ : {x,y,z:A} -> x == y -> y == z -> x == z
	  p ==> q = trans p q

  lem1 : {n : Nat} -> (e : Expr n) -> (ρ : Vec n A) -> eq[ e ≡ normalise e ↓ ] ρ
  lem1  zro    ρ = refl
  lem1 (var i) ρ = sym idR
  lem1 (a ○ b) ρ = trans step1 (trans step2 step3)
    where
      step1 : eq[ a ○ b ≡ normalise a ↓ ○ b ] ρ
      step1 = congR (lem1 a ρ)

      step2 : eq[ normalise a ↓ ○ b ≡ normalise a ↓ ○ normalise b ↓ ] ρ
      step2 = congL (lem1 b ρ)

      step3 : eq[ normalise a ↓ ○ normalise b ↓ ≡ (normalise a ⊕ normalise b) ↓ ] ρ
      step3 = lem0 (normalise a) (normalise b) ρ

  lem2 : {n : Nat} -> (xs, ys : NF n) -> (ρ : Vec n A) -> IsTrue (xs =NF= ys) -> eq[ xs ↓ ≡ ys ↓ ] ρ
  lem2 xs ys ρ eq = substNF {_}{xs}{ys} (\z -> eq[ xs ↓ ≡ z ↓ ] ρ) eq refl

  prove : {n : Nat} -> (thm : Theorem n) -> Proof thm
  prove thm = proof (provable thm) thm (\h -> h)
    where
      proof : {n : Nat}(valid : Bool)(thm : Theorem n) -> (IsTrue valid -> IsTrue (provable thm)) -> Prf thm valid
      proof false  _	  _	  = no-proof
      proof true  (a ≡ b) isValid = lambda eq[ a ≡ b ] \ρ ->
	  trans (step-a ρ) (trans (step-ab ρ) (step-b ρ))
	where

	  step-a : forall ρ -> eq[ a ≡ normalise a ↓ ] ρ
	  step-a ρ = lem1 a ρ

	  step-b : forall ρ -> eq[ normalise b ↓ ≡ b ] ρ
	  step-b ρ = sym (lem1 b ρ)

	  step-ab : forall ρ -> eq[ normalise a ↓ ≡ normalise b ↓ ] ρ
	  step-ab ρ = lem2 (normalise a) (normalise b) ρ (isValid tt)

n0 = zero
n1 = suc n0
n2 = suc n1
n3 = suc n2
n4 = suc n3
n5 = suc n4
n6 = suc n5
n7 = suc n6
n8 = suc n7
n9 = suc n8

open Provable

thmRefl = theorem n1 \x -> x ≡ x
thmCom  = theorem n2 \x y -> x ○ y ≡ y ○ x
thm1 = theorem n3 \x y z -> x ○ (y ○ z) ≡ (z ○ y) ○ x
thm2 = theorem n3 \x y z -> x ○ (y ○ z) ≡ (z ○ x) ○ x

thm3 = theorem n5 \a b c d e -> (a ○ (a ○ b)) ○ ((c ○ d) ○ (e ○ e)) ≡ b ○ ((e ○ (c ○ a)) ○ (d ○ (e ○ a)))

infix 10 _===_
_===_ = \n m -> IsTrue (n =Nat= m)

postulate
  refl  : {x : Nat}	  -> x === x
  sym   : {x, y : Nat}	  -> x === y -> y === x
  trans : {x, y, z : Nat} -> x === y -> y === z -> x === z
  idL	: {x : Nat}	  -> (zero + x) === x
  idR	: {x : Nat}	  -> (x + zero) === x
  comm  : {x, y : Nat}	  -> (x + y) === (y + x)
  assoc : {x, y, z : Nat} -> (x + (y + z)) === ((x + y) + z)
  congL : {x, y, z : Nat} -> y === z -> x + y === x + z
  congR : {x, y, z : Nat} -> x === y -> x + z === y + z

module NatPlus = Semantics _===_ _+_ zero
			    (\{x} -> refl {x})	-- weird
			    (\{x}{y} -> sym {x}{y})
			    (\{x}{y}{z} -> trans {x}{y}{z})
			    (\{x} -> idL {x})
			    (\{x} -> idR {x})
			    (\{x}{y} -> comm {x}{y})
			    (\{x}{y}{z} -> assoc {x}{y}{z})
			    (\{x}{y}{z} -> congL {x}{y}{z})
			    (\{x}{y}{z} -> congR {x}{y}{z})
open NatPlus

test : (x, y, z : Nat) -> x + (y + z) === (z + x) + y
test = prove (theorem n3 \x y z -> x ○ (y ○ z) ≡ (z ○ x) ○ y)

