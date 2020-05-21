(*
 * This example uses late binding to enable super classes to call child classes (see tapl sec. 18.10).
 * It also uses thunks (see tapl, sec. 18.11) to work around divergence if fix's argument is used too early.
 * TODO sudy these examples carefully to make sure you understand the consequences of late binding and thunks.
 *)
setCounterClass = l r:{x:Ref Nat}. l self:Unit->{get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}. l _:Unit. {get=(l _:Unit. !(r.x)), set=(l i:Nat. r.x:=i), inc=(l _:Unit. (self unit).set (succ((self unit).get unit)))}

newSetCounter = l _:Unit. let r = {x=ref succ 0} in (fix (setCounterClass r)) unit

sc = newSetCounter unit
(sc.set (succ succ succ 0)); sc.get unit

instrCounterClass = l r:{x:Ref Nat,a:Ref Nat}. l self:Unit->{get:Unit->Nat,set:Nat->Unit,inc:Unit->Unit,accesses:Unit->Nat}. l _:Unit. let super = setCounterClass r self unit in {get=super.get, set=l i:Nat. ((r.a:=succ(!(r.a))); (super.set i)), inc=super.inc, accesses=l _:Unit. !(r.a)}

newInstrCounter = l _:Unit. let r = {x=ref succ 0, a= ref 0} in (fix (instrCounterClass r)) unit

ic = newInstrCounter unit
