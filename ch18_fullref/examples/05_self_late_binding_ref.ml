(* 
 * This example avoids re-computing the method table for an object every time a
 * method on that object is called (like the fix approach). It does so by
 * allocating a cell to hold a dummy method table for each new object and pass
 * a reference to that cell to the class constructor.
 *
 * To support inheritence, Source references are used in order to pass a child
 * class' refernce (i.e. a reference to its method table) to the a parent class
 * constructor.
 *)
setCounterClass = l r:{x:Ref Nat}. l self: Source {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}. {get=l _:Unit. !(r.x), set=l i:Nat. r.x := i, inc=l _:Unit. (!self).set (succ ((!self).get unit))}

dummySetCounter = {get=l _:Unit. 0, set=l i:Nat. unit, inc=l _:Unit. unit}

newSetCounter = l _:Unit. let r = {x=ref succ 0} in let cAux = ref dummySetCounter in ((cAux := (setCounterClass r cAux)); !cAux)

sc = newSetCounter unit
(sc.set (succ succ succ 0)); sc.get unit

instrCounterClass = l r:{x:Ref Nat, a:Ref Nat}. l self:Source {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat}. let super = setCounterClass r self in {get=super.get, set=l i:Nat. ((r.a := succ(!(r.a))); super.set i), inc=super.inc, accesses=l _:Unit. !(r.a)}

dummyInstrCounter = {get=l _:Unit. 0, set=l i:Nat. unit, inc=l _:Unit. unit, accesses=l _:Unit. 0}

newInstrCounter = l _:Unit. let r = {x=ref succ 0, a=ref 0} in let cAux = ref dummyInstrCounter in ((cAux := (instrCounterClass r cAux)); !cAux)

ic =  newInstrCounter unit
(ic.set (succ succ succ 0)); ic.get unit
ic.accesses unit
