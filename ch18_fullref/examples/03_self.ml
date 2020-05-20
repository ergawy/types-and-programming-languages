(* A simple counter class where member methods are mutually recursive: *)
setCounterClass = l r:{x:Ref Nat}. fix (l self:{get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}. {get=l _:Unit. !(r.x), set=l i:Nat. r.x := i, inc=l _:Unit. self.set (succ (self.get unit))})

newSetCounter = l _:Unit. let r = {x = ref succ 0} in setCounterClass r

sc = newSetCounter unit
sc.inc unit; sc.get unit
(sc.set (succ succ succ succ 0)); sc.get unit
