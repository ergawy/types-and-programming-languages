(* 
 * A simple counter class: 
 * class operations - get, succ.
 *)
counterClass = l r:{x:Ref Nat}. {get = l _:Unit. !(r.x), inc = l _:Unit. r.x := succ(!r.x)}
(* object generator: *)
newCounter = l _:Unit. let r = {x = ref succ 0} in counterClass r

(*
 * A reset counter class:
 *   inherits implementation of get and inc operations from counterClass
 *   add a new operation - reset
 *)
resetCounterClass = l r:{x:Ref Nat}. let super = counterClass r in {get = super.get, inc = super.inc, reset = l _:Unit. r.x := succ 0}
(* object generator: *)
newResetCounter = l _:Unit. let r = {x = ref succ 0} in resetCounterClass r

(* Another class that inherits from newResetCounter and adds a dec operation: *)
decResetCounterClass = l r:{x:Ref Nat}. let super = resetCounterClass r in {get = super.get, inc = super.inc, reset = super.reset, dec = l _:Unit. r.x := pred(!r.x)}
(* object generator: *)
newDecResetCounter = l _:Unit. let r = {x = ref succ 0} in decResetCounterClass r

rc = newResetCounter unit
rc.inc unit; rc.inc unit; rc.get unit

drs = newDecResetCounter unit
drs.inc unit; drs.dec unit; drs.get unit

(*
 * A child of resetCounterClass that add a backup operation and maitains an
 * extended object representation (by adding a new field to the record of
 * reference cells):
 *)
backupCounterClass = l r:{x:Ref Nat, b:Ref Nat}. let super = resetCounterClass r in {get = super.get, inc = super.inc, reset = l _:Unit. r.x := (!r.b), backup = l _:Unit. r.b := (!r.x)}
newBackupCounter = l _:Unit. let r = {x = ref succ 0, b = ref succ 0} in backupCounterClass r

bc = newBackupCounter unit
bc.get unit
bc.backup unit; bc.inc unit; bc.get unit
bc.reset unit; bc.get unit

(* A child of backupCounterClass that calls super class's operations: *)
funnyBackupCounterClass = l r:{x:Ref Nat, b:Ref Nat}. let super = backupCounterClass r in {get = super.get, inc = l _:Unit. (super.backup unit; super.inc unit), reset = super.reset, backup = super.backup}
newFunnyBackupCounter = l _:Unit. let r = {x = ref succ 0, b = ref succ 0} in funnyBackupCounterClass r
