structure Flow =
struct

(* 
  A flow graph has four components:
  * control: a directed graph wherein each node represents an instruction (or,  perhaps, a basic block);
  * def: a table of the temporaries defined at each node (destination registers of the instruction);
  * use: a table of the temporaries used at each node (source registers of the  instruction);
  * ismove: tells whether each instruction is a move instruction, which could be detected if the def and use are identical. 
*)

  datatype node = FNODE of {id : int, def : Temp.temp list, use : Temp.temp list, ismove : bool, succ : node list ref, prev : node list ref, liveOut : Temp.temp list ref}
  type flowgraph = node list

end
