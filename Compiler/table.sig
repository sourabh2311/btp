signature TABLE = 
sig
    type key
    type 'a table
    val empty : 'a table
    val enter : 'a table * key * 'a -> 'a table
    val look  : 'a table * key -> 'a option
    val listItems  : 'a table -> 'a list 
    val listItemsi  : 'a table -> (int * 'a) list 
    val inDomain  : 'a table * key -> bool
    val lookup : 'a table * key -> 'a
end

