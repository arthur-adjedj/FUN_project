type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

program
  fun [a] (b: bool)(e1: a)(e2: a): a =
    match b return a with
    | True {} -> e1
    | False {} -> e2
    | True {} -> e1
    end
  