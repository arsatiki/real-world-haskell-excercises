lastButOne (x : _ : []) = x
lastButOne (x : []) = error "Empty list!"
lastButOne (_ : r) = lastButOne r

