let poisci_optiomalni_tocki list =
  let rec poisci trenutni = function
  |[] -> trenutni
  |x :: xs ->