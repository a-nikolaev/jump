
let rec fold_up f acc x xe = if x < xe then fold_up f (f acc x) (x+1) xe else acc

let any_from_list ls =
  let len = List.length ls in List.nth ls (Random.int len)

