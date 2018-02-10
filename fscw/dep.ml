let flatten ~(depends:'a->'a list) ~(roots:'a list) :'a list =
  let rec f path roots result =
    List.fold_left
      (fun r x ->
        if List.mem x path then raise (Failure "cyclic dependency") else
        if List.mem x r then r else
        let x_deps = depends x in
        x::(f (x::path) x_deps r))
      result roots
  in
  List.rev (f [] roots [])



