open C_abstree
open Locterm

let f t =
  List.filter
    (fun d -> match locval d with
        PdeclVariable _ -> true
      | _ -> false) t

