# ppx_xmatch

Mess with the semantics of pattern matching :-)

# %resume extension

Allow to resume the execution of a pattern matching branch.

Add two new forms:

```ocaml
match%resume expr with
| pattern -> resumable_expr
| pattern when expr -> resumable_expr
| ..

function%resume
| pattern -> resumable_expr
| pattern when expr -> resumable_expr
| ..
```

A `resumable_expr` is an expression where `[%resume]` is allowed in a return
position. In this case the control flow will jump to the next matching
branch.

For instance:

```
let rec map_view view = function%resume
  | x :: rest ->
    begin match view x with
      | None -> [%resume]
      | Some x' -> x' :: list
    end
  | x :: rest -> x :: map_view view rest
```
