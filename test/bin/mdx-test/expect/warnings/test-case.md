No warning is printed by default:

```ocaml
let () =
  let f ~x:() = () in
  f ();;
let x = 4
```

Warning attributes must be set to print them:

```ocaml version>=4.03
[@@@warning "+6"]
let () =
  let f ~x:() = () in
  f ();;
let x = 4
```
```mdx-error
...
Warning 6: label x was omitted in the application of this function.
```
