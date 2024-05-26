let hello: int = 42
let this_is_test: String = "some string"

let id = fun (x: int): int -> {
    x
}

id: (T: type) -> (int, T) -> T
id = fun (T: type): (T, int) -> T { fun (a: int, x: T): T -> x }
// typeof(id) = ?

let id = fun (T: type) -> (x: T): T -> { x }
let id = fun (T: type) -> (x: T): T -> x

let id = fun (T: type) -> (x: T): T { x }

let id = fun (T: type, x: T): T -> { x }

