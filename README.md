## deep - recursive cloning, deallocation, comparison and binary encoding using RTTI in Odin

deep is an Odin package for _semi-automatic_ memory management using RTTI on (almost) any generic type:

- deep cloning and dropping (deallocation)
- deep equality comparison
- schemaless encoding/decoding from and to bytes
- random value creating of any data type, great for testing
- deep hashing of values to a u64

This package is basically a kitchen sink of useful RTTI traversing functions.

## Usage:

```odin
// you have some complicated type Foo:
foo: Foo = random(Foo) // create random instance of Foo
fmt.println(foo)

foo_cloned: Foo = clone(foo) // deep cloning of foo and all of its allocations into context.allocator
bytes: [dynamic]u8 = encode(foo) // encode foo to buffer
foo_from_bytes, err := decode(Foo, bytes[:]) // decode foo from buffer

assert(err == .None)
assert(equal(foo, foo_cloned)) // deep equality comparison of two Foo's
assert(equal(foo, foo_from_bytes))

drop(&foo) // drop Foo values, cleaning up all internal allocations
drop(&foo_cloned)
drop(&foo_from_bytes)

// all allocations freed up, no memory leaks should remain
Foo :: struct {
    name:         string,
    bar:          union { int, bool, []Item },
    tiles:        [dynamic][2]f32,
    connected_to: map[string]^Foo,
    dirs:         map[Dir][]int,
}
Item :: struct {
    abilities: map[u64][dynamic]string,
    cost:      u64,
}
Dir :: enum { North, South, West, East }
```

Currently not supported:

- SOA types, multi pointers, procedure pointers
- calculating hashes of hashmaps, because that would require sorting.

Author: Tadeo Hepperle, 2025
