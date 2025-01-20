package deep

import "base:intrinsics"
import "base:runtime"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:reflect"
import "core:strings"

// showcases how the library should be used:
main :: proc() {
	// you have some complicated type Foo:
	foo: Foo = random(Foo) // create random instance of Foo
	fmt.println(foo)
	foo_cloned: Foo = clone(foo) // deep cloning of foo and all of its allocations into context.allocator
	bytes: [dynamic]u8 = encode(foo) // encode foo to buffer
	foo_from_bytes, err := decode(Foo, bytes[:]) // decode foo from buffer
	assert(equal(foo, foo_cloned)) // deep equality comparison of two Foo's
	assert(equal(foo, foo_from_bytes))

	drop(&foo) // drop Foo values, cleaning up all internal allocations
	drop(&foo_cloned)
	drop(&foo_from_bytes)

	// all allocations freed up, no memory leaks should remain

	Foo :: struct {
		name:         string,
		age:          union {
			int,
			bool,
			[]int,
		},
		tiles:        [dynamic][2]f32,
		connected_to: map[string]^Foo,
		dirs:         map[Dir][]int,
	}
	Item :: struct {
		abilities: map[u64][dynamic]string,
		cost:      u64,
	}
	Dir :: enum {
		North,
		South,
		West,
		East,
	}
}
