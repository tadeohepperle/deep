package deep

import "base:intrinsics"
import "base:runtime"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:reflect"
import "core:strings"

TRY_OUT_STUFF :: true
main2 :: proc() {
	Ty :: struct {
		age:  int,
		nums: []int,
		name: string,
	}
	Ty2 :: struct {
		name: string,
		age:  int,
		nums: [dynamic]int,
	}

	val := Ty {
		age  = 24,
		name = "Tadeo",
		nums = []int{2, 3, 4, 5},
	}
	buf: [dynamic]u8
	schemaful_encode_to(val, &buf)
	print(buf)
	val_d, err := schemaful_decode(Ty2, buf[:])
	if err == .None {
		print("Success:")
		print(val_d)
	} else {
		print("Error", err)
	}
	// res, err := schema_of_type(type_info_of(Ty))
	// assert(err == nil, tprint(err))
	// print_schema(res.types)
	// buf: [dynamic]u8
	// encode_schema_to(res.types, &buf)
	// print(buf)
	// cursor := buf[:]
	// schema_b, derr := decode_schema(&cursor)
	// assert(derr == .None, tprint(derr))
	// print_schema(schema_b)
}
print_schema :: proc(schema: Schema) {
	for t, i in schema {
		print(i, ": ", t.header)
		print("    ", t.kind)
	}
}

// showcases how the library should be used:
main :: proc() {
	if TRY_OUT_STUFF {
		main2()
		return
	}
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
		bar:          union {
			int,
			bool,
			[]Item,
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
