package deep


import "base:intrinsics"
import "base:runtime"
import "core:fmt"
import "core:mem"
import "core:testing"

@(test)
test_everything :: proc(t: ^testing.T) {
	for seed in 1 ..= u64(10) {
		test_for_type(map[string]string, t, seed)
		test_for_type(MyUnion, t, seed)
		test_for_type([][][]int, t, seed)
		test_for_type(MyStruct, t, seed)
		test_for_type(^MyStruct, t, seed)
	}

	MyStruct :: struct {
		num:      int,
		name:     string,
		leads_to: map[string]^MyStruct,
		ty2:      MyUnion,
		e:        [MyEnum][dynamic]bool,
	}
	MyUnion :: union {
		^MyStruct,
		[MyEnum]string,
		[2]f32,
	}
	MyEnum :: enum {
		A,
		B,
		C,
	}
}


// tests for the type:
// - random creation
// - deep cloning and dropping (deallocating)
// - equality comparison
// - encoding and decoding into bytes
test_for_type :: proc($T: typeid, t: ^testing.T, seed: u64) {
	track: mem.Tracking_Allocator
	mem.tracking_allocator_init(&track, context.allocator)
	context.allocator = mem.tracking_allocator(&track)
	a := random(T, seed)
	b := clone(a)
	testing.expect(t, equal(a, b), tprint("Should equal, A =", a, ", B =", b))
	when !intrinsics.type_is_pointer(T) {
		// for non ptr types, collision chance of random values should be
		// low enough, that we can do a NOT_EQ test, for a different random value.
		c := random(T, seed + 10)
		testing.expect(t, !equal(a, c), tprint("Should !equal, A =", a, ", C =", c))
		testing.expect(t, !equal(b, c), tprint("Should !equal, B =", b, ", C =", c))
		drop(&c)
	}

	// test simple encoding, decoding
	buf := encode(a)
	a_decoded, err := decode(T, buf[:])
	testing.expect_value(t, err, DecodeError.None)
	testing.expect(
		t,
		equal(a, a_decoded),
		tprint("Should equal, A =", a, ", A_DECODED =", a_decoded),
	)
	delete(buf)
	drop(&a_decoded)

	// drop memory, should have no leaks left
	drop(&a)
	drop(&b)
	assert(len(track.bad_free_array) == 0)
	assert(len(track.allocation_map) == 0)
	mem.tracking_allocator_destroy(&track)
}

// use like this:
//
// log := Logging_Allocator_Data{context.allocator}
// context.allocator = logging_allocator(&log)
Logging_Allocator_Data :: struct {
	backing: mem.Allocator,
}

logging_allocator :: proc(data: ^Logging_Allocator_Data) -> mem.Allocator {
	return mem.Allocator {
		data = data,
		procedure = proc(
			allocator_data: rawptr,
			mode: runtime.Allocator_Mode,
			size, alignment: int,
			old_memory: rawptr,
			old_size: int,
			location := #caller_location,
		) -> (
			[]byte,
			runtime.Allocator_Error,
		) {
			data := cast(^Logging_Allocator_Data)allocator_data
			fmt.printfln(
				"{}: size={} alignment={} old_memory={} old_size={}, loc={}",
				mode,
				size,
				alignment,
				old_memory,
				old_size,
				location,
			)
			result, err := data.backing.procedure(
				data.backing.data,
				mode,
				size,
				alignment,
				old_memory,
				old_size,
				location,
			)
			fmt.printfln(" = %p  %v", result, err)
			return result, err
		},
	}
}
