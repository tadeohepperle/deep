package deep

import "base:intrinsics"
import "base:runtime"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:reflect"
import "core:strings"

print :: fmt.println
tprint :: fmt.tprint
Type_Info :: ^runtime.Type_Info
Allocator :: runtime.Allocator
type_info_base :: runtime.type_info_base
Raw_String :: runtime.Raw_String
Raw_Slice :: runtime.Raw_Slice
Raw_Map :: runtime.Raw_Map
Raw_Dynamic_Array :: runtime.Raw_Dynamic_Array
main :: proc() {

	Ex :: struct {
		a:   string,
		b:   B,
		num: int,
	}
	B :: struct {
		name: string,
		nums: [3]int,
		arr:  [dynamic]string,
		arr2: [dynamic]int,
		arr3: []int,
		m:    map[int][3]f32,
	}
	if true {
		schema, err := type_to_schema(type_info_of(Ex))
		if err != nil {
			print(err)
		} else {
			for ty, idx in schema.registry {
				print(idx, ": ", ty)
			}

		}
		return
	}
	// A :: struct {
	// 	p: ^B,
	// }
	// B :: struct {
	// 	p: ^A,
	// }
	// MyStruct :: struct {
	// 	m:        ^MyStruct,
	// 	v:        int,
	// 	name:     string,
	// 	nums:     []int,
	// 	contacts: map[string]int,
	// }
	// a := MyStruct {
	// 	v    = 3,
	// 	name = "Tadeo",
	// 	nums = {33, 44, 55},
	// }
	// a.contacts["Ju"] = 8888
	// a.contacts["Foo Bar"] = 9999
	// buf := encode(a)
	// print(buf)

	// remove_range(&buf, 57, 60)
	// validate_err := validate_encoding(MyStruct, buf[:])
	// print("validate err:", validate_err)


	// b, err := decode(MyStruct, buf[:])
	// if err != .None {
	// 	print("Error:", err)
	// } else {
	// 	print("Success: ", b)
	// }
	// MyStruct :: struct {
	// 	num:      int,
	// 	name:     string,
	// 	leads_to: map[string]^MyStruct,
	// }
	// val := random(MyStruct)
	// print(val)
	// drop(&val)


	// MyStruct :: struct {
	// 	f: f32,
	// 	i: int,
	// }
	// MyTy :: struct {
	// 	f: f32,
	// 	i: int,
	// 	s: string,
	// 	m: map[string]^Thing,
	// }
	// Thing :: struct {
	// 	name:    string,
	// 	numbers: []int,
	// }
	// a := MyTy {
	// 	f = 1.2,
	// 	i = 3,
	// 	s = "Hello",
	// }
	// a.m["one"] = &Thing{name = "Clause", numbers = {1, 2, 3}}
	// a.m["two"] = &Thing{name = "Clause", numbers = {1, 222, 3}}

	// b := MyTy {
	// 	f = 1.2,
	// 	i = 3,
	// 	s = fmt.aprint("Hello"),
	// }
	// b_nums := make([]int, 3)
	// b_nums[0] = 1
	// b_nums[1] = 2
	// b_nums[2] = 3
	// b.m["one"] = &Thing{name = "Clause", numbers = b_nums}
	// b.m["two"] = &Thing{name = "Clause", numbers = b_nums}

	// print(equal(a, b))
	// print(a)
	// print(b)
}

// main :: proc() {
// 	// context.logger = log.create_console_logger()
// 	// logging_allocator_data := Logging_Allocator_Data {
// 	// 	backing = context.allocator,
// 	// }
// 	// context.allocator = logging_allocator(&logging_allocator_data)

// 	track: mem.Tracking_Allocator
// 	mem.tracking_allocator_init(&track, context.allocator)
// 	context.allocator = mem.tracking_allocator(&track)

// 	defer {
// 		if len(track.allocation_map) > 0 {
// 			fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
// 			for _, entry in track.allocation_map {
// 				fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
// 			}
// 		}
// 		if len(track.bad_free_array) > 0 {
// 			fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
// 			for entry in track.bad_free_array {
// 				fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
// 			}
// 		}
// 		mem.tracking_allocator_destroy(&track)
// 	}

// 	Data :: struct {
// 		m:    map[string][]int,
// 		name: string,
// 	}
// 	a := Data {
// 		name = "Hello",
// 	}
// 	a.m["What"] = {2, 3, 4, 5, 3, 3}
// 	a.m["This"] = {2, 4, 5, 3, 3, 3}
// 	a.m["I Can See"] = nil
// 	a.m["lol lol"] = {3, 4, 5}
// 	print("\nCLONE\n")
// 	b := clone(a)
// 	delete(a.m)
// 	print("\nDROP\n")
// 	drop(&b)


// 	print(equal(a, b))

// 	// print(a)
// 	// print(b)
// 	// drop(&b)
// 	// print(a)
// 	// print(b)
// }

dbg_type :: proc(id: typeid) {
	ty := runtime.type_info_base(type_info_of(id))
	print(ty^)
	#partial switch var in ty.variant {
	case runtime.Type_Info_Struct:
		print("    flags:", var.flags)
		print("    equal proc:", var.equal)
	}
}
is_type_supported :: proc(type_id: typeid) -> bool {
	return _is_type_supported(type_info_of(type_id))
}
_is_type_supported :: proc(ty: Type_Info) -> bool {
	#partial switch var in ty.variant {
	case runtime.Type_Info_Integer,
	     runtime.Type_Info_Rune,
	     runtime.Type_Info_Float,
	     runtime.Type_Info_Complex,
	     runtime.Type_Info_Quaternion,
	     runtime.Type_Info_Boolean,
	     runtime.Type_Info_Bit_Set,
	     runtime.Type_Info_Enum,
	     runtime.Type_Info_Matrix,
	     runtime.Type_Info_Simd_Vector,
	     runtime.Type_Info_Type_Id:
		return true
	case runtime.Type_Info_Named:
		return _is_type_supported(type_info_base(ty))
	case runtime.Type_Info_Pointer:
		return _is_type_supported(var.elem)
	case runtime.Type_Info_Slice:
		return _is_type_supported(var.elem)
	case runtime.Type_Info_Dynamic_Array:
		return _is_type_supported(var.elem)
	case runtime.Type_Info_Array:
		return _is_type_supported(var.elem)
	case runtime.Type_Info_Enumerated_Array:
		return _is_type_supported(var.elem)
	case runtime.Type_Info_Struct:
		// todo: allow for ignoring fields here.
		for f_idx in 0 ..< var.field_count {
			if !_is_type_supported(var.types[f_idx]) {
				return false
			}
		}
		return true
	case runtime.Type_Info_Union:
		for variant_ty in var.variants {
			if !_is_type_supported(variant_ty) {
				return false
			}
		}
		return true
	case runtime.Type_Info_Map:
		if var.map_info == nil {
			return false
		}
		return _is_type_supported(var.key) && _is_type_supported(var.value)
	}
	return false
}

is_copy_type :: proc(ty: Type_Info) -> bool {
	#partial switch var in ty.variant {
	case runtime.Type_Info_Integer,
	     runtime.Type_Info_Rune,
	     runtime.Type_Info_Float,
	     runtime.Type_Info_Complex,
	     runtime.Type_Info_Quaternion,
	     runtime.Type_Info_Boolean,
	     runtime.Type_Info_Bit_Set,
	     runtime.Type_Info_Enum,
	     runtime.Type_Info_Matrix,
	     runtime.Type_Info_Simd_Vector,
	     runtime.Type_Info_Type_Id:
		return true
	case runtime.Type_Info_Array:
		return is_copy_type(var.elem)
	case runtime.Type_Info_Enumerated_Array:
		return is_copy_type(var.elem)
	case runtime.Type_Info_Named:
		return is_copy_type(type_info_base(ty))
	case runtime.Type_Info_Struct:
		// todo: or field is tagged as static or borrowed, then also is copy
		for f_idx in 0 ..< var.field_count {
			if !is_copy_type(var.types[f_idx]) {
				return false
			}
		}
		return true
	case runtime.Type_Info_Union:
		for variant_ty in var.variants {
			if !is_copy_type(variant_ty) {
				return false
			}
		}
		return true
	}
	return false
}

_get_union_tag_for_non_ptr_union :: proc(
	union_info: runtime.Type_Info_Union,
	union_ptr: rawptr,
) -> (
	val: int,
) {
	tag_ptr := uintptr(union_ptr) + union_info.tag_offset
	tag_any := any{rawptr(tag_ptr), union_info.tag_type.id}

	switch i in tag_any {
	case u8:
		val = int(i)
	case i8:
		val = int(i)
	case u16:
		val = int(i)
	case i16:
		val = int(i)
	case u32:
		val = int(i)
	case i32:
		val = int(i)
	case u64:
		val = int(i)
	case i64:
		val = int(i)
	case:
		unimplemented(tprint("unsupported union tag:", union_info.tag_type.id))
	}
	return val
}
_set_union_tag_for_non_ptr_union :: proc(
	union_info: runtime.Type_Info_Union,
	union_ptr: rawptr,
	val: int,
) {
	tag_ptr := uintptr(union_ptr) + union_info.tag_offset
	tag_any := any{rawptr(tag_ptr), union_info.tag_type.id}
	switch &tag in tag_any {
	case u8:
		tag = u8(val)
	case i8:
		tag = i8(val)
	case u16:
		tag = u16(val)
	case i16:
		tag = i16(val)
	case u32:
		tag = u32(val)
	case i32:
		tag = i32(val)
	case u64:
		tag = u64(val)
	case i64:
		tag = i64(val)
	case:
		unimplemented(tprint("unsupported union tag:", union_info.tag_type.id))
	}
}

Logging_Allocator_Data :: struct {
	backing: mem.Allocator,
}

logging_allocator :: proc(data: ^Logging_Allocator_Data) -> mem.Allocator {
	// `package log` uses context.temp_allocator and temp_allocator is lazily initialized.  Dummy allocate here
	// to make sure it doesn't use context.allocator within our custom allocator procedure.
	dummy := new(int, context.temp_allocator)

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
			log.infof(
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
			log.infof(" = %p  %v", result, err)
			return result, err
		},
	}
}
