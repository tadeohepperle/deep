package automm

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

main :: proc() {
	context.logger = log.create_console_logger()
	logging_allocator_data := Logging_Allocator_Data {
		backing = context.allocator,
	}
	context.allocator = logging_allocator(&logging_allocator_data)

	Data :: struct {
		m:    map[string][]int,
		name: string,
	}
	a := Data {
		name = "Hello",
	}
	a.m["What"] = {2, 3, 4, 5, 3, 3}
	a.m["This"] = {2, 4, 5, 3, 3, 3}
	a.m["I Can See"] = nil
	print("\nCLONE\n")
	b := clone(a)
	print("\nDROP\n")
	drop(&b)
	// print(a)
	// print(b)
	// drop(&b)
	// print(a)
	// print(b)
}

dbg_type :: proc(id: typeid) {
	ty := runtime.type_info_base(type_info_of(id))
	print(ty^)
	#partial switch var in ty.variant {
	case runtime.Type_Info_Struct:
		print("    flags:", var.flags)
		print("    equal proc:", var.equal)
	}
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
) -> int {
	tag_ptr := uintptr(union_ptr) + union_info.tag_offset
	tag_any := any{rawptr(tag_ptr), union_info.tag_type.id}

	tag: int = ---
	switch i in tag_any {
	case u8:
		tag = int(i)
	case i8:
		tag = int(i)
	case u16:
		tag = int(i)
	case i16:
		tag = int(i)
	case u32:
		tag = int(i)
	case i32:
		tag = int(i)
	case u64:
		tag = int(i)
	case i64:
		tag = int(i)
	case:
		unimplemented()
	}
	return tag
}
