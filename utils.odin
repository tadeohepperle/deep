package deep

import "base:intrinsics"
import "base:runtime"
import "core:fmt"
import "core:hash"
import "core:mem"
import "core:strings"

PANIC_ON_UNSUPPORTED_TYPES := false

Error :: Maybe(string)

None :: struct {}

print :: fmt.println
tprint :: fmt.tprint
Type_Info :: ^runtime.Type_Info
Allocator :: runtime.Allocator
type_info_base :: runtime.type_info_base
Raw_String :: runtime.Raw_String
Raw_Slice :: runtime.Raw_Slice
Raw_Map :: runtime.Raw_Map
Raw_Dynamic_Array :: runtime.Raw_Dynamic_Array

Tracker :: struct {
	allocator: Allocator,
	tracked:   [dynamic]Alloc,
}
Alloc :: struct {
	ptr:  rawptr,
	size: int,
}
tracker_create :: proc(allocator: Allocator) -> Tracker {
	return Tracker {
		allocator = allocator,
		tracked = make([dynamic]Alloc, allocator = context.temp_allocator),
	}
}
tracker_free_all :: proc(tracker: ^Tracker) {
	for a in tracker.tracked {
		mem.free_with_size(a.ptr, a.size, tracker.allocator)
	}
}
tracker_alloc :: proc(tracker: ^Tracker, size: int, align: int) -> rawptr {
	ptr, err := mem.alloc(size, align, tracker.allocator)
	assert(err == .None)
	assert(ptr != nil)
	append(&tracker.tracked, Alloc{ptr, size})
	return ptr
}
// can also manually add allocations made with e.g. make([]string, 20)
tracker_add :: proc(tracker: ^Tracker, ptr: rawptr, size: int) {
	append(&tracker.tracked, Alloc{ptr, size})
}

tracker_clone_string :: proc(tracker: ^Tracker, s: string) -> string {
	if s == "" {
		return ""
	} else {
		cloned := strings.clone(s, tracker.allocator)
		append(&tracker.tracked, Alloc{raw_data(cloned), len(s)})
		return cloned
	}
}

hash_string :: proc "contextless" (h: ^u64, s: string) {
	h^ = hash.fnv64(transmute([]u8)s, seed = h^)
}
hash_data :: proc "contextless" (h: ^u64, t: $T) {
	t := t
	h^ = hash.fnv64(transmute([]u8)Raw_Slice{&t, size_of(T)}, seed = h^)
}
hash_slice :: proc "contextless" (h: ^u64, slice: []$T) {
	h^ = hash.fnv64(transmute([]u8)Raw_Slice{raw_data(slice), len(slice) * size_of(T)}, seed = h^)
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

// just like runtime.type_info_base, but also converts e.g. Maybe(^T) to ^T
type_info_base_union_opt :: proc "contextless" (info: Type_Info) -> Type_Info {
	base := info
	loop: for {
		#partial switch var in base.variant {
		case runtime.Type_Info_Named:
			base = var.base
		case runtime.Type_Info_Union:
			if len(var.variants) == 1 {
				only_ty := var.variants[0]
				#partial switch v in only_ty.variant {
				case runtime.Type_Info_Pointer,
				     runtime.Type_Info_Multi_Pointer,
				     runtime.Type_Info_Procedure:
					return only_ty
				case runtime.Type_Info_String:
					if v.is_cstring {
						return only_ty
					}
				}
			}
			break loop
		case:
			break loop
		}
	}
	return base
}
