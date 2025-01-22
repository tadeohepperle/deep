package deep

import "base:intrinsics"
import "base:runtime"
import "core:mem"

equal :: proc(a: $T, b: T) -> bool {
	a := a
	b := b
	return _any_eq(type_info_of(T), &a, &b, false)
}
// todo: maybe return an int like runtime.memory_compare instead here?
//  1 if a > b
// -1 if b > a
//  0 if a == b
_any_eq :: proc(ty: Type_Info, a: rawptr, b: rawptr, $ASSERT_NON_COPY_TYPE: bool) -> bool {
	when !ASSERT_NON_COPY_TYPE {
		if is_copy_type(ty) {
			return runtime.memory_compare(a, b, ty.size) == 0
		}
	}
	#partial switch var in ty.variant {
	case runtime.Type_Info_Named:
		base_ty := type_info_base(ty)
		return _any_eq(base_ty, a, b, true)
	case runtime.Type_Info_Pointer:
		if var.elem != nil {
			a_elem_place := (cast(^rawptr)a)^
			b_elem_place := (cast(^rawptr)b)^
			if a_elem_place == nil || b_elem_place == nil {
				return a_elem_place == b_elem_place // so true if nil,nil    false otherwise
			}
			return _any_eq(var.elem, a_elem_place, b_elem_place, false)
		}
	case runtime.Type_Info_Slice:
		a_slice := cast(^Raw_Slice)a
		b_slice := cast(^Raw_Slice)b
		return _slice_eq(var.elem, a_slice, b_slice)
	case runtime.Type_Info_Dynamic_Array:
		// this is possible because the first two fields of Raw_Dynamic_Array 
		// and Raw_Slice are the same `data: rawptr, len: int`
		// we don't want to compare the allocator or the capacity really.
		a_slice := cast(^Raw_Slice)a
		b_slice := cast(^Raw_Slice)b
		return _slice_eq(var.elem, a_slice, b_slice)
	case runtime.Type_Info_Array:
		// if we get here, we already know it is not an array of copy types, clone all values inplace:
		for idx in 0 ..< var.count {
			offset := uintptr(idx * var.elem_size)
			a_elem_place := rawptr(uintptr(a) + offset)
			b_elem_place := rawptr(uintptr(b) + offset)
			if !_any_eq(var.elem, a_elem_place, b_elem_place, true) {
				return false
			}
		}
		return true
	case runtime.Type_Info_Enumerated_Array:
		if var.is_sparse {
			enum_ty, ok := var.index.variant.(runtime.Type_Info_Enum)
			assert(ok, "index of Type_Info_Enumerated_Array should be Type_Info_Enum")
			// in sparse case, no need to iterate the empty slots:
			for val in enum_ty.values {
				idx := int(val - var.min_value)
				assert(idx >= 0 && idx < var.count)
				offset := uintptr(idx * var.elem_size)
				a_elem_place := rawptr(uintptr(a) + offset)
				b_elem_place := rawptr(uintptr(b) + offset)
				if !_any_eq(var.elem, a_elem_place, b_elem_place, true) {
					return false
				}
			}
			return true
		} else {
			for idx in 0 ..< var.count {
				offset := uintptr(idx * var.elem_size)
				a_elem_place := rawptr(uintptr(a) + offset)
				b_elem_place := rawptr(uintptr(b) + offset)
				if !_any_eq(var.elem, a_elem_place, b_elem_place, true) {
					return false
				}
			}
			return true
		}
	case runtime.Type_Info_String:
		if var.is_cstring {
			return (cast(^cstring)a)^ == (cast(^cstring)b)^
		} else {
			return (cast(^string)a)^ == (cast(^string)b)^
		}
	case runtime.Type_Info_Struct:
		// todo: allow for ignoring fields here.
		for f_idx in 0 ..< var.field_count {
			offset := var.offsets[f_idx]
			a_field_place := rawptr(uintptr(a) + offset)
			b_field_place := rawptr(uintptr(b) + offset)
			if !_any_eq(var.types[f_idx], a_field_place, b_field_place, false) {
				return false
			}
		}
		return true
	case runtime.Type_Info_Union:
		// special case handling of ptr unions with nil niche optimization:
		if len(var.variants) == 1 {
			#partial switch v in var.variants[0].variant {
			case runtime.Type_Info_Pointer:
				return _any_eq(v.elem, (cast(^rawptr)a)^, (cast(^rawptr)b)^, false)
			case runtime.Type_Info_Multi_Pointer:
				unimplemented("Multi_Pointer not supported")
			case runtime.Type_Info_Procedure:
				return (cast(^rawptr)a)^ == (cast(^rawptr)b)^
			case runtime.Type_Info_String:
				if v.is_cstring {
					return (cast(^cstring)a)^ == (cast(^cstring)b)^
				}
			}
		}
		a_tag := _get_union_tag_for_non_ptr_union(var, a)
		b_tag := _get_union_tag_for_non_ptr_union(var, b)
		if a_tag != b_tag || (!var.no_nil && a_tag == 0) {
			return false
		}
		variant_idx := a_tag if var.no_nil else a_tag - 1
		return _any_eq(var.variants[variant_idx], a, b, false)
	case runtime.Type_Info_Map:
		a_map: Raw_Map = (cast(^Raw_Map)a)^
		b_map: Raw_Map = (cast(^Raw_Map)b)^

		a_map_len := runtime.map_len(a_map)
		b_map_len := runtime.map_len(b_map)
		if a_map_len != b_map_len {
			return false
		}
		if a_map_len == 0 {
			return true
		}
		key_ty := var.key
		value_ty := var.value
		key_ty_is_copy := is_copy_type(key_ty)
		value_ty_is_copy := is_copy_type(value_ty)
		map_info := var.map_info
		assert(map_info != nil)
		// iterate over entries of a, one by one and look them up in b to compare
		ks, vs, hs, _, _ := runtime.map_kvh_data_dynamic(a_map, map_info)
		for bucket_index in 0 ..< uintptr(runtime.map_cap(a_map)) {
			runtime.map_hash_is_valid(hs[bucket_index]) or_continue
			a_key := rawptr(runtime.map_cell_index_dynamic(ks, map_info.ks, bucket_index))
			a_value := rawptr(runtime.map_cell_index_dynamic(vs, map_info.vs, bucket_index))

			b_hash := map_info.key_hasher(a_key, runtime.map_seed(b_map))
			b_key, b_value := runtime.__dynamic_map_get_key_and_value(
				&b_map,
				map_info,
				b_hash,
				a_key,
			)
			if b_key == nil || b_value == nil {
				// return false if lookup failed
				return false
			}
			assert(a_key != nil && a_value != nil) // slots should be valid
			if key_ty_is_copy {
				if runtime.memory_compare(a_key, b_key, key_ty.size) != 0 {
					return false
				}
			} else {
				if !_any_eq(key_ty, a_key, b_key, true) {
					return false
				}
			}
			if value_ty_is_copy {
				if runtime.memory_compare(a_value, b_value, value_ty.size) != 0 {
					return false
				}
			} else {
				if !_any_eq(value_ty, a_value, b_value, true) {
					return false
				}
			}
		}
		return true
	}
	if PANIC_ON_UNSUPPORTED_TYPES {
		panic(tprint("Unsupported type for equality check: ", ty))
	} else {
		return runtime.memory_compare(a, b, ty.size) == 0
	}
}

_slice_eq :: proc(elem_ty: Type_Info, a_slice: ^Raw_Slice, b_slice: ^Raw_Slice) -> bool {
	if a_slice.len != b_slice.len {
		return false
	}
	if a_slice.data == b_slice.data {
		return true
	}
	if is_copy_type(elem_ty) {
		return runtime.memory_compare(a_slice.data, b_slice.data, a_slice.len * elem_ty.size) == 0
	}
	for idx in 0 ..< a_slice.len {
		offset := uintptr(idx * elem_ty.size)
		a_elem_place := rawptr(uintptr(a_slice.data) + offset)
		b_elem_place := rawptr(uintptr(b_slice.data) + offset)
		if !_any_eq(elem_ty, a_elem_place, b_elem_place, true) {
			return false
		}
	}
	return true
}
