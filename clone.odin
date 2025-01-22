package deep

import "base:intrinsics"
import "base:runtime"
import "core:mem"
import "core:strings"

clone :: proc(this: $T, allocator := context.allocator) -> (res: T) {
	src := this
	_clone_any(type_info_of(T), &src, &res, allocator)
	return res
}
_clone_any :: proc(ty: Type_Info, src: rawptr, dst: rawptr, allocator: Allocator) {
	intrinsics.mem_copy_non_overlapping(dst, src, ty.size)
	_clone_allocations_inplace(ty, dst, allocator, false)
}
_clone_allocations_inplace :: proc(
	ty: Type_Info,
	place: rawptr,
	allocator: Allocator,
	$ASSERT_NON_COPY_TYPE: bool,
) {
	when !ASSERT_NON_COPY_TYPE {
		if is_copy_type(ty) {
			return
		}
	}
	#partial switch var in ty.variant {
	case runtime.Type_Info_Named:
		base_ty := type_info_base(ty)
		_clone_allocations_inplace(base_ty, place, allocator, true)
		return
	case runtime.Type_Info_Pointer:
		if var.elem != nil {
			_clone_ptr_type_allocation_inplace(var.elem, place, allocator)
			return
		}
	case runtime.Type_Info_Slice:
		raw_slice := cast(^Raw_Slice)place
		if raw_slice.data == nil || raw_slice.len == 0 {
			return
		}
		raw_slice.data = _copy_into_new_slice(var.elem, raw_slice.len, raw_slice.data, allocator)
		return
	case runtime.Type_Info_Dynamic_Array:
		raw_arr := cast(^Raw_Dynamic_Array)place
		old_arr: Raw_Dynamic_Array = raw_arr^
		raw_arr^ = Raw_Dynamic_Array {
			data      = nil,
			len       = old_arr.len,
			cap       = old_arr.len, // note: len not cap!! this is intended.
			allocator = allocator,
		}
		if old_arr.data == nil || old_arr.len == 0 {
			return
		}
		// allocate and copy over the slice:
		raw_arr.data = _copy_into_new_slice(var.elem, old_arr.len, old_arr.data, allocator)
		return
	case runtime.Type_Info_Array:
		// if we get here, we already know it is not an array of copy types, clone all values inplace:
		for idx in 0 ..< var.count {
			offset := uintptr(idx * var.elem_size)
			elem_place := rawptr(uintptr(place) + offset)
			_clone_allocations_inplace(var.elem, elem_place, allocator, true)
		}
		return
	case runtime.Type_Info_Enumerated_Array:
		if var.is_sparse {
			enum_ty, ok := var.index.variant.(runtime.Type_Info_Enum)
			assert(ok, "index of Type_Info_Enumerated_Array should be Type_Info_Enum")
			// in sparse case, no need to iterate the empty slots:
			for val in enum_ty.values {
				idx := val - var.min_value
				assert(idx >= 0 && int(idx) < var.count)
				offset := uintptr(idx) * uintptr(var.elem_size)
				elem_place := rawptr(uintptr(place) + offset)
				_clone_allocations_inplace(var.elem, elem_place, allocator, true)
			}
		} else {
			for idx in 0 ..< var.count {
				offset := uintptr(idx * var.elem_size)
				elem_place := rawptr(uintptr(place) + offset)
				_clone_allocations_inplace(var.elem, elem_place, allocator, true)
			}
		}
		return
	case runtime.Type_Info_String:
		if var.is_cstring {
			place_ptr := cast(^rawptr)place
			c_string := strings.clone_to_cstring(string(cstring(place_ptr^)), allocator)
			place_ptr^ = rawptr(c_string)
		} else {
			raw_string := cast(^Raw_Slice)place
			new_bytes := make([]u8, raw_string.len, allocator)
			intrinsics.mem_copy_non_overlapping(
				raw_data(new_bytes),
				raw_string.data,
				raw_string.len,
			)
			raw_string.data = raw_data(new_bytes)
		}
		return
	case runtime.Type_Info_Struct:
		for f_idx in 0 ..< var.field_count {
			field_place := rawptr(uintptr(place) + var.offsets[f_idx])
			_clone_allocations_inplace(var.types[f_idx], field_place, allocator, false)
		}
		return
	case runtime.Type_Info_Union:
		// special case handling of ptr unions with nil niche optimization:
		if len(var.variants) == 1 {
			only_ty := var.variants[0]
			#partial switch v in only_ty.variant {
			case runtime.Type_Info_Pointer:
				_clone_ptr_type_allocation_inplace(v.elem, place, allocator)
				return
			case runtime.Type_Info_Multi_Pointer:
				unimplemented("Multi_Pointer not supported")
			case runtime.Type_Info_Procedure:
				return // is okay, the proc ptr is simple copy, and is already copied
			case runtime.Type_Info_String:
				if v.is_cstring {
					place_ptr := cast(^rawptr)place
					if place_ptr^ == nil {
						return
					}
					c_string := strings.clone_to_cstring(string(cstring(place_ptr^)), allocator)
					place_ptr^ = rawptr(c_string)
					return
				}
			}
		}
		tag := _get_union_tag_for_non_ptr_union(var, place)
		if !var.no_nil && tag == 0 {
			return
		}
		variant_idx := tag if var.no_nil else tag - 1
		_clone_allocations_inplace(var.variants[variant_idx], place, allocator, false)
		return
	case runtime.Type_Info_Map:
		map_info := var.map_info
		assert(map_info != nil)
		raw_map := cast(^Raw_Map)place
		old_map: Raw_Map = raw_map^
		old_len := runtime.map_len(old_map)
		old_cap := runtime.map_cap(old_map)
		raw_map^ = Raw_Map {
			data      = 0,
			len       = 0,
			allocator = allocator,
		}
		if old_len == 0 {
			return
		}

		key_ty := var.key
		value_ty := var.value
		key_ty_is_copy := is_copy_type(key_ty)
		value_ty_is_copy := is_copy_type(value_ty)

		err := runtime.map_reserve_dynamic(raw_map, map_info, uintptr(old_cap))
		assert(err == .None)

		// iterate entries and put them into new hashmap one by one
		ks, vs, hs, _, _ := runtime.map_kvh_data_dynamic(old_map, map_info)
		for bucket_index in 0 ..< uintptr(old_cap) {
			runtime.map_hash_is_valid(hs[bucket_index]) or_continue
			old_key := rawptr(runtime.map_cell_index_dynamic(ks, map_info.ks, bucket_index))
			old_val := rawptr(runtime.map_cell_index_dynamic(vs, map_info.vs, bucket_index))

			hash := map_info.key_hasher(old_key, runtime.map_seed(raw_map^))
			runtime.__dynamic_map_set(raw_map, map_info, hash, old_key, old_val)
			key_place, value_place := runtime.__dynamic_map_get_key_and_value(
				raw_map,
				map_info,
				hash,
				old_key,
			)
			if !key_ty_is_copy {
				// print("key: ", any{old_key, key_ty.id}, " from ", old_key, "to", key_place)
				_clone_allocations_inplace(key_ty, key_place, allocator, true)
			}
			if !value_ty_is_copy {
				// print("value:", any{old_val, value_ty.id}, " from ", old_val, "to", value_place)
				_clone_allocations_inplace(value_ty, value_place, allocator, true)
			}
		}
		return
	}
	if PANIC_ON_UNSUPPORTED_TYPES {
		panic(tprint("Unsupported type for cloning: ", ty))
	}

}
_copy_into_new_slice :: proc(
	elem_ty: Type_Info,
	n_elems: int,
	old_ptr: rawptr,
	allocator: Allocator,
) -> (
	new_ptr: rawptr,
) {
	mem_region_size := n_elems * elem_ty.size
	err: mem.Allocator_Error
	new_ptr, err = mem.alloc(mem_region_size, elem_ty.align, allocator)
	assert(err == nil)
	intrinsics.mem_copy_non_overlapping(new_ptr, old_ptr, mem_region_size)
	if !is_copy_type(elem_ty) {
		for idx in 0 ..< n_elems {
			offset := uintptr(idx * elem_ty.size)
			new_elem_place := rawptr(uintptr(new_ptr) + offset)
			_clone_allocations_inplace(elem_ty, new_elem_place, allocator, true)
		}
	}
	return new_ptr
}
_clone_ptr_type_allocation_inplace :: proc(
	elem_ty: Type_Info,
	place: rawptr,
	allocator: Allocator,
) {
	old_ptr := (cast(^rawptr)place)^
	if old_ptr == nil {
		return
	}
	new_ptr, err := mem.alloc(elem_ty.size, elem_ty.align, allocator)
	assert(err == .None)
	(cast(^rawptr)place)^ = new_ptr
	_clone_any(elem_ty, old_ptr, new_ptr, allocator)
}
