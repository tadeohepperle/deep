package deep

import "base:intrinsics"
import "base:runtime"
import "core:mem"

drop :: proc(this: ^$T, allocator := context.allocator) {
	_drop_allocations_inplace(type_info_of(T), this, allocator, false)
}
_drop_allocations_inplace :: proc(
	ty: Type_Info,
	place: rawptr,
	allocator: Allocator,
	$ASSERT_NON_COPY_TYPE: bool,
) {
	// zero out the memory of this thing after freeing all reachable memory regions:
	defer {mem.zero(place, ty.size)}
	when !ASSERT_NON_COPY_TYPE {
		if is_copy_type(ty) {
			return
		}
	}
	#partial switch var in ty.variant {
	case runtime.Type_Info_Named:
		base_ty := type_info_base(ty)
		_drop_allocations_inplace(base_ty, place, allocator, true)
		return
	case runtime.Type_Info_Pointer:
		_drop_ptr_type_allocation_inplace(var.elem, place, allocator)
		return
	case runtime.Type_Info_Slice:
		raw_slice := cast(^Raw_Slice)place
		if raw_slice.data != nil {
			if !is_copy_type(var.elem) {
				for idx in 0 ..< raw_slice.len {
					elem_place := rawptr(uintptr(raw_slice.data) + uintptr(idx * var.elem_size))
					_drop_allocations_inplace(var.elem, elem_place, allocator, true)
				}
			}
			mem.free_with_size(raw_slice.data, raw_slice.len * var.elem_size, allocator)
		}
		return
	case runtime.Type_Info_Dynamic_Array:
		raw_arr := cast(^Raw_Dynamic_Array)place
		if raw_arr.data != nil {
			if !is_copy_type(var.elem) {
				for idx in 0 ..< raw_arr.len {
					elem_place := rawptr(uintptr(raw_arr.data) + uintptr(idx * var.elem_size))
					_drop_allocations_inplace(var.elem, elem_place, allocator, true)
				}
			}
			// maybe assert that raw_arr.allocator == allocator ??? Or should we omit this?
			assert(raw_arr.allocator == allocator)
			mem.free_with_size(raw_arr.data, raw_arr.cap * var.elem_size, raw_arr.allocator)
		}
		return
	case runtime.Type_Info_Array:
		// if we get here, we already know it is not an array of copy types, clone all values inplace:
		for idx in 0 ..< var.count {
			elem_place := rawptr(uintptr(place) + uintptr(idx * var.elem_size))
			_drop_allocations_inplace(var.elem, elem_place, allocator, true)
		}
		return
	case runtime.Type_Info_Enumerated_Array:
		if var.is_sparse {
			enum_ty, ok := var.index.variant.(runtime.Type_Info_Enum)
			assert(ok, "index of Type_Info_Enumerated_Array should be Type_Info_Enum")
			// in sparse case, no need to iterate the empty slots:
			for val in enum_ty.values {
				idx := int(val - var.min_value)
				assert(idx >= 0 && idx < var.count)
				elem_place := rawptr(uintptr(place) + uintptr(idx * var.elem_size))
				_drop_allocations_inplace(var.elem, elem_place, allocator, true)
			}
		} else {
			for idx in 0 ..< var.count {
				elem_place := rawptr(uintptr(place) + uintptr(idx * var.elem_size))
				_drop_allocations_inplace(var.elem, elem_place, allocator, true)
			}
		}
		return
	case runtime.Type_Info_String:
		if var.is_cstring {
			mem.free(cast(^rawptr)place)
		} else {
			raw_string := cast(^Raw_Slice)place
			mem.free_with_size(raw_string.data, raw_string.len)
		}
		return
	case runtime.Type_Info_Struct:
		// todo: allow for ignoring fields here.
		for f_idx in 0 ..< var.field_count {
			field_place := rawptr(uintptr(place) + var.offsets[f_idx])
			_drop_allocations_inplace(var.types[f_idx], field_place, allocator, false)
		}
		return
	case runtime.Type_Info_Union:
		// special case handling of ptr unions with nil niche optimization:
		if len(var.variants) == 1 {
			#partial switch v in var.variants[0].variant {
			case runtime.Type_Info_Pointer:
				_drop_ptr_type_allocation_inplace(v.elem, place, allocator)
				return
			case runtime.Type_Info_Multi_Pointer:
				unimplemented("Multi_Pointer not supported")
			case runtime.Type_Info_Procedure:
				return // is okay, the proc ptr is simple copy, and is already copied
			case runtime.Type_Info_String:
				if v.is_cstring {
					mem.free(cast(^rawptr)place)
					return
				}
			}
		}
		tag := _get_union_tag_for_non_ptr_union(var, place)
		variant_ty := var.variants[tag] if var.no_nil else var.variants[tag - 1]
		_drop_allocations_inplace(variant_ty, place, allocator, false)
		return
	case runtime.Type_Info_Map:
		raw_map: Raw_Map = (cast(^Raw_Map)place)^
		if raw_map.data == 0 {
			return
		}

		map_info := var.map_info
		assert(map_info != nil)
		map_len := runtime.map_len(raw_map)
		map_cap := runtime.map_cap(raw_map)

		if map_len > 0 {
			key_ty := var.key
			value_ty := var.value
			key_ty_is_copy := is_copy_type(key_ty)
			value_ty_is_copy := is_copy_type(value_ty)

			// iterate entries and put them into new hashmap one by one
			ks, vs, hs, _, _ := runtime.map_kvh_data_dynamic(raw_map, map_info)
			for bucket_index in 0 ..< uintptr(map_cap) {
				runtime.map_hash_is_valid(hs[bucket_index]) or_continue
				key_place := rawptr(runtime.map_cell_index_dynamic(ks, map_info.ks, bucket_index))
				value_place := rawptr(
					runtime.map_cell_index_dynamic(vs, map_info.vs, bucket_index),
				)
				if !key_ty_is_copy {
					// print("drop key allocation:", any{key_place, key_ty.id})
					_drop_allocations_inplace(key_ty, key_place, allocator, true)
				}
				if !value_ty_is_copy {
					// print("drop value allocation:", any{value_place, value_ty.id})
					_drop_allocations_inplace(value_ty, value_place, allocator, true)
				}
			}
		}
		err := runtime.map_free_dynamic(raw_map, map_info)
		assert(err == .None)
		return
	}
	if true {
		panic(tprint("Unsupported type for dropping: ", ty))
	}
}
_drop_ptr_type_allocation_inplace :: proc(
	elem_ty: Type_Info,
	place: rawptr,
	allocator: Allocator,
) {
	old_ptr := (cast(^rawptr)place)^
	if old_ptr == nil {
		return
	}
	_drop_allocations_inplace(elem_ty, old_ptr, allocator, false)
	mem.free(old_ptr, allocator)
}
