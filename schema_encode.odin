package deep

import "base:intrinsics"
import "base:runtime"
import "core:mem"


_encode_with_schema :: proc(
	ty_idx: u32,
	place: rawptr,
	buf: ^[dynamic]u8,
	schema: SchemaWithPtrs,
) {
	ty := schema.types[ty_idx]
	if ty.is_copy {
		_write(place, ty.size, buf)
		return
	}
	switch kind in ty.kind {
	case CopyPrimitiveTy, EnumTy:
		panic("CopyPrimitiveTy and EnumTy should have ty.is_copy and not reach this point")
	case PtrTy:
		elem_place := (cast(^rawptr)place)^
		if elem_place != nil {
			_write_marker(.NonNilPtr, buf)
			_encode_with_schema(kind.elem_ty_idx, elem_place, buf, schema)
		} else {
			_write_marker(.NilPtr, buf)
		}
	case StringTy:
		str: string
		if kind.is_cstring {
			// this is O(n) operation searching for the null terminator:
			str = string((cast(^cstring)place)^)
		} else {
			str = (cast(^string)place)^
		}
		_write_string(str, buf)
	case ArrayTy:
		// if we get here array is not copy type:
		elem_size := schema.types[kind.elem_ty_idx].size
		for idx in 0 ..< int(kind.count) {
			elem_place := rawptr(uintptr(place) + uintptr(idx * elem_size))
			_encode_with_schema(kind.elem_ty_idx, elem_place, buf, schema)
		}
		return
	case SparseEnumArrayTy:
		idx_type, ok := schema.types[kind.index_ty_idx].kind.(EnumTy)
		assert(ok, "index_ty_idx of SparseEnumArrayTy should lead to EnumTy")
		// in sparse case, no need to iterate the empty slots:
		elem_size := schema.types[kind.elem_ty_idx].size
		for val in idx_type.values {
			idx := int(val - kind.min_value)
			assert(idx >= 0 && u32(idx) < kind.count)
			elem_place := rawptr(uintptr(place) + uintptr(idx * elem_size))
			_encode_with_schema(kind.elem_ty_idx, elem_place, buf, schema)
		}
	case SeqTy:
		// dynamic arrays or slices
		raw_slice := cast(^Raw_Slice)place // works for dynamic arrays and slices because they share the same start
		_write_marker(.SeqStart, buf)
		_write_len(raw_slice.len, buf)
		if raw_slice.len == 0 {
			return
		}
		elem_ty := schema.types[kind.elem_ty_idx]
		if elem_ty.is_copy {
			_write(raw_slice.data, raw_slice.len * elem_ty.size, buf)
		} else {
			for idx in 0 ..< raw_slice.len {
				elem_place := rawptr(uintptr(raw_slice.data) + uintptr(idx * elem_ty.size))
				_encode_with_schema(kind.elem_ty_idx, elem_place, buf, schema)
			}
		}
	case StructTy:
		for field in kind.fields {
			field_place := rawptr(uintptr(place) + uintptr(field.offset))
			_encode_with_schema(field.ty_idx, field_place, buf, schema)
		}
	case UnionTy:
		// write tag as int first, then write the data for this union variant
		tag: int
		offset := uintptr(kind.tag_offset)
		switch kind.tag_size {
		case .U8:
			tag = int((cast(^u8)rawptr(uintptr(place) + offset))^)
		case .U16:
			tag = int((cast(^u16)rawptr(uintptr(place) + offset))^)
		case .U32:
			tag = int((cast(^u32)rawptr(uintptr(place) + offset))^)
		case .U64:
			tag = int((cast(^u64)rawptr(uintptr(place) + offset))^)
		}
		_write_len(tag, buf)
		if !kind.no_nil && tag == 0 {
			return
		}
		v_ty_idx := kind.variant_indices[tag] if kind.no_nil else kind.variant_indices[tag - 1]
		_encode_with_schema(v_ty_idx, place, buf, schema)
	case MapTy:
		type_info_map, is_map_ty := schema.info_ptrs[ty_idx].variant.(runtime.Type_Info_Map)
		assert(is_map_ty, "Schema type MapTy should point to runtime.Type_Info_Map")
		map_info := type_info_map.map_info
		assert(map_info != nil)

		raw_map: Raw_Map = (cast(^Raw_Map)place)^
		map_len := runtime.map_len(raw_map)
		map_cap := runtime.map_cap(raw_map)

		_write_marker(.MapStart, buf)
		_write_len(map_len, buf)
		if map_len == 0 || raw_map.data == 0 {
			return
		}

		// iterate entries and put them into new hashmap one by one
		ks, vs, hs, _, _ := runtime.map_kvh_data_dynamic(raw_map, map_info)
		for bucket_index in 0 ..< uintptr(map_cap) {
			runtime.map_hash_is_valid(hs[bucket_index]) or_continue
			key_place := rawptr(runtime.map_cell_index_dynamic(ks, map_info.ks, bucket_index))
			value_place := rawptr(runtime.map_cell_index_dynamic(vs, map_info.vs, bucket_index))

			_write_marker(.MapKeyStart, buf)
			_encode_with_schema(kind.key_ty_idx, key_place, buf, schema)
			_write_marker(.MapValStart, buf)
			_encode_with_schema(kind.val_ty_idx, value_place, buf, schema)
		}
	}
}
