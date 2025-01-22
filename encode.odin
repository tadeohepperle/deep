package deep

import "base:intrinsics"
import "base:runtime"
import "core:mem"

encode :: proc(this: $T, allocator := context.allocator) -> (buf: [dynamic]u8) {
	buf = make([dynamic]u8, len = 0, cap = size_of(T), allocator = allocator)
	encode_to(this, &buf)
	return buf
}

encode_to :: proc(this: $T, buf: ^[dynamic]u8) {
	this := this
	_encode_any_to(type_info_of(T), &this, buf, false)
}
_encode_any_to :: proc(
	ty: Type_Info,
	place: rawptr,
	buf: ^[dynamic]u8,
	$ASSERT_NON_COPY_TYPE: bool,
) {
	when !ASSERT_NON_COPY_TYPE {
		if is_copy_type(ty) {
			_write(place, ty.size, buf)
			return
		}
	}
	#partial switch var in ty.variant {
	case runtime.Type_Info_Named:
		_encode_any_to(type_info_base(ty), place, buf, true)
		return
	case runtime.Type_Info_Pointer:
		if var.elem != nil {
			elem_place := (cast(^rawptr)place)^
			if elem_place != nil {
				_write_marker(.NonNilPtr, buf)
				_encode_any_to(var.elem, elem_place, buf, false)
			} else {
				_write_marker(.NilPtr, buf)
			}
			return
		}
	case runtime.Type_Info_Slice:
		raw_slice := cast(^Raw_Slice)place
		_encode_slice_to(raw_slice^, var.elem, buf)
		return
	case runtime.Type_Info_Dynamic_Array:
		raw_slice := cast(^Raw_Slice)place // works because Raw_Dynamic_Array also starts with `data: rawptr, len: int`
		_encode_slice_to(raw_slice^, var.elem, buf)
		return
	case runtime.Type_Info_Array:
		// if we get here, we already know it is not an array of copy types, clone all values inplace:
		for idx in 0 ..< var.count {
			elem_place := rawptr(uintptr(place) + uintptr(idx * var.elem_size))
			_encode_any_to(var.elem, elem_place, buf, true)
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
				_encode_any_to(var.elem, elem_place, buf, true)
			}
		} else {
			for idx in 0 ..< var.count {
				elem_place := rawptr(uintptr(place) + uintptr(idx * var.elem_size))
				_encode_any_to(var.elem, elem_place, buf, true)
			}
		}
		return
	case runtime.Type_Info_String:
		str: string
		if var.is_cstring {
			// this is O(n) operation searching for the null terminator:
			str = string((cast(^cstring)place)^)
		} else {
			str = (cast(^string)place)^
		}
		_write_string(str, buf)
		return
	case runtime.Type_Info_Struct:
		// todo: allow for ignoring fields here.
		for f_idx in 0 ..< var.field_count {
			field_place := rawptr(uintptr(place) + var.offsets[f_idx])
			_encode_any_to(var.types[f_idx], field_place, buf, false)
		}
		return
	case runtime.Type_Info_Union:
		// special case handling of ptr unions with nil niche optimization:
		if len(var.variants) == 1 {
			only_ty := var.variants[0]
			#partial switch v in var.variants[0].variant {
			case runtime.Type_Info_Pointer:
				_encode_any_to(only_ty, place, buf, true)
			case runtime.Type_Info_Multi_Pointer:
				unimplemented("multi pointers cannot be encoded!")
			case runtime.Type_Info_Procedure:
				unimplemented("procedure pointers cannot be encoded!")
			case runtime.Type_Info_String:
				if v.is_cstring {
					_encode_any_to(only_ty, place, buf, true)
					return
				}
			}
		}
		// write tag as int first, then write the data for this union variant
		tag := _get_union_tag_for_non_ptr_union(var, place)
		_write_len(tag, buf)
		variant_ty := var.variants[tag] if var.no_nil else var.variants[tag - 1]
		_encode_any_to(variant_ty, place, buf, false)
		return
	case runtime.Type_Info_Map:
		raw_map: Raw_Map = (cast(^Raw_Map)place)^

		map_info := var.map_info
		assert(map_info != nil)
		map_len := runtime.map_len(raw_map)
		map_cap := runtime.map_cap(raw_map)

		_write_marker(.MapStart, buf)
		_write_len(map_len, buf)
		if map_len == 0 || raw_map.data == 0 {
			return
		}

		key_ty := var.key
		value_ty := var.value
		key_ty_is_copy := is_copy_type(key_ty)
		value_ty_is_copy := is_copy_type(value_ty)

		// iterate entries and put them into new hashmap one by one
		ks, vs, hs, _, _ := runtime.map_kvh_data_dynamic(raw_map, map_info)
		for bucket_index in 0 ..< uintptr(map_cap) {
			runtime.map_hash_is_valid(hs[bucket_index]) or_continue
			key_place := rawptr(runtime.map_cell_index_dynamic(ks, map_info.ks, bucket_index))
			value_place := rawptr(runtime.map_cell_index_dynamic(vs, map_info.vs, bucket_index))

			// print("key: ", any{key_place, key_ty.id}, "is copy:", key_ty_is_copy)
			// print("value: ", any{value_place, value_ty.id}, "is copy:", value_ty_is_copy)
			_write_marker(.MapKeyStart, buf)
			if key_ty_is_copy {
				_write(key_place, key_ty.size, buf)
			} else {
				_encode_any_to(key_ty, key_place, buf, true)
			}
			_write_marker(.MapValStart, buf)
			if value_ty_is_copy {
				_write(value_place, value_ty.size, buf)
			} else {
				_encode_any_to(value_ty, value_place, buf, true)
			}
		}
		return
	}
	unimplemented(tprint("Unsupported type for encoding: ", ty))
}

_encode_slice_to :: proc(raw_slice: Raw_Slice, elem_ty: Type_Info, buf: ^[dynamic]u8) {
	_write_marker(.SeqStart, buf)
	_write_len(raw_slice.len, buf)
	if raw_slice.len == 0 {
		return
	}
	if is_copy_type(elem_ty) {
		_write(raw_slice.data, raw_slice.len * elem_ty.size, buf)
	} else {
		for idx in 0 ..< raw_slice.len {
			elem_place := rawptr(uintptr(raw_slice.data) + uintptr(idx * elem_ty.size))
			_encode_any_to(elem_ty, elem_place, buf, true)
		}
	}
	return
}

Marker :: enum u8 {
	SeqStart,
	MapStart,
	NonNilPtr,
	NilPtr,
	MapKeyStart,
	MapValStart,
	TypeStart,
}
MarkerBytes :: [4]u8

@(rodata)
MARKER_BYTES := [Marker]MarkerBytes {
	.SeqStart    = {255, 255, 255, 255},
	.MapStart    = {244, 244, 244, 244},
	.NonNilPtr   = {236, 236, 236, 236},
	.NilPtr      = {230, 230, 230, 230},
	.MapKeyStart = {133, 133, 133, 133},
	.MapValStart = {134, 134, 134, 134},
	.TypeStart   = {255, 254, 253, 252},
}
_write_marker :: proc(marker: Marker, buf: ^[dynamic]u8) {
	_write(&MARKER_BYTES[marker], size_of(MarkerBytes), buf)
}
_write_len :: proc(i: int, buf: ^[dynamic]u8) {
	assert(i >= 0)
	i := i
	_write(&i, size_of(int), buf)
}
_write_string :: proc(s: string, buf: ^[dynamic]u8) {
	assert(len(s) < int(max(u32)))
	s_len_u32 := u32(len(s))
	_write(&s_len_u32, 4, buf)
	_write(raw_data(s), len(s), buf)
}
_write_nothing :: proc(n_bytes: int, buf: ^[dynamic]u8) {
	raw := cast(^Raw_Dynamic_Array)buf
	_buffer_grow(raw, n_bytes)
}
_write_data :: #force_inline proc(from: ^$T, buf: ^[dynamic]u8) {
	_write(from, size_of(T), buf)
}
_write :: proc(from: rawptr, n_bytes: int, buf: ^[dynamic]u8) {
	raw := cast(^Raw_Dynamic_Array)buf
	write_offset := uintptr(raw.len)
	_buffer_grow(raw, n_bytes)
	dst := rawptr(uintptr(raw.data) + write_offset)
	mem.copy_non_overlapping(dst, from, n_bytes)
}
_write_at :: proc(byte_idx: int, from: rawptr, n_bytes: int, buf: ^[dynamic]u8) {
	assert(byte_idx + n_bytes <= len(buf))
	dst := rawptr(uintptr(raw_data(buf^)) + uintptr(byte_idx))
	mem.copy_non_overlapping(dst, from, n_bytes)
}
_buffer_grow :: proc(raw: ^Raw_Dynamic_Array, n_bytes: int) {
	new_len := raw.len + n_bytes
	// resize if capacity is not enough:
	if new_len > raw.cap {
		new_cap := 2 * raw.cap + 8
		if raw.allocator.procedure == nil {
			raw.allocator = context.allocator
		}
		new_data, err := mem.resize(raw.data, raw.cap, new_cap, allocator = raw.allocator) // could be non_zero_mem_resize
		assert(err == .None)
		assert(new_data != nil)
		raw.data = new_data
		raw.cap = new_cap
	}
	raw.len = new_len
}
_read_marker :: proc(expected_marker: Marker, cursor: Cursor) -> DecodeError {
	if len(cursor) < size_of(MarkerBytes) {
		return .NotEnoughBytes
	}
	expected := MARKER_BYTES[expected_marker]
	got := (cast(^MarkerBytes)raw_data(cursor^))^
	if got != expected {
		switch expected_marker {
		case .SeqStart:
			return .Marker_SeqStart_Missing
		case .MapStart:
			return .Marker_MapStart_Missing
		case .MapKeyStart:
			return .Marker_MapKeyStart_Missing
		case .MapValStart:
			return .Marker_MapValStart_Missing
		case .TypeStart:
			return .Marker_TypeStart_Missing
		case .NilPtr, .NonNilPtr:
			panic("Dont call _read_marker with .NilPtr, .NonNilPtr, use _read_ptr_marker")
		}
	}
	cursor^ = cursor[size_of(MarkerBytes):]
	return .None
}
_read_ptr_marker :: proc(cursor: Cursor) -> (is_nil: bool, err: DecodeError) {
	if len(cursor) < 4 {
		return {}, .NotEnoughBytes
	}
	got := (cast(^MarkerBytes)raw_data(cursor^))^
	if got == MARKER_BYTES[.NilPtr] {
		is_nil = true
	} else if got == MARKER_BYTES[.NonNilPtr] {
		is_nil = false
	} else {
		return {}, .MarkerForPtrMissing
	}
	cursor^ = cursor[size_of(MarkerBytes):]
	return is_nil, .None
}
_read_len :: proc(cursor: Cursor) -> (i: int, err: DecodeError) {
	if len(cursor) < size_of(int) {
		return 0, .NotEnoughBytes
	}
	i = (cast(^int)raw_data(cursor^))^
	if i < 0 {
		return 0, .NegativeLength
	}
	cursor^ = cursor[size_of(int):]
	return i, .None
}
// not that the string points to bytes directly in the buffer
_read_string :: proc(cursor: Cursor) -> (s: string, err: DecodeError) {
	if len(cursor) < 4 {
		return "", .NotEnoughBytes
	}
	s_len_u32 := (cast(^u32)raw_data(cursor^))^
	s_len := int(s_len_u32)
	s_bytes_total := 4 + s_len
	if len(cursor) < s_bytes_total {
		return "", .NotEnoughBytes
	}
	s = transmute(string)Raw_String{&cursor[4], s_len}
	cursor^ = cursor[s_bytes_total:]
	return s, .None

}
_read :: proc(to: rawptr, n_bytes: int, cursor: Cursor) -> DecodeError {
	if n_bytes > len(cursor) {
		return .NotEnoughBytes
	}
	mem.copy_non_overlapping(to, raw_data(cursor^), n_bytes)
	cursor^ = cursor[n_bytes:] // skip over read bytes
	return .None
}
_read_data :: proc($T: typeid, cursor: Cursor) -> (res: T, err: DecodeError) {
	T_SIZE :: size_of(T)
	if T_SIZE > len(cursor) {
		return {}, .NotEnoughBytes
	}
	mem.copy_non_overlapping(&res, raw_data(cursor^), T_SIZE)
	cursor^ = cursor[T_SIZE:]
	return res, .None
}
_skip_over :: #force_inline proc(n_bytes: int, cursor: Cursor) -> DecodeError {
	if n_bytes > len(cursor) {
		return .NotEnoughBytes
	}
	cursor^ = cursor[n_bytes:] // skip over read bytes
	return .None
}

DecodeError :: enum {
	None,
	UnsupportedType,
	InvalidUnionTag,
	NegativeLength,
	NotEnoughBytes,
	TooManyBytes,
	Marker_SeqStart_Missing,
	Marker_MapStart_Missing,
	Marker_MapKeyStart_Missing,
	Marker_MapValStart_Missing,
	MarkerForPtrMissing,
	Marker_TypeStart_Missing,
	DoubleKeyInMap,
	InvalidSchemaTypeTag,
	InvalidCopyPrimitiveTag,
	InvalidNumberOfSchemaBytes,
	SchemaIsEmpty,
	TypeIdxTooHigh,
	BoolByteNotZeroOrOne,
	FieldOffsetGreaterThanStructSize,
	UnionTagOffsetGreaterThanSize,
	InvalidUnionTagSize,
	UnionWithZeroVariants,
	InvalidTypeForSchema,
	NoTypeSimilarityBetweenOriginAndTargetSchema,
}
Cursor :: ^[]u8
decode :: proc(
	$T: typeid,
	buf: []u8,
	allocator := context.allocator,
) -> (
	res: T,
	err: DecodeError,
) {
	cursor := buf
	tracker := tracker_create(allocator)
	err = _decode_any(type_info_of(T), &res, &cursor, &tracker, false)
	if err != .None {
		tracker_free_all(&tracker)
	}
	return res, err
}
validate_encoding :: proc($T: typeid, buf: []u8) -> (err: DecodeError) {
	cursor := buf
	err = _validate_encoding_any(type_info_of(T), &cursor, false)
	if err == .None && len(cursor) != 0 {
		err = .TooManyBytes
	}
	return err
}


_validate_encoding_any :: proc(
	ty: Type_Info,
	cursor: ^[]u8,
	$ASSERT_NON_COPY_TYPE: bool,
) -> (
	err: DecodeError,
) {
	when !ASSERT_NON_COPY_TYPE {
		if is_copy_type(ty) {
			return _skip_over(ty.size, cursor)
		}
	}
	#partial switch var in ty.variant {
	case runtime.Type_Info_Named:
		return _validate_encoding_any(type_info_base(ty), cursor, true)
	case runtime.Type_Info_Pointer:
		if var.elem != nil {
			is_nil := _read_ptr_marker(cursor) or_return
			if !is_nil {
				return _validate_encoding_any(var.elem, cursor, false)
			} else {
				return .None
			}
		}
	case runtime.Type_Info_Slice:
		return _validate_seq(var.elem, cursor)
	case runtime.Type_Info_Dynamic_Array:
		return _validate_seq(var.elem, cursor)
	case runtime.Type_Info_Array:
		// if we get here, we already know it is not an array of copy types, clone all values inplace:
		for _ in 0 ..< var.count {
			_validate_encoding_any(var.elem, cursor, true) or_return
		}
		return .None
	case runtime.Type_Info_Enumerated_Array:
		n := var.count
		if var.is_sparse {
			enum_ty, ok := var.index.variant.(runtime.Type_Info_Enum)
			assert(ok, "index of Type_Info_Enumerated_Array should be Type_Info_Enum")
			n = len(enum_ty.values)
		}
		for _ in 0 ..< n {
			_validate_encoding_any(var.elem, cursor, true) or_return
		}
		return .None
	case runtime.Type_Info_String:
		n := _read_len(cursor) or_return
		_skip_over(n, cursor) or_return
		return .None
	case runtime.Type_Info_Struct:
		for f_idx in 0 ..< var.field_count {
			_validate_encoding_any(var.types[f_idx], cursor, false) or_return
		}
		return .None
	case runtime.Type_Info_Union:
		// special case handling of ptr unions with nil niche optimization:
		if len(var.variants) == 1 {
			only_ty := var.variants[0]
			#partial switch v in var.variants[0].variant {
			case runtime.Type_Info_Pointer:
				_validate_encoding_any(only_ty, cursor, true)
			case runtime.Type_Info_Multi_Pointer:
				unimplemented("multi pointers cannot be encoded/decoded!")
			case runtime.Type_Info_Procedure:
				unimplemented("procedure pointers cannot be encoded/decoded!")
			case runtime.Type_Info_String:
				if v.is_cstring {
					n := _read_len(cursor) or_return
					_skip_over(n, cursor) or_return
					return .None
				}
			}
		}
		// read tag, then based on that read data
		tag := _read_len(cursor) or_return
		if !var.no_nil && tag == 0 {
			// this means the union was nil
			return .None
		}
		variant_idx := tag if var.no_nil else tag - 1
		if variant_idx < 0 || variant_idx >= len(var.variants) {
			return .InvalidUnionTag
		}
		variant_ty := var.variants[variant_idx]
		_validate_encoding_any(variant_ty, cursor, false)
		return .None
	case runtime.Type_Info_Map:
		_read_marker(.MapStart, cursor) or_return
		n := _read_len(cursor) or_return
		if n == 0 {
			return .None
		}
		key_ty := var.key
		value_ty := var.value
		key_ty_is_copy := is_copy_type(key_ty)
		value_ty_is_copy := is_copy_type(value_ty)
		for _ in 0 ..< n {
			_read_marker(.MapKeyStart, cursor) or_return
			if key_ty_is_copy {
				_skip_over(key_ty.size, cursor) or_return
			} else {
				_validate_encoding_any(key_ty, cursor, true)
			}
			_read_marker(.MapValStart, cursor) or_return
			if value_ty_is_copy {
				_skip_over(value_ty.size, cursor) or_return
			} else {
				_validate_encoding_any(value_ty, cursor, true)
			}
		}
		return .None
	}
	return .UnsupportedType
}
_validate_seq :: proc(elem_ty: Type_Info, cursor: Cursor) -> (err: DecodeError) {
	_read_marker(.SeqStart, cursor) or_return
	n := _read_len(cursor) or_return
	if is_copy_type(elem_ty) {
		_skip_over(n * elem_ty.size, cursor) or_return // just advance cursor by n elements
	} else {
		for _ in 0 ..< n {
			_validate_encoding_any(elem_ty, cursor, true) or_return
		}
	}
	return .None
}

// the `tracker` arg is for tracking all allocations that happen to deallocate them in the error case.
_decode_any :: proc(
	ty: Type_Info,
	place: rawptr,
	cursor: Cursor,
	tracker: ^Tracker,
	$ASSERT_NON_COPY_TYPE: bool,
) -> (
	err: DecodeError,
) {
	when !ASSERT_NON_COPY_TYPE {
		if is_copy_type(ty) {
			return _read(place, ty.size, cursor)
		}
	}
	#partial switch var in ty.variant {
	case runtime.Type_Info_Named:
		return _decode_any(type_info_base(ty), place, cursor, tracker, true)
	case runtime.Type_Info_Pointer:
		if var.elem != nil {
			is_nil := _read_ptr_marker(cursor) or_return
			ptr_place := cast(^rawptr)place
			if !is_nil {
				ptr := tracker_alloc(tracker, var.elem.size, var.elem.align)
				_decode_any(var.elem, ptr, cursor, tracker, false) or_return
				ptr_place^ = ptr
			} else {
				ptr_place^ = nil
			}
			return .None
		}
	case runtime.Type_Info_Slice:
		raw_slice := cast(^Raw_Slice)place
		raw_slice^ = _decode_seq(var.elem, cursor, tracker) or_return
		return .None
	case runtime.Type_Info_Dynamic_Array:
		raw_arr := cast(^Raw_Dynamic_Array)place
		raw_slice := _decode_seq(var.elem, cursor, tracker) or_return
		raw_arr.data = raw_slice.data
		raw_arr.len = raw_slice.len
		raw_arr.cap = raw_slice.len
		raw_arr.allocator = tracker.allocator
		return .None
	case runtime.Type_Info_Array:
		// if we get here, we already know it is not an array of copy types, clone all values inplace:
		for idx in 0 ..< var.count {
			elem_place := rawptr(uintptr(place) + uintptr(idx * var.elem_size))
			_decode_any(var.elem, elem_place, cursor, tracker, true) or_return
		}
		return .None
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
				_decode_any(var.elem, elem_place, cursor, tracker, true) or_return
			}
		} else {
			for idx in 0 ..< var.count {
				elem_place := rawptr(uintptr(place) + uintptr(idx * var.elem_size))
				_decode_any(var.elem, elem_place, cursor, tracker, true) or_return
			}
		}
		return .None
	case runtime.Type_Info_String:
		str := _read_string(cursor) or_return
		raw_str := transmute(Raw_String)str
		if raw_str.len == 0 {
			return .None
		}
		ptr := tracker_alloc(tracker, raw_str.len, 1)
		mem.copy_non_overlapping(ptr, raw_str.data, raw_str.len)
		if var.is_cstring {
			(cast(^rawptr)place)^ = ptr
		} else {
			(cast(^Raw_String)place)^ = Raw_String{cast([^]u8)ptr, raw_str.len}
		}
		return .None
	case runtime.Type_Info_Struct:
		for f_idx in 0 ..< var.field_count {
			field_place := rawptr(uintptr(place) + var.offsets[f_idx])
			_decode_any(var.types[f_idx], field_place, cursor, tracker, false) or_return
		}
		return .None
	case runtime.Type_Info_Union:
		// special case handling of ptr unions with nil niche optimization:
		if len(var.variants) == 1 {
			only_ty := var.variants[0]
			#partial switch v in var.variants[0].variant {
			case runtime.Type_Info_Pointer:
				return _decode_any(only_ty, place, cursor, tracker, true)
			case runtime.Type_Info_Multi_Pointer:
				unimplemented("multi pointers cannot be encoded/decoded!")
			case runtime.Type_Info_Procedure:
				unimplemented("procedure pointers cannot be encoded/decoded!")
			case runtime.Type_Info_String:
				if v.is_cstring {
					return _decode_any(only_ty, place, cursor, tracker, true)
				}
			}
		}


		// read tag, then based on that read data
		tag := _read_len(cursor) or_return
		if !var.no_nil && tag == 0 {
			// this means the union was nil
			mem.zero(place, ty.size)
			return .None
		}
		_set_union_tag_for_non_ptr_union(var, place, tag)
		variant_idx := tag if var.no_nil else tag - 1
		if variant_idx < 0 || variant_idx >= len(var.variants) {
			return .InvalidUnionTag
		}
		variant_ty := var.variants[variant_idx]
		return _decode_any(variant_ty, place, cursor, tracker, false)
	case runtime.Type_Info_Map:
		_read_marker(.MapStart, cursor) or_return
		n := _read_len(cursor) or_return
		raw_map := cast(^Raw_Map)place
		raw_map^ = Raw_Map {
			data      = 0,
			len       = 0,
			allocator = tracker.allocator,
		}
		if n == 0 {
			return .None
		}
		key_ty := var.key
		value_ty := var.value
		key_ty_is_copy := is_copy_type(key_ty)
		value_ty_is_copy := is_copy_type(value_ty)

		map_info := var.map_info
		assert(map_info != nil)

		key_scratch, value_scratch: rawptr
		alloc_err: runtime.Allocator_Error
		key_scratch, alloc_err = mem.alloc(key_ty.size, key_ty.align, context.temp_allocator)
		assert(alloc_err == .None)
		value_scratch, alloc_err = mem.alloc(value_ty.size, value_ty.align, context.temp_allocator)
		assert(alloc_err == .None)
		defer {
			mem.free(key_scratch, context.temp_allocator)
			mem.free(value_scratch, context.temp_allocator)
			// such that the map gets deallocated if any error occurs later:
			tracker_add(tracker, rawptr(raw_map.data), runtime.map_cap(raw_map^))
		}
		for _ in 0 ..< n {
			_read_marker(.MapKeyStart, cursor) or_return
			if key_ty_is_copy {
				_read(key_scratch, key_ty.size, cursor) or_return
			} else {
				_decode_any(key_ty, key_scratch, cursor, tracker, true) or_return
			}
			_read_marker(.MapValStart, cursor) or_return
			if value_ty_is_copy {
				_read(value_scratch, value_ty.size, cursor) or_return
			} else {
				_decode_any(value_ty, value_scratch, cursor, tracker, true) or_return
			}
			// calculate hash and insert key and value into map
			hash := map_info.key_hasher(key_scratch, runtime.map_seed(raw_map^))
			found := runtime.__dynamic_map_get(raw_map, map_info, hash, key_scratch)
			if found != nil {
				return .DoubleKeyInMap
			}
			runtime.__dynamic_map_set(raw_map, map_info, hash, key_scratch, value_scratch)
		}
		return .None
	}
	return .UnsupportedType
}

_decode_seq :: proc(
	elem_ty: Type_Info,
	cursor: ^[]u8,
	tracker: ^Tracker,
) -> (
	raw_slice: Raw_Slice,
	err: DecodeError,
) {
	_read_marker(.SeqStart, cursor) or_return
	n := _read_len(cursor) or_return
	if n == 0 {
		return Raw_Slice{nil, 0}, .None
	}
	ptr := tracker_alloc(tracker, elem_ty.size * n, elem_ty.align)
	if is_copy_type(elem_ty) {
		_read(ptr, n * elem_ty.size, cursor) or_return
	} else {
		u_ptr := uintptr(ptr)
		for idx in 0 ..< n {
			elem_place := rawptr(u_ptr + uintptr(idx * elem_ty.size))
			_decode_any(elem_ty, elem_place, cursor, tracker, true) or_return
		}
	}
	return Raw_Slice{ptr, n}, .None
}
