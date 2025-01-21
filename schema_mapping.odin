package deep
import "base:runtime"
import "core:mem"


schemaful_encode_to :: proc(value: $T, buf: ^[dynamic]u8) {
	value := value
	schema, err := schema_of_type(type_info_of(T), context.temp_allocator)
	assert(err == nil, tprint(err))
	encode_schema_to(schema.types, buf)
	_encode_with_schema(0, &value, buf, schema)
}

schemaful_decode :: proc(
	$T: typeid,
	bytes: []u8,
	allocator := context.allocator,
) -> (
	val: T,
	err: DecodeError,
) {
	cursor := bytes
	a_schema := decode_schema(&cursor, context.temp_allocator) or_return
	b_schema, schema_build_err := schema_of_type(type_info_of(T), context.temp_allocator)
	if err != nil {
		return {}, .InvalidTypeForSchema
	}
	policy := make_decoding_policy(a_schema, b_schema, context.temp_allocator)
	print("policy", policy)
	if _, is_skip := policy.(SkipPolicy); is_skip {
		return {}, .NoTypeSimilarityBetweenOriginAndTargetSchema
	}
	decode_ctx := PolicyDecodeCtx {
		a_schema = a_schema,
		b_schema = b_schema,
		cursor   = &cursor,
		tracker  = tracker_create(allocator),
	}
	err = _policy_decode(policy, &val, &decode_ctx)
	if err != .None {
		tracker_free_all(&decode_ctx.tracker)
		return {}, err
	} else {
		return val, .None
	}
}

ConversionTable :: struct {
	policy_allocator: Allocator,
	root_policy:      Policy,
	a_schema:         Schema,
	b_schema:         SchemaWithPtrs,
}
policy_drop :: proc(policy: Policy) {
	// this is necessary, because a policy is a web of allocated things where some are duplicated
	// e.g. no single owner. so we need to make sure each slice is only dropped once.
	kind := policy_kind(policy)
	if kind == .Unresolved || kind == .Skip || kind == .Match {
		return
	}
	dropped: map[rawptr]None
	stack: [dynamic]Policy
	defer {delete(dropped);delete(stack)}
	append(&stack, policy)
	for len(stack) > 0 {
		switch p in pop(&stack) {
		case SkipPolicy, MatchPolicy, UnresolvedPolicy:
		case StructConversion:
			if raw_data(p.field_conversions) not_in dropped {
				dropped[raw_data(p.field_conversions)] = None{}
				delete(p.field_conversions)
			}
		}
	}
}

Policy :: union #no_nil {
	SkipPolicy,
	MatchPolicy,
	StructConversion,
	UnresolvedPolicy,
}
PolicyKind :: enum {
	Skip,
	Match,
	Struct,
	Unresolved,
}
policy_kind :: proc(policy: Policy) -> PolicyKind {
	switch p in policy {
	case SkipPolicy:
		return .Skip
	case MatchPolicy:
		return .Match
	case StructConversion:
		return .Struct
	case UnresolvedPolicy:
		return .Unresolved
	}
	unreachable()
}

StructConversion :: struct {
	a_idx:             u32,
	b_idx:             u32,
	field_conversions: []FieldConversion,
}
FieldConversion :: struct {
	// a_ty_idx:    u32,
	// b_ty_idx:    u32,
	offset_in_b: uintptr, // offset of field in the b struct this field is in
	policy:      Policy,
}
MatchPolicy :: struct {
	b_idx: u32,
}
SkipPolicy :: struct {
	a_idx: u32,
}
UnresolvedPolicy :: struct {
	a_idx: u32,
	b_idx: u32,
}
make_decoding_policy :: proc(
	a_schema: Schema,
	b_schema: SchemaWithPtrs,
	policy_allocator := context.temp_allocator,
) -> Policy {
	print("a_schema")
	print_schema(a_schema)
	print("b_schema")
	print_schema(b_schema.types)

	assert(len(a_schema) > 0)
	assert(len(b_schema.types) > 0)
	TyIdxPair :: struct {
		a_idx: u32,
		b_idx: u32,
	}
	Ctx :: struct {
		policy_allocator: Allocator,
		a_schema:         Schema,
		b_schema:         SchemaWithPtrs,
		policy_lookup:    map[TyIdxPair]Policy,
	}
	ctx := Ctx {
		policy_allocator = policy_allocator,
		a_schema         = a_schema,
		b_schema         = b_schema,
		policy_lookup    = make(map[TyIdxPair]Policy, context.temp_allocator),
	}
	root_policy := determine_policy(&ctx, 0, 0)
	// fill_in_unresolved_policies(&root_policy, ctx.policy_lookup, ctx.policy_allocator)
	return root_policy

	determine_policy :: proc(ctx: ^Ctx, a_idx: u32, b_idx: u32) -> (policy: Policy) {
		print("determine policy for a_idx,b_idx", a_idx, ",", b_idx)
		if policy, ok := ctx.policy_lookup[{a_idx, b_idx}]; ok {
			print("    Found", policy)
			return policy
		}
		print("    Found nothing")
		a_ty := ctx.a_schema[a_idx]
		b_ty := ctx.b_schema.types[b_idx]
		if a_ty.hash == b_ty.hash {
			policy = MatchPolicy{b_idx}
		} else {
			// insert unresolved to not get into infinite recursion:
			ctx.policy_lookup[{a_idx, b_idx}] = UnresolvedPolicy{a_idx, b_idx}

			policy = SkipPolicy{a_idx}
			#partial it: switch a_kind in a_ty.kind {
			case SeqTy:
				b_kind := b_ty.kind.(SeqTy) or_break it
				// don't care if one is slice or the other is dynamic array, treat as match:
				policy = MatchPolicy{b_idx}
			case StructTy:
				b_kind := b_ty.kind.(StructTy) or_break it
				// both are structs, iterate b fields to find matching fields in a:
				// - if all skip, the struct policy can be skip altogether.
				// - all match should not be possible here.
				// - if mix, a struct conversion policy is put in.
				field_conversions := make(
					[]FieldConversion,
					len(a_kind.fields),
					ctx.policy_allocator,
				)
				n_fields_skipped := 0
				for a_f, i in a_kind.fields {
					a_f_ty := ctx.a_schema[a_f.ty_idx]
					a_f_conversion := &field_conversions[i]
					found_field_in_b := false
					for b_f in b_kind.fields {
						if b_f.name == a_f.name {
							conversion_policy := determine_policy(ctx, a_f.ty_idx, b_f.ty_idx)
							a_f_conversion^ = FieldConversion {
								// a_ty_idx    = a_f.ty_idx,
								// b_ty_idx    = b_f.ty_idx,
								offset_in_b = uintptr(b_f.offset),
								policy      = conversion_policy,
							}
							found_field_in_b = true
							break
						}
					}
					if !found_field_in_b {
						n_fields_skipped += 1
						a_f_conversion^ = FieldConversion {
							// a_ty_idx    = a_f.ty_idx,
							// b_ty_idx    = max(u32),
							offset_in_b = 0,
							policy      = SkipPolicy{a_f.ty_idx},
						}
					}
				}
				// now check if all fields are just skipped because no match found in b_struct
				// then skip the entire struct, otherwise store the conversion table:
				if n_fields_skipped != len(a_kind.fields) {
					// so if not all fields are skipped, insert the conversion table:
					policy = StructConversion{a_idx, b_idx, field_conversions}
				} else {
					// policy stays: "skip entire struct"
					delete(field_conversions, ctx.policy_allocator)
				}
			}
		}
		print("    Insert for a_idx,b_idx", a_idx, ",", b_idx, "  ", policy)
		ctx.policy_lookup[{a_idx, b_idx}] = policy
		return policy
	}

	// traverses the policy tree to find unresolved policies and replaces them with the resolved ones.
	// this step is postponed until the end, because then we can be sure, that the root policy
	// and all conversions have been figured out already and the lookup should contain a
	// not-unresolved policy for each type mapping type idx pair.
	fill_in_unresolved_policies :: proc(
		policy: ^Policy,
		lookup: map[TyIdxPair]Policy,
		allocator: Allocator,
	) {
		switch &p in policy {
		case SkipPolicy, MatchPolicy:
			return
		case StructConversion:
			n_fields_skipped := 0
			for &field_conv in p.field_conversions {
				fill_in_unresolved_policies(&field_conv.policy, lookup, allocator)
				if _, is_skip := field_conv.policy.(SkipPolicy); is_skip {
					n_fields_skipped += 1
				}
			}
			if n_fields_skipped == len(p.field_conversions) {
				// now the entire struct can just be skipped, no conversion table needed anymore:
				delete(p.field_conversions, allocator)
				policy^ = SkipPolicy{p.a_idx}
			}
		case UnresolvedPolicy:
			resolved, ok := lookup[{p.a_idx, p.b_idx}]
			if _, is_unresolved := resolved.(UnresolvedPolicy); is_unresolved || !ok {
				panic(
					`When calling fill_in_unresolved_policies, policy_lookup should contain a resolved policy for each type pair!`,
				)
			}
			policy^ = resolved
		}
	}
}

PolicyDecodeCtx :: struct {
	a_schema: Schema,
	b_schema: SchemaWithPtrs,
	cursor:   Cursor,
	tracker:  Tracker,
}
_policy_decode :: proc(
	policy: Policy,
	place: rawptr,
	ctx: ^PolicyDecodeCtx,
) -> (
	err: DecodeError,
) {
	switch pol in policy {
	case SkipPolicy:
		return _skip_decode(pol.a_idx, ctx.cursor, ctx.a_schema)
	case MatchPolicy:
		return _match_decode(pol.b_idx, place, ctx.cursor, ctx.b_schema, &ctx.tracker)
	case StructConversion:
		for field_conv in pol.field_conversions {
			field_target_place := rawptr(uintptr(place) + field_conv.offset_in_b)
			_policy_decode(field_conv.policy, field_target_place, ctx) or_return
		}
	case UnresolvedPolicy:
		panic("unresolved policy")
	}
	return .None
}

_match_decode :: proc(
	ty_idx: u32,
	place: rawptr,
	cursor: Cursor,
	schema: SchemaWithPtrs,
	tracker: ^Tracker,
) -> DecodeError {
	ty := schema.types[ty_idx]
	if ty.is_copy {
		return _read(place, ty.size, cursor)
	}
	switch kind in ty.kind {
	case CopyPrimitiveTy, EnumTy:
		panic("CopyPrimitiveTy and EnumTy should have ty.is_copy and not reach this point")
	case PtrTy:
		is_nil := _read_ptr_marker(cursor) or_return
		elem_ty := schema.types[kind.elem_ty_idx]
		if !is_nil {
			ptr := tracker_alloc(tracker, elem_ty.size, elem_ty.align)
			_match_decode(kind.elem_ty_idx, ptr, cursor, schema, tracker) or_return
			(cast(^rawptr)place)^ = ptr
		}
	case StringTy:
		str := _read_string(cursor) or_return
		raw_str := transmute(Raw_String)str
		if raw_str.len != 0 {
			ptr := tracker_alloc(tracker, raw_str.len, 1)
			mem.copy_non_overlapping(ptr, raw_str.data, raw_str.len)
			if kind.is_cstring {
				(cast(^rawptr)place)^ = ptr
			} else {
				(cast(^Raw_String)place)^ = Raw_String{cast([^]u8)ptr, raw_str.len}
			}
		}
	case ArrayTy:
		// if we get here array is not copy type:
		elem_size := schema.types[kind.elem_ty_idx].size
		for idx in 0 ..< int(kind.count) {
			elem_place := rawptr(uintptr(place) + uintptr(idx * elem_size))
			_match_decode(kind.elem_ty_idx, elem_place, cursor, schema, tracker) or_return
		}
	case SparseEnumArrayTy:
		idx_type, ok := schema.types[kind.index_ty_idx].kind.(EnumTy)
		assert(ok, "index_ty_idx of SparseEnumArrayTy should lead to EnumTy")
		// in sparse case, no need to iterate the empty slots:
		elem_size := schema.types[kind.elem_ty_idx].size
		for val in idx_type.values {
			idx := int(val - kind.min_value)
			assert(idx >= 0 && u32(idx) < kind.count)
			elem_place := rawptr(uintptr(place) + uintptr(idx * elem_size))
			_match_decode(kind.elem_ty_idx, elem_place, cursor, schema, tracker) or_return
		}
	case SeqTy:
		// dynamic arrays or slices
		_read_marker(.SeqStart, cursor) or_return
		n := _read_len(cursor) or_return
		ptr: rawptr = nil
		if n != 0 {
			elem_ty := schema.types[kind.elem_ty_idx]
			ptr = tracker_alloc(tracker, elem_ty.size * n, elem_ty.align)
			if elem_ty.is_copy {
				_read(ptr, n * elem_ty.size, cursor) or_return
			} else {
				for idx in 0 ..< n {
					elem_place := rawptr(uintptr(ptr) + uintptr(idx * elem_ty.size))
					_match_decode(kind.elem_ty_idx, elem_place, cursor, schema, tracker) or_return
				}
			}
		}
		if kind.is_dyn_arr {
			(cast(^Raw_Slice)place)^ = Raw_Slice{ptr, n}
		} else {
			(cast(^Raw_Dynamic_Array)place)^ = Raw_Dynamic_Array {
				data      = ptr,
				len       = n,
				cap       = n,
				allocator = tracker.allocator,
			}
		}
	case StructTy:
		for field in kind.fields {
			field_place := rawptr(uintptr(place) + uintptr(field.offset))
			_match_decode(field.ty_idx, field_place, cursor, schema, tracker) or_return
		}
	case UnionTy:
		// write tag as int first, then write the data for this union variant
		tag := _read_len(cursor) or_return
		if !kind.no_nil && tag == 0 {
			return .None
		}
		tag_place := rawptr(uintptr(place) + uintptr(kind.tag_offset))
		switch kind.tag_size {
		case .U8:
			(cast(^u8)tag_place)^ = u8(tag)
		case .U16:
			(cast(^u16)tag_place)^ = u16(tag)
		case .U32:
			(cast(^u32)tag_place)^ = u32(tag)
		case .U64:
			(cast(^u64)tag_place)^ = u64(tag)
		}
		v_idx := tag if kind.no_nil else tag - 1
		if v_idx < 0 || v_idx >= len(kind.variant_indices) {
			return .InvalidUnionTag
		}
		return _match_decode(kind.variant_indices[v_idx], place, cursor, schema, tracker)
	case MapTy:
		_read_marker(.MapStart, cursor) or_return
		n := _read_len(cursor) or_return
		raw_map: ^Raw_Map = cast(^Raw_Map)place
		raw_map^ = Raw_Map {
			data      = 0,
			len       = 0,
			allocator = tracker.allocator,
		}
		if n == 0 {
			return .None
		}
		type_info_map, is_map_ty := schema.info_ptrs[ty_idx].variant.(runtime.Type_Info_Map)
		assert(is_map_ty, "Schema type MapTy should point to runtime.Type_Info_Map")
		map_info := type_info_map.map_info
		assert(map_info != nil)

		key_ty := schema.types[kind.key_ty_idx]
		val_ty := schema.types[kind.val_ty_idx]

		key_scratch :=
			mem.alloc(key_ty.size, key_ty.align, context.temp_allocator) or_else panic("alloc err")
		val_scratch :=
			mem.alloc(val_ty.size, val_ty.align, context.temp_allocator) or_else panic("alloc err")
		defer {
			mem.free(key_scratch, context.temp_allocator)
			mem.free(val_scratch, context.temp_allocator)
			// such that the map gets deallocated if any error occurs later:
			tracker_add(tracker, rawptr(raw_map.data), runtime.map_cap(raw_map^))
		}
		for _ in 0 ..< n {
			_read_marker(.MapKeyStart, cursor) or_return
			_match_decode(kind.key_ty_idx, key_scratch, cursor, schema, tracker) or_return
			_read_marker(.MapValStart, cursor) or_return
			_match_decode(kind.val_ty_idx, val_scratch, cursor, schema, tracker) or_return

			// calculate hash and insert key and value into map
			hash := map_info.key_hasher(key_scratch, runtime.map_seed(raw_map^))
			found := runtime.__dynamic_map_get(raw_map, map_info, hash, key_scratch)
			if found != nil {
				return .DoubleKeyInMap
			}
			runtime.__dynamic_map_set(raw_map, map_info, hash, key_scratch, val_scratch)
		}
	}
	return .None
}

_skip_decode :: proc(ty_idx: u32, cursor: Cursor, schema: Schema) -> DecodeError {
	// assert(ty_idx < u32(len(schema)))
	ty := schema[ty_idx]
	if ty.is_copy {
		return _skip_over(ty.size, cursor)
	}
	switch kind in ty.kind {
	case CopyPrimitiveTy, EnumTy:
		panic("CopyPrimitiveTy and EnumTy should have ty.is_copy and not reach this point")
	case PtrTy:
		is_nil := _read_ptr_marker(cursor) or_return
		if !is_nil {
			return _skip_decode(kind.elem_ty_idx, cursor, schema)
		}
	case StringTy:
		_ = _read_string(cursor) or_return
	case ArrayTy:
		// if we get here array is not copy type:
		for _ in 0 ..< int(kind.count) {
			_skip_decode(kind.elem_ty_idx, cursor, schema) or_return
		}
	case SparseEnumArrayTy:
		idx_type, ok := schema[kind.index_ty_idx].kind.(EnumTy)
		assert(ok, "index_ty_idx of SparseEnumArrayTy should lead to EnumTy")
		// decode as many times as there are non-empty slots
		for _ in 0 ..< len(idx_type.values) {
			_skip_decode(kind.elem_ty_idx, cursor, schema) or_return
		}
	case SeqTy:
		_read_marker(.SeqStart, cursor) or_return
		seq_len := _read_len(cursor) or_return
		if seq_len == 0 {
			return .None
		}
		elem_ty := schema[kind.elem_ty_idx]
		if elem_ty.is_copy {
			_skip_over(seq_len * elem_ty.size, cursor) or_return
		} else {
			for idx in 0 ..< seq_len {
				_skip_decode(kind.elem_ty_idx, cursor, schema) or_return
			}
		}
	case StructTy:
		for field in kind.fields {
			_skip_decode(field.ty_idx, cursor, schema) or_return
		}
	case UnionTy:
		tag: int = _read_len(cursor) or_return
		if !kind.no_nil && tag == 0 {
			return .None
		}
		v_ty_idx := kind.variant_indices[tag] if kind.no_nil else kind.variant_indices[tag - 1]
		return _skip_decode(v_ty_idx, cursor, schema)
	case MapTy:
		_read_marker(.MapStart, cursor) or_return
		map_len := _read_len(cursor) or_return
		for _ in 0 ..< map_len {
			_read_marker(.MapKeyStart, cursor) or_return
			_skip_decode(kind.key_ty_idx, cursor, schema) or_return
			_read_marker(.MapValStart, cursor) or_return
			_skip_decode(kind.val_ty_idx, cursor, schema) or_return
		}
	}
	return .None
}
