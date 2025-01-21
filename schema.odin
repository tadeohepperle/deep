package deep

import "base:intrinsics"
import "base:runtime"
import "core:hash"
import "core:mem"

// A Schema can be built from a type. If this is done, 
// the original type info ptrs are stored alongside the `Type` structs.
// If a schema is decoded from bytes, the `info` fields of TypeWithTypeInfo
// will all be nil and it cannot be used to 
SchemaWithPtrs :: struct {
	types:     Schema,
	info_ptrs: []Type_Info,
}
Schema :: []Type // schema with nil ptrs for info fields

encode_schema_to :: proc(schema: Schema, buf: ^[dynamic]u8) {
	buf_intial_len := len(buf)
	_write_nothing(8, buf) // leave 2*4 bytes of space for n_types and n_bytes
	for &type in schema {
		encode_type_to(&type, buf)
	}
	n_types := u32(len(schema))
	n_bytes := u32(len(buf) - buf_intial_len - 8)
	_write_at(buf_intial_len, &[2]u32{n_types, n_bytes}, 8, buf)
}
encode_type_to :: proc(type: ^Type, buf: ^[dynamic]u8) {
	_write_marker(.TypeStart, buf)
	_write(&type.header, size_of(TypeHeader), buf)
	tag := type_kind_as_tag(type.kind)
	_write(&tag, size_of(TypeKindAsTag), buf)
	switch &kind in type.kind {
	case CopyPrimitiveTy:
		_write_data(&kind, buf)
	case EnumTy:
		n_values := len(kind.values)
		_write_len(n_values, buf)
		_write(raw_data(kind.values), size_of(i64) * n_values, buf)
	case PtrTy:
		_write_data(&kind, buf)
	case StringTy:
		_write_data(&kind, buf)
	case ArrayTy:
		_write_data(&kind, buf)
	case SparseEnumArrayTy:
		_write_data(&kind, buf)
	case SeqTy:
		_write_data(&kind, buf)
	case StructTy:
		_write_len(len(kind.fields), buf)
		for &field in kind.fields {
			encode_struct_field_to(&field, buf)
		}
	case UnionTy:
		_write_data(&kind.header, buf)
		_write_len(len(kind.variant_indices), buf)
		_write(raw_data(kind.variant_indices), size_of(u32) * len(kind.variant_indices), buf)
	case MapTy:
		_write(&kind, size_of(MapTy), buf)
	}
}
encode_struct_field_to :: proc(field: ^StructField, buf: ^[dynamic]u8) {
	_write_data(&[2]u32{field.ty_idx, field.offset}, buf)
	_write_string(field.name, buf)
}

decode_schema :: proc(
	cursor: Cursor,
	allocator := context.temp_allocator,
) -> (
	schema: Schema,
	err: DecodeError,
) {
	tracker := tracker_create(allocator)
	defer if err != .None {
		schema = nil
		tracker_free_all(&tracker)
	}

	n_types := _read_data(u32, cursor) or_return
	n_bytes := _read_data(u32, cursor) or_return
	if n_types == 0 {
		return {}, .SchemaIsEmpty
	}
	max_ty_idx := n_types - 1
	cursor_len_before_types := len(cursor^)

	schema = make([]Type, int(n_types), allocator)
	tracker_add(&tracker, raw_data(schema), size_of(Type) * int(n_types))
	for i in 0 ..< n_types {
		schema[i] = decode_type(cursor, &tracker, max_ty_idx) or_return
	}

	if cursor_len_before_types + int(n_bytes) == len(cursor^) {
		return {}, .InvalidNumberOfSchemaBytes
	}
	return schema, .None
}

decode_type :: proc(
	cursor: Cursor,
	tracker: ^Tracker,
	max_ty_idx: u32,
) -> (
	type: Type,
	err: DecodeError,
) {
	_read_marker(.TypeStart, cursor) or_return
	type.header = _read_data(TypeHeader, cursor) or_return
	validate_bool(type.header.is_copy) or_return
	validate_positive(type.size)
	validate_positive(type.align)
	tag := _read_data(TypeKindAsTag, cursor) or_return
	if tag > max(TypeKindAsTag) {
		return {}, .InvalidSchemaTypeTag
	}
	switch tag {
	case .CopyPrimitiveTy:
		copy_ty := _read_data(CopyPrimitiveTy, cursor) or_return
		if copy_ty > max(CopyPrimitiveTy) {
			return {}, .InvalidCopyPrimitiveTag
		}
		type.kind = copy_ty
	case .EnumTy:
		n_values := _read_len(cursor) or_return
		n_bytes := size_of(i64) * n_values
		raw_vals: Raw_Slice
		raw_vals.len = n_values
		raw_vals.data = tracker_alloc(tracker, n_bytes, size_of(i64))
		_read(raw_vals.data, n_bytes, cursor) or_return
		type.kind = EnumTy {
			values = transmute([]i64)raw_vals,
		}
	case .PtrTy:
		ptr_ty := _read_data(PtrTy, cursor) or_return
		validate_ty_idx(ptr_ty.elem_ty_idx, max_ty_idx) or_return
		type.kind = ptr_ty
	case .StringTy:
		bool_byte := _read_data(u8, cursor) or_return
		if bool_byte != 0 && bool_byte != 1 {
			return {}, .BoolByteNotZeroOrOne
		}
		type.kind = StringTy{transmute(bool)bool_byte}
	case .ArrayTy:
		arr_ty := _read_data(ArrayTy, cursor) or_return
		validate_ty_idx(arr_ty.elem_ty_idx, max_ty_idx) or_return
		type.kind = arr_ty
	case .SparseEnumArrayTy:
		sparse_arr := _read_data(SparseEnumArrayTy, cursor) or_return
		validate_ty_idx(sparse_arr.elem_ty_idx, max_ty_idx) or_return
		validate_ty_idx(sparse_arr.index_ty_idx, max_ty_idx) or_return
		type.kind = sparse_arr
	case .SeqTy:
		seq_ty := _read_data(SeqTy, cursor) or_return
		validate_ty_idx(seq_ty.elem_ty_idx, max_ty_idx) or_return
		validate_bool(seq_ty.is_dyn_arr)
		type.kind = seq_ty
	case .StructTy:
		n_fields := _read_len(cursor) or_return
		fields: []StructField = nil
		if n_fields != 0 {
			fields = make([]StructField, n_fields, tracker.allocator)
			tracker_add(tracker, raw_data(fields), size_of(StructField) * n_fields)
			for i in 0 ..< n_fields {
				field := decode_struct_field(cursor, tracker, max_ty_idx) or_return
				if field.offset > u32(type.size) {
					return {}, .FieldOffsetGreaterThanStructSize
				}
				fields[i] = field
			}
		}
		type.kind = StructTy{fields}
	case .UnionTy:
		u_header := _read_data(UnionTyHeader, cursor) or_return
		if u_header.tag_offset > u32(type.size) {
			return {}, .UnionTagOffsetGreaterThanSize
		}
		if u_header.tag_size > max(UnionTagSize) {
			return {}, .InvalidUnionTagSize
		}
		validate_bool(u_header.no_nil) or_return
		n_variants := _read_len(cursor) or_return
		if n_variants == 0 {
			return {}, .UnionWithZeroVariants
		}
		variant_ty_indices := make([]u32, n_variants, tracker.allocator)
		n_variant_bytes := size_of(u32) * n_variants
		tracker_add(tracker, raw_data(variant_ty_indices), n_variant_bytes)
		_read(raw_data(variant_ty_indices), n_variant_bytes, cursor) or_return
		for v_ty_idx in variant_ty_indices {
			validate_ty_idx(v_ty_idx, max_ty_idx) or_return
		}
		type.kind = UnionTy{u_header, variant_ty_indices}
	case .MapTy:
		map_ty := _read_data(MapTy, cursor) or_return
		validate_ty_idx(map_ty.key_ty_idx, max_ty_idx) or_return
		validate_ty_idx(map_ty.val_ty_idx, max_ty_idx) or_return
		type.kind = map_ty
	case:
		return {}, .InvalidSchemaTypeTag
	}
	return type, .None
}

validate_bool :: #force_inline proc(b: bool) -> DecodeError {
	u := transmute(u8)b
	if u != 1 && u != 0 {
		return .BoolByteNotZeroOrOne
	}
	return .None
}
validate_ty_idx :: #force_inline proc(ty_idx: u32, max_ty_idx: u32) -> DecodeError {
	if ty_idx > max_ty_idx {
		return .TypeIdxTooHigh
	}
	return .None
}
validate_positive :: #force_inline proc(len: int) -> DecodeError {
	if len < 0 {
		return .NegativeLength
	}
	return .None
}

decode_struct_field :: proc(
	cursor: Cursor,
	tracker: ^Tracker,
	max_ty_idx: u32,
) -> (
	field: StructField,
	err: DecodeError,
) {
	field.ty_idx = _read_data(u32, cursor) or_return
	field.offset = _read_data(u32, cursor) or_return
	validate_ty_idx(field.ty_idx, max_ty_idx) or_return
	name := _read_string(cursor) or_return
	field.name = tracker_clone_string(tracker, name)
	return field, .None
}

TypeHeader :: struct {
	size:    int,
	align:   int,
	is_copy: bool,
	hash:    u64,
}
Type :: struct {
	using header: TypeHeader,
	kind:         TypeKind,
}
TypeKind :: union #no_nil {
	// nil = unsupported type
	CopyPrimitiveTy,
	EnumTy,
	PtrTy,
	StringTy,
	ArrayTy,
	SparseEnumArrayTy,
	SeqTy,
	StructTy,
	UnionTy,
	MapTy,
}
type_kind_as_tag :: proc(type_kind: TypeKind) -> TypeKindAsTag {
	switch kind in type_kind {
	case CopyPrimitiveTy:
		return .CopyPrimitiveTy
	case EnumTy:
		return .EnumTy
	case PtrTy:
		return .PtrTy
	case StringTy:
		return .StringTy
	case ArrayTy:
		return .ArrayTy
	case SparseEnumArrayTy:
		return .SparseEnumArrayTy
	case SeqTy:
		return .SeqTy
	case StructTy:
		return .StructTy
	case UnionTy:
		return .UnionTy
	case MapTy:
		return .MapTy
	}
	panic("all union cases covered")
}
TypeKindAsTag :: enum u8 {
	CopyPrimitiveTy,
	EnumTy,
	PtrTy,
	StringTy,
	ArrayTy,
	SparseEnumArrayTy,
	SeqTy,
	StructTy,
	UnionTy,
	MapTy,
}
CopyPrimitiveTy :: enum u8 {
	Bool,
	Int,
	IntSigned,
	Float,
	BitSet, // todo: actually we could look at the names and do name remapping to new bits as well, but this is really tedious.
	Rune,
	Complex,
	Matrix,
	SimdVector,
	Quaternion,
	BitField,
}
// also used for enum arrays because mem layout is the same.
EnumTy :: struct {
	values: []i64,
}
ArrayTy :: struct {
	elem_ty_idx: u32,
	count:       u32,
}
SparseEnumArrayTy :: struct {
	elem_ty_idx:  u32,
	index_ty_idx: u32,
	min_value:    i64,
	count:        u32,
}
SeqTy :: struct {
	elem_ty_idx: u32,
	is_dyn_arr:  bool,
}
StringTy :: struct {
	is_cstring: bool,
}
PtrTy :: struct {
	elem_ty_idx: u32,
}
MapTy :: struct {
	key_ty_idx: u32,
	val_ty_idx: u32,
}
StructTy :: struct {
	fields: []StructField,
}
StructField :: struct {
	ty_idx: u32,
	offset: u32,
	name:   string,
}
UnionTyHeader :: struct {
	tag_offset: u32,
	tag_size:   UnionTagSize,
	no_nil:     bool,
}
UnionTagSize :: enum u8 {
	U8,
	U16,
	U32,
	U64,
}
UnionTy :: struct {
	using header:    UnionTyHeader,
	variant_indices: []u32, // ty_idx for each variant
}

schema_of_type :: proc(
	ty: Type_Info,
	allocator := context.temp_allocator,
) -> (
	schema: SchemaWithPtrs,
	err: Error,
) {
	reg := _registry_builder(ty, allocator)
	defer if err != nil {
		tracker_free_all(&reg.tracker)
		delete(reg.types)
		delete(reg.type_info_ptrs)
	}
	root_base_ty := type_info_base_union_opt(ty)
	info := _registry_add(&reg, root_base_ty) or_return
	assert(info.idx == 0)

	shrink(&reg.types)
	shrink(&reg.type_info_ptrs)
	schema.types = reg.types[:]
	schema.info_ptrs = reg.type_info_ptrs[:]
	return schema, nil
}

RegistryBuilder :: struct {
	tracker:        Tracker,
	types:          [dynamic]Type, // in the allocator
	type_info_ptrs: [dynamic]Type_Info, // in the allocator, same size as types
	type_to_idx:    map[Type_Info]u32, // in tmp
	scratch:        [dynamic]u8,
}
_registry_builder :: proc(root: Type_Info, allocator: Allocator) -> RegistryBuilder {
	return RegistryBuilder {
		tracker = tracker_create(allocator),
		types = make([dynamic]Type, allocator),
		type_info_ptrs = make([dynamic]Type_Info, allocator),
		type_to_idx = make(map[Type_Info]u32, context.temp_allocator),
		scratch = make([dynamic]u8, context.temp_allocator),
	}
}


RegInfo :: struct {
	idx:     u32,
	is_copy: bool,
	hash:    u64,
}

_registry_add_or_get :: proc(reg: ^RegistryBuilder, ty: Type_Info) -> (info: RegInfo, err: Error) {
	ty := type_info_base_union_opt(ty)
	if idx, ok := reg.type_to_idx[ty]; ok {
		return RegInfo{idx, reg.types[idx].is_copy, reg.types[idx].hash}, nil
	} else {
		return _registry_add(reg, ty)
	}
}
// call this function only on base types!

HASH_SEED :: u64(0xcbf29ce484222325)
_registry_add :: proc(reg: ^RegistryBuilder, ty: Type_Info) -> (info: RegInfo, err: Error) {
	idx := u32(len(reg.types))
	reg.type_to_idx[ty] = idx
	append_nothing(&reg.types)
	append_nothing(&reg.type_info_ptrs)

	scratch := &reg.scratch
	h: u64 = HASH_SEED // the hash
	hash_data(&h, ty.size)
	kind: TypeKind
	is_non_copy: bool = false
	switch var in ty.variant {
	case runtime.Type_Info_Named:
		panic("call _ty_to_reg_ty only on base types")
	case runtime.Type_Info_Boolean:
		kind = CopyPrimitiveTy.Bool
	case runtime.Type_Info_Integer:
		if var.signed {
			kind = CopyPrimitiveTy.IntSigned
		} else {
			kind = CopyPrimitiveTy.Int
		}
	case runtime.Type_Info_Rune:
		kind = CopyPrimitiveTy.Rune
	case runtime.Type_Info_Float:
		kind = CopyPrimitiveTy.Float
	case runtime.Type_Info_Complex:
		kind = CopyPrimitiveTy.Complex
	case runtime.Type_Info_Bit_Set:
		kind = CopyPrimitiveTy.BitSet
	case runtime.Type_Info_Matrix:
		kind = CopyPrimitiveTy.Matrix
	case runtime.Type_Info_Simd_Vector:
		kind = CopyPrimitiveTy.SimdVector
	case runtime.Type_Info_Quaternion:
		kind = CopyPrimitiveTy.Quaternion
	case runtime.Type_Info_Bit_Field:
		kind = CopyPrimitiveTy.BitField
	case runtime.Type_Info_Enum:
		values := make([]i64, len(var.values), reg.tracker.allocator)
		for v, idx in var.values {
			values[idx] = i64(v)
		}
		tracker_add(&reg.tracker, raw_data(values), len(var.values) * size_of(i64))
		kind = EnumTy{values}
		hash_slice(&h, values)
	case runtime.Type_Info_String:
		is_non_copy = true
		kind = StringTy {
			is_cstring = var.is_cstring,
		}
		hash_data(&h, var.is_cstring)
	case runtime.Type_Info_Pointer:
		is_non_copy = true
		elem := _registry_add_or_get(reg, var.elem) or_return
		kind = PtrTy{elem.idx}
		hash_data(&h, elem.hash)
	case runtime.Type_Info_Array:
		elem := _registry_add_or_get(reg, var.elem) or_return
		is_non_copy = !elem.is_copy
		assert(var.count <= int(max(u32)))
		kind = ArrayTy{elem.idx, u32(var.count)}
		hash_data(&h, elem.hash)
		hash_data(&h, var.count)
	case runtime.Type_Info_Enumerated_Array:
		elem := _registry_add_or_get(reg, var.elem) or_return
		is_non_copy = !elem.is_copy
		hash_data(&h, elem.hash)
		hash_data(&h, var.count)
		hash_data(&h, var.is_sparse)
		if var.is_sparse {
			enu := _registry_add_or_get(reg, var.index) or_return
			hash_data(&h, enu.hash)
			enu_ty, ok := reg.types[enu.idx].kind.(EnumTy)
			if !ok {
				return {}, "type at enum_ty_idx is not enum type"
			}
			assert(var.count <= int(max(u32)))
			kind = SparseEnumArrayTy{elem.idx, enu.idx, i64(var.min_value), u32(var.count)}
		} else {
			assert(var.count <= int(max(u32)))
			kind = ArrayTy{elem.idx, u32(var.count)}
		}
	case runtime.Type_Info_Dynamic_Array:
		is_non_copy = true
		elem := _registry_add_or_get(reg, var.elem) or_return
		hash_data(&h, elem.hash)
		hash_data(&h, true)
		kind = SeqTy {
			elem_ty_idx = elem.idx,
			is_dyn_arr  = true,
		}
	case runtime.Type_Info_Slice:
		is_non_copy = true
		elem := _registry_add_or_get(reg, var.elem) or_return
		hash_data(&h, elem.hash)
		hash_data(&h, false)
		kind = SeqTy {
			elem_ty_idx = elem.idx,
			is_dyn_arr  = false,
		}
	case runtime.Type_Info_Struct:
		fields := make([]StructField, int(var.field_count), reg.tracker.allocator)
		tracker_add(&reg.tracker, raw_data(fields), len(fields) * size_of(StructField))
		for f_idx in 0 ..< var.field_count {
			f_ty := _registry_add_or_get(reg, var.types[f_idx]) or_return
			if !f_ty.is_copy {
				is_non_copy = true
			}
			field_name := tracker_clone_string(&reg.tracker, var.names[f_idx])
			hash_string(&h, field_name)
			hash_data(&h, f_ty.hash)
			fields[f_idx] = StructField {
				name   = field_name,
				ty_idx = f_ty.idx,
				offset = u32(var.offsets[f_idx]),
			}
		}
		kind = StructTy{fields}
	case runtime.Type_Info_Union:
		// no need to check for niche optimized single ptr unions e.g. union{^T}, because they are already
		// converted to ^T when added to the type registry building the schema, see type_info_base_union_opt

		variants := make([]u32, len(var.variants), reg.tracker.allocator)
		tracker_add(&reg.tracker, raw_data(variants), len(variants) * size_of(u32))
		for v, i in var.variants {
			var_ty := _registry_add_or_get(reg, v) or_return
			hash_data(&h, var_ty.hash)
			if !var_ty.is_copy {
				is_non_copy = true
			}
			variants[i] = var_ty.idx
		}

		tag_size: UnionTagSize
		switch var.tag_type.size {
		case 1:
			tag_size = .U8
		case 2:
			tag_size = .U16
		case 4:
			tag_size = .U32
		case 8:
			tag_size = .U64
		case:
			panic(tprint("invalid union tag size: ", var.tag_type.size))
		}
		u_header := UnionTyHeader {
			tag_offset = u32(var.tag_offset),
			tag_size   = tag_size,
			no_nil     = var.no_nil,
		}
		hash_data(&h, u_header)
		kind = UnionTy{u_header, variants}
	case runtime.Type_Info_Map:
		is_non_copy = true
		key_ty := _registry_add_or_get(reg, var.key) or_return
		val_ty := _registry_add_or_get(reg, var.value) or_return
		hash_data(&h, key_ty.hash)
		hash_data(&h, val_ty.hash)
		kind = MapTy {
			key_ty_idx = key_ty.idx,
			val_ty_idx = val_ty.idx,
		}
	case runtime.Type_Info_Procedure,
	     runtime.Type_Info_Parameters,
	     runtime.Type_Info_Soa_Pointer,
	     runtime.Type_Info_Type_Id,
	     runtime.Type_Info_Any,
	     runtime.Type_Info_Multi_Pointer:
		return {}, tprint("unsupported type:", ty, "for type registry")
	}
	kind_tag := type_kind_as_tag(kind)
	hash_data(&h, kind_tag)
	if prim, ok := kind.(CopyPrimitiveTy); ok {
		hash_data(&h, prim)
	}

	type := Type {
		size    = ty.size,
		align   = ty.size,
		is_copy = !is_non_copy,
		hash    = h,
		kind    = kind,
	}
	reg.types[idx] = type
	reg.type_info_ptrs[idx] = ty
	return RegInfo{idx, type.is_copy, type.hash}, nil
}
