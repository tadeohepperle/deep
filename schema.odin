package deep

import "base:intrinsics"
import "base:runtime"
import "core:hash"
import "core:mem"

// SCHEMAFUL ENCODING, CURRENTLY WIP!

// encode_schema_to :: proc(schema: Schema, buf: ^[dynamic]u8) {
// 	_write_len(len(schema), buf)
// }

Schema :: []Type
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
	SimplePrimitiveTy,
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
SimplePrimitiveTy :: enum u8 {
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
	elem_ty_idx: u32,
	index_ty:    u32,
}
SeqTy :: struct {
	elem_ty_idx: u32,
	is_dyn_arr:  bool,
}
StringTy :: struct {
	is_cstring: bool,
}
PtrTy :: struct {
	elem_ty: u32,
}
MapTy :: struct {
	key_ty_idx: u32,
	val_ty_idx: u32,
}
StructTy :: struct {
	fields: []StructField,
}
StructField :: struct {
	name:   string,
	ty_idx: u32,
	offset: uintptr,
}
UnionTy :: struct {
	var_indices: []u32, // ty_idx for each variant
	tag_offset:  uintptr,
	tag_size:    u8,
	no_nil:      bool,
}

type_to_schema :: proc(
	ty: Type_Info,
	allocator := context.temp_allocator,
) -> (
	schema: Schema,
	err: Error,
) {
	reg := _registry_builder(ty, allocator)
	defer if err != nil {
		tracker_free_all(&reg.tracker)
		delete(reg.types)
	}
	root_base_ty := type_info_base(ty)
	info := _registry_add(&reg, root_base_ty) or_return
	assert(info.idx == 0)
	shrink(&reg.types, len(reg.types))
	return reg.types[:], nil
}
Error :: Maybe(string)

RegistryBuilder :: struct {
	tracker:     Tracker,
	types:       [dynamic]Type, // in the allocator
	type_to_idx: map[Type_Info]u32, // in tmp
	sorted:      []Type_Info, // in tmp
	cycled:      map[Type_Info]None, // in tmp
	scratch:     [dynamic]u8,
}
_registry_builder :: proc(root: Type_Info, allocator: Allocator) -> RegistryBuilder {
	return RegistryBuilder {
		tracker = tracker_create(allocator),
		types = make([dynamic]Type, allocator),
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
	ty := type_info_base(ty)
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

	scratch := &reg.scratch
	h: u64 = HASH_SEED // the hash
	hash_data(&h, ty.size)
	kind: TypeKind
	is_non_copy: bool = false
	switch var in ty.variant {
	case runtime.Type_Info_Named:
		panic("call _ty_to_reg_ty only on base types")
	case runtime.Type_Info_Boolean:
		kind = SimplePrimitiveTy.Bool
	case runtime.Type_Info_Integer:
		if var.signed {
			kind = SimplePrimitiveTy.IntSigned
		} else {
			kind = SimplePrimitiveTy.Int
		}
	case runtime.Type_Info_Rune:
		kind = SimplePrimitiveTy.Rune
	case runtime.Type_Info_Float:
		kind = SimplePrimitiveTy.Float
	case runtime.Type_Info_Complex:
		kind = SimplePrimitiveTy.Complex
	case runtime.Type_Info_Bit_Set:
		kind = SimplePrimitiveTy.BitSet
	case runtime.Type_Info_Matrix:
		kind = SimplePrimitiveTy.Matrix
	case runtime.Type_Info_Simd_Vector:
		kind = SimplePrimitiveTy.SimdVector
	case runtime.Type_Info_Quaternion:
		kind = SimplePrimitiveTy.Quaternion
	case runtime.Type_Info_Bit_Field:
		kind = SimplePrimitiveTy.BitField
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
			kind = SparseEnumArrayTy{elem.idx, enu.idx}
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
				offset = var.offsets[f_idx],
			}
		}
		kind = StructTy{fields}
	case runtime.Type_Info_Union:
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
		tag_size := var.tag_type.size
		assert(tag_size > 0 && tag_size <= 64)
		union_ty := UnionTy {
			tag_offset = var.tag_offset,
			tag_size   = u8(tag_size),
			no_nil     = var.no_nil,
		}
		hash_data(&h, union_ty)
		union_ty.var_indices = variants // ty_idx for each variant, put in after hash here
		kind = union_ty
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
	kind_tag := _get_union_tag_for_non_ptr_union(type_kind_union_ty, &kind)
	hash_data(&h, kind_tag)
	if simp, ok := kind.(SimplePrimitiveTy); ok {
		hash_data(&h, simp)
	}

	type := Type {
		size    = ty.size,
		align   = ty.size,
		is_copy = !is_non_copy,
		hash    = h,
		kind    = kind,
	}
	reg.types[idx] = type
	return RegInfo{idx, type.is_copy, type.hash}, nil
}
type_kind_union_ty := type_info_base(type_info_of(TypeKind)).variant.(runtime.Type_Info_Union)


None :: struct {}
