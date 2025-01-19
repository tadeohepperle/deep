package deep

import "base:intrinsics"
import "base:runtime"
import "core:hash"
import "core:mem"

Schema :: struct {
	registry:      []Type,
	root_type_idx: u32,
}
TypeHeader :: struct {
	size:   int,
	align:  int,
	simple: bool,
	hash:   u64,
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
	elem_ty: u32,
	count:   u32,
}
SparseEnumArrayTy :: struct {
	elem_ty:  u32,
	index_ty: u32,
}
SeqTy :: struct {
	elem_ty:    u32,
	is_dyn_arr: bool,
}
StringTy :: struct {
	is_cstring: bool,
}
PtrTy :: struct {
	elem_ty: u32,
}
MapTy :: struct {
	key_ty: u32,
	val_ty: u32,
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
	variants:   []u32, // ty_idx for each variant
	tag_offset: uintptr,
	tag_size:   u8,
	no_nil:     bool,
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
	print(reg.sorted)
	_registry_build(&reg) or_return
	root_ty_base := type_info_base(ty)
	print("root_ty_base", root_ty_base)
	root_type_idx, ok := reg.type_to_idx[type_info_base(ty)]
	if !ok {
		return {}, "root type not in registry"
	}
	assert(int(root_type_idx) < len(reg.types))
	shrink(&reg.types, len(reg.types))
	return Schema{reg.types[:], root_type_idx}, nil
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
	sorted, cycled := _topo_sort_types(root)
	return RegistryBuilder {
		tracker = tracker_create(allocator),
		types = make([dynamic]Type, allocator),
		type_to_idx = make(map[Type_Info]u32, context.temp_allocator),
		sorted = sorted,
		cycled = cycled,
		scratch = make([dynamic]u8, context.temp_allocator),
	}
}
_registry_build :: proc(reg: ^RegistryBuilder) -> (err: Error) {
	for ty in reg.sorted {
		if type_info_base(ty) != ty do continue
		print("add type", ty)
		reg_ty := _ty_to_reg_ty(reg, ty) or_return
		ty_idx := len(reg.types)
		reg.type_to_idx[ty] = u32(ty_idx)

		print("reg_ty", ty, u32(ty_idx))
		append(&reg.types, reg_ty)
		print(ty_idx, reg_ty)
	}
	for ty in reg.cycled {

	}
	return nil
}

_get_ty_idx :: proc(reg: ^RegistryBuilder, ty: Type_Info) -> (u32, Error) {
	idx, ok := reg.type_to_idx[ty]
	if !ok {
		return 0, tprint("idx for type", ty, "not found")
	}
	return idx, nil
}
// call this function on the topological order of types and only on base types
_ty_to_reg_ty :: proc(reg: ^RegistryBuilder, ty: Type_Info) -> (reg_ty: Type, err: Error) {

	is_simple := true
	kind: TypeKind
	switch var in ty.variant {
	case runtime.Type_Info_Named:
		panic("call _ty_to_reg_ty only on base types")
	case runtime.Type_Info_Boolean:
		kind = SimplePrimitiveTy.Bool
	case runtime.Type_Info_Integer:
		kind = SimplePrimitiveTy.Int
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
	case runtime.Type_Info_String:
		is_simple = false
		kind = StringTy {
			is_cstring = var.is_cstring,
		}
	case runtime.Type_Info_Pointer:
		is_simple = false
		elem_ty := _get_ty_idx(reg, type_info_base(var.elem)) or_return
		kind = PtrTy{elem_ty}
	case runtime.Type_Info_Array:
		elem_ty := _get_ty_idx(reg, type_info_base(var.elem)) or_return
		is_simple = reg.types[elem_ty].simple

		assert(var.count <= int(max(u32)))
		kind = ArrayTy{elem_ty, u32(var.count)}
	case runtime.Type_Info_Enumerated_Array:
		elem_ty := _get_ty_idx(reg, type_info_base(var.elem)) or_return
		is_simple = reg.types[elem_ty].simple

		if var.is_sparse {
			enum_ty := _get_ty_idx(reg, var.index) or_return
			enum_ty_val, ok := reg.types[enum_ty].kind.(EnumTy)
			if !ok {
				return {}, "type at enum_ty_idx is not enum type"
			}
			kind = ArrayTy{elem_ty, enum_ty}
		} else {
			assert(var.count <= int(max(u32)))
			kind = ArrayTy{elem_ty, u32(var.count)}
		}
	case runtime.Type_Info_Dynamic_Array:
		is_simple = false
		elem_ty := _get_ty_idx(reg, type_info_base(var.elem)) or_return
		kind = SeqTy {
			elem_ty    = elem_ty,
			is_dyn_arr = true,
		}
	case runtime.Type_Info_Slice:
		is_simple = false
		elem_ty := _get_ty_idx(reg, type_info_base(var.elem)) or_return
		kind = SeqTy {
			elem_ty    = elem_ty,
			is_dyn_arr = false,
		}
	case runtime.Type_Info_Struct:
		fields := make([]StructField, int(var.field_count), reg.tracker.allocator)
		tracker_add(&reg.tracker, raw_data(fields), len(fields) * size_of(StructField))
		for f_idx in 0 ..< var.field_count {
			f_ty := type_info_base(var.types[f_idx])
			field_ty_idx := _get_ty_idx(reg, f_ty) or_return
			if is_simple && !reg.types[field_ty_idx].simple {
				is_simple = false
			}
			field_name := tracker_clone_string(&reg.tracker, var.names[f_idx])
			fields[f_idx] = StructField {
				name   = field_name,
				ty_idx = field_ty_idx,
				offset = var.offsets[f_idx],
			}
		}
		kind = StructTy{fields}
	case runtime.Type_Info_Union:
		variants := make([]u32, len(var.variants), reg.tracker.allocator)
		tracker_add(&reg.tracker, raw_data(variants), len(variants) * size_of(u32))
		for var_ty, i in var.variants {
			var_ty_idx := _get_ty_idx(reg, type_info_base(var_ty)) or_return
			if is_simple && !reg.types[var_ty_idx].simple {
				is_simple = false
			}
			variants[i] = var_ty_idx
		}
		tag_size := var.tag_type.size
		assert(tag_size > 0 && tag_size <= 64)
		kind = UnionTy {
			variants   = variants, // ty_idx for each variant
			tag_offset = var.tag_offset,
			tag_size   = u8(tag_size),
			no_nil     = var.no_nil,
		}
	case runtime.Type_Info_Map:
		is_simple = false
		key_ty := _get_ty_idx(reg, type_info_base(var.key)) or_return
		val_ty := _get_ty_idx(reg, type_info_base(var.value)) or_return
		kind = MapTy{key_ty, val_ty}
	case runtime.Type_Info_Procedure,
	     runtime.Type_Info_Parameters,
	     runtime.Type_Info_Soa_Pointer,
	     runtime.Type_Info_Type_Id,
	     runtime.Type_Info_Any,
	     runtime.Type_Info_Multi_Pointer:
		return {}, tprint("unsupported type:", ty, "for type registry")
	}
	reg_ty = Type {
		size   = ty.size,
		align  = ty.align,
		hash   = 0,
		simple = is_simple,
		kind   = kind,
	}
	return reg_ty, nil
}
_topo_sort_types :: proc(root: Type_Info) -> (ordered: []Type_Info, cycled: map[Type_Info]None) {
	ctx: TopoCtx = tmp_cycle_ctx(root)
	// add all dependencies, then toposort:
	for len(ctx.stack) > 0 {
		ty := pop(&ctx.stack)
		#partial switch var in ty.variant {
		case runtime.Type_Info_Named:
			add_dep(&ctx, ty, var.base)
		case runtime.Type_Info_Pointer:
			add_dep(&ctx, ty, var.elem)
		case runtime.Type_Info_Multi_Pointer:
			add_dep(&ctx, ty, var.elem)
		case runtime.Type_Info_Array:
			add_dep(&ctx, ty, var.elem)
		case runtime.Type_Info_Enumerated_Array:
			add_dep(&ctx, ty, var.elem)
			add_dep(&ctx, ty, var.index)
		case runtime.Type_Info_Dynamic_Array:
			add_dep(&ctx, ty, var.elem)
		case runtime.Type_Info_Slice:
			add_dep(&ctx, ty, var.elem)
		case runtime.Type_Info_Struct:
			for f_idx in 0 ..< var.field_count {
				add_dep(&ctx, ty, var.types[f_idx])
			}
		case runtime.Type_Info_Union:
			for var_ty in var.variants {
				add_dep(&ctx, ty, var_ty)
			}
		case runtime.Type_Info_Map:
			add_dep(&ctx, ty, var.key)
			add_dep(&ctx, ty, var.value)
		}
	}
	// toposort (using ctx.stack for sorted):
	sorted := ctx.stack
	for ty, dep in ctx.deps {
		if dep.n_children == 0 {
			append(&sorted, ty)
		}
	}
	for i := 0; i < len(sorted); i += 1 {
		root_deps := ctx.deps[sorted[i]]
		for parent in root_deps.parents {
			parent_dep := &ctx.deps[parent]
			parent_dep.n_children -= 1
			if parent_dep.n_children == 0 {
				append(&sorted, parent)
			}
		}
	}

	cycled = make(map[Type_Info]None, context.temp_allocator)
	for root, deps in ctx.deps {
		if deps.n_children != 0 {
			cycled[root] = None{}
		}
	}
	return sorted[:], cycled

	TopoCtx :: struct {
		visited: map[Type_Info]None,
		deps:    map[Type_Info]Dep,
		stack:   [dynamic]Type_Info,
	}
	tmp_cycle_ctx :: proc(root: Type_Info) -> (ctx: TopoCtx) {
		ctx = TopoCtx {
			visited = make(map[Type_Info]None, context.temp_allocator),
			deps    = make(map[Type_Info]Dep, context.temp_allocator),
			stack   = make([dynamic]Type_Info, context.temp_allocator),
		}
		append(&ctx.stack, root)
		return ctx
	}
	Dep :: struct {
		n_children: int,
		parents:    [dynamic]Type_Info,
	}
	add_dep :: proc(ctx: ^TopoCtx, parent, child: Type_Info) {
		if child not_in ctx.visited {
			append(&ctx.stack, child)
			ctx.visited[child] = None{}
		}

		if child_dep, ok := &ctx.deps[child]; ok {
			append(&child_dep.parents, parent)
		} else {
			child_dep := Dep{0, make([dynamic]Type_Info, context.temp_allocator)}
			append(&child_dep.parents, parent)
			ctx.deps[child] = child_dep
		}
		if parent_dep, ok := &ctx.deps[parent]; ok {
			parent_dep.n_children += 1
		} else {
			parent_dep := Dep{0, make([dynamic]Type_Info, context.temp_allocator)}
			parent_dep.n_children += 1
			ctx.deps[parent] = parent_dep
		}
	}
}


/*

U8,
	I8,
	U16,
	I16,
	U32,
	I32,
	U64,
	I64,
	U128,
	I128,
	F16,
	F32,
	F64,

*/

None :: struct {}
