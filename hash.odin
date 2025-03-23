package deep

import "base:intrinsics"
import "base:runtime"
import core_hash "core:hash"
import "core:mem"
import "core:reflect"
import "core:strings"

// can be overwritten by user to use a different hash function
INITIAL_HASH_VALUE: u64 = u64(0xcbf29ce484222325)
HASH_FN: proc "contextless" (data: []byte, seed: u64) -> u64 = core_hash.fnv64_no_a
hash :: proc(this: $T) -> u64 {
	a: any = this // saves extra stack copy
	hash_val := INITIAL_HASH_VALUE
	_hash_any(type_info_of(a.id), a.data, &hash_val)
	return hash_val
}

_hash_slice :: proc(elem_ty: Type_Info, sli: Raw_Slice, hash_val: ^u64) {
	_hash_len(sli.len, hash_val)
	if is_simple_copy_type(elem_ty) {
		sli_as_bytes := transmute([]u8)Raw_Slice{sli.data, sli.len * elem_ty.size}
		hash_val^ = HASH_FN(sli_as_bytes, hash_val^)
	} else {
		start := uintptr(sli.data)
		for idx in 0 ..< sli.len {
			elem_place := rawptr(start + uintptr(idx * elem_ty.size))
			_hash_any(elem_ty, elem_place, hash_val)
		}
	}
}
_hash_len :: proc(len: int, hash_val: ^u64) {
	len := len
	hash_val^ = HASH_FN(transmute([]u8)Raw_Slice{&len, 4}, hash_val^)
}
// e.g. if a struct field has the tag `deep:"nohash"` it is skipped when computing the hash
_struct_tag_says_skip_hash :: proc(tags: string) -> bool {
	tag_for_deep := reflect.struct_tag_lookup(reflect.Struct_Tag(tags), "deep") or_return
	return strings.contains(tag_for_deep, "nohash")
}

_hash_any :: proc(ty: Type_Info, place: rawptr, hash_val: ^u64) {
	// cannot use is_copy_type here, because structs might have field tags for skipping fields when hashing!!
	if is_simple_copy_type(ty) {
		val_as_bytes := transmute([]u8)Raw_Slice{place, ty.size}
		hash_val^ = HASH_FN(val_as_bytes, hash_val^)
		return
	}

	#partial switch var in ty.variant {
	case runtime.Type_Info_Named:
		base_ty := type_info_base(ty)
		_hash_any(base_ty, place, hash_val)
		return
	case runtime.Type_Info_Pointer:
		if var.elem != nil {
			elem_place := (cast(^rawptr)place)^
			if elem_place == nil {
				hash_val^ = HASH_FN([]u8{0}, hash_val^)
			} else {
				_hash_any(var.elem, elem_place, hash_val)
			}
		}
		return
	case runtime.Type_Info_Slice:
		sli := (cast(^Raw_Slice)place)^
		_hash_slice(var.elem, sli, hash_val)
		return
	case runtime.Type_Info_Dynamic_Array:
		// this is possible because the first two fields of Raw_Dynamic_Array 
		// and Raw_Slice are the same `data: rawptr, len: int`
		sli := (cast(^Raw_Slice)place)^
		_hash_slice(var.elem, sli, hash_val)
	case runtime.Type_Info_Array:
		sli := Raw_Slice{place, var.count}
		_hash_slice(var.elem, sli, hash_val)
		return
	case runtime.Type_Info_Enumerated_Array:
		if var.is_sparse {
			enum_ty, ok := var.index.variant.(runtime.Type_Info_Enum)
			assert(ok, "index of Type_Info_Enumerated_Array should be Type_Info_Enum")
			// in sparse case, no need to iterate the empty slots:
			_hash_len(len(enum_ty.values), hash_val)
			for val in enum_ty.values {
				idx := int(val - var.min_value)
				assert(idx >= 0 && idx < var.count)
				offset := uintptr(idx * var.elem_size)
				elem_place := rawptr(uintptr(place) + offset)
				_hash_any(var.elem, elem_place, hash_val)
			}
			return
		} else {
			sli := Raw_Slice{place, var.count}
			_hash_slice(var.elem, sli, hash_val)
		}
		return
	case runtime.Type_Info_String:
		str: string
		if var.is_cstring {
			c_str := (cast(^cstring)place)^
			str = string(c_str)
		} else {
			str = (cast(^string)place)^
		}
		hash_val^ = HASH_FN(transmute([]u8)str, hash_val^)
		return
	case runtime.Type_Info_Struct:
		// todo: allow for ignoring fields here.
		for f_idx in 0 ..< var.field_count {
			offset := var.offsets[f_idx]
			tag := var.tags[f_idx]
			if _struct_tag_says_skip_hash(tag) do continue
			field_place := rawptr(uintptr(place) + offset)
			_hash_any(var.types[f_idx], field_place, hash_val)
		}
		return
	case runtime.Type_Info_Union:
		// special case handling of ptr unions with nil niche optimization:
		if len(var.variants) == 1 {
			#partial switch v in var.variants[0].variant {
			case runtime.Type_Info_Pointer:
				if v.elem != nil {
					elem_place := (cast(^rawptr)place)^
					if elem_place == nil {
						hash_val^ = HASH_FN([]u8{0}, hash_val^)
					} else {
						_hash_any(v.elem, elem_place, hash_val)
					}
				}
				return
			case runtime.Type_Info_Multi_Pointer:
				unimplemented("Multi_Pointer not supported")
			case runtime.Type_Info_Procedure:
				proc_ptr := (cast(^rawptr)place)^
				ptr_as_bytes := transmute([]u8)Raw_Slice{&proc_ptr, size_of(rawptr)}
				hash_val^ = HASH_FN(ptr_as_bytes, hash_val^)
				return
			case runtime.Type_Info_String:
				if v.is_cstring {
					c_str := (cast(^cstring)place)^
					c_str_bytes := transmute([]u8)(string(c_str))
					hash_val^ = HASH_FN(c_str_bytes, hash_val^)
				}
			}
		}
		tag := _get_union_tag_for_non_ptr_union(var, place)
		_hash_len(tag, hash_val)
		if !var.no_nil && tag == 0 {
			return
		}
		variant_idx := tag if var.no_nil else tag - 1
		_hash_any(var.variants[variant_idx], place, hash_val)
		return
	case runtime.Type_Info_Map:
		panic(
			"Hashing of maps currently not implemented because sorting of keys would be required...",
		)
	}
	panic(tprint("Unsupported type for hashing: ", ty))
}
