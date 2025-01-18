package deep
import "base:intrinsics"
import "base:runtime"
import "core:math/rand"
import "core:mem"
import "core:strings"

RandomOptions :: struct {
	max_depth:             int,
	max_slice_len:         int,
	max_map_len:           int,
	max_int:               int,
	sometimes_static_ptrs: bool,
	sometimes_nil_ptrs:    bool, // for objects stored as pointers in e.g. a struct
}
RANDOM_OPTIONS_DEFAULT :: RandomOptions {
	max_depth             = 3,
	max_int               = 100,
	sometimes_static_ptrs = false,
	max_map_len           = 3,
	max_slice_len         = 5,
	sometimes_nil_ptrs    = true,
}
random :: proc(
	$T: typeid,
	seed: u64 = 0,
	options := RANDOM_OPTIONS_DEFAULT,
	allocator := context.allocator,
) -> (
	res: T,
) {
	state := rand.create(seed)
	context.random_generator = rand.default_random_generator(&state)
	_construct_any_random(type_info_of(T), &res, options, allocator)
	return res
}

r_bool :: proc() -> bool {
	return rand.float32() > 0.5
}

_construct_any_random :: proc(
	ty: Type_Info,
	place: rawptr,
	options: RandomOptions,
	allocator: Allocator,
) {
	switch var in ty.variant {
	case runtime.Type_Info_Named:
		_construct_any_random(type_info_base(ty), place, options, allocator)
		return
	case runtime.Type_Info_Integer:
		int_any := any{place, ty.id}
		val := rand.int_max(options.max_int)
		switch &i in int_any {
		case int:
			i = val
		case u8:
			i = u8(val)
		case i8:
			i = i8(val)
		case u16:
			i = u16(val)
		case i16:
			i = i16(val)
		case u32:
			i = u32(val)
		case i32:
			i = i32(val)
		case u64:
			i = u64(val)
		case i64:
			i = i64(val)
		case i128:
			i = i128(val)
		case u128:
			i = u128(val)
		case:
			unimplemented(tprint("integer type not supported: ", ty.id))
		}
		return
	case runtime.Type_Info_Rune:
		val := rune(rand.int_max(94) + 33) // rough ascii range (33,..,126)
		(cast(^rune)place)^ = val
	case runtime.Type_Info_Float:
		float_any := any{place, ty.id}
		val := rand.float32()
		switch &f in float_any {
		case f16:
			f = f16(val)
		case f32:
			f = val
		case f64:
			f = f64(val)
		case:
			unimplemented(tprint("float type not supported: ", ty.id))
		}
		return
	case runtime.Type_Info_Complex:
		complex_any := any{place, ty.id}
		val1 := rand.float32()
		val2 := rand.float32()
		switch &c in complex_any {
		case complex32:
			c = complex(f16(val1), f16(val2))
		case complex64:
			c = complex(val1, val2)
		case complex128:
			c = complex(f64(val1), f64(val2))
		case:
			unimplemented(tprint("float type not supported: ", ty.id))
		}
		return
	case runtime.Type_Info_Simd_Vector,
	     runtime.Type_Info_Bit_Set,
	     runtime.Type_Info_Matrix,
	     runtime.Type_Info_Quaternion,
	     runtime.Type_Info_Bit_Field:
		return // just leave all zeros for now.
	case runtime.Type_Info_String:
		str := rand.choice(RANDOM_STRINGS)
		if var.is_cstring {
			cstr := strings.unsafe_string_to_cstring(str)
			if !options.sometimes_static_ptrs || r_bool() {
				cstr = strings.clone_to_cstring(str, allocator)
			}
			(cast(^cstring)place)^ = cstr
		} else {
			if !options.sometimes_static_ptrs || r_bool() {
				str = strings.clone(str, allocator)
			}
			(cast(^string)place)^ = str
		}
		return
	case runtime.Type_Info_Boolean:
		(cast(^bool)place)^ = r_bool()
		return
	case runtime.Type_Info_Any:
		raw_any := cast(^runtime.Raw_Any)place
		elem_ty := type_info_of(raw_any.id)
		_construct_any_random(elem_ty, raw_any.data, options, allocator)
		return
	case runtime.Type_Info_Type_Id:
		(cast(^typeid)place)^ = typeid_of(bool) // not random, but should be a rare case anyway
		return
	case runtime.Type_Info_Pointer:
		if options.sometimes_nil_ptrs && r_bool() {
			(cast(^rawptr)place)^ = nil
			return
		}
		ptr, err := mem.alloc(var.elem.size, var.elem.align, allocator)
		(cast(^rawptr)place)^ = ptr
		_construct_any_random(var.elem, ptr, options, allocator)
		return
	case runtime.Type_Info_Multi_Pointer:
		if options.sometimes_nil_ptrs {
			(cast(^rawptr)place)^ = nil
			return
		} else {
			unimplemented("cannot generate random Type_Info_Multi_Pointer")
		}
	case runtime.Type_Info_Procedure:
		if options.sometimes_nil_ptrs {
			(cast(^rawptr)place)^ = nil
			return
		} else {
			unimplemented("cannot generate random Type_Info_Procedure")
		}
	case runtime.Type_Info_Array:
		for idx in 0 ..< var.count {
			elem_place := rawptr(uintptr(place) + uintptr(idx * var.elem_size))
			_construct_any_random(var.elem, elem_place, options, allocator)
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
				_construct_any_random(var.elem, elem_place, options, allocator)
			}
		} else {
			for idx in 0 ..< var.count {
				elem_place := rawptr(uintptr(place) + uintptr(idx * var.elem_size))
				_construct_any_random(var.elem, elem_place, options, allocator)
			}
		}
		return
	case runtime.Type_Info_Dynamic_Array:
		raw_arr := cast(^Raw_Dynamic_Array)place
		// we can just cast the array to a slice because the first 2 fields (data: rawptr, len: int) are the same:
		_construct_random_slice(cast(^Raw_Slice)raw_arr, var.elem, options, allocator)
		raw_arr.cap = raw_arr.len
		raw_arr.allocator = allocator
		return
	case runtime.Type_Info_Slice:
		raw_slice := cast(^Raw_Slice)place
		_construct_random_slice(raw_slice, var.elem, options, allocator)
		return
	case runtime.Type_Info_Struct:
		for f_idx in 0 ..< var.field_count {
			field_place := rawptr(uintptr(place) + var.offsets[f_idx])
			_construct_any_random(var.types[f_idx], field_place, options, allocator)
		}
		return
	case runtime.Type_Info_Union:
		// special case handling of ptr unions with nil niche optimization:
		if len(var.variants) == 1 {
			only_ty := var.variants[0]
			#partial switch v in var.variants[0].variant {
			case runtime.Type_Info_Pointer,
			     runtime.Type_Info_Multi_Pointer,
			     runtime.Type_Info_Procedure:
				_construct_any_random(only_ty, place, options, allocator)
				return
			case runtime.Type_Info_String:
				if v.is_cstring {
					_construct_any_random(only_ty, place, options, allocator)
					return
				}
			}
		}
		// generate a random tag:
		variant_idx := rand.int_max(len(var.variants))
		tag_val := variant_idx if var.no_nil else variant_idx + 1
		tag_any := any{rawptr(uintptr(place) + var.tag_offset), var.tag_type.id}
		switch &tag in tag_any {
		case u8:
			tag = u8(tag_val)
		case i8:
			tag = i8(tag_val)
		case u16:
			tag = u16(tag_val)
		case i16:
			tag = i16(tag_val)
		case u32:
			tag = u32(tag_val)
		case i32:
			tag = i32(tag_val)
		case u64:
			tag = u64(tag_val)
		case i64:
			tag = i64(tag_val)
		case:
			unimplemented(tprint("unsupported tag:", var.tag_type.id, "for", ty))
		}
		variant_ty := var.variants[variant_idx]
		_construct_any_random(variant_ty, place, options, allocator)
		return
	case runtime.Type_Info_Enum:
		// could be optimized for contiguous enums where len(E) == cap(E), so no holes
		val := rand.choice(var.values)
		as_int_any := any{place, var.base.id}
		switch &i in as_int_any {
		case int:
			i = int(val)
		case u8:
			i = u8(val)
		case i8:
			i = i8(val)
		case u16:
			i = u16(val)
		case i16:
			i = i16(val)
		case u32:
			i = u32(val)
		case i32:
			i = i32(val)
		case u64:
			i = u64(val)
		case i64:
			i = i64(val)
		case i128:
			i = i128(val)
		case u128:
			i = u128(val)
		case:
			unimplemented(tprint("enum backing not supported: ", var.base.id, "for", ty))
		}
		return
	case runtime.Type_Info_Map:
	case runtime.Type_Info_Parameters:
		unimplemented("cannot generate random Type_Info_Parameters")
	case runtime.Type_Info_Soa_Pointer:
		unimplemented("cannot generate random Type_Info_Soa_Pointer")
	}
	panic("should have covered all types at the end of _construct_any_random")
}

_construct_random_slice :: proc(
	raw_slice: ^Raw_Slice,
	elem_ty: Type_Info,
	options: RandomOptions,
	allocator: Allocator,
) {
	slice_len := rand.int_max(options.max_slice_len + 1)
	if slice_len == 0 {
		raw_slice^ = Raw_Slice{nil, 0}
		return
	}
	raw_slice.len = slice_len
	err: runtime.Allocator_Error
	raw_slice.data, err = mem.alloc(slice_len * elem_ty.size, elem_ty.align, allocator)
	assert(err == .None)
	for idx in 0 ..< slice_len {
		elem_place := rawptr(uintptr(raw_slice.data) + uintptr(idx * elem_ty.size))
		_construct_any_random(elem_ty, elem_place, options, allocator)
	}
}

@(rodata)
RANDOM_STRINGS := []string {
	"Hello",
	"World",
	"Odin",
	"Flower",
	"Prince",
	"Moon",
	"Crab Cake",
	"English Muffin",
	"Trinket",
	"Candle",
	"Jon Snow",
	"Crazy Frog",
}
