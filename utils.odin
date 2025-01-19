package deep

import "base:intrinsics"
import "base:runtime"
import "core:hash"
import "core:mem"
import "core:strings"

Tracker :: struct {
	allocator: Allocator,
	tracked:   [dynamic]Alloc,
}
Alloc :: struct {
	ptr:  rawptr,
	size: int,
}
tracker_create :: proc(allocator: Allocator) -> Tracker {
	return Tracker {
		allocator = allocator,
		tracked = make([dynamic]Alloc, allocator = context.temp_allocator),
	}
}
tracker_free_all :: proc(tracker: ^Tracker) {
	for a in tracker.tracked {
		mem.free_with_size(a.ptr, a.size, tracker.allocator)
	}
}
tracker_alloc :: proc(tracker: ^Tracker, size: int, align: int) -> rawptr {
	ptr, err := mem.alloc(size, align, tracker.allocator)
	assert(err == .None)
	assert(ptr != nil)
	append(&tracker.tracked, Alloc{ptr, size})
	return ptr
}
// can also manually add allocations made with e.g. make([]string, 20)
tracker_add :: proc(tracker: ^Tracker, ptr: rawptr, size: int) {
	append(&tracker.tracked, Alloc{ptr, size})
}

tracker_clone_string :: proc(tracker: ^Tracker, s: string) -> string {
	cloned := strings.clone(s, tracker.allocator)
	append(&tracker.tracked, Alloc{raw_data(cloned), len(s)})
	return cloned
}
