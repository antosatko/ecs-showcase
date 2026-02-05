const std = @import("std");

pub fn Slab(comptime T: type) type {
    return struct {
        alloc: std.mem.Allocator,
        data: std.ArrayList(Allocation(T)),
        thrash: std.ArrayList(usize),
        len: usize,

        const Self = @This();

        pub fn init(alloc: std.mem.Allocator) Self {
            return Self{ .alloc = alloc, .data = .empty, .thrash = .empty, .len = 0 };
        }
    };
}

fn Allocation(comptime T: type) type {
    return struct {
        data: T,
        version: usize,

        const Self = @This();

        pub fn is_alive(self: Self) bool {
            return self.version & 1;
        }
    };
}
