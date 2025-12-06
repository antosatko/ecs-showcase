#![allow(dead_code)]
#![feature(portable_simd)]

mod ecs;
mod helpers;
mod procedural;
mod soa;
mod timer;

fn main() {
    ecs::move_all();
    procedural::move_all();
    soa::move_all();
    soa::simd::move_all();
}
