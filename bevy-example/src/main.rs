#![allow(dead_code)]
#![feature(portable_simd)]

mod bevy;
mod helpers;
mod my_ecs;
mod procedural;
mod soa;
mod timer;

fn main() {
    bevy::move_all();
    procedural::move_all();
    soa::move_all();
    soa::simd::move_all();
    my_ecs::move_all();
}
