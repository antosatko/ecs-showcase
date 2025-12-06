use std::simd::f32x2;

use rand::random_range;

use crate::{
    helpers::{AnyData, BIG_NUMBER},
    timer::MeasureTask,
};

pub fn move_all() {
    struct UnitSoa {
        entities: usize,
        positions: Vec<f32x2>,
        velocities: Vec<f32x2>,
        any_data: Vec<AnyData>,
    }

    MeasureTask::run(
        Some("move_all - soa-simd"),
        || {
            let mut state = UnitSoa {
                entities: BIG_NUMBER,
                positions: Vec::with_capacity(BIG_NUMBER),
                velocities: Vec::with_capacity(BIG_NUMBER),
                any_data: Vec::with_capacity(BIG_NUMBER),
            };

            for _ in 0..state.entities {
                state.positions.push(f32x2::from_array([
                    random_range(0.0..100.0),
                    random_range(0.0..100.0),
                ]));
                state.velocities.push(f32x2::from_array([
                    random_range(-5.0..5.0),
                    random_range(-5.0..5.0),
                ]));

                state.any_data.push(AnyData::default());
            }

            state
        },
        |state| {
            for i in 0..state.entities {
                state.velocities[i] += f32x2::from_array([0.0, 0.03]);
                // uncomment to make the code 6x slower :)
                // state.velocities[i][1] += 0.03;
                state.positions[i] += state.velocities[i];
            }
        },
    );
}
