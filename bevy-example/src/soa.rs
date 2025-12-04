use crate::{
    helpers::{BIG_NUMBER, Float2, Garbage},
    timer::MeasureTask,
};

pub fn move_all() {
    struct UnitSoa {
        entities: usize,
        positions: Vec<Float2>,
        velocities: Vec<Float2>,
        garbage: Vec<Garbage>,
    }

    MeasureTask::run(
        Some("move_all - soa"),
        || {
            let mut state = UnitSoa {
                entities: BIG_NUMBER,
                positions: Vec::with_capacity(BIG_NUMBER),
                velocities: Vec::with_capacity(BIG_NUMBER),
                garbage: Vec::with_capacity(BIG_NUMBER),
            };

            for _ in 0..state.entities {
                state.positions.push(Float2::random(0.0..100.0));
                state.velocities.push(Float2::random(-5.0..5.0));
                state.garbage.push(Garbage::default());
            }

            state
        },
        |state| {
            for unit in 0..state.entities {
                state.velocities[unit].1 += 0.03;
                state.positions[unit].0 += state.velocities[unit].0;
                state.positions[unit].1 += state.velocities[unit].1;
            }
        },
    );
}
