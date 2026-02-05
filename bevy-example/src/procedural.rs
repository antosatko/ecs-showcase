use crate::{
    helpers::{AnyData, BIG_NUMBER, Float2},
    timer::MeasureTask,
};

pub fn move_all() {
    struct Unit {
        pub any_data: AnyData,
        pub position: Float2,
        pub velocity: Float2,
        pub has_gravity: bool,
    }

    impl Unit {
        pub fn movement(&mut self) {
            self.position.0 += self.velocity.0;
            self.position.1 += self.velocity.1;
        }

        pub fn gravity(&mut self) {
            if self.has_gravity {
                self.velocity.1 += 0.03;
            }
        }
    }

    MeasureTask::run(
        Some("move_all - procedural"),
        || {
            let mut units = Vec::with_capacity(BIG_NUMBER);

            for _ in 0..BIG_NUMBER {
                units.push(Unit {
                    position: Float2::random(0.0..100.0),
                    velocity: Float2::random(-5.0..5.0),
                    any_data: AnyData::default(),
                    has_gravity: true,
                });
            }

            units
        },
        |units| {
            for unit in units {
                unit.movement();
                unit.gravity();
            }
        },
    );
}
