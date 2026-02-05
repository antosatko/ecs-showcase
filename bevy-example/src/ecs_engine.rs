use crate::{
    helpers::{AnyData, BIG_NUMBER, Float2},
    timer::MeasureTask,
};
use engine_playground::EntitySpawner;
use engine_playground::Querry;
use engine_playground::{TypedComponentRef, World};

pub fn move_all() {
    struct Program {
        world: World,
        c_position: TypedComponentRef<Float2>,
        c_velocity: TypedComponentRef<Float2>,
        c_anydata: TypedComponentRef<AnyData>,
        c_gravity: TypedComponentRef<()>,
    }

    MeasureTask::run(
        Some("move_all - engine"),
        || {
            let mut world = World::new();

            let c_position = world.define_component();
            let c_velocity = world.define_component();
            let c_gravity = world.define_component();
            let c_anydata = world.define_component();

            for i in 0..BIG_NUMBER {
                if i % 2 == 0 {
                    EntitySpawner::new(&mut world)
                        .component(Float2::random(0.0..100.0), c_position)
                        .component(Float2::random(-5.0..5.0), c_velocity)
                        .component(AnyData::default(), c_anydata)
                        .component((), c_gravity)
                        .spawn();
                } else {
                    EntitySpawner::new(&mut world)
                        .component(Float2::random(-5.0..5.0), c_velocity)
                        .component(AnyData::default(), c_anydata)
                        .component((), c_gravity)
                        .spawn();
                }
            }
            Program {
                world,
                c_position,
                c_velocity,
                c_anydata,
                c_gravity,
            }
        },
        |prog| {
            let mut b_position = Default::default();
            let mut b_velocity = Default::default();

            prog.world.run_querry(
                &Querry::new()
                    .include_bind(prog.c_position, &mut b_position)
                    .include(prog.c_gravity),
                |world| {
                    let position = world.get_mut_bound(b_position);
                    position.1 += 0.03;
                },
            );

            prog.world.run_querry(
                &Querry::new()
                    .include_bind(prog.c_position, &mut b_position)
                    .include_bind(prog.c_velocity, &mut b_velocity),
                |world| {
                    let velocity = world.get_bound_unchecked(b_velocity);
                    let position = world.get_mut_bound(b_position);

                    position.0 += velocity.0;
                    position.1 += velocity.1;
                },
            );
        },
    );
}
