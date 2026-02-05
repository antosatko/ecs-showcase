use bevy_ecs::{
    component::Component, query::With, schedule::Schedule, system::Query, world::World,
};

use crate::{
    helpers::{AnyData, BIG_NUMBER, Float2},
    timer::MeasureTask,
};

pub fn move_all() {
    #[derive(Debug, Component)]
    struct Position(pub Float2);

    #[derive(Debug, Component)]
    struct Velocity(pub Float2);

    #[derive(Debug, Component)]
    struct GravityTag;

    fn movement(mut query: Query<(&mut Position, &Velocity)>) {
        for (mut position, velocity) in &mut query {
            position.0.0 += velocity.0.0;
            position.0.1 += velocity.0.1;
        }
    }

    fn gravity(mut query: Query<&mut Velocity, With<GravityTag>>) {
        for mut velocity in &mut query {
            velocity.0.1 += 0.03;
        }
    }

    MeasureTask::run(
        Some("move_all - ecs"),
        || {
            let mut world = World::new();
            let mut schedule = Schedule::default();

            for i in 0..BIG_NUMBER {
                match i % 2 {
                    0 => world.spawn((
                        Position(Float2::random(0.0..100.0)),
                        Velocity(Float2::random(-5.0..5.0)),
                        AnyData::default(),
                        GravityTag,
                    )),
                    _ => world.spawn((
                        Velocity(Float2::random(-5.0..5.0)),
                        AnyData::default(),
                        GravityTag,
                    )),
                };
            }

            schedule.add_systems(gravity);
            schedule.add_systems(movement);
            (schedule, world)
        },
        |system| {
            system.0.run(&mut system.1);
        },
    );
}
