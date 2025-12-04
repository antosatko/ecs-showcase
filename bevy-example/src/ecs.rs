use bevy_ecs::{
    component::Component, query::With, schedule::Schedule, system::Query, world::World,
};

use crate::{
    helpers::{BIG_NUMBER, Float2, Garbage},
    timer::MeasureTask,
};

#[derive(Debug, Component)]
struct Position(pub Float2);

#[derive(Debug, Component)]
struct Velocity(pub Float2);

#[derive(Debug, Component)]
struct GravityTag;

pub fn move_all() {
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

            world.spawn_batch((0..BIG_NUMBER).map(|_| {
                (
                    Position(Float2::random(0.0..100.0)),
                    Velocity(Float2::random(-5.0..5.0)),
                    Garbage::default(),
                    GravityTag,
                )
            }));

            schedule.add_systems(gravity);
            schedule.add_systems(movement);

            (schedule, world)
        },
        |system| {
            system.0.run(&mut system.1);
        },
    );
}
