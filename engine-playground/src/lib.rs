use core::slice;
use std::{
    any::{Any, TypeId},
    cell::UnsafeCell,
    collections::HashMap,
    marker::PhantomData,
    mem::MaybeUninit,
    ptr::NonNull,
};

use any_vec::{
    AnyVec,
    any_value::{AnyValue, AnyValueRaw, AnyValueTypeless, AnyValueWrapper},
    mem::{Heap, MemBuilder},
};
use arena::{Arena, DynArena, DynKey, Key};
use smallbitvec::SmallBitVec;

use crate::bitset::Bitset;

mod bitset;

pub type Unit = u8;
pub type Mutability = bool;
pub type Required = bool;
pub type Array = AnyVec;
pub type ComponentId = usize;
pub type ComponentRef = usize;
pub type ComponentSize = usize;
/// An ordered list of components
pub type ExplicitSignature = Vec<ComponentRef>;
/// An unordered list of components (ordering defaults to ComponentRef <)
pub type ImplicitSignature = Bitset;
pub type ArchetypeId = usize;
pub type ComponentQuerry = (ComponentRef, Required, Mutability);
pub type EntityRef = DynKey<EntityTag>;
pub type ArrayParents = Vec<EntityRef>;
pub type TypedComponentRef<T> = (usize, PhantomData<T>);

#[derive(Debug, Copy, Clone)]
pub struct ArchetypeTag;
#[derive(Debug, Copy, Clone)]
pub struct EntityTag;

pub struct World {
    pub entities: DynArena<Entity, EntityTag>,
    pub archetypes: Arena<Archetype, ArchetypeTag>,
    components: Vec<ComponentData>,
}

pub struct Archetype {
    signature: ImplicitSignature,
    components: Vec<UnsafeCell<ComponentData>>,
    entities: ArrayParents,
    edges: HashMap<ComponentRef, ArcheTypeEdge>,
}

pub struct ArcheTypeEdge {
    next: Option<Key<Archetype>>,
    prev: Option<Key<Archetype>>,
}

pub struct Entity {
    pub archetype_key: Key<ArchetypeTag>,
    pub archetype_id: ArchetypeId,
}

pub struct EntitySpawner<'world> {
    sig: ImplicitSignature,
    components: Vec<(AnyVec, usize)>,
    world: &'world mut World,
    spawned: bool,
}

struct ComponentData {
    pub size: usize,
    pub container: AnyVec,
}

pub struct WorldRefSig<'world, 'querry> {
    arch_entity_idx: usize,
    components: &'world [UnsafeCell<ComponentData>],
    signature: &'world ImplicitSignature,
    binds: &'querry [Option<ComponentRef>],
}

#[derive(Clone, Hash, Default)]
struct QuerrySignature {
    include: ImplicitSignature,
}

impl QuerrySignature {
    pub fn new(comps: usize) -> Self {
        Self {
            include: Bitset::with_capacity(comps),
        }
    }
}

pub struct Querry {
    signature: QuerrySignature,
    // <(ComponentRef, is_bound)> - this information is later
    // used for generating the bind table
    ordered_signature: Vec<(ComponentRef, bool)>,
    bound_count: usize,
}

impl World {
    pub fn new() -> Self {
        Self {
            entities: DynArena::new(),
            archetypes: Arena::new(),
            components: Vec::new(),
        }
    }

    pub fn define_component<T>(&mut self) -> TypedComponentRef<T>
    where
        T: Sized + Copy + 'static,
    {
        let id = self.components.len();
        self.components.push(ComponentData {
            size: size_of::<T>(),
            container: AnyVec::new::<T>(),
        });
        (id, Default::default())
    }

    fn add_entity(&mut self, arch_key: &Key<ArchetypeTag>, arch_id: ArchetypeId) -> EntityRef {
        let id = self.entities.push(Entity {
            archetype_key: *arch_key,
            archetype_id: arch_id,
        });
        self.archetypes
            .get_mut_unchecked(arch_key)
            .entities
            .push(id);
        id
    }

    pub fn entity_count(&self) -> usize {
        self.entities.len()
    }

    pub fn archetype_count(&self) -> usize {
        self.archetypes.len()
    }

    fn add_archetype_with_init(&mut self, mut archetype: Archetype) -> Key<ArchetypeTag> {
        for comp_ref in archetype.signature.iter_inserted() {
            let c_desc = &self.components[comp_ref];
            archetype.components.push(UnsafeCell::new(ComponentData {
                container: c_desc.container.clone_empty(),
                size: c_desc.size,
            }));
        }
        self.archetypes.push(archetype)
    }

    pub fn append_component<T>(&mut self, e: &EntityRef, value: T, c_type: TypedComponentRef<T>) {
        let arch_key = &self.entities.get_unchecked(e).archetype_key;
        let arch = self.archetypes.get_unchecked(&arch_key);
        todo!();
    }

    pub fn delete_entity(&mut self, e: &EntityRef) {
        let entity = self.entities.get_unchecked(&e);
        let arch = self.archetypes.get_mut_unchecked(&entity.archetype_key);
        match arch.remove_entity(entity.archetype_id) {
            Some(moved) => {
                self.entities.get_mut_unchecked(&moved).archetype_id = entity.archetype_id;
            }
            None => (),
        }
        self.entities.delete(&e);
    }

    fn copy_component_intersection(&mut self, src: &EntityRef, dst: &EntityRef) {
        let src_arch_key = self.entities.get_unchecked(src).archetype_key;
        let dst_arch_key = self.entities.get_unchecked(dst).archetype_key;
        todo!();
    }

    pub fn archetypes_matching<'a>(
        &'a self,
        signature: &ImplicitSignature,
    ) -> impl Iterator<Item = &'a Archetype> {
        self.archetypes
            .iter()
            .filter(|arch| arch.signature.is_superset(signature))
    }

    pub fn run_querry<'world>(
        &'world mut self,
        querry: &Querry,
        mut cb: impl FnMut(&WorldRefSig<'world, '_>),
    ) {
        let mut binds = Vec::new();
        for arch in self.archetypes_matching(&querry.signature.include) {
            if querry.bound_count > 0 {
                binds.clear();
                binds.extend(
                    querry
                        .ordered_signature
                        .iter()
                        .filter(|(_, bound)| *bound)
                        .map(|(comp_ref, _)| {
                            arch.signature
                                .iter_inserted()
                                .position(|found_ref| &found_ref == comp_ref)
                        }),
                );
            }
            let mut w_ref = WorldRefSig {
                arch_entity_idx: 0,
                components: &arch.components,
                signature: &arch.signature,
                binds: &binds,
            };
            for (idx, _) in arch.entities.iter().enumerate() {
                w_ref.arch_entity_idx = idx;
                cb(&w_ref);
            }
        }
    }
}

impl Archetype {
    pub fn new_uninit(signature: ImplicitSignature) -> Self {
        Self {
            signature,
            components: Vec::new(),
            entities: Vec::new(),
            edges: HashMap::new(),
        }
    }

    /// Swaps the entity with the last and removes record of it.
    /// This function does not delete any component data
    ///
    /// Returns: ref to the entity that is now in its place
    ///
    /// Panics: If invalid index is passed
    pub(crate) fn remove_entity(&mut self, e: usize) -> Option<EntityRef> {
        let len = self.entities.len();
        assert!(e < len, "Invalid entity index");

        if len == 1 {
            self.entities.clear();
            for component in &mut self.components {
                component.get_mut().container.clear();
            }
            return None;
        }
        if e == len - 1 {
            self.entities.pop();
            for component in &mut self.components {
                component.get_mut().container.pop();
            }
            return None;
        }

        for component in &mut self.components {
            let com = component.get_mut();
            com.container.swap_remove(e);
        }

        let e_moved = self
            .entities
            .pop()
            .expect("There must be at least 2 entities at this point");
        self.entities[e] = e_moved;
        Some(e_moved)
    }
}

impl<'world> WorldRefSig<'world, '_> {
    pub fn get_unchecked<T: 'static>(&self, component: TypedComponentRef<T>) -> &'world T {
        let idx = self
            .signature
            .iter_inserted()
            .enumerate()
            .find(|(_, cref)| *cref == component.0)
            .unwrap()
            .0;

        unsafe {
            let column = &*self.components[idx].get();
            column
                .container
                .get_unchecked(self.arch_entity_idx)
                .downcast_ref_unchecked()
        }
    }

    pub fn get_mut_unchecked<T: 'static>(&self, component: TypedComponentRef<T>) -> &'world mut T {
        let idx = self
            .signature
            .iter_inserted()
            .enumerate()
            .find(|(_, cref)| *cref == component.0)
            .unwrap()
            .0;

        unsafe {
            let column = &mut *self.components[idx].get();
            column
                .container
                .get_unchecked_mut(self.arch_entity_idx)
                .downcast_mut_unchecked()
        }
    }

    pub fn get_bound_unchecked<T: 'static>(&self, typed_bind: TypedComponentRef<T>) -> &'world T {
        let idx = unsafe { self.binds[typed_bind.0].unwrap_unchecked() };

        unsafe {
            let column = &*self.components[idx].get();
            column
                .container
                .get_unchecked(self.arch_entity_idx)
                .downcast_ref_unchecked()
        }
    }

    pub fn get_mut_bound<T: 'static>(&self, typed_bind: TypedComponentRef<T>) -> &'world mut T {
        let idx = unsafe { self.binds[typed_bind.0].unwrap_unchecked() };

        unsafe {
            let column = &mut *self.components[idx].get();
            column
                .container
                .get_unchecked_mut(self.arch_entity_idx)
                .downcast_mut_unchecked()
        }
    }
}

impl<'world> EntitySpawner<'world> {
    pub fn new(world: &'world mut World) -> Self {
        Self {
            sig: Bitset::with_capacity(world.components.len()),
            components: Vec::with_capacity(10),
            world,
            spawned: false,
        }
    }

    pub fn clear(&mut self) {
        self.components.clear();
        self.sig.zero_all();
        self.spawned = false;
    }

    pub fn component<T: 'static>(mut self, data: T, kind: TypedComponentRef<T>) -> Self {
        if self.spawned {
            self.clear();
        }
        self.sig.insert(kind.0);
        let mut vec = AnyVec::with_capacity::<T>(1);
        vec.push(AnyValueWrapper::new(data));
        self.components.push((vec, kind.0));

        self
    }

    pub fn spawn(&mut self) -> EntityRef {
        self.spawned = true;
        self.components.sort_by(|l, r| l.1.cmp(&r.1));

        let key = self
            .world
            .archetypes
            .iter_pairs()
            .find(|(_, arch)| arch.signature.eq(&self.sig))
            .map(|(key, _)| key);

        match key {
            Some(key) => {
                let arch = self.world.archetypes.get_mut_unchecked(&key);
                for (idx, _) in arch.signature.iter_inserted().enumerate() {
                    let com = arch.components[idx].get_mut();
                    let src = &mut self.components[idx].0;

                    com.container
                        .push(src.pop().expect("Must contain exactly 1 element"))
                }
                let len = arch.entities.len();
                return self.world.add_entity(&key, len);
            }
            None => {
                let arch = Archetype::new_uninit(self.sig.clone());
                let arch_key = self.world.add_archetype_with_init(arch);
                let arch = self.world.archetypes.get_mut_unchecked(&arch_key);
                for (idx, flag) in arch.signature.iter_inserted().enumerate() {
                    let mut container = self.world.components[flag].container.clone_empty();
                    container.push(
                        self.components[idx]
                            .0
                            .pop()
                            .expect("Must contain exactly 1 element"),
                    );
                    arch.components[idx] = UnsafeCell::new(ComponentData {
                        size: self.world.components[flag].size,
                        container,
                    })
                }
                return self.world.add_entity(&arch_key, 0);
            }
        }
    }
}

impl Querry {
    pub fn new(world: &World) -> Self {
        Self {
            signature: QuerrySignature::new(world.components.len()),
            ordered_signature: Vec::with_capacity(10),
            bound_count: 0,
        }
    }

    pub fn include<T>(mut self, component: TypedComponentRef<T>) -> Self {
        self.signature.include.insert(component.0);
        self.ordered_signature.push((component.0, false));
        self
    }

    pub fn include_bind<T>(
        mut self,
        component: TypedComponentRef<T>,
        bind: &mut (usize, PhantomData<T>),
    ) -> Self {
        *bind = (self.bound_count, Default::default());
        self.bound_count += 1;
        self.signature.include.insert(component.0);
        self.ordered_signature.push((component.0, true));
        self
    }
}

#[cfg(test)]
mod tests {
    use core::panic;
    use std::any::Any;

    use any_vec::any_value::{AnyValue, AnyValueRaw, AnyValueWrapper};

    use crate::{EntitySpawner, Querry, World};

    #[test]
    fn arch() {
        let mut world = World::new();

        let c_position = world.define_component();
        let c_velocity = world.define_component();
        let c_player_tag = world.define_component();

        EntitySpawner::new(&mut world)
            .component((0.0, 0.0), c_position)
            .component((1.1, 5.5), c_velocity)
            .component((), c_player_tag)
            .spawn();

        EntitySpawner::new(&mut world)
            .component((100.0, 5.0), c_position)
            .component((-10.0, 0.0), c_velocity)
            .spawn();

        world.run_querry(
            &Querry::new(&world).include(c_position).include(c_velocity),
            |world| {
                let pos = world.get_mut_unchecked(c_position);
                let vel = world.get_unchecked(c_velocity);
                pos.0 += vel.0;
                pos.1 += vel.1;
            },
        );
        world.run_querry(
            &Querry::new(&world)
                .include(c_position)
                .include(c_velocity)
                .include(c_player_tag),
            |world| {
                let pos = world.get_unchecked(c_position);
                println!(
                    "position of an entity that is a player is: ({}, {})",
                    pos.0, pos.1
                );
            },
        );
    }

    #[test]
    fn arch_multi_archetype_delete_every_5th() {
        let mut world = World::new();

        let c_position = world.define_component();
        let c_velocity = world.define_component();
        let c_player_tag = world.define_component();
        let c_health = world.define_component();

        const N: usize = 100;
        let mut kept = Vec::new();

        for i in 0..N {
            let mut spawner = EntitySpawner::new(&mut world).component((i, 0), c_position);

            if i % 2 == 0 {
                spawner = spawner.component((1.0f32, 0.0f32), c_velocity);
            }
            if i % 3 == 0 {
                spawner = spawner.component((), c_player_tag);
            }
            if i % 7 == 0 {
                spawner = spawner.component(100i32, c_health);
            }

            let e = spawner.spawn();
            if i % 5 == 0 {
                kept.push(e);
            }
        }

        for e in &kept {
            world.delete_entity(e);
        }

        let expected_remaining = N - kept.len();
        let mut count = 0;
        world.run_querry(&Querry::new(&world), |_| {
            count += 1;
        });

        assert_eq!(count, expected_remaining);
        assert_eq!(world.entity_count(), expected_remaining);

        let mut bind = Default::default();
        // world.run_querry(
        //     &Querry::new(&world).include_bind(c_position, &mut bind),
        //     |world| print!("{:?}, ", world.get_bound_unchecked(bind)),
        // );

        world.run_querry(
            &Querry::new(&world).include_bind(c_position, &mut bind),
            |world| assert_ne!(world.get_bound_unchecked(bind).0 % 5, 0),
        );
    }
}
