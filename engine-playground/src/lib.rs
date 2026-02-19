use core::slice;
use std::{cell::UnsafeCell, collections::HashMap, marker::PhantomData};

use arena::{Arena, DynArena, DynKey, Key};
use bit_set::BitSet;

pub type Unit = u8;
pub type Mutability = bool;
pub type Required = bool;
pub type Array = Vec<Unit>;
pub type ComponentId = usize;
pub type ComponentRef = usize;
pub type ComponentSize = usize;
/// An ordered list of components
pub type ExplicitSignature = Vec<ComponentRef>;
/// An unordered list of components (ordering defaults to ComponentRef <)
pub type ImplicitSignature = BitSet;
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
    pub components: Vec<ComponentSize>,
}

pub struct Archetype {
    signature: ImplicitSignature,
    components: Vec<UnsafeCell<(Array, ComponentSize)>>,
    entites: ArrayParents,
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

pub struct EntitySpawner<'world, 'data> {
    components: Vec<(&'data [u8], usize)>,
    world: &'world mut World,
}

pub struct WorldRefSig<'world, 'querry> {
    arch_entity_idx: usize,
    components: &'world [UnsafeCell<(Array, ComponentSize)>],
    signature: &'world ImplicitSignature,
    binds: &'querry [Option<ComponentRef>],
}

#[derive(Clone, Hash, Default)]
struct QuerrySignature {
    include: ImplicitSignature,
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
        T: Sized + Copy,
    {
        let id = self.components.len();
        self.components.push(size_of::<T>());
        (id, Default::default())
    }

    fn add_entity(&mut self, arch_key: &Key<ArchetypeTag>, arch_id: ArchetypeId) -> EntityRef {
        let id = self.entities.push(Entity {
            archetype_key: *arch_key,
            archetype_id: arch_id,
        });
        self.archetypes.get_mut_unchecked(arch_key).entites.push(id);
        id
    }

    fn add_archetype_with_init(&mut self, mut archetype: Archetype) -> Key<ArchetypeTag> {
        for comp_ref in &archetype.signature {
            let size = self.components[comp_ref];
            archetype
                .components
                .push(UnsafeCell::new((Vec::new(), size)));
        }
        self.archetypes.push(archetype)
    }

    pub fn append_component<T>(&mut self, e: &EntityRef, value: T, c_type: TypedComponentRef<T>) {
        let arch_key = &self.entities.get_unchecked(e).archetype_key;
        let arch = &mut self.archetypes.get(&arch_key);
        todo!();
    }

    pub fn delete_entity(&mut self, e: &EntityRef) {
        let entity = self.entities.get_unchecked(&e);
        let arch = self.archetypes.get_mut_unchecked(&entity.archetype_key);
        todo!();
    }

    fn copy_component_intersection(&mut self, src: &EntityRef, dst: &EntityRef) {
        let src_arch_key = self.entities.get_unchecked(src).archetype_key;
        let dst_arch_key = self.entities.get_unchecked(dst).archetype_key;
        todo!();
    }

    fn arch_remove_entity(&mut self, arch_key: &Key<ArchetypeTag>) {
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
                binds.extend(
                    querry
                        .ordered_signature
                        .iter()
                        .filter(|(_, bound)| *bound)
                        .map(|(comp_ref, _)| {
                            arch.signature
                                .iter()
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
            for (idx, _) in arch.entites.iter().enumerate() {
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
            entites: Vec::new(),
            edges: HashMap::new(),
        }
    }
}

impl<'world> WorldRefSig<'world, '_> {
    pub fn get_unchecked<T>(&self, component: TypedComponentRef<T>) -> &'world T {
        let idx = self
            .signature
            .iter()
            .enumerate()
            .find(|(_, cref)| *cref == component.0)
            .unwrap()
            .0;
        let column = unsafe { &*self.components[idx].get() };
        let size = column.1;

        let start = self.arch_entity_idx * size;
        let end = start + size;
        let slice = &column.0[start..end];

        unsafe { &*(slice.as_ptr() as *const T) }
    }

    pub fn get_mut_unchecked<T>(&self, component: TypedComponentRef<T>) -> &'world mut T {
        let idx = self
            .signature
            .iter()
            .enumerate()
            .find(|(_, cref)| *cref == component.0)
            .unwrap()
            .0;
        let column = unsafe { &mut *self.components[idx].get() };
        let size = column.1;
        let start = self.arch_entity_idx * size;
        let end = start + size;
        let slice = &mut column.0[start..end];

        unsafe { &mut *(slice.as_mut_ptr() as *mut T) }
    }

    pub fn get_bound_unchecked<T>(&self, typed_bind: (usize, PhantomData<T>)) -> &'world T {
        let idx = self.binds[typed_bind.0].unwrap();
        let column = unsafe { &*self.components[idx].get() };
        let size = column.1;
        let start = self.arch_entity_idx * size;
        let end = start + size;
        let slice = &column.0[start..end];

        unsafe { &*(slice.as_ptr() as *const T) }
    }

    pub fn get_mut_bound<T>(&self, typed_bind: (usize, PhantomData<T>)) -> &'world mut T {
        let idx = self.binds[typed_bind.0].unwrap();
        let column = unsafe { &*self.components[idx].get() };
        let size = column.1;
        let start = self.arch_entity_idx * size;
        let end = start + size;
        let slice = &column.0[start..end];

        unsafe { &mut *(slice.as_ptr() as *mut T) }
    }
}

impl<'world, 'data> EntitySpawner<'world, 'data> {
    pub fn new(world: &'world mut World) -> Self {
        Self {
            components: Vec::with_capacity(10),
            world,
        }
    }

    pub fn clear(&mut self) {
        self.components.clear();
    }

    pub fn component<T>(mut self, data_ref: &T, kind: TypedComponentRef<T>) -> Self {
        assert!(
            !self.components.iter().any(|(_, s)| *s == kind.0),
            "A component can be assigned only once"
        );

        let bytes: &[u8] =
            unsafe { slice::from_raw_parts(data_ref as *const T as *const u8, size_of::<T>()) };

        self.components.push((bytes, kind.0));

        self
    }

    pub fn spawn(&mut self) -> EntityRef {
        let mut impl_sig = BitSet::new();
        for (_, com_ref) in &self.components {
            impl_sig.insert(*com_ref);
        }

        let key = self
            .world
            .archetypes
            .iter_pairs()
            .find(|(_, arch)| arch.signature.eq(&impl_sig))
            .map(|(key, _)| key);

        match key {
            Some(key) => {
                let arch = self.world.archetypes.get_mut_unchecked(&key);
                for (idx, signature) in arch.signature.iter().enumerate() {
                    let dst = unsafe { &mut *arch.components[idx].get() };
                    let src_idx = self
                        .components
                        .iter()
                        .map(|(_, sig)| sig)
                        .position(|sig| sig == &signature)
                        .unwrap();
                    let src = &self.components[src_idx];
                    assert_eq!(dst.1, src.0.len());
                    dst.0.extend_from_slice(src.0);
                }
                let len = arch.entites.len();
                return self.world.add_entity(&key, len);
            }
            None => {
                let arch = Archetype::new_uninit(impl_sig.clone());
                let arch_key = self.world.add_archetype_with_init(arch);
                let arch = self.world.archetypes.get_mut_unchecked(&arch_key);
                for (idx, comp_ref) in impl_sig.iter().enumerate() {
                    let src = self
                        .components
                        .iter()
                        .map(|(_, sig)| sig)
                        .position(|comp_found| comp_found == &comp_ref)
                        .unwrap();
                    arch.components[idx] = UnsafeCell::new((
                        self.components[src].0.to_vec(),
                        self.components[src].0.len(),
                    ));
                }
                return self.world.add_entity(&arch_key, 0);
            }
        }
    }
}

impl Querry {
    pub fn new() -> Self {
        Self {
            signature: Default::default(),
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

    use crate::{EntitySpawner, Querry, World};

    #[test]
    fn arch() {
        let mut world = World::new();

        let c_position = world.define_component();
        let c_velocity = world.define_component();
        let c_player_tag = world.define_component();

        EntitySpawner::new(&mut world)
            .component(&(0.0, 0.0), c_position)
            .component(&(1.1, 5.5), c_velocity)
            .component(&(), c_player_tag)
            .spawn();

        EntitySpawner::new(&mut world)
            .component(&(100.0, 5.0), c_position)
            .component(&(-10.0, 0.0), c_velocity)
            .spawn();

        world.run_querry(
            &Querry::new().include(c_position).include(c_velocity),
            |world| {
                let pos = world.get_mut_unchecked(c_position);
                let vel = world.get_unchecked(c_velocity);
                pos.0 += vel.0;
                pos.1 += vel.1;
            },
        );
        world.run_querry(
            &Querry::new()
                .include(c_position)
                .include(c_velocity)
                .include(c_player_tag),
            |world| {
                let pos = world.get_unchecked(c_position);
                println!(
                    "position of an entity that is a player is: ({}, {})",
                    pos.0, pos.1
                )
            },
        );

        panic!("nothing to worry about")
    }
}
