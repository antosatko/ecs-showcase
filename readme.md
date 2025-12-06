# ECS (Entity Component System)

> My attempt to provide an overview of how ECS systems work. This repository is not a complete guide
> it only serves as a material I can use to explain certain topics involved.


## Introduction

ECS can be described as a programming pattern where you specify data accesses through a set of queries
into the central system which then optimizes memory structure for high efficiency based on those queries.
Its main use is in simulations and game development but will be useful in any area with data consisting
of large entity counts.

## Main concepts:

  - [**Entities**](#Entities) - An entity is a set of *components*
  - [**Components**](#Components) - Atomic unit of data
  - [**Systems**](#Systems) - A procedure that *queries* for input to run on
  - [**World**](#World) - Central system managing *entities* and processing *queries*
  - [**Queries**](#Queries) - Describes what kind of data should be passed to a *system*

### Entities

Entities represent objects in *World*. In standard coding practices (mainly but not exclusively OOP)
objects derive meaning from their methods and attributes. This aproach forces the programmer to think
of an object as a whole which leads to tight coupling between data and behaviour. This is fine for smaller
less complicated models but as the project grows hiearchies become harder to maintain.

In ECS *entities* are only represented as sets of *components* and their meaning is derived purely by data.
Additionally *components* can be dynamically added or removed on existing *entities* to alter ther behaviour.

Let's define how to create an entity in simple pseudo code

```js
// empty entity
spawn {
  // any number of components ...
}

// entity with Velocity and Position
spawn {
  Velocity(50, 50)
  Position(0, 0)
}
```

### Components

Components are actual data that *entities* may contain. Behaviour of the *entity* is based only on the
*components* it contains. Each *component* either has data type or has no data (those are sometimes called
tags).

In pseudo code we can define *components* in two ways

```js
// Component defined only with it's name (also called tag)
component Sleepy

// Component defined with name and data type
component Velocity(float, float)

// Component that references another entity
component BestFriend(entity)
```

To use a *component*, just pass it to an *entity* on creation

```js
// an entity that is fast moving and sleepy
spawn {
  Velocity(50, 50)
  Sleepy
}
```

### Systems

Any behaviour of a *world* is programmed using *systems*. Every *system* is defined with *query*
for data that will be used as an input to a procedure that will be executed at some point in time.
*Systems* may mutate *worlds* data if they explicitly *query* for mutable references.
More on *queries* later.

In pseudo code *system* implementation looks very similar to function implementations with the diference
of having *query* instead of parameters.

```js
// A system that moves all entities with Velocity and Position components
system movement(entity: Velocity & mut Position) {
  entity.Position = entity.Position + entity.Velocity
}

// A system that slowly slows everything down
system friction(entity: mut Velocity) {
  entity.Velocity = entity.Velocity * 0.995
}
```

### World

World manages all of the things described above and more. It optimizes inner data representation for
the highest perfmormance while *querying*. It also manages paralelism for *systems* that can run at
the same time. This is not the same as ordering which can not be infered from the *world* structure
and falls as a responsibility of the programmer.

```js
// A world where friction is run first and then follows movement
schedule {
  friction
  movement
}
```

In this example `movement` depends on `friction` since it *queries* for a *component* that was
mutated there (`Velocity`). Which means that those two *systems* can not be run in paralel and their
ordering will be determined by position in *schedule*.

### Queries

A *query* describes which *components* a *system* needs access to in order to execute.

This includes:
  - Which *components* are required
  - Whether access is read-only or mutable
  - Optional filters that exclude or include *entities* based on conditions

In pseudo code:

**Simple query**

```js
entity: C1
```

**Simple query with mutable access to component**

```js
entity: mut C1
```

**Multiple required components on entity**
```js
entity: C1 & C2 & C3
```

**Optional Component**
```js
entity: ?C1
```

**Excluded Component**
```js
entity: !C1
```

**Join on reference**
```js
entity: C1, entity.C1: C2
```

**Mutable access to entity**
```js
mut entity: C1
```
