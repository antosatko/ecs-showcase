# ECS (Entity Component System)

> My attempt to provide an overview of how ECS systems work. This repository is not a complete guide
> it only serves as a material I can use to explain certain topics involved.


## Introduction

ECS can be described as a programming pattern where you specify data accesses through a set of queries
into the central system which then optimizes memory structure for high efficiency based on those queries.
Its main use is in simulations and game development but will be useful in any area with data consisting
of large entity counts.

### Main concepts:

  - [**Entities**](readme#Entities) - An entity is a set of *components*
  - [**Components**](readme#Components) - Atomic unit of data
  - **Systems** - A procedure that *queries* for input to run on
  - **World** - Central system managing *entities* and processing *queries*

### Entities

Entities represent objects in *World*. In standard coding practices (mainly but not exclusively OOP)
objects derive meaning from their methods and attributes. This aproach forces the programmer to think
of an object as a whole which leads to tight coupling between data and behaviour. This is fine for smaller
less complicated models but as the project grows hiearchies become harder to maintain.

In ECS *entities* are only represented as sets of *components* and their meaning is derived purely by data.
Additionally *components* can be dynamically added or removed on existing *entities* to alter ther behaviour.

Let's define how to create an entity in simple pseudo code

```
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

