# Examples

This directory contains benchmarks of multiple design patterns including explanations for
the results they generate. But to fully understand the results it is important to have
some idea of how memory works. Here is a simplified explanation for
[memory caching](./bevy-example/caching.md).


## Benchmark explanation

These benchmarks compare how different data layouts affect memory access and therefore performance.

### move_all – procedural
Objects store all fields together, which is simple but causes poor cache locality.
Each iteration loads unnecessary data.

```
Main duration: 2.5489691s
```

### move_all – ecs
ECS groups components by usage. Queries skip unrelated data, improving iteration speed.
Initialization takes longer due to internal bookkeeping.

```
Main duration:  1.4699925s
```

### move_all – soa
Structure of Arrays stores each field in its own dense array. This greatly improves
cache efficiency when processing large batches.

```
Main duration:  994.106ms
```

### move_all – soa-simd
Identical layout to SoA but processed using SIMD instructions, updating multiple
values per CPU instruction. Fastest but least flexible.

```
Main duration:  614.2116ms
```

### Benchmark results table

| Pattern       | Init duration | **Main duration** | Total duration  |
|---------------|--------------:|------------------:|----------------:|
| *Procedural*  | 196.4732ms    | 2.5489691s        | 2.7454423s      |
| *ECS*         | 501.6176ms    | 1.4699925s        | 1.9716101s      |
| *SoA*         | 198.7740ms    | 994.1060ms        | 1.1928800s      |
| *SoA + SIMD*  | 196.6388ms    | 614.2116ms        | 810.8504ms      |
