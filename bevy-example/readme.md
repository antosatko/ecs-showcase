# Examples

This directory contains benchmarks of multiple design patterns including explanations for
the results they generate. But to fully understand the results it is important to have
some idea of how memory works.

## Memory caching

A common understanding of memory is that the CPU reads directly from RAM and thats it.
This model can be represented as a function that takes a memory address (and maybe size)
as input and outputs the stored data.

This could be the case in a perfect world, but main memory is much slower than the CPU
and your program can not wait every time it needs to read/write memory.

To solve this issue the CPU has its own built in memory where it temporarily caches data.
The trade-off is that this memory is very small, so it only holds a limited amount of
data. Every memory access stores not only the fetched address but also a small memory
region around the fetched address.

This means that while accessing densely packed structures, the CPU already has nearby values
cached after a single fetch. This saves CPU cycles and speeds up the program. In practice,
CPU caching can be used to optimize large object counts stored in a single array.

### Example of caching

Lets say we have an array of structures **A**, where each instance holds **n** values and we want
to read the value **a0** of each instance. If we iterate over those structures as they are now,
we also load a lot of additional memory with no use for it on each instance.

```
cpu cache:
[ # # # # # # # a0 a1 a2 a3 a4 a5 a6 # # # # # # # # # ]

main memory:
[ a0 a1 a2 a3 a4 a5 a6 b0 b1 b2 b3 b4 b5 b6 c0 c1 c2 c3 c4 c5 c6 ]
```

If we now want to access **b0**, we would need to load additional memory, which is costly.

To optimize memory usage you want to group memory regions based on how they are accessed,
so that when you need those values, you can reuse cached memory. In practice this means
that instead of storing all structures in a single array you store each field in a separate
array. This pattern is called a **Structure of Arrays** or **SoA**.

After rewriting the structure using this pattern, you can get much faster results.

```
cpu cache:
[ # # # # # # # a0 b0 c0 a1 b1 c1 # # # # # # # # # ]

main memory:
[ a0 b0 c0 a1 b1 c1 a2 b2 c2 a3 b3 c3 a4 b4 c4 a5 b5 c5 a6 b6 c6 ]
```

Here you can see that all of the *0th* fields are already in memory and ready for CPU access.
