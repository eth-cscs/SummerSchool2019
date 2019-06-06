# OpenACC practical exercises

- Both Cray and PGI compilers are supported.
- Compile each example with `make`
- Run as `srun --reserv=course -Cgpu <exec-name> [ARRAY_SIZE]`

`ARRAY_SIZE` is always a power of 2, so putting 10 would mean an array of 1024 elements.
