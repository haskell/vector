# Dependence on random numbers.

These benchmarks depend on random numbers which are by default generated using a static seed.

To avoid misleading results:
* Run the benchmarks using the static seed for a first approximation.
* Verify the results by running the benchmark with different seeds.

Seeds can be given by using `./algorithms --seed 42`
Any seed given MUST be the first argument to the executable or it will be ignored.
