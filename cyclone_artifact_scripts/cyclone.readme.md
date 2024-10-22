## Files Structure
The files in this artifact are split into the following directory tree:
```
/
    home
        cyclist
        scripts
        paper_data
```

### *Cyclone* Implementation
The `/home/cyclist` directory contains the source code for Cyclist together with the integration of our implementation of *Cyclone*.
The code of our implementation can be found in the `src/generic` directory, with the most relevant files being `cyclone.c`, `criterion.flat_cycles.c`, `criterion.descending_unicycles.c` and `criterion.trace_manifold.c`.


### Reproducing our Results
The `/home/scripts` directory contains scripts for generating all of the data that we used in our paper and our results.

#### `generate_database.sh`
This script generates a database of sloped graphs.
This script creates the directory `/home/database` which contains two directories, one for each logic: `fo` and `sl`.
Each of these directory contains `.json` files, one for each sloped graph that was generated.

> Note that because Cyclist timesout while searching a proof for some of the sequents in the test suite, the amount of graphs in the database can differ slightly from the amount that we present in the paper.


#### `generate_stats.sh`
This script generates a `/home/stats.csv` CSV file containing the following statistics for each sloped graph in the database:
The sloped graph filename, the metrics described in section 3 in the paper, the runtime and the answer of each incomplete methods and other information described in the CSV header.

This script should be run after the [`generate_database.sh`](#generate_databasesh) script, as it uses the generated sloped graphs.


#### `evaluate_cyclone.sh`
This script creates a `/home/evaluation.csv` CSV file containing the runtime evaluation of *Cyclone*, with the runtimes of each complete method presented in the paper, together with the sloped graph file name, and the amount of its edges.

This script should be run after both the [`generate_database.sh`](#generate_databasesh) and the[`generate_stats.sh`](#generate_statssh) scripts, as it uses the sloped graphs in the database and some of their calculated metrics.

This script might take a few hours, mainly because it is running the very slow SLA method. 
It is possible to add `-method <method name>` flags, with `<method name>` being one of the following: `VLA`, `SLA`, `FWK`, `OR`, indicating which methods to run in the evaluation.
Note that by default all methods run and if any `-method` flag is passed, only the flagged methods run.


### The Data Used in our Paper
The `/home/paper_data` directory contains a `database` directory containing the sloped graphs database from our paper, the `stats.csv` file that we used in our paper (as described in the [generate stats section](#generate_statssh)) and the `evaluation.csv` file that we used in our paper (as described in the [evaluate cyclone section](#evaluate_cyclonesh)).