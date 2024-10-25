# Artifact for the TACAS Submission 'Cyclone: A Heterogeneous Tool for Checking Infinite Descent'

This `README.md` file describes the artifact for submission #3251 to TACAS25 Tool Artefacts.

The artifact is associated with paper submission #7026 to TACAS25.

## Introduction

### Abstract

The artifact is an augmented version of the Cyclist automated theorem prover, containing the implementations of our new algorithms and their integration into a pipeline, Cyclone, that can be called by Cyclist as a back-end Infinite Descent check. The artifact also contains the database of sloped graphs that we harvested from Cyclist's first-order logic and Separation Logic test suites. Lastly, it contains scripts for recreating the database from scratch, and running the experimental evaluation that we report in the paper.

### Artifact Contents

The artifact submission contains the following.

```text
├── README.md
├── TACAS25_paper_7026.pdf
├── cyclist.zip
├── paper_data.zip
```

The current file is `README.md`.

The file `TACAS25_paper_7026.pdf` is the paper submitted to TACAS 2025.

The `cyclist.zip` archive contains the following.

```text
├── cyclist.zip
    ├── LICENSE.md
    ├── cyclone_artifact_scripts/
        ├── cyclone.evaluate_cyclone.sh
        ├── cyclone.generate_database.sh
        ├── cyclone.graphs_stats.sh
        ├── create_figures.sh
    ...
```

The `LICENSE.md` contains the 3-clause-BSD license under which the Cyclist theorem prover framework is distributed.

The `cyclist` directory contains the source code for the Cyclist theorem prover framework, into which we have incorporated the implementation of our tool, Cyclone.
See the [*Cyclone* Implementation](#cyclone-implementation) section below for more details.
The `cyclone_artifact_scripts` subdirectory contains scripts for recreating the experimental data and analysis.
Instructions for running these scripts are given in the [Early Light Review](#early-light-review) section below.

The `paper_data.zip` archive contains the following.

```text
├── paper_data.zip
    ├── paper_data/
        ├── stats.csv
        ├── evaluation.csv
        ├── database/
```

The `dataase` directory contains the database of sloped graphs and CSV files comprising the analysis and experimental evaluation reported in our paper.
See the [Data used in our Paper](#the-data-used-in-our-paper) section below for more details.

## Zenodo Deposit

The artifact has been deposited on [Zenodo](https://zenodo.org/records/13990264), under the following DOI: [10.5281/zenodo.13990264](https://doi.org/10.5281/zenodo.13990264)

The zenodo deposit also contains two compressed Docker images, allowing the artifact to be run 'out of the box'.

* `cyclone_artifact-amd.tar.gz`
* `cyclone_artifact-arm.tar.gz`

These two Docker images have identical contents, and are built to support the x86_64/amd64 and arm64 architectures, respectively.

## *Cyclone* Implementation

The `cyclist` directory contains the source code for Cyclist together with the integration of our implementation of *Cyclone*.

The code of our implementation can be found in the `cyclist/src/generic` subdirectory, with the most relevant files being `cyclone.c`, `criterion.flat_cycles.c`, `criterion.descending_unicycles.c` and `criterion.trace_manifold.c`.

This code mirrors the contents of the `TACAS2025-Submission` tag of the [Cyclist github repository](https://github.com/cyclist-org/cyclist/releases/tag/TACAS2025-Submission).

## The Data used in our Paper

The `paper_data` directory contains:

* The sloped graphs database from our paper, in a `database` subdirectory.
* The `stats.csv` file that we used in our paper (as described in the [graph stats](#graphs_statssh) section).
* The `evaluation.csv` file that we used in our paper (as described in the [evaluate cyclone](#evaluate_cyclonesh) section).

## Reproducing our Results

### Docker Image

These instructions assume you have Docker installed and configured on your local system.

Extract and save to your local system the artifact Docker image file approrpiate to your system's architecture: `cyclone_artifact-amd.tar.gz` for x86_64/amd64 systems, or `cyclone_artifact-arm.tar.gz` for arm64 systems (e.g. Apple Mac).

Run the following command from the location in which you have saved the artifact Docker image file:

```bash
docker load -i <image-file>
```

where `<image-file>` is replaced by either `cyclone_artifact-amd.tar.gz` or `cyclone_artifact-arm.tar.gz`, as appropriate.

This will load the Docker image onto your system.
You can then spin up a new container (i.e. a running instance) based on the image, and attach it to your current terminal, by running one of the following commands as appropriate for the docker image you have are using.

```bash
docker run -ti cyclone_artifact:amd
```

or

```bash
docker run -ti cyclone_artifact:arm
```

The files in the docker image are split into the following directory tree:

```text
/
├── home
    ├── cyclist/
    ├── scripts/
        ├── generate_database.sh
        ├── graphs_stats.sh
        ├── evaluate_cyclone.sh
        ├── create_figures.py
```

The `scripts` directory contains scripts for generating all of the data that we used in our paper and our results.

You can detach from and suspend the container by sending the EXIT signal (using the key combination CTL+D).

#### Determining the Name/ID of the Docker Container

You can determine the unique ID and name of the container by running the following command on your local machine where the container is running/suspended (i.e. **not** within a terminal that is *attached* to the running container itself).

```bash
docker ps -a
```

This will list all of the containers (currently running or not) on your system.
You should look for the container that is associated with the `cyclone_artifact` image.
For example:

```text
CONTAINER ID   IMAGE                  COMMAND                  CREATED             STATUS             PORTS     NAMES
beaf952628b9   cyclone_artifact:amd   "opam exec -- /bin/s…"   About an hour ago   Up About an hour             hardcore_bohr
```

In the above example, the container has the ID `beaf952628b9`, and the name `hardcore_bohr`.

#### Restarting an Existing Container

You can use this name or ID to restart the container again.

To restart an existing, suspended container, and attach the terminal to it at the same time, run the following commands, where `<container-id-or-name>` is replaced by the ID or name of the container.

```bash
docker restart <container-id-or-name>
docker attach <container-id-or-name>
```

### Early Light Review

An early light review of the artifact can be performed by running the following four scripts, in order.

#### `generate_database.sh`

This script generates a database of sloped graphs, storing it in the directory `/home/database` which contains two directories, one for each logic: `fo` and `sl`.
Each of these directories contains `.json` files, one for each sloped graph that was generated.
It can be run with the following command.

```bash
/home/scripts/generate_database.sh
```

This script may take around 23 minutes to run.

Note that because Cyclist times out while searching for a proof for some of the test cases in the test suite, depending on the performance of your system (and whether you are running Cyclist in a Docker container or not) the amount of graphs in the database can differ slightly from the amount that we report in the paper.
However, the proof search and naming scheme for the sloped graph files is deterministic.
This means that any file generated by the script with the same name as a file contained in the `paper_data/database` directory will contain the same sloped graph representation.

The timeout is currently set at 30 seconds.
You can change this by editing the `generate_database.sh` script, in which you will find a line

```bash
TIMEOUT=30
```

To modify the timeout, simply change the number assigned to the `TIMEOUT` environment variable.
The number is interpreted as the number of seconds.
Changing the timeout will affect how long the script takes to run.
The figure of 23 mintes given above is based on a timeout of 30 seconds.

#### `graphs_stats.sh`

This script generates a CSV file, `/home/stats.csv`, containing metrics of the sloped graph database.
This script should be run after the [`generate_database.sh`](#generate_databasesh) script, as it uses the generated sloped graphs.

It can be run with the following command.

```bash
/home/scripts/graphs_stats.sh
```

This script may take around 6 minutes to run.

The following statistics for each sloped graph in the database are generated:

* The sloped graph filename,
* The metrics described in section 3 in the paper,
* The runtime and the answer of each incomplete method,
* Other information described in the CSV header.

#### `evaluate_cyclone.sh`

This script creates a CSV file, `/home/evaluation.csv`, containing the runtime evaluation of *Cyclone*.
This script should be run after both the [`generate_database.sh`](#generate_databasesh) and the[`graphs_stats.sh`](#graphs_statssh) scripts, as it uses the sloped graphs in the database and some of their calculated metrics.

It can be run with the following command.

```bash
/home/scripts/evaluate_cyclone.sh
```

This script may take around 25 minutes to run.

Note that, by default, this script excludes the SLA method, since this takes several hours to run.
See the section below on [customing the `evaluate_cyclone.sh` script](#customising-evaluate_cyclonesh) for details about how to generate data for the SLA method as well.

The following information is given for each sloped graph in the database.

* The name of the file containing the sloped graph,
* The runtimes of each complete method presented in the paper,
* The number of edges.

#### `create_figures.py`

This script generates graphs using the CSV files generated by the previous scripts.
It can be run with the following command.

```bash
python3 /home/scripts/create_figures.py
```

This script may take around 15 seconds to run.

It generates the following files, in the `/home/figures` directory, corresponding to the respectively numbered figures in the main body of the submitted paper:

* `figure3a.png`
* `figure3b.png`
* `figure3c.png`
* `figure5.png`
* `figure7a.png`
* `figure7b.png`
* `figure7c.png`
* `figure8a.png`
* `figure8b.png`
* `figure9a.png`
* `figure9b.png`
* `figure9c.png`

It also generates the following figures, related to the Trace Manifold condition, included in the appendix of the submitted paper.

* `figure12a.png`
* `figure12b.png`
* `figure12c.png`
* `figure12d.png`
* `figure13a.png`
* `figure13b.png`
* `figure13c.png`
* `figure14a.png`
* `figure14b.png`
* `figure14c.png`

In addition, it also generates the following graphs, comparing the performance of the Trace Manifold algorithm with the order-reduced Ramsey-theoretic algorithm.

* `OR_overhead_percentage_over_TM.png`
* `TM_vs_OR.png`

We explain these two figures further in the section on the [`stats.csv` file](#the-statscsv-file), below.

To view these graphs, you can copy them from the Docker container to your local system.
First, determine the name or ID of the contains (see [section above](#determining-the-nameid-of-the-docker-container) section above).
Then run the following command, where `<container-id-or-name>` is replaced by the name or ID of your container.

```bash
docker container cp <container-id-or-name>:/home/figures .
```

This will copy the entire `figures` directory, containing all the PNG files, to the current directory in which you ran the command.

## Instructions for Further Review and Use of the Artifact

We now give some further information and instructions that should allow a more detailed review of the artifact and the results of our evaluation.

### The `stats.csv` File

Claims we have made in the paper about the database, and the coverage and performance of the various incomplete methods, can be verified by examining the data in the `stats.csv` file, generated by the `graphs_stats.sh` script described in the [section above](#graphs_statssh).

This file contains a row for every sloped graph in the database, and the information for each sloped graph is separated into the following columns:

* **test suite**: containing the string `fo` if the graph came from the first order logic test suite or `sl` if it came from the separation logic test suite.
* **graph name**: containing the name of the file in the database containing the graph. The file names adhere to the format `<test_name>.graph-<index>` with `<index>` being the ID of the sloped graph that Cyclist produced while trying to prove the sequent in `<test_name>`. The IDs are assigned by Cyclist according to the order that they were generated during proof search.
* **width**: containing the vertex width of the sloped graph (i.e. the maximum number of positions occurring in any node).
* **buds**: containing the number of buds in the sloped graph.
* **edges**: containing the number of edges in the sloped graph.
* **nodes**: containing the number of nodes in the sloped graph.
* **amount of backedges**: containing the number of backedges in the sloped graph.
* **amount of trace manifold graph edges**: containing the number of edges in the trace manifold graph of the sloped graph.
* **amount of trace manifold graph nodes**: containing the number of nodes in the trace manifold graph of the sloped graph.
* **size of structural connectivity relation**: containing the number of related pairs of buds in the structural connectivity relation of the sloped graph.
* **amount of positions in all cycles**: containing the sum of the number of positions in all nodes that are part of any cycle in the sloped graph.
* **amount of SCC**: containing the number of strongly connected components in the sloped graph.
* **has overlapping cycles**: containing `yes` if the sloped graph has overlapping cycles and `no` otherwise.
* **is in cycle normal form**: containing `yes` if the sloped graph is in cycle normal form and `no` otherwise.
* **FC duration microseconds**: containing the number of microseconds that the Flat Cycles algorithm took to run on the sloped graph
* **FC answer:** containing `no` if the Flat Cycles algorithm returned that the sloped graph does not satisfy Infinite Descent and `don't know` otherwise.
* **DU duration microseconds**: containing the number of microseconds that the Descending Unicycles algorithm took to run on the sloped graph
* **DU answer**: containing `no` if the Descending Unicycles algorithm returned that the sloped graph does not satisfy Infinite Descent, `yes` if it does and `don't know` otherwise.
* **TM duration microseconds**: containing the number of microseconds that the Trace Manifold algorithm took to run on the sloped graph
* **TM answer**: containing `yes` if the Trace Manifold algorithm returned that the sloped graph satisfies Infinite Descent and `don't know` otherwise.
* **OR duration microseconds**: containing the number of microseconds that the Order Reduced Transitive Closure algorithm took to run on the sloped graph.
* **OR answer**: containing `yes` if the Order Reduced Transitive Closure algorithm returned that the sloped graph does satisfies Infinite Descent and `no` otherwise.

The data that we reported in the paper comes from the `stats.csv` file in the `paper_data` directory of the artifact archive.
You can copy the `stats.csv` generated by the Docker image to your local file system with the following command.

```bash
docker container cp <container-id-or-name>:/home/stats.csv .
```

You can then load the CSV file into a spreadsheet for further analysis, e.g. to sort by different columns and examine the coverage of the various methods.

The `stats.csv` file contains data on the runtimes of each of the incomplete methods, and the order-reduced (OR) Ramsey-theoretic complete method. In the paper, we report on the relative performance of the Flat Cycles (FC) and Descending Unicycles (DU) methods compared to the OR method (see figure 8b).
We do not report a similar comparison between OR and the Trace Manifold (TM) algorithm, as this does not form a part of the pipeline implemented within our Cyclone tool.
However, the plots contained in `OR_overhead_percentage_over_TM.png` and `TM_vs_OR.png`, generated by the [`create_figures.py` script](#create_figurespy) and based on the data in the `stats.csv` file, show the performance of TM compared with OR.

### The `evaluation.csv` File

The `evaluation.csv` file contains the runtime in milliseconds that every complete method took for every sloped graph in the database.
As in the [`stats.csv`](#the-statscsv-file), the file contains columns for the name of the sloped graph and the number of its edges.
The columns `CY`, `OR`, `FWK`, `VLA`, `SLA` contain the runtime in milliseconds of Cyclone, the Order Reduced, the Floyd-Warshall-Kleene, the Vertex Language Automata, and the Sloped Language Automata methods, respectively.

Note that if you simply ran the script without any command line arguments, as described in the [`evaluate_cyclone.sh`](#evaluate_cyclonesh) section above:

```bash
/home/scripts/evaluate_cyclone.sh
```

then the `evaluate_cyclone.csv` file will not have a column for SLA, as the script does not run that method by default.

We now desrcibe how to generate the column for the SLA method.

#### Customising `evaluate_cyclone.sh`

It is possible to add `--method <method name>` flags, with `<method name>` being one of the following: `VLA`, `SLA`, `FWK`, `OR`, indicating which methods to run in the evaluation in addition to Cyclone.
If you wish to run all of the methods, run the following command:

```bash
/home/scripts/evaluate_cyclone.sh --method "OR" --method "FWK" --method "VLA" --method "SLA"
```

Note that with the SLA method the script can take several hours to run.

### Validating Individual Sloped Graphs

You can use Cyclist to check if (one or more) indivdual sloped graphs satisfy infinite descent, using the standalone `checkproof` utility that is part of Cyclist.
This utility allows you to specify the files containing the sloped graphs to check, and to choose the (complete) method to use for the check.

From the root of the Cyclist source directory, run the following command.

```bash
dune exec src/generic/checkproof.exe -- <method> -R json -f <graph-file>
```

Where `<graph-file>` is the path of the file containing the JSON representation of the graph that you want to check, and `<method>` specifies which method to use to check for Infinite Descent.
See the [section below](#the-json-format-for-sloped-graphs) for more details on the JSON representation format used by Cyclist/Cyclone.
The `<methdod>` command line arguments can be one of the following:

* `-VLA`:
    Use the VLA automata-theoretic method.
* `-SLA`:
    Use the SLA automata-theoretic method.
* `-FWK`:
    Use the Floyd-Warshall-Kleene Ramsey-theoretic method.
* `-OR -min -scc -ff`:
    Use the order-reduced Ramsey-theoretic method.
    The additional flags `-min`, `-scc`, `-ff` should be used for best performance here, but the method will work without them.
* `-CY`:
    Use Cyclone.

Cyclist will output either `OK` or `NOT OK`, indicating whether the graph satisfies Infinite Descent or not.

You can actually specify more than one file, using multiple `-f` command line arguments, and Cyclist will check them all in order, outputting the results on successive lines of the standard output.

#### Using the Incomplete Methods to Check Individual Sloped Graphs

The cyclist source code also contains a standalone C++ program that can be used to invoke the incomplete methods for checking Infinite Descent on individual files containing the JSON representation of sloped graphs.

This program can be found in the `src/generic/test` subdirectory of the Cyclist source code.

If you navigate to this directory, you can compile the program using the following command.

```bash
make soundness
```

This will compile an executable called `soundness` that can be run from the command line using the following command.

```bash
./soundness <graph-file> <method>
```

Where `<graph-file>` is the name of the file containing the JSON representation of the sloped graph to be checked, and `<method>` specified which method to use to check for Infinite Descent.
The program is hard-coded to look for files in the `data` subdirectory with the extension `.json`, so you should give just the base name of the file, which must be present in the `data` subdirectory.

The `<method>` command line argument can be one of the following.

* `L`:
    Use the flat cycles method.
* `D`:
    Use the descending unicycles method
* `M`:
    Use the trace manifold method.

So, for example if you run the following command

```bash
./soundness graph_1 D
```

The program will check the graph described in the `data/graph_1.json` file using the Descending Unicycles method.

The program will outout `SOUND` if the graph satisfies Infinite Descent, `UNSOUND` if the graph does **not** satisfy Infinite Desscent, or `UNKNOWN` if the incomplete method could not determine whether or not Infinite Descent was satisfied.

This program can also invoke the complete methods to check for Infinite Descent, by using the following for the `<method>` command line argument.

* `V`:
    The VLA automata-theoretic method.
* `S`:
    The SLA automata-theoretic method.
* `F`:
    The Floyd-Warshall-Kleene Ramsey-theoretic method.
* `Omsf`:
    The order-reduced Ramsey-theoretic method (the `msf` corresponds to the `-min`, `-scc`, and `-ff` flags that were mentioned above; again, `msf` should be included for best performance but omitting it will still work).
* `C`:
    Use Cyclone.

### The JSON Format for Sloped Graphs

Our implementation of Cyclone and modification of Cyclist uses a JSON representation for sloped graphs.

We now give the details of this representation, so that you can modify the existing example or create new examples.

In the JSON representation, sloped graphs are JSON objects containing the following four fields: `"Node"`, `"Height"`, `"Bud"`, and `"Edge"`. The value of each of these fields should be an array.

We now describe each of these fields in detail.

* `"Node"`:
    The value of this field is an array containing integers, which are the IDs for the nodes, or vertices, of the sloped graph.
* `"Height"`:
    The value of this field is an array containing integers, which denote the positions associated with nodes of the sloped graphs. Positions were referred to as "heights" in historical (pre-publication) formulations of the sloped graph formalism. Thus, for legacy reasons, these are still called "heights" in the implementation.
* `"Bud"`:
    The value of this field is an array of integers, identifying the IDs of the nodes in the sloped graph that are buds.
    A sloped graph is permitted to have an empty array of buds.
* `"Edge"`:
    The value of this field is an array whose elements are arrays, each of which describes one edge in the sloped graph.
    The arrays describing an edge have exactly two elements, both of which are again arrays:
    1. The first array represents the edge itself: it has exactly two elements, which are integers, being the ID of the source node and the ID of the destination/target node of the edge, respectively.
    2. The second array represents the sloped relation along the edge. It has arrays as its elements, themselves each containing exactly three integer elements:
        1. the ID of a position in source node of the edge,
        2. the ID of a position in the target node of the edge, and then
        3. either the integer `0` or the integer `1` denoting a flat slope and a downward slope, respectively, connecting these positions.

The `src/generic/test/data` subdirectory of the Cyclist source code contains a number of sloped graphs in this JSON format that were hand-crafted to test out the implementations of the various methods for deciding Infinite Descent.

For example, the file `graph_3.json` contains the following sloped graph representation.

```json
{
    "Node" : [
        [0, [0]],
        [1, [1]]
    ],
    "Bud" : [1],
    "Edge" : [
        [[0,1],[[0,1,1]]],
        [[1,0],[[1,0,0]]]
    ],
    "Height" : [1,0]
}
```

This represents a sloped graph containing two nodes, with IDs `0` and `1`, respectively.
The positions in the graph have IDs `0` and `1`, with the position `0` belonging to the node `0`, and the position `1` belonging to the node `1`.
The sloped graph has two edges: one from node `0` to node `1`, and the other in the opposite direction from node `1` to node `0`.
The graph thus has a cyle.
Node `1` is identified as a bud node.
The slope along the edge from node `0` to node `1`, connects position `0` to position `1` with a downward slope.
The slope along the backlink from node `1` to node `0`, connects position `1` to position `0` with a flat slope.

### Using Cyclone as a Backend for Cyclist

The various provers implemented in Cyclist allow the user to choose which backend method is used to check Infinite Descent during proof search.
We have integrated Cyclone into Cyclist so that it can be used as a backend check by passing the command line flag `-CY` to Cyclist.

Cyclist contains a substantial test suite of benchmark examples for the various different provers that it implements, and these can be found in the `benchmarks` subdirectory of the Cyclist source code. This contains various further subdirectories for the different logics and provers.
For example, the benchmarks for the first-order logic prover are in the `fo` subdirectory, and those for the Separation Logic benchmarks are in the `sl` subdirectory.

To run Cyclist's first-order logic prover, you can use the following command from the root of the Cyclist source code.

```bash
dune exec src/firstorder/prove.exe -- -D examples/fo.defs <method> -S "<sequent>"
```

where `<method>` is the backend Infinite Descent check to be used, and `<sequent>` is the first-order logic entailment to be proved.

To use Cyclone, specify `-CY` as the method. The other options are:

* `-VLA`: the VLA automata-theoretic method.
* `-SLA`: the SLA automata-theoretic method.
* `-FWK`: the Floyd-Warshall-Kleene Ramsey-theoretic method.
* `-OR`: the order-reduced Ramsey-theoretic method. For this method, you should also pass the flags `-min`, `-scc`, and `-ff` for best performance (although the OR backend check will still work without them).

You can obtain sequents from the benchmark test files in the `benchmarks/fo` subdirectory of the Cyclist source code.

The `-D <file>` command line argument specifies where Cyclist should look for the definitions of inductive predicates that are used in the sequent to the proved.

If you also pass the `-p` command line option to Cyclist, then it will output the proof that it finds.

If you pass the `-s` command line option to Cyclist, then it will also output various statistics about the proof search process.

To run Cyclist's Separation logic prover, you can use the following command from the root of the Cyclist source code.

```bash
dune exec src/seplog/prove.exe -- -D examples/sl.defs <method> -S "<sequent>"
```

The command line options are the same as described above.

Cyclist's own Separation Logic benchmark test suite examples can be found in the `benchmarks/sl/base` subdirectory of the Cyclist source code.
You can find Separation Logic entailment sequents in the various `.tst` files contained therein.

The Separation logic benchmark suite also contains some test cases from the Songbird prover and from the ATVA 2014 paper by Iosif Et Al in the subdirectories `benchmarks/sl/songbird` and `benchmarks/sl/atva-2014`, respectively.
If you want to run Cyclist on the entailments found in the test files in these directories, you need to specify different `.defs` files containing the appropriate inductive predicate definitions.
The definitions for the Songbird examples are in the file `examples/songbird.defs`, and the definitions for the ATVA 2014 examples are in the file `examples/IosifEtAl-ATVA2014.defs`.

```bash
dune exec src/seplog/prove.exe -- -D examples/songbird.defs <method> -S "<sequent>"
dune exec src/seplog/prove.exe -- -D examples/IosifEtAl-ATVA2014.defs <method> -S "<sequent>"
```

### Building Cyclist/Cyclone Manually

You can build Cyclist and Cyclone manually if you have `pkg-config`, OCaml/OPAM, and the Spot model checking library installed on your system.

The `pkg-config` package usually comes on Linux distributions as standard, but we mention it here as it is an explicit dependency of Cyclist.
More information about `pkg-config` can be found [here](https://www.freedesktop.org/wiki/Software/pkg-config/).

Installation instructions for OCaml/OPAM can be found [here](https://ocaml.org/install).
Currently, we have tested Cyclist/Cyclone with OCaml version 4.14.
We are not sure if Cyclist/Cyclone will compile with later 5.x versions of OCaml, due to possible incompatibilities with the latest version of OCaml's standard library.
Once you have installed OCaml/OPAM, you can ensure that you are running version 4.14 by executing the following command.

```bash
opam switch create 4.14.2 
```

Installation instructions for the Spot model-checking library can be found [here](https://spot.lre.epita.fr/install.html).
Cyclist requires Spot to be installed in such a way that it is visible to `pkg-config`.
The version of Spot that has been used in our experimental evaluation is 2.11.6.
However, later versions of Spot should also work.

Once the above dependencies are installed, Please clone the Cyclist source code from its Github page: [https://github.com/cyclist-org/cyclist](https://github.com/cyclist-org/cyclist).

You should then check out the `TACAS2025-Submission` tag.

```bash
git checkout TACAS2025-Submission
```

You can then make sure that all of the necessary OCaml packages for building Cyclist are installed by running the following command.

```bash
opam install -y --deps-only ./cyclist.opam
```

Once the dependencies have successfully installed, you can build Cyclist/Cyclone using dune by executing the following command.

```bash
dune build
```
