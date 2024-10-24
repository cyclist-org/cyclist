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
├── cyclist/
    ├── LICENSE.md
    ├── cyclone_artifact_scripts/
        ├── cyclone.evaluate_cyclone.sh
        ├── cyclone.generate_database.sh
        ├── cyclone.graph_stats.sh
        ├── create_figures.sh
    ...
├── paper_data/
    ├── stats.csv
    ├── evaluation.csv
    ├── database/
```

The current file is `README.md`.

The `LICENSE.md` contains the 3-clause-BSD license under which the Cyclist theorem prover framework is distributed.

The `paper_data` directory contains the database of sloped graphs and CSV files comprising the analysis and experimental evaluation reported in our paper.
See the [Data used in our Paper](#the-data-used-in-our-paper) section below for more details.

The `cyclist` directory contains the source code for the Cyclist theorem prover framework, into which we have incorporated the implementation of our tool, Cyclone.
See the [*Cyclone* Implementation](#cyclone-implementation) section below for more details.
The `cyclone_artifact_scripts` subdirectory contains scripts for recreating the experimental data and analysis.
Instructions for running these scripts are given in the [Early Light Review](#early-light-review) section below.

## Zenodo Deposit

The artifact has been deposited on [Zenodo](XXXX), under the following DOI: XXXX

The zenodo deposit also contains a compressed Docker image, `cyclone-artifact.tar.gz`, allowing the artifact to be run 'out of the box'.

## *Cyclone* Implementation

The `cyclist` directory contains the source code for Cyclist together with the integration of our implementation of *Cyclone*.

The code of our implementation can be found in the `cyclist/src/generic` subdirectory, with the most relevant files being `cyclone.c`, `criterion.flat_cycles.c`, `criterion.descending_unicycles.c` and `criterion.trace_manifold.c`.

This code mirrors the contents of the `TACAS2025-Submission` tag of the [Cyclist github repository](https://github.com/cyclist-org/cyclist/releases/tag/TACAS2025-Submission).

## The Data used in our Paper

The `paper_data` directory contains:

* The sloped graphs database from our paper, in a `database` subdirectory.
* The `stats.csv` file that we used in our paper (as described in the [graph stats](#graph_statssh) section).
* The `evaluation.csv` file that we used in our paper (as described in the [evaluate cyclone](#evaluate_cyclonesh) section).

## Reproducing our Results

### Docker Image

These instructions assume you have Docker installed and configured on your local system.

Extract and save the artifact Docker image file `cyclone-artifact.tar.gz` to your local system.

Run the following command from the location in which you have saved the artifact Docker image file:

```bash
docker load -i cyclone-artifact.tar.gz
```

This will load the Docker image onto your system.
You can then spin up a new container (i.e. a running instance) based on the image, and attach it to your current terminal, by running the following command.

```bash
docker run -ti cyclone-artifact:latest
```

The files in the docker image are split into the following directory tree:

```text
/
├── home
    ├── cyclist/
    ├── scripts/
        ├── generate_database.sh
        ├── graph_stats.sh
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
You should look for the container that is associated with the `cyclone-artifact` image.
For example:

```text
CONTAINER ID   IMAGE                     COMMAND                  CREATED             STATUS             PORTS     NAMES
beaf952628b9   cyclone-artifact:latest   "opam exec -- /bin/s…"   About an hour ago   Up About an hour             hardcore_bohr
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

Note that because Cyclist times out while searching for a proof for some of the test cases in the test suite, the amount of graphs in the database can differ slightly from the amount that we report in the paper.
However, the proof search and naming scheme for the sloped graph files is deterministic.
This means that any file generated by the script with the same name as a file contained in the `paper_data/database` directory will contain the same sloped graph representation.

#### `graph_stats.sh`

This script generates a CSV file, `/home/stats.csv`, containing metrics of the sloped graph database.
This script should be run after the [`generate_database.sh`](#generate_databasesh) script, as it uses the generated sloped graphs.

It can be run with the following command.

```bash
/home/scripts/graph_stats.sh
```

This script may take around 6 minutes to run.

The following statistics for each sloped graph in the database are generated:

* The sloped graph filename,
* The metrics described in section 3 in the paper,
* The runtime and the answer of each incomplete method,
* Other information described in the CSV header.

#### `evaluate_cyclone.sh`

This script creates a CSV file, `/home/evaluation.csv`, containing the runtime evaluation of *Cyclone*.
This script should be run after both the [`generate_database.sh`](#generate_databasesh) and the[`graph_stats.sh`](#graph_statssh) scripts, as it uses the sloped graphs in the database and some of their calculated metrics.

It can be run with the following command.

```bash
/home/scripts/evaluate_cyclone.sh
```

This script may take around 25 minutes to run.

The following information is given for each sloped graph in the database.

* The name of the file containing the sloped graph,
* The runtimes of each complete method presented in the paper,
* The number of edges.

By default, it does not include the SLA method, as this can take several hours.

It is possible to add `--method <method name>` flags, with `<method name>` being one of the following: `VLA`, `SLA`, `FWK`, `OR`, indicating which methods to run in the evaluation.
If you wish to run all of the methods, run the following command:

```bash
/home/scripts/evaluate_cyclone.sh --method "OR" --method "FWK" --method "VLA" --method "SLA"
```

#### `create_figures.py`

This script generates graphs using the CSV files generated by the previous scripts.
It can be run with the following command.

```bash
python3 /home/scripts/create_figures.py
```

This script may take around 11 seconds to run.

It generates the following files, in the `/home/figures` directory, corresponding to the respectively numbered figures in the submitted paper:

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
* `OR_overhead_percentage_over_TM.png`
* `TM_vs_OR.png`


To view these graphs, you can copy them from the Docker container to your local system.
First, determine the name or ID of the contains (see [section above](#determining-the-nameid-of-the-docker-container) section above).
Then run the following command, where `<container-id-or-name>` is replaced by the name or ID of your container.

```bash
docker container cp <container-id-or-name>:/home/figures .
```

This will copy the entire `figures` directory, containing all the PNG files, to the current directory in which you ran the command.

## Instructions for Further Review and Use of the Artifact

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

### Building Cyclist/Cyclone Manually

You can build Cyclist and Cyclone manually if you have OCaml/OPAM, `pkg-config`, and the Spot model checking library installed on your system.

Installation instructions for OCaml/OPAM can be found [here](https://ocaml.org/install).

The `pkg-config` package usually comes on Linux distributions as standard, but we mention it here as it is an explicit dependency of Cyclist.
More information about `pkg-config` can be found [here](https://www.freedesktop.org/wiki/Software/pkg-config/).

Installation instructions for the Spot model-checking library can be found [here](https://spot.lre.epita.fr/install.html).
Cyclist requires Spot to be installed in such a way that it is visible to `pkg-config`.

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
