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
        ├── create_figures.sh
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

To view these graphs, you can copy them from the Docker container to your local system.
First, determine the name or ID of the contains (see [section above](#determining-the-nameid-of-the-docker-container) section above).
Then run the following command, where `<container-id-or-name>` is replaced by the name or ID of your container.

```bash
docker container cp <container-id-or-name>:/home/figures/figure3a.png .
```

This will copy the `figure3a.png` file to the current directory in which you ran the command.
Note that you have to copy each file individually; you cannot use glob patterns in the filename.

## Instructions for Further Review and Use of the Artifact
