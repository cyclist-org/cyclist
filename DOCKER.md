# Building Docker Images of Cyclist

This repostory contains two Docker configuration files for building Docker images of Cyclist.

* `Dockerfile`:
  the standard configuration
* `Dockerfile.POPL2024`:
  the configuration allowing to run the benchmarking tests reported in the
  POPL 2024 paper "The Complex(ity) Landscape of Checking Infinite Descent".

## The Standard Configuration

The standard configuration installs the Spot model checker using the Linux
distribution's package manager, and also downloads the Cyclist source code
respository directly from Github, using the master branch.

This can be built using the following command, where `<cyclist-src-path>`
indicates the location of the Cyclist source code repository on your system.

```[bash]
  docker build -t cyclist <cyclist-src-path>
```

This tags the image with the name `cyclist`, and it can then be run using

```[bash]
  docker run -ti cyclist
```

## POPL2024 Configuration

The `Dockerfile.POPL2024` file builds a configuration allowing to reproduce all
the tests reported in the POPL 2024 paper
[The Complex(ity) Landscape of Checking Infinite Descent](https://doi.org/10.1145/3632888).
Some of the tests require a non-standard version of Spot to be compiled and
installed.
This configuration file downloads and compiles the standard and required
non-standard versions of Spot from source, and installs them in the locations
specified in the dune-workspace.POPL2024 build file, so that they are available
to Cyclist's build and execution process.
It installs the same version of Spot (2.10.4) that was used to produce the test
results reported in the paper.

This configuration can be built using the following commmand, which should be
run from the root of the Cyclist source code repository, since it copies the
local source to the Docker image.

```[bash]
  docker build -t cyclist -f Dockerfile.POPL2024 .
```

This tags the image with the name `cyclist`, and it can then be run using

```[bash]
  docker run -ti cyclist
```

This configuration mirrors the one captured presented in the paper's associated
[Zenodo artifact](https://doi.org/10.5281/zenodo.10073582),
and the resulting container can be used to run the commands documented in the
artifact's `README.md` to reproduce the tests and associated log files.
