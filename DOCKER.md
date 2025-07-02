# Building Docker Images of Cyclist

This repostory contains a Docker configuration file for building Docker images of Cyclist.

This configuration installs the Spot model checker using the Linux
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
