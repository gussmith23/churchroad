# Churchroad Backend Plugin for Yosys

We provide a plugin
  for Yosys
  which produces Churchroad code.

To use the Churchroad plugin:

```sh
cd yosys-plugin/
make
yosys -m churchroad.so
```

## Note on Building

Yosys plugins are simply shared libraries
  that can be loaded into Yosys at runtime.
As a result,
  you are required to build
  a Yosys plugin using the same
  build environment
  as your `yosys` binary.
The easiest way to ensure this
  is to simply build Yosys
  from source.
If you prefer to use
  a prebuilt Yosys binary,
  Yosys provides a Docker image
  which sets up the appropriate
  environment:
  <https://github.com/YosysHQ-GmbH/tabby-cad-plugin-build>
We don't yet support
  using this option easily.
If you would like this feature,
  please open an issue.
Until then, all plugin users should build Yosys
  from source.
