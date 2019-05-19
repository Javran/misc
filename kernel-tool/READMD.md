# kernel-tool

For simplifying maintenance work for kernel updates.

## TODO

Planned features:

- `kernel-tool switch`: switch to latest version with `eselect`,
  copy over config the current system is running on, and change directory to `/usr/src/linux`
- `kernel-tool install`: assume that kernel has been built,
  follow up with `make install && make modules-install && emerge @module-rebuild`
  and approriate update to `grub.conf` with a template.
- `kernel-tool clean`: remove (`mv` to `/tmp` for safety) all kernels lower than current one,
  might provide an option to keep the closest-to-latest kernel version.

For simplicity, all required environments are read from environment variables.
