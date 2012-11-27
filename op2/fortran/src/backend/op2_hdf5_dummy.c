#include <op_lib_core.h>

op_set op_decl_set_hdf5(char const *file, char const *name) {
  printf ("HDF5 calls not supported for this back-end\n");
  exit (0);
}

op_map op_decl_map_hdf5(op_set from, op_set to, int dim, char const *file, char const *name) {
  printf ("HDF5 calls not supported for this back-end\n");
  exit (0);
}

op_dat op_decl_dat_hdf5(op_set set, int dim, char const *type, char const *file, char const *name) {
  printf ("HDF5 calls not supported for this back-end\n");
  exit (0);
}
