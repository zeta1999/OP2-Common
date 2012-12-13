// FORTRAN reference does not support HDF5 for the time being
op_set op_decl_set_hdf5(char const *file, char const *name) { return NULL; }
op_map op_decl_map_hdf5(op_set from, op_set to, int dim, char const *file, char const *name) { return NULL; }
op_dat op_decl_dat_hdf5(op_set set, int dim, char const *type, char const *file, char const *name) { return NULL; }
void op_get_const_hdf5(char const *name, int dim, char const *type, char* const_data,
  char const *file_name) {}
void op_write_hdf5(char const * file_name) {}
void op_write_const_hdf5(char const *name, int dim, char const *type, char* const_data,
  char const *file_name) {}
