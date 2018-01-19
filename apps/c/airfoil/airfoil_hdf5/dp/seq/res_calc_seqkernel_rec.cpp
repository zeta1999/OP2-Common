#include "op_lib_cpp.h"

// global constants
extern double gam;
extern double gm1;
extern double cfl;
extern double eps;
extern double mach;
extern double alpha;
extern double qinf[4];

// user function
#include "../res_calc.h"

// user function
extern "C" {
void op_par_loop_res_calc_rec_execute(op_kernel_descriptor *desc);

// host stub function
void op_par_loop_res_calc_rec_execute(op_kernel_descriptor *desc) {

  op_set set = desc->set;
  char const *name = desc->name;
  int nargs = 8;

  op_arg arg0 = desc->args[0];
  op_arg arg1 = desc->args[1];
  op_arg arg2 = desc->args[2];
  op_arg arg3 = desc->args[3];
  op_arg arg4 = desc->args[4];
  op_arg arg5 = desc->args[5];
  op_arg arg6 = desc->args[6];
  op_arg arg7 = desc->args[7];

  op_arg args[8] = {arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7};

  // initialise timers
  double cpu_t1, cpu_t2, wall_t1, wall_t2;
  op_timing_realloc(2);
  op_timers_core(&cpu_t1, &wall_t1);

  if (OP_diags > 2) {
    printf(" kernel routine with indirection: res_calc\n");
  }

  int set_size = op_mpi_halo_exchanges(set, nargs, args);

  if (set->size > 0) {

    for (int n = 0; n < set_size; n++) {
      if (n == set->core_size) {
        op_mpi_wait_all(nargs, args);
      }
      int map0idx = arg0.map_data[n * arg0.map->dim + 0];
      int map1idx = arg0.map_data[n * arg0.map->dim + 1];
      int map2idx = arg2.map_data[n * arg2.map->dim + 0];
      int map3idx = arg2.map_data[n * arg2.map->dim + 1];

      res_calc(&((double *)arg0.data)[2 * map0idx],
               &((double *)arg0.data)[2 * map1idx],
               &((double *)arg2.data)[4 * map2idx],
               &((double *)arg2.data)[4 * map3idx],
               &((double *)arg4.data)[1 * map2idx],
               &((double *)arg4.data)[1 * map3idx],
               &((double *)arg6.data)[4 * map2idx],
               &((double *)arg6.data)[4 * map3idx]);
    }
  }

  if (set_size == 0 || set_size == set->core_size) {
    op_mpi_wait_all(nargs, args);
  }
  // combine reduction data
  op_mpi_set_dirtybit(nargs, args);

  // update kernel record
  op_timers_core(&cpu_t2, &wall_t2);
  OP_kernels[2].name = name;
  OP_kernels[2].count += 1;
  OP_kernels[2].time += wall_t2 - wall_t1;
  OP_kernels[2].transfer += (float)set->size * arg0.size;
  OP_kernels[2].transfer += (float)set->size * arg2.size;
  OP_kernels[2].transfer += (float)set->size * arg4.size;
  OP_kernels[2].transfer += (float)set->size * arg6.size * 2.0f;
  OP_kernels[2].transfer += (float)set->size * arg0.map->dim * 4.0f;
  OP_kernels[2].transfer += (float)set->size * arg2.map->dim * 4.0f;
}
}