//
// auto-generated by op2.py
//

//user function
#include "../bres_calc.h"

// host stub function
void op_par_loop_bres_calc(char const *name, op_set set,
  op_arg arg0,
  op_arg arg1,
  op_arg arg2,
  op_arg arg3,
  op_arg arg4,
  op_arg arg5){

  int nargs = 6;
  op_arg args[6];

  args[0] = arg0;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  args[5] = arg5;

  // initialise timers
  double cpu_t1, cpu_t2, wall_t1, wall_t2;
  op_timing_realloc(3);
  op_timers_core(&cpu_t1, &wall_t1);

  if (OP_diags>2) {
    printf(" kernel routine with indirection: bres_calc\n");
  }

  int set_size = op_mpi_halo_exchanges(set, nargs, args);
     int my_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  if (set->size >0) {

    op_map prime_map = arg4.map; //TODO works only with arg4...
    op_reversed_map rev_map = OP_reversed_map_list[prime_map->index];

    if (rev_map != NULL) {

        int prime_map_dim = prime_map->dim;
        int set_from_size = prime_map->from->size + prime_map->from->exec_size;
        int set_to_size = prime_map->to->size + prime_map->to->exec_size;// + prime_map->to->nonexec_size;

        int required_tmp_incs_size = set_from_size * prime_map_dim * arg4.dat->size;
        
        if (op_repr_incs[arg4.dat->index].tmp_incs == NULL){
            op_repr_incs[arg4.dat->index].tmp_incs = (void *)op_malloc(required_tmp_incs_size);
            op_repr_incs[arg4.dat->index].tmp_incs_size = required_tmp_incs_size;
        } else if (op_repr_incs[arg4.dat->index].tmp_incs_size < required_tmp_incs_size){
            op_realloc(op_repr_incs[arg4.dat->index].tmp_incs, required_tmp_incs_size);
            op_repr_incs[arg4.dat->index].tmp_incs_size = required_tmp_incs_size;
        }
        
        double *tmp_incs = (double *)op_repr_incs[arg4.dat->index].tmp_incs;


        for (int i=0; i<set_from_size * prime_map_dim * arg4.dim; i++){
          tmp_incs[i]=0;
        }

        op_mpi_wait_all(nargs, args);

    //    for ( int i=0; i<set_size; i++ ){
        for (int c=0; c<rev_map->number_of_colors;c++){
            for ( int i=rev_map->color_based_exec_row_starts[c]; i<rev_map->color_based_exec_row_starts[c+1]; i++ ){
              int n=rev_map->color_based_exec[i];
        //      printf("%d;%d;%d;%d\n",n,OP_set_global_ids_list[prime_map->from->index]->global_ids[n],c,my_rank);
  /*            if (n==set->core_size) {
                op_mpi_wait_all(nargs, args);
              }*/
             // int n=rev_map->from_elements_sorted_by_global_id[i];
              int map0idx = arg0.map_data[n * arg0.map->dim + 0];
              int map1idx = arg0.map_data[n * arg0.map->dim + 1];
              int map2idx = arg2.map_data[n * arg2.map->dim + 0];


              bres_calc(
                &((double*)arg0.data)[2 * map0idx],
                &((double*)arg0.data)[2 * map1idx],
                &((double*)arg2.data)[4 * map2idx],
                &((double*)arg3.data)[1 * map2idx],
                &((double*)arg4.data)[4 * map2idx],
                //&tmp_incs[(n*prime_map_dim+0)*arg4.dim],
                &((int*)arg5.data)[1 * n]);
            }
        }


        for ( int n=0; n<set_to_size; n++ ){
            for ( int i=0; i<rev_map->row_start_idx[n+1] - rev_map->row_start_idx[n]; i++){
                for (int d=0; d<arg4.dim; d++){
                    ((double*)arg4.data)[arg4.dim * n + d] +=
                    tmp_incs[rev_map->reversed_map[rev_map->row_start_idx[n]+i] * arg4.dim + d];
                }
            }
        }



    }

  }

  if (set_size == 0 || set_size == set->core_size) {
    op_mpi_wait_all(nargs, args);
  }
  // combine reduction data
  op_mpi_set_dirtybit(nargs, args);

  // update kernel record
  op_timers_core(&cpu_t2, &wall_t2);
  OP_kernels[3].name      = name;
  OP_kernels[3].count    += 1;
  OP_kernels[3].time     += wall_t2 - wall_t1;
  OP_kernels[3].transfer += (float)set->size * arg0.size;
  OP_kernels[3].transfer += (float)set->size * arg2.size;
  OP_kernels[3].transfer += (float)set->size * arg3.size;
  OP_kernels[3].transfer += (float)set->size * arg4.size * 2.0f;
  OP_kernels[3].transfer += (float)set->size * arg5.size;
  OP_kernels[3].transfer += (float)set->size * arg0.map->dim * 4.0f;
  OP_kernels[3].transfer += (float)set->size * arg2.map->dim * 4.0f;
}
