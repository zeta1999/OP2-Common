//
// auto-generated by op2.py
//

#include "../update.h"
//user function
__device__ void update_gpu( double *data, int *count) {
  data[0] = 0.0;
  (*count)++;
}

// CUDA kernel function
__global__ void op_cuda_update(
  double *arg0,
  int *arg1,
  int   set_size ) {

  int arg1_l[1];
  for ( int d=0; d<1; d++ ){
    arg1_l[d]=ZERO_int;
  }

  //process set elements
  for ( int n=threadIdx.x+blockIdx.x*blockDim.x; n<set_size; n+=blockDim.x*gridDim.x ){

    //user-supplied kernel call
    update_gpu(arg0+n*4,
           arg1_l);
  }

  //global reductions

  for ( int d=0; d<1; d++ ){
    op_reduction<OP_INC>(&arg1[d+blockIdx.x*1],arg1_l[d]);
  }
}


//host stub function
void op_par_loop_update(char const *name, op_set set,
  op_arg arg0,
  op_arg arg1){

  int*arg1h = (int *)arg1.data;
  int nargs = 2;
  op_arg args[2];

  args[0] = arg0;
  args[1] = arg1;

  // initialise timers
  double cpu_t1, cpu_t2, wall_t1, wall_t2;
  op_timing_realloc(1);
  op_timers_core(&cpu_t1, &wall_t1);
  OP_kernels[1].name      = name;
  OP_kernels[1].count    += 1;


  if (OP_diags>2) {
    printf(" kernel routine w/o indirection:  update");
  }

  op_mpi_halo_exchanges_cuda(set, nargs, args);
  if (set->size > 0) {

    //set CUDA execution parameters
    #ifdef OP_BLOCK_SIZE_1
      int nthread = OP_BLOCK_SIZE_1;
    #else
      int nthread = OP_block_size;
    //  int nthread = 128;
    #endif

    int nblocks = 200;

    //transfer global reduction data to GPU
    int maxblocks = nblocks;
    int reduct_bytes = 0;
    int reduct_size  = 0;
    reduct_bytes += ROUND_UP(maxblocks*1*sizeof(int));
    reduct_size   = MAX(reduct_size,sizeof(int));
    reallocReductArrays(reduct_bytes);
    reduct_bytes = 0;
    arg1.data   = OP_reduct_h + reduct_bytes;
    arg1.data_d = OP_reduct_d + reduct_bytes;
    for ( int b=0; b<maxblocks; b++ ){
      for ( int d=0; d<1; d++ ){
        ((int *)arg1.data)[d+b*1] = ZERO_int;
      }
    }
    reduct_bytes += ROUND_UP(maxblocks*1*sizeof(int));
    mvReductArraysToDevice(reduct_bytes);

    int nshared = reduct_size*nthread;
    op_cuda_update<<<nblocks,nthread,nshared>>>(
      (double *) arg0.data_d,
      (int *) arg1.data_d,
      set->size );
    //transfer global reduction data back to CPU
    mvReductArraysToHost(reduct_bytes);
    for ( int b=0; b<maxblocks; b++ ){
      for ( int d=0; d<1; d++ ){
        arg1h[d] = arg1h[d] + ((int *)arg1.data)[d+b*1];
      }
    }
    arg1.data = (char *)arg1h;
    op_mpi_reduce(&arg1,arg1h);
  }
  op_mpi_set_dirtybit_cuda(nargs, args);
  cutilSafeCall(cudaDeviceSynchronize());
  //update kernel record
  op_timers_core(&cpu_t2, &wall_t2);
  OP_kernels[1].time     += wall_t2 - wall_t1;
  OP_kernels[1].transfer += (float)set->size * arg0.size * 2.0f;
}
