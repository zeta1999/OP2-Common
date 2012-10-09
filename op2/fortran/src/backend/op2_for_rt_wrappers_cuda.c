#include <op_lib_core.h>
#include <cuda.h>
#include <cuda_runtime.h>
#include <cuda_runtime_api.h>

void
op_get_dat (op_dat dat) {
  cudaMemcpy (dat->data, dat->data_d,
    dat->size * dat->set->size,cudaMemcpyDeviceToHost);

  cudaThreadSynchronize();
}
<<<<<<< HEAD

void
op_put_dat (op_dat dat) {
  cudaMemcpy (dat->data_d, dat->data,
    dat->size * dat->set->size,cudaMemcpyHostToDevice);

  cudaThreadSynchronize();
}

void
op_get_dat_mpi (op_dat dat) {
  cudaMemcpy (dat->data, dat->data_d,
    dat->size * (dat->set->size + dat->set->exec_size + dat->set->nonexec_size), cudaMemcpyDeviceToHost);
  cudaThreadSynchronize();
}

void
op_put_dat_mpi (op_dat dat) {
  cudaMemcpy (dat->data_d, dat->data,
    dat->size * (dat->set->size + dat->set->exec_size + dat->set->nonexec_size),cudaMemcpyHostToDevice);
  cudaThreadSynchronize();
}
=======

void
op_put_dat (op_dat dat) {
  cudaMemcpy (dat->data_d, dat->data,
    dat->size * dat->set->size,cudaMemcpyHostToDevice);

  cudaThreadSynchronize();
}
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
