//
// auto-generated by op2.py
//

//global constants
#ifndef MAX_CONST_SIZE
#define MAX_CONST_SIZE 128
#endif

__constant__ double gam_cuda;
__constant__ double gm1_cuda;
__constant__ double cfl_cuda;
__constant__ double eps_cuda;
__constant__ double mach_cuda;
__constant__ double alpha_cuda;
__constant__ double qinf_cuda[4];

//header
#include "op_lib_cpp.h"
#include "op_cuda_rt_support.h"
#include "op_cuda_reduction.h"

void op_decl_const_char(int dim, char const *type,
int size, char *dat, char const *name){
  if (!OP_hybrid_gpu) return;
  if (!strcmp(name,"gam")) {
    cutilSafeCall(cudaMemcpyToSymbol(gam_cuda, dat, dim*size));
  }
  else
  if (!strcmp(name,"gm1")) {
    cutilSafeCall(cudaMemcpyToSymbol(gm1_cuda, dat, dim*size));
  }
  else
  if (!strcmp(name,"cfl")) {
    cutilSafeCall(cudaMemcpyToSymbol(cfl_cuda, dat, dim*size));
  }
  else
  if (!strcmp(name,"eps")) {
    cutilSafeCall(cudaMemcpyToSymbol(eps_cuda, dat, dim*size));
  }
  else
  if (!strcmp(name,"mach")) {
    cutilSafeCall(cudaMemcpyToSymbol(mach_cuda, dat, dim*size));
  }
  else
  if (!strcmp(name,"alpha")) {
    cutilSafeCall(cudaMemcpyToSymbol(alpha_cuda, dat, dim*size));
  }
  else
  if (!strcmp(name,"qinf")) {
    cutilSafeCall(cudaMemcpyToSymbol(qinf_cuda, dat, dim*size));
  }
  else
  {
    printf("error: unknown const name\n"); exit(1);
  }
}

//user kernel files
#include "save_soln_kernel.cu"
#include "adt_calc_kernel.cu"
#include "res_calc_kernel.cu"
#include "bres_calc_kernel.cu"
#include "update_kernel.cu"
#include "update1_kernel.cu"
