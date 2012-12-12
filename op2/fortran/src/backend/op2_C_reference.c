/*
 * This file implements the reference (sequential) version of op_par_loop calls
 * to be used in Fortran
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "../../include/op2_C_reference.h"
#include "../../include/op2_reference_macros.h"
#include "../../include/op2_for_C_wrappers.h"

#include <op_lib_core.h>

#ifdef NO_MPI

#else

#include <op_lib_mpi.h>
#endif

// only for debugging!!!


int op2_stride = 1;
#define OP2_STRIDE(arr, idx) arr[idx]

char blank_args[512]; // scratch space to use for blank args

inline void op_arg_set(int n, op_arg arg, char **p_arg, int halo){
  if (arg.argtype == OP_ARG_NULL)
  { //for null arguments, p_arg is not used in the user kernel
    *p_arg = NULL;
    return;
  }

  *p_arg = arg.data;

  if (arg.argtype==OP_ARG_GBL) {
    if (halo && (arg.acc != OP_READ)) *p_arg = blank_args;
  } else if ( arg.argtype == OP_ARG_DAT ) {
    if (arg.map==NULL)         // identity mapping
      *p_arg += arg.size*n;
    else                       // standard pointers (decremented for FORTRAN)
                               // undone decrementing because everything now
                               // runs in OP2
      *p_arg += arg.size*(arg.map->map[arg.idx+n*arg.map->dim]);
  }
}

void op_arg_copy_in(int n, op_arg arg, char **p_arg) {
  for (int i = 0; i < -1*arg.idx; ++i)
    p_arg[i] = arg.data + arg.map->map[i+n*arg.map->dim]*arg.size;
}

void op_args_check(op_set set, int nargs, op_arg *args,
                                      int *ninds, const char *name) {
  for (int n=0; n<nargs; n++)
    op_arg_check(set,n,args[n],ninds,name);
}


#define CHARP_LIST(N) COMMA_LIST(N,CHARP)
#define CHARP(x) char*

#define CHARP_LIST_2(N) SEMI_LIST(N,CHARP2)
#define CHARP2(x) char* ptr##x

#define ARG_SET_LIST(N) SEMI_LIST(N,ARGSET)
#define ARGSET(x) if (args[x-1].idx < -1) op_arg_copy_in (n, args[x-1], (char **) p_a[x-1]); else {op_arg_set(n,args[x-1],&p_a[x-1],halo);}

#define PTR_LIST(N) COMMA_LIST(N,PTRL)
#define PTRL(x) p_a[x-1]

#define ZERO_LIST(N) COMMA_LIST(N,ZERO)
#define ZERO(x) 0

#define ARG_ARR_LIST(N) COMMA_LIST(N,ARG_ARR)
#define ARG_ARR(x) *arg##x

#define ARG_LIST_POINTERS(N) COMMA_LIST(N,ARG_POINTERS)
#define ARG_POINTERS(x) op_arg * arg##x

#define ALLOC_POINTER_LIST(N) SEMI_LIST(N,ALLOC_POINTER)
#define ALLOC_POINTER(x) if (arg##x->idx < -1) { p_a[x-1] = (char *) malloc (-1*args[x-1].idx*arg##x->dim);}

#define FREE_LIST(N) SEMI_LIST(N,FREE)
#define FREE(x) if (arg##x->idx < -1) {free (p_a[x-1]);}

#define REDUCE_LIST(N) SEMI_LIST(N,REDUCE)
#define REDUCE(x) if (arg##x->argtype == OP_ARG_GBL ) { if ( arg##x->acc == OP_INC || arg##x->acc == OP_MAX || arg##x->acc == OP_MIN ) { {if (strncmp (arg##x->type, "double", 6) == 0) {op_mpi_reduce_double(arg##x,(double *)p_a[x-1]);} else if (strncmp (arg##x->type, "float", 5) == 0) op_mpi_reduce_float(arg##x,(float *)p_a[x-1]); else if ( strncmp (arg##x->type, "int", 3) == 0 ){op_mpi_reduce_int(arg##x,(int *)p_a[x-1]);} else if ( strncmp (arg##x->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg##x,(bool *)p_a[x-1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg##x->type); exit (0);}}}}


#define OP_LOOP(N) \
  void op_par_loop_##N(void (*kernel)(CHARP_LIST(N)), op_set_core * set, ARG_LIST_POINTERS(N)) { \
    char * p_a[N] = {ZERO_LIST(N)};                                     \
    op_arg args[N] = {ARG_ARR_LIST(N)};                                 \
    int halo = 0;                                                       \
    int n_upper, rank;                                                  \
    ALLOC_POINTER_LIST(N)                                               \
    n_upper = op_mpi_halo_exchanges_seq (set, N, args);                 \
    if ( n_upper == 0 ) {                                               \
      op_mpi_wait_all_seq (N,args);                                     \
      op_mpi_set_dirtybit (N, args);                                    \
      REDUCE_LIST(N)                                                    \
      return;                                                           \
    }                                                                   \
    for ( int n=0; n<n_upper; n++ ) {                                   \
      if ( n==set->core_size ) {                                        \
        op_mpi_wait_all_seq (N,args);                                   \
      }                                                                 \
      if ( n==set->size) halo = 1;                                      \
      ARG_SET_LIST(N);                                                  \
      (*kernel)(PTR_LIST(N));                                           \
    }                                                                   \
    if ( n_upper == set->core_size ) op_mpi_wait_all_seq (N,args);      \
    op_mpi_set_dirtybit (N, args);                                      \
    REDUCE_LIST(N)                                                      \
    FREE_LIST(N)                                                        \
 }

OP_LOOP(1)  OP_LOOP(2)  OP_LOOP(3)  OP_LOOP(4)  OP_LOOP(5)  OP_LOOP(6)  OP_LOOP(7)  OP_LOOP(8)  OP_LOOP(9)  OP_LOOP(10)
OP_LOOP(11) OP_LOOP(12) OP_LOOP(13) OP_LOOP(14) OP_LOOP(15) OP_LOOP(16) OP_LOOP(17) OP_LOOP(18) OP_LOOP(19) OP_LOOP(20)
OP_LOOP(21) OP_LOOP(22) OP_LOOP(23) OP_LOOP(24) OP_LOOP(25) OP_LOOP(26) OP_LOOP(27) OP_LOOP(28) OP_LOOP(29) OP_LOOP(30)
OP_LOOP(31) OP_LOOP(32) OP_LOOP(33) OP_LOOP(34) OP_LOOP(35) OP_LOOP(36) OP_LOOP(37) OP_LOOP(38) OP_LOOP(39) OP_LOOP(40)
OP_LOOP(41) OP_LOOP(42)

void op_par_loop_accu(void (*kernel)(char*, char*, char*), op_set_core * set, op_arg * arg1, op_arg * arg2, op_arg * arg3) {     char * p_a[3] = {0, 0, 0};                                         op_arg args[3] = {*arg1, *arg2, *arg3};                                     int halo = 0;                                                           int n_upper, rank;

    op_monitor_dat_mpi (arg3->dat, 723129);
    MPI_Comm_rank (MPI_COMM_WORLD, &rank);
                                                      if (arg1->idx < -1) { p_a[1-1] = (char *) malloc (-1*args[1-1].idx*arg1->dim);} ; if (arg2->idx < -1) { p_a[2-1] = (char *) malloc (-1*args[2-1].idx*arg2->dim);} ; if (arg3->idx < -1) { p_a[3-1] = (char *) malloc (-1*args[3-1].idx*arg3->dim);}                                                   n_upper = op_mpi_halo_exchanges_seq (set, 3, args);                     if ( n_upper == 0 ) {                                                     op_mpi_wait_all_seq (3,args);                                           op_mpi_set_dirtybit (3, args);                                          if (arg1->argtype == OP_ARG_GBL ) { if ( arg1->acc == OP_INC || arg1->acc == OP_MAX || arg1->acc == OP_MIN ) { {if (strncmp (arg1->type, "double", 6) == 0) {op_mpi_reduce_double(arg1,(double *)p_a[1-1]);} else if (strncmp (arg1->type, "float", 5) == 0) op_mpi_reduce_float(arg1,(float *)p_a[1-1]); else if ( strncmp (arg1->type, "int", 3) == 0 ){op_mpi_reduce_int(arg1,(int *)p_a[1-1]);} else if ( strncmp (arg1->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg1,(bool *)p_a[1-1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg1->type); exit (0);}}}} ; if (arg2->argtype == OP_ARG_GBL ) { if ( arg2->acc == OP_INC || arg2->acc == OP_MAX || arg2->acc == OP_MIN ) { {if (strncmp (arg2->type, "double", 6) == 0) {op_mpi_reduce_double(arg2,(double *)p_a[2-1]);} else if (strncmp (arg2->type, "float", 5) == 0) op_mpi_reduce_float(arg2,(float *)p_a[2-1]); else if ( strncmp (arg2->type, "int", 3) == 0 ){op_mpi_reduce_int(arg2,(int *)p_a[2-1]);} else if ( strncmp (arg2->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg2,(bool *)p_a[2-1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg2->type); exit (0);}}}} ; if (arg3->argtype == OP_ARG_GBL ) { if ( arg3->acc == OP_INC || arg3->acc == OP_MAX || arg3->acc == OP_MIN ) { {if (strncmp (arg3->type, "double", 6) == 0) {op_mpi_reduce_double(arg3,(double *)p_a[3-1]);} else if (strncmp (arg3->type, "float", 5) == 0) op_mpi_reduce_float(arg3,(float *)p_a[3-1]); else if ( strncmp (arg3->type, "int", 3) == 0 ){op_mpi_reduce_int(arg3,(int *)p_a[3-1]);} else if ( strncmp (arg3->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg3,(bool *)p_a[3-1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg3->type); exit (0);}}}}                                                          return;                                                               }

    printf ("rank = %d: core_size = %d, exec_size = %d, nonexec_size = %d, set size = %d, n_upper = %d\n", rank, set->core_size, set->exec_size, set->nonexec_size, set->size, n_upper);


for ( int n=0; n<n_upper; n++ ) {
  if ( n==set->core_size ) {
    printf ("Rank = %d, now waiting for comms\n", rank);
    op_mpi_wait_all_seq (3,args);
  }                                                                       if ( n==set->size) halo = 1;                                            if (args[1-1].idx < -1) op_arg_copy_in (n, args[1-1], (char **) p_a[1-1]); else {op_arg_set(n,args[1-1],&p_a[1-1],halo);} ; if (args[2-1].idx < -1) op_arg_copy_in (n, args[2-1], (char **) p_a[2-1]); else {op_arg_set(n,args[2-1],&p_a[2-1],halo);} ; if (args[3-1].idx < -1) op_arg_copy_in (n, args[3-1], (char **) p_a[3-1]); else {op_arg_set(n,args[3-1],&p_a[3-1],halo);};                                                        (*kernel)(p_a[1-1], p_a[2-1], p_a[3-1]);                                               }                                                                       if ( n_upper == set->core_size ) op_mpi_wait_all_seq (3,args);          op_mpi_set_dirtybit (3, args);                                          if (arg1->argtype == OP_ARG_GBL ) { if ( arg1->acc == OP_INC || arg1->acc == OP_MAX || arg1->acc == OP_MIN ) { {if (strncmp (arg1->type, "double", 6) == 0) {op_mpi_reduce_double(arg1,(double *)p_a[1-1]);} else if (strncmp (arg1->type, "float", 5) == 0) op_mpi_reduce_float(arg1,(float *)p_a[1-1]); else if ( strncmp (arg1->type, "int", 3) == 0 ){op_mpi_reduce_int(arg1,(int *)p_a[1-1]);} else if ( strncmp (arg1->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg1,(bool *)p_a[1-1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg1->type); exit (0);}}}} ; if (arg2->argtype == OP_ARG_GBL ) { if ( arg2->acc == OP_INC || arg2->acc == OP_MAX || arg2->acc == OP_MIN ) { {if (strncmp (arg2->type, "double", 6) == 0) {op_mpi_reduce_double(arg2,(double *)p_a[2-1]);} else if (strncmp (arg2->type, "float", 5) == 0) op_mpi_reduce_float(arg2,(float *)p_a[2-1]); else if ( strncmp (arg2->type, "int", 3) == 0 ){op_mpi_reduce_int(arg2,(int *)p_a[2-1]);} else if ( strncmp (arg2->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg2,(bool *)p_a[2-1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg2->type); exit (0);}}}} ; if (arg3->argtype == OP_ARG_GBL ) { if ( arg3->acc == OP_INC || arg3->acc == OP_MAX || arg3->acc == OP_MIN ) { {if (strncmp (arg3->type, "double", 6) == 0) {op_mpi_reduce_double(arg3,(double *)p_a[3-1]);} else if (strncmp (arg3->type, "float", 5) == 0) op_mpi_reduce_float(arg3,(float *)p_a[3-1]); else if ( strncmp (arg3->type, "int", 3) == 0 ){op_mpi_reduce_int(arg3,(int *)p_a[3-1]);} else if ( strncmp (arg3->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg3,(bool *)p_a[3-1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg3->type); exit (0);}}}}                                                          if (arg1->idx < -1) {free (p_a[1-1]);} ; if (arg2->idx < -1) {free (p_a[2-1]);} ; if (arg3->idx < -1) {free (p_a[3-1]);}                                                         }
