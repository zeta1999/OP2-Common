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
//    printf ("NULL argument\n");
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

//      n_upper = op_mpi_halo_exchanges_seq (set, N, args);               \
//      op_mpi_wait_all_seq (N,args);                                     \


#define OP_LOOP(N) \
  void op_par_loop_##N(void (*kernel)(CHARP_LIST(N)), op_set_core * set, ARG_LIST_POINTERS(N)) { \
    char * p_a[N] = {ZERO_LIST(N)};                                     \
    op_arg args[N] = {ARG_ARR_LIST(N)};                                 \
    int halo = 0;                                                       \
    int n_upper, rank;                                                  \
    if ( set->size == 0 ) {                                             \
      n_upper = op_mpi_halo_exchanges_seq (set, N, args);               \
      op_mpi_wait_all_seq (N,args);                                     \
      op_mpi_set_dirtybit (N, args);                                    \
      REDUCE_LIST(N)                                                    \
      return;                                                           \
    }                                                                   \
    ALLOC_POINTER_LIST(N)                                               \
    n_upper = op_mpi_halo_exchanges_seq (set, N, args);                 \
    for ( int n=0; n<n_upper; n++ ) {                                   \
      if ( n==set->core_size ) {                                        \
        op_mpi_wait_all_seq (N,args);                                   \
      }                                                                 \
      if ( n==set->size) halo = 1;                                      \
      ARG_SET_LIST(N);                                                  \
      (*kernel)(PTR_LIST(N));                                           \
    }                                                                   \
    op_mpi_set_dirtybit (N, args);                                      \
    REDUCE_LIST(N)                                                      \
    FREE_LIST(N)                                                        \
 }

/*    printf ("before set dirt bit\n");                                   \
    printf ("before allocating pointers\n");                            \

    printf ("before reduce\n");                                         \
    printf ("before free list\n");                                      \

//      printf ("before wait all\n");                                     \
//      printf ("before argset list\n");                                  \
//      printf ("at the end\n");                                          \
    printf ("after halo exchanges\n");                                  \

//    printf ("execution size = %d: set->size = %d, set->exec_size = %d \n", n_upper,set->size,set->exec_size); \
//    fflush (0);                                                         \

*/

OP_LOOP(1)  OP_LOOP(2)  OP_LOOP(3)  OP_LOOP(4)  OP_LOOP(5)  OP_LOOP(6)  OP_LOOP(7)  OP_LOOP(8)  OP_LOOP(9)  OP_LOOP(10)
OP_LOOP(11) OP_LOOP(12) OP_LOOP(13) OP_LOOP(14) OP_LOOP(15) OP_LOOP(16) OP_LOOP(17) OP_LOOP(18) OP_LOOP(19) OP_LOOP(20)
OP_LOOP(21) OP_LOOP(22) OP_LOOP(23) OP_LOOP(24) OP_LOOP(25) OP_LOOP(26) OP_LOOP(27) OP_LOOP(28) OP_LOOP(29) OP_LOOP(30)
OP_LOOP(31) OP_LOOP(32) OP_LOOP(33) OP_LOOP(34) OP_LOOP(35) OP_LOOP(36) OP_LOOP(37) OP_LOOP(38) OP_LOOP(39) OP_LOOP(40)
OP_LOOP(41) OP_LOOP(42)


/* void op_par_loop_6_per(void (*kernel)(char*, char*, char*, char*, char*, char*), op_set_core * set, op_arg * arg1, op_arg * arg2, op_arg * arg3, op_arg * arg4, op_arg * arg5, op_arg * arg6) {  */

/*   char * p_a[6] = {0, 0, 0, 0, 0, 0}; */
/*   op_arg args[6] = {*arg1, *arg2, *arg3, *arg4, *arg5, *arg6};  */
/*   int halo = 0; */
/*   int n_upper, n, rank, l, i, j; */

/*   op_mpi_rank (&rank); */

/* // op_monitor_map_mpi(arg5->map, ); */
/* //  fflush (stdout); */
/* /\*  if ( rank == 1 ) { */
/*     printf ("Global index for qb in local index 41446 is %d\n", */
/*       OP_part_list[set->index]->g_index[41446]); */
/*     fflush (stdout); */
/*   } */
/* *\/ */

/* //  op_monitor_dat_mpi (arg6->dat, 851171); */


/* //  printf ("before alloc  \n");  */
/*   if (arg1->idx < -1) { p_a[1 -1] = (char *) malloc (-1*args[1 -1].idx*arg1->dim);}  */
/*   if (arg2->idx < -1) { p_a[2 -1] = (char *) malloc (-1*args[2 -1].idx*arg2->dim);} */
/*   if (arg3->idx < -1) { p_a[3 -1] = (char *) malloc (-1*args[3 -1].idx*arg3->dim);} */
/*   if (arg4->idx < -1) { p_a[4 -1] = (char *) malloc (-1*args[4 -1].idx*arg4->dim);} */
/*   if (arg5->idx < -1) { p_a[5 -1] = (char *) malloc (-1*args[5 -1].idx*arg5->dim);} */
/*   if (arg6->idx < -1) { p_a[6 -1] = (char *) malloc (-1*args[6 -1].idx*arg6->dim);} */
/* //  printf ("before halo exch  \n"); */
/*   n_upper = op_mpi_halo_exchanges_seq (set, 6, args); */


/*   printf ("rank %d, n_upper = %d set size = %d, and core size = %d\n", rank, n_upper, set->size, set->core_size); */
/*   fflush (stdout); */

/*   for ( n=0; n<n_upper; n++ ) { */
/*     if ( n==set->core_size ) op_mpi_wait_all_seq (6,args); */
/*     if ( n==set->size) halo = 1; */

/*     if (args[1 -1].idx < -1) op_arg_copy_in (n, args[1 -1], (char **) p_a[1 -1]); */
/*     else {op_arg_set(n,args[1 -1],&p_a[1 -1],halo);} */

/*     if (args[2 -1].idx < -1) op_arg_copy_in (n, args[2 -1], (char **) p_a[2 -1]); */
/*     else {op_arg_set(n,args[2 -1],&p_a[2 -1],halo);} */

/*     if (args[3 -1].idx < -1) op_arg_copy_in (n, args[3 -1], (char **) p_a[3 -1]); */
/*     else {op_arg_set(n,args[3 -1],&p_a[3 -1],halo);} */

/*     if (args[4 -1].idx < -1) op_arg_copy_in (n, args[4 -1], (char **) p_a[4 -1]); */
/*     else {op_arg_set(n,args[4 -1],&p_a[4 -1],halo);} */

/*     if (args[5 -1].idx < -1) op_arg_copy_in (n, args[5 -1], (char **) p_a[5 -1]); */
/*     else {op_arg_set(n,args[5 -1],&p_a[5 -1],halo);} */

/*     if (args[6 -1].idx < -1) op_arg_copy_in (n, args[6 -1], (char **) p_a[6 -1]); */
/*     else {op_arg_set(n,args[6 -1],&p_a[6 -1],halo);} */


/*     (*kernel)(p_a[1 -1], p_a[2 -1], p_a[3 -1], p_a[4 -1], p_a[5 -1], p_a[6 -1]); */

/* /\*    if ( arg6->map->map[arg6->idx+n*arg6->map->dim] == 421489 ) { */
/*       printf ("Rank %d modifying 851171/421489 at iteration %d\n", rank, n); */
/*       fflush (stdout); */
/*     } */
/* *\/ */

/* /\*    if (n == 41446 && rank == 1) { */

/*       printf ("it 41446: qb %lf %lf %lf %lf %lf %lf\n",  */
/*         ((double **) p_a)[4][0], */
/*         ((double **) p_a)[4][1], */
/*         ((double **) p_a)[4][2], */
/*         ((double **) p_a)[4][3], */
/*         ((double **) p_a)[4][4], */
/*         ((double **) p_a)[4][5]); */

/*       fflush (stdout);     */

/*     } */
/* *\/ */
/* /\*        int map = arg5->map->map[arg5->idx + n*arg5->map->dim]; */
/*         printf ("Mapping for iteration 4262 is %d and max size of dataset is %d and max import halo size is %d and offset is %d and halo size is %d, local element index is %d\n",  */
/*           map,  */
/*           arg5->dat->set->size, */
/*           arg5->dat->set->size + OP_import_nonexec_list[arg5->dat->set->index]->size, */
/*           291088 - arg5->dat->set->size, */
/*           OP_import_nonexec_list[arg5->dat->set->index]->size, */
/*           OP_import_nonexec_list[arg5->dat->set->index]->list[4633]); */


/* *\/ */
/* /\*        printf ("From op_dat, size of arg5 is %d, map val %d it = %d, qo: %.12lf, %.12lf, %.12lf, %.12lf, %.12lf, %.12lf\n", */
/*           arg5->size, */
/*           map, */
/*           n, */
/*           (double) (arg5->data[map]), */
/*           (double) (arg5->data[map+1*8]), */
/*           (double) (arg5->data[map+2*8]), */
/*           (double) (arg5->data[map+3*8]), */
/*           (double) (arg5->data[map+4*8]), */
/*           (double) (arg5->data[map+5*8])); */
/* *\/ */
/* //        printf ("Size of import exec is %d from rank 1 it is %d\n",  */
/* //          OP_import_exec_list[set->index]->size); */

/* /\*        for (i = 0; i < OP_import_exec_list[set->index]->ranks_size; i++) { */
/*             printf ("Rank %d, size %d\n", OP_import_exec_list[set->index]->ranks[i], OP_import_exec_list[set->index]->sizes[i]); */
/*           for ( j = 0; j < OP_import_exec_list[set->index]->sizes[i]; j++ ) { */
/*             printf ("value of imported exec from rank %d is %d\n", OP_import_exec_list[set->index]->ranks[i], OP_import_exec_list[set->index]->list[OP_import_exec_list[set->index]->disps[i]+j]); */
/*           } */

/*         }         */
/* *\/ */

/* /\*        for (i = 0; i < OP_export_exec_list[set->index]->size; i++) { */
/*           printf ("value of export exec at i = %d is %d\n", i, OP_export_exec_list[set->index]->list[i]); */
/*         } */
/* *\/ */


/* /\*        for (l = 0; l < OP_import_nonexec_list[arg5->dat->set->index]->ranks_size; l++) { */
/*           printf ("i = %d (max = %d), disps = %d, ranks = %d\n", l, */
/*             OP_import_nonexec_list[arg5->dat->set->index]->ranks_size, */
/*             OP_import_nonexec_list[arg5->dat->set->index]->disps[l], */
/*             OP_import_nonexec_list[arg5->dat->set->index]->ranks[l]); */
/*         } */
/* *\/ */
/* //        fflush (stdout); */
/* //    } */

/*   } */

/* //  op_monitor_dat_mpi (arg6->dat, 851171); */

/* //  printf ("before set dirty bit  \n"); */
/*   op_mpi_set_dirtybit (6, args); */
/* //  printf ("before reduce  \n"); */
/*   if (arg1->argtype == 0) { */
/*     if (strncmp (arg1->type, "double", 6) == 0) { */
/*       op_mpi_reduce_double(arg1,(double *)p_a[1 -1]); */
/*     } */
/*     else if (strncmp (arg1->type, "float", 5) == 0) op_mpi_reduce_float(arg1,(float *)p_a[1 -1]); */
/*     else if ( strncmp (arg1->type, "int", 3) == 0 ){op_mpi_reduce_int(arg1,(int *)p_a[1 -1]);} */
/*     else if ( strncmp (arg1->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg1,(_Bool *)p_a[1 -1]); */
/*     else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg1->type); exit (0);} */
/*   } */
/*   if (arg2->argtype == 0) {if (strncmp (arg2->type, "double", 6) == 0) {op_mpi_reduce_double(arg2,(double *)p_a[2 -1]);} else if (strncmp (arg2->type, "float", 5) == 0) op_mpi_reduce_float(arg2,(float *)p_a[2 -1]); else if ( strncmp (arg2->type, "int", 3) == 0 ){op_mpi_reduce_int(arg2,(int *)p_a[2 -1]);} else if ( strncmp (arg2->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg2,(_Bool *)p_a[2 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg2->type); exit (0);}} ; if (arg3->argtype == 0) {if (strncmp (arg3->type, "double", 6) == 0) {op_mpi_reduce_double(arg3,(double *)p_a[3 -1]);} else if (strncmp (arg3->type, "float", 5) == 0) op_mpi_reduce_float(arg3,(float *)p_a[3 -1]); else if ( strncmp (arg3->type, "int", 3) == 0 ){op_mpi_reduce_int(arg3,(int *)p_a[3 -1]);} else if ( strncmp (arg3->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg3,(_Bool *)p_a[3 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg3->type); exit (0);}} ; if (arg4->argtype == 0) {if (strncmp (arg4->type, "double", 6) == 0) {op_mpi_reduce_double(arg4,(double *)p_a[4 -1]);} else if (strncmp (arg4->type, "float", 5) == 0) op_mpi_reduce_float(arg4,(float *)p_a[4 -1]); else if ( strncmp (arg4->type, "int", 3) == 0 ){op_mpi_reduce_int(arg4,(int *)p_a[4 -1]);} else if ( strncmp (arg4->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg4,(_Bool *)p_a[4 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg4->type); exit (0);}} ; if (arg5->argtype == 0) {if (strncmp (arg5->type, "double", 6) == 0) {op_mpi_reduce_double(arg5,(double *)p_a[5 -1]);} else if (strncmp (arg5->type, "float", 5) == 0) op_mpi_reduce_float(arg5,(float *)p_a[5 -1]); else if ( strncmp (arg5->type, "int", 3) == 0 ){op_mpi_reduce_int(arg5,(int *)p_a[5 -1]);} else if ( strncmp (arg5->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg5,(_Bool *)p_a[5 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg5->type); exit (0);}} ; if (arg6->argtype == 0) {if (strncmp (arg6->type, "double", 6) == 0) {op_mpi_reduce_double(arg6,(double *)p_a[6 -1]);} else if (strncmp (arg6->type, "float", 5) == 0) op_mpi_reduce_float(arg6,(float *)p_a[6 -1]); else if ( strncmp (arg6->type, "int", 3) == 0 ){op_mpi_reduce_int(arg6,(int *)p_a[6 -1]);} else if ( strncmp (arg6->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg6,(_Bool *)p_a[6 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg6->type); exit (0);}} */
/* //  printf ("before free\n"); */
/*   if (arg1->idx < -1) {free (p_a[1 -1]);} */
/*   if (arg2->idx < -1) {free (p_a[2 -1]);} */
/*   if (arg3->idx < -1) {free (p_a[3 -1]);} */
/*   if (arg4->idx < -1) {free (p_a[4 -1]);} */
/*   if (arg5->idx < -1) {free (p_a[5 -1]);} */
/*   if (arg6->idx < -1) {free (p_a[6 -1]);}  */
/* } */


/* void op_par_loop_19_accu(void (*kernel)(char*, char*, char*, char*, char*, char*, char*, char*, char*, char*, char*, char*, char*, char*, char*, char*, char*, char*, char*), op_set_core * set, op_arg * arg1, op_arg * arg2, op_arg * arg3, op_arg * arg4, op_arg * arg5, op_arg * arg6, op_arg * arg7, op_arg * arg8, op_arg * arg9, op_arg * arg10, op_arg * arg11, op_arg * arg12, op_arg * arg13, op_arg * arg14, op_arg * arg15, op_arg * arg16, op_arg * arg17, op_arg * arg18, op_arg * arg19) { char * p_a[19] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}; op_arg args[19] = {*arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8, *arg9, *arg10, *arg11, *arg12, *arg13, *arg14, *arg15, *arg16, *arg17, *arg18, *arg19}; int halo = 0; int n_upper; if (arg1->idx < -1) { p_a[1 -1] = (char *) malloc (-1*args[1 -1].idx*arg1->dim);} ; if (arg2->idx < -1) { p_a[2 -1] = (char *) malloc (-1*args[2 -1].idx*arg2->dim);} ; if (arg3->idx < -1) { p_a[3 -1] = (char *) malloc (-1*args[3 -1].idx*arg3->dim);} ; if (arg4->idx < -1) { p_a[4 -1] = (char *) malloc (-1*args[4 -1].idx*arg4->dim);} ; if (arg5->idx < -1) { p_a[5 -1] = (char *) malloc (-1*args[5 -1].idx*arg5->dim);} ; if (arg6->idx < -1) { p_a[6 -1] = (char *) malloc (-1*args[6 -1].idx*arg6->dim);} ; if (arg7->idx < -1) { p_a[7 -1] = (char *) malloc (-1*args[7 -1].idx*arg7->dim);} ; if (arg8->idx < -1) { p_a[8 -1] = (char *) malloc (-1*args[8 -1].idx*arg8->dim);} ; if (arg9->idx < -1) { p_a[9 -1] = (char *) malloc (-1*args[9 -1].idx*arg9->dim);} ; if (arg10->idx < -1) { p_a[10 -1] = (char *) malloc (-1*args[10 -1].idx*arg10->dim);} ; if (arg11->idx < -1) { p_a[11 -1] = (char *) malloc (-1*args[11 -1].idx*arg11->dim);} ; if (arg12->idx < -1) { p_a[12 -1] = (char *) malloc (-1*args[12 -1].idx*arg12->dim);} ; if (arg13->idx < -1) { p_a[13 -1] = (char *) malloc (-1*args[13 -1].idx*arg13->dim);} ; if (arg14->idx < -1) { p_a[14 -1] = (char *) malloc (-1*args[14 -1].idx*arg14->dim);} ; if (arg15->idx < -1) { p_a[15 -1] = (char *) malloc (-1*args[15 -1].idx*arg15->dim);} ; if (arg16->idx < -1) { p_a[16 -1] = (char *) malloc (-1*args[16 -1].idx*arg16->dim);} ; if (arg17->idx < -1) { p_a[17 -1] = (char *) malloc (-1*args[17 -1].idx*arg17->dim);} ; if (arg18->idx < -1) { p_a[18 -1] = (char *) malloc (-1*args[18 -1].idx*arg18->dim);} ; if (arg19->idx < -1) { p_a[19 -1] = (char *) malloc (-1*args[19 -1].idx*arg19->dim);} n_upper = op_mpi_halo_exchanges_seq_accu (set, 19, args); for ( int n=0; n<n_upper; n++ ) { if ( n==set->core_size ) op_mpi_wait_all_seq (19,args); if ( n==set->size) halo = 1; if (args[1 -1].idx < -1) op_arg_copy_in (n, args[1 -1], (char **) p_a[1 -1]); else {op_arg_set(n,args[1 -1],&p_a[1 -1],halo);} ; if (args[2 -1].idx < -1) op_arg_copy_in (n, args[2 -1], (char **) p_a[2 -1]); else {op_arg_set(n,args[2 -1],&p_a[2 -1],halo);} ; if (args[3 -1].idx < -1) op_arg_copy_in (n, args[3 -1], (char **) p_a[3 -1]); else {op_arg_set(n,args[3 -1],&p_a[3 -1],halo);} ; if (args[4 -1].idx < -1) op_arg_copy_in (n, args[4 -1], (char **) p_a[4 -1]); else {op_arg_set(n,args[4 -1],&p_a[4 -1],halo);} ; if (args[5 -1].idx < -1) op_arg_copy_in (n, args[5 -1], (char **) p_a[5 -1]); else {op_arg_set(n,args[5 -1],&p_a[5 -1],halo);} ; if (args[6 -1].idx < -1) op_arg_copy_in (n, args[6 -1], (char **) p_a[6 -1]); else {op_arg_set(n,args[6 -1],&p_a[6 -1],halo);} ; if (args[7 -1].idx < -1) op_arg_copy_in (n, args[7 -1], (char **) p_a[7 -1]); else {op_arg_set(n,args[7 -1],&p_a[7 -1],halo);} ; if (args[8 -1].idx < -1) op_arg_copy_in (n, args[8 -1], (char **) p_a[8 -1]); else {op_arg_set(n,args[8 -1],&p_a[8 -1],halo);} ; if (args[9 -1].idx < -1) op_arg_copy_in (n, args[9 -1], (char **) p_a[9 -1]); else {op_arg_set(n,args[9 -1],&p_a[9 -1],halo);} ; if (args[10 -1].idx < -1) op_arg_copy_in (n, args[10 -1], (char **) p_a[10 -1]); else {op_arg_set(n,args[10 -1],&p_a[10 -1],halo);} ; if (args[11 -1].idx < -1) op_arg_copy_in (n, args[11 -1], (char **) p_a[11 -1]); else {op_arg_set(n,args[11 -1],&p_a[11 -1],halo);} ; if (args[12 -1].idx < -1) op_arg_copy_in (n, args[12 -1], (char **) p_a[12 -1]); else {op_arg_set(n,args[12 -1],&p_a[12 -1],halo);} ; if (args[13 -1].idx < -1) op_arg_copy_in (n, args[13 -1], (char **) p_a[13 -1]); else {op_arg_set(n,args[13 -1],&p_a[13 -1],halo);} ; if (args[14 -1].idx < -1) op_arg_copy_in (n, args[14 -1], (char **) p_a[14 -1]); else {op_arg_set(n,args[14 -1],&p_a[14 -1],halo);} ; if (args[15 -1].idx < -1) op_arg_copy_in (n, args[15 -1], (char **) p_a[15 -1]); else {op_arg_set(n,args[15 -1],&p_a[15 -1],halo);} ; if (args[16 -1].idx < -1) op_arg_copy_in (n, args[16 -1], (char **) p_a[16 -1]); else {op_arg_set(n,args[16 -1],&p_a[16 -1],halo);} ; if (args[17 -1].idx < -1) op_arg_copy_in (n, args[17 -1], (char **) p_a[17 -1]); else {op_arg_set(n,args[17 -1],&p_a[17 -1],halo);} ; if (args[18 -1].idx < -1) op_arg_copy_in (n, args[18 -1], (char **) p_a[18 -1]); else {op_arg_set(n,args[18 -1],&p_a[18 -1],halo);} ; if (args[19 -1].idx < -1) op_arg_copy_in (n, args[19 -1], (char **) p_a[19 -1]); else {op_arg_set(n,args[19 -1],&p_a[19 -1],halo);}; (*kernel)(p_a[1 -1], p_a[2 -1], p_a[3 -1], p_a[4 -1], p_a[5 -1], p_a[6 -1], p_a[7 -1], p_a[8 -1], p_a[9 -1], p_a[10 -1], p_a[11 -1], p_a[12 -1], p_a[13 -1], p_a[14 -1], p_a[15 -1], p_a[16 -1], p_a[17 -1], p_a[18 -1], p_a[19 -1]); } op_mpi_set_dirtybit (19, args); if (arg1->argtype == 0) {if (strncmp (arg1->type, "double", 6) == 0) {op_mpi_reduce_double(arg1,(double *)p_a[1 -1]);} else if (strncmp (arg1->type, "float", 5) == 0) op_mpi_reduce_float(arg1,(float *)p_a[1 -1]); else if ( strncmp (arg1->type, "int", 3) == 0 ){op_mpi_reduce_int(arg1,(int *)p_a[1 -1]);} else if ( strncmp (arg1->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg1,(_Bool *)p_a[1 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg1->type); exit (0);}} ; if (arg2->argtype == 0) {if (strncmp (arg2->type, "double", 6) == 0) {op_mpi_reduce_double(arg2,(double *)p_a[2 -1]);} else if (strncmp (arg2->type, "float", 5) == 0) op_mpi_reduce_float(arg2,(float *)p_a[2 -1]); else if ( strncmp (arg2->type, "int", 3) == 0 ){op_mpi_reduce_int(arg2,(int *)p_a[2 -1]);} else if ( strncmp (arg2->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg2,(_Bool *)p_a[2 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg2->type); exit (0);}} ; if (arg3->argtype == 0) {if (strncmp (arg3->type, "double", 6) == 0) {op_mpi_reduce_double(arg3,(double *)p_a[3 -1]);} else if (strncmp (arg3->type, "float", 5) == 0) op_mpi_reduce_float(arg3,(float *)p_a[3 -1]); else if ( strncmp (arg3->type, "int", 3) == 0 ){op_mpi_reduce_int(arg3,(int *)p_a[3 -1]);} else if ( strncmp (arg3->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg3,(_Bool *)p_a[3 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg3->type); exit (0);}} ; if (arg4->argtype == 0) {if (strncmp (arg4->type, "double", 6) == 0) {op_mpi_reduce_double(arg4,(double *)p_a[4 -1]);} else if (strncmp (arg4->type, "float", 5) == 0) op_mpi_reduce_float(arg4,(float *)p_a[4 -1]); else if ( strncmp (arg4->type, "int", 3) == 0 ){op_mpi_reduce_int(arg4,(int *)p_a[4 -1]);} else if ( strncmp (arg4->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg4,(_Bool *)p_a[4 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg4->type); exit (0);}} ; if (arg5->argtype == 0) {if (strncmp (arg5->type, "double", 6) == 0) {op_mpi_reduce_double(arg5,(double *)p_a[5 -1]);} else if (strncmp (arg5->type, "float", 5) == 0) op_mpi_reduce_float(arg5,(float *)p_a[5 -1]); else if ( strncmp (arg5->type, "int", 3) == 0 ){op_mpi_reduce_int(arg5,(int *)p_a[5 -1]);} else if ( strncmp (arg5->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg5,(_Bool *)p_a[5 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg5->type); exit (0);}} ; if (arg6->argtype == 0) {if (strncmp (arg6->type, "double", 6) == 0) {op_mpi_reduce_double(arg6,(double *)p_a[6 -1]);} else if (strncmp (arg6->type, "float", 5) == 0) op_mpi_reduce_float(arg6,(float *)p_a[6 -1]); else if ( strncmp (arg6->type, "int", 3) == 0 ){op_mpi_reduce_int(arg6,(int *)p_a[6 -1]);} else if ( strncmp (arg6->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg6,(_Bool *)p_a[6 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg6->type); exit (0);}} ; if (arg7->argtype == 0) {if (strncmp (arg7->type, "double", 6) == 0) {op_mpi_reduce_double(arg7,(double *)p_a[7 -1]);} else if (strncmp (arg7->type, "float", 5) == 0) op_mpi_reduce_float(arg7,(float *)p_a[7 -1]); else if ( strncmp (arg7->type, "int", 3) == 0 ){op_mpi_reduce_int(arg7,(int *)p_a[7 -1]);} else if ( strncmp (arg7->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg7,(_Bool *)p_a[7 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg7->type); exit (0);}} ; if (arg8->argtype == 0) {if (strncmp (arg8->type, "double", 6) == 0) {op_mpi_reduce_double(arg8,(double *)p_a[8 -1]);} else if (strncmp (arg8->type, "float", 5) == 0) op_mpi_reduce_float(arg8,(float *)p_a[8 -1]); else if ( strncmp (arg8->type, "int", 3) == 0 ){op_mpi_reduce_int(arg8,(int *)p_a[8 -1]);} else if ( strncmp (arg8->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg8,(_Bool *)p_a[8 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg8->type); exit (0);}} ; if (arg9->argtype == 0) {if (strncmp (arg9->type, "double", 6) == 0) {op_mpi_reduce_double(arg9,(double *)p_a[9 -1]);} else if (strncmp (arg9->type, "float", 5) == 0) op_mpi_reduce_float(arg9,(float *)p_a[9 -1]); else if ( strncmp (arg9->type, "int", 3) == 0 ){op_mpi_reduce_int(arg9,(int *)p_a[9 -1]);} else if ( strncmp (arg9->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg9,(_Bool *)p_a[9 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg9->type); exit (0);}} ; if (arg10->argtype == 0) {if (strncmp (arg10->type, "double", 6) == 0) {op_mpi_reduce_double(arg10,(double *)p_a[10 -1]);} else if (strncmp (arg10->type, "float", 5) == 0) op_mpi_reduce_float(arg10,(float *)p_a[10 -1]); else if ( strncmp (arg10->type, "int", 3) == 0 ){op_mpi_reduce_int(arg10,(int *)p_a[10 -1]);} else if ( strncmp (arg10->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg10,(_Bool *)p_a[10 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg10->type); exit (0);}} ; if (arg11->argtype == 0) {if (strncmp (arg11->type, "double", 6) == 0) {op_mpi_reduce_double(arg11,(double *)p_a[11 -1]);} else if (strncmp (arg11->type, "float", 5) == 0) op_mpi_reduce_float(arg11,(float *)p_a[11 -1]); else if ( strncmp (arg11->type, "int", 3) == 0 ){op_mpi_reduce_int(arg11,(int *)p_a[11 -1]);} else if ( strncmp (arg11->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg11,(_Bool *)p_a[11 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg11->type); exit (0);}} ; if (arg12->argtype == 0) {if (strncmp (arg12->type, "double", 6) == 0) {op_mpi_reduce_double(arg12,(double *)p_a[12 -1]);} else if (strncmp (arg12->type, "float", 5) == 0) op_mpi_reduce_float(arg12,(float *)p_a[12 -1]); else if ( strncmp (arg12->type, "int", 3) == 0 ){op_mpi_reduce_int(arg12,(int *)p_a[12 -1]);} else if ( strncmp (arg12->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg12,(_Bool *)p_a[12 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg12->type); exit (0);}} ; if (arg13->argtype == 0) {if (strncmp (arg13->type, "double", 6) == 0) {op_mpi_reduce_double(arg13,(double *)p_a[13 -1]);} else if (strncmp (arg13->type, "float", 5) == 0) op_mpi_reduce_float(arg13,(float *)p_a[13 -1]); else if ( strncmp (arg13->type, "int", 3) == 0 ){op_mpi_reduce_int(arg13,(int *)p_a[13 -1]);} else if ( strncmp (arg13->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg13,(_Bool *)p_a[13 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg13->type); exit (0);}} ; if (arg14->argtype == 0) {if (strncmp (arg14->type, "double", 6) == 0) {op_mpi_reduce_double(arg14,(double *)p_a[14 -1]);} else if (strncmp (arg14->type, "float", 5) == 0) op_mpi_reduce_float(arg14,(float *)p_a[14 -1]); else if ( strncmp (arg14->type, "int", 3) == 0 ){op_mpi_reduce_int(arg14,(int *)p_a[14 -1]);} else if ( strncmp (arg14->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg14,(_Bool *)p_a[14 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg14->type); exit (0);}} ; if (arg15->argtype == 0) {if (strncmp (arg15->type, "double", 6) == 0) {op_mpi_reduce_double(arg15,(double *)p_a[15 -1]);} else if (strncmp (arg15->type, "float", 5) == 0) op_mpi_reduce_float(arg15,(float *)p_a[15 -1]); else if ( strncmp (arg15->type, "int", 3) == 0 ){op_mpi_reduce_int(arg15,(int *)p_a[15 -1]);} else if ( strncmp (arg15->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg15,(_Bool *)p_a[15 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg15->type); exit (0);}} ; if (arg16->argtype == 0) {if (strncmp (arg16->type, "double", 6) == 0) {op_mpi_reduce_double(arg16,(double *)p_a[16 -1]);} else if (strncmp (arg16->type, "float", 5) == 0) op_mpi_reduce_float(arg16,(float *)p_a[16 -1]); else if ( strncmp (arg16->type, "int", 3) == 0 ){op_mpi_reduce_int(arg16,(int *)p_a[16 -1]);} else if ( strncmp (arg16->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg16,(_Bool *)p_a[16 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg16->type); exit (0);}} ; if (arg17->argtype == 0) {if (strncmp (arg17->type, "double", 6) == 0) {op_mpi_reduce_double(arg17,(double *)p_a[17 -1]);} else if (strncmp (arg17->type, "float", 5) == 0) op_mpi_reduce_float(arg17,(float *)p_a[17 -1]); else if ( strncmp (arg17->type, "int", 3) == 0 ){op_mpi_reduce_int(arg17,(int *)p_a[17 -1]);} else if ( strncmp (arg17->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg17,(_Bool *)p_a[17 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg17->type); exit (0);}} ; if (arg18->argtype == 0) {if (strncmp (arg18->type, "double", 6) == 0) {op_mpi_reduce_double(arg18,(double *)p_a[18 -1]);} else if (strncmp (arg18->type, "float", 5) == 0) op_mpi_reduce_float(arg18,(float *)p_a[18 -1]); else if ( strncmp (arg18->type, "int", 3) == 0 ){op_mpi_reduce_int(arg18,(int *)p_a[18 -1]);} else if ( strncmp (arg18->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg18,(_Bool *)p_a[18 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg18->type); exit (0);}} ; if (arg19->argtype == 0) {if (strncmp (arg19->type, "double", 6) == 0) {op_mpi_reduce_double(arg19,(double *)p_a[19 -1]);} else if (strncmp (arg19->type, "float", 5) == 0) op_mpi_reduce_float(arg19,(float *)p_a[19 -1]); else if ( strncmp (arg19->type, "int", 3) == 0 ){op_mpi_reduce_int(arg19,(int *)p_a[19 -1]);} else if ( strncmp (arg19->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg19,(_Bool *)p_a[19 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg19->type); exit (0);}} if (arg1->idx < -1) {free (p_a[1 -1]);} ; if (arg2->idx < -1) {free (p_a[2 -1]);} ; if (arg3->idx < -1) {free (p_a[3 -1]);} ; if (arg4->idx < -1) {free (p_a[4 -1]);} ; if (arg5->idx < -1) {free (p_a[5 -1]);} ; if (arg6->idx < -1) {free (p_a[6 -1]);} ; if (arg7->idx < -1) {free (p_a[7 -1]);} ; if (arg8->idx < -1) {free (p_a[8 -1]);} ; if (arg9->idx < -1) {free (p_a[9 -1]);} ; if (arg10->idx < -1) {free (p_a[10 -1]);} ; if (arg11->idx < -1) {free (p_a[11 -1]);} ; if (arg12->idx < -1) {free (p_a[12 -1]);} ; if (arg13->idx < -1) {free (p_a[13 -1]);} ; if (arg14->idx < -1) {free (p_a[14 -1]);} ; if (arg15->idx < -1) {free (p_a[15 -1]);} ; if (arg16->idx < -1) {free (p_a[16 -1]);} ; if (arg17->idx < -1) {free (p_a[17 -1]);} ; if (arg18->idx < -1) {free (p_a[18 -1]);} ; if (arg19->idx < -1) {free (p_a[19 -1]);} }  */



/* void op_par_loop_2_real(void (*kernel)(char*, char*), op_set_core * set, op_arg * arg1, op_arg * arg2) { char * p_a[2] = {0, 0}; op_arg args[2] = {*arg1, *arg2}; int halo = 0; int n_upper; if (arg1->idx < -1) { p_a[1 -1] = (char *) malloc (-1*args[1 -1].idx*arg1->dim);} ; if (arg2->idx < -1) { p_a[2 -1] = (char *) malloc (-1*args[2 -1].idx*arg2->dim);} n_upper = op_mpi_halo_exchanges_seq_real (set, 2, args); for ( int n=0; n<n_upper; n++ ) { if ( n==set->core_size ) op_mpi_wait_all_seq (2,args); if ( n==set->size) halo = 1; if (args[1 -1].idx < -1) op_arg_copy_in (n, args[1 -1], (char **) p_a[1 -1]); else {op_arg_set(n,args[1 -1],&p_a[1 -1],halo);} ; if (args[2 -1].idx < -1) op_arg_copy_in (n, args[2 -1], (char **) p_a[2 -1]); else {op_arg_set(n,args[2 -1],&p_a[2 -1],halo);}; (*kernel)(p_a[1 -1], p_a[2 -1]); } op_mpi_set_dirtybit (2, args); if (arg1->argtype == 0) {if (strncmp (arg1->type, "double", 6) == 0) {op_mpi_reduce_double(arg1,(double *)p_a[1 -1]);} else if (strncmp (arg1->type, "float", 5) == 0) op_mpi_reduce_float(arg1,(float *)p_a[1 -1]); else if ( strncmp (arg1->type, "int", 3) == 0 ){op_mpi_reduce_int(arg1,(int *)p_a[1 -1]);} else if ( strncmp (arg1->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg1,(_Bool *)p_a[1 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg1->type); exit (0);}} ; if (arg2->argtype == 0) {if (strncmp (arg2->type, "double", 6) == 0) {op_mpi_reduce_double(arg2,(double *)p_a[2 -1]);} else if (strncmp (arg2->type, "float", 5) == 0) op_mpi_reduce_float(arg2,(float *)p_a[2 -1]); else if ( strncmp (arg2->type, "int", 3) == 0 ){op_mpi_reduce_int(arg2,(int *)p_a[2 -1]);} else if ( strncmp (arg2->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg2,(_Bool *)p_a[2 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg2->type); exit (0);}} if (arg1->idx < -1) {free (p_a[1 -1]);} ; if (arg2->idx < -1) {free (p_a[2 -1]);} } */
