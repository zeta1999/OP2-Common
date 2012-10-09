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

<<<<<<< HEAD
// only for debugging!!!
#include <op_lib_mpi.h>

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

=======
int op2_stride = 1;
#define OP2_STRIDE(arr, idx) arr[idx]

char blank_args[512]; // scratch space to use for blank args                                                                                                                                                       

inline void op_arg_set(int n, op_arg arg, char **p_arg, int halo){
  *p_arg = arg.data;

  if (arg.argtype==OP_ARG_GBL) {
    if (halo && (arg.acc != OP_READ)) *p_arg = blank_args;
  }
  else {
    if (arg.map==NULL)         // identity mapping
      *p_arg += arg.size*n;
    else                       // standard pointers
      *p_arg += arg.size*arg.map->map[arg.idx+n*arg.map->dim];
  }
}

void op_arg_copy_in(int n, op_arg arg, char **p_arg) {
  for (int i = 0; i < -1*arg.idx; ++i)
    p_arg[i] = arg.data + arg.map->map[i+n*arg.map->dim]*arg.size;
}

>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
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
<<<<<<< HEAD

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
#define REDUCE(x) if (arg##x->argtype == OP_ARG_GBL) {if (strncmp (arg##x->type, "double", 6) == 0) {op_mpi_reduce_double(arg##x,(double *)p_a[x-1]);} else if (strncmp (arg##x->type, "float", 5) == 0) op_mpi_reduce_float(arg##x,(float *)p_a[x-1]); else if ( strncmp (arg##x->type, "int", 3) == 0 ){op_mpi_reduce_int(arg##x,(int *)p_a[x-1]);} else if ( strncmp (arg##x->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg##x,(bool *)p_a[x-1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg##x->type); exit (0);}}

//    printf ("before alloc  \n");                                        \
//    printf ("before halo exch  \n");                                    \
//    printf ("before loop, n_upper = %d  \n", n_upper);                  \
//    printf ("before set dirty bit  \n");                                \
//    printf ("before reduce  \n");                                       \
//    printf ("before free\n");                                           \
//    printf("n_upper = %d\n", n_upper);                                  \
//    fflush(0);                                                          \

#define OP_LOOP(N) \
  void op_par_loop_##N(void (*kernel)(CHARP_LIST(N)), op_set_core * set, ARG_LIST_POINTERS(N)) { \
    char * p_a[N] = {ZERO_LIST(N)};                                     \
    op_arg args[N] = {ARG_ARR_LIST(N)};                                 \
    int halo = 0;                                                       \
    int n_upper;                                                        \
    ALLOC_POINTER_LIST(N)                                               \
    n_upper = op_mpi_halo_exchanges_seq (set, N, args);                 \
    for ( int n=0; n<n_upper; n++ ) {                                   \
      if ( n==set->core_size ) op_mpi_wait_all_seq (N,args);            \
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

=======

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
#define REDUCE(x) if (strncmp (arg##x->type, "double", 6) == 0) op_mpi_reduce_double(arg##x,(double *)p_a[x-1]); else if (strncmp (arg##x->type, "float", 5) == 0) op_mpi_reduce_float(arg##x,(float *)p_a[x-1]); else op_mpi_reduce_int(arg##x,(int *)p_a[x-1]);

#define OP_LOOP(N) \
  void op_par_loop_##N(void (*kernel)(CHARP_LIST(N)), op_set_core * set, ARG_LIST_POINTERS(N)) { \
    char * p_a[N] = {ZERO_LIST(N)};                                     \
    op_arg args[N] = {ARG_ARR_LIST(N)};                                 \
    int halo = 0;                                                       \
    int n_upper;                                                        \
    ALLOC_POINTER_LIST(N)                                               \
    n_upper = op_mpi_halo_exchanges (set, N, args);                     \
    for ( int n=0; n<n_upper; n++ ) {                                   \
      if ( n==set->core_size ) op_mpi_wait_all (N,args);                \
      if ( n==set->size) halo = 1;                                      \
      ARG_SET_LIST(N);                                                  \
      (*kernel)(PTR_LIST(N));                                           \
    }                                                                   \
    op_mpi_set_dirtybit (N, args);                                      \
    REDUCE_LIST(N)                                                      \
    FREE_LIST(N)                                                        \
 }


>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
OP_LOOP(1)  OP_LOOP(2)  OP_LOOP(3)  OP_LOOP(4)  OP_LOOP(5)  OP_LOOP(6)  OP_LOOP(7)  OP_LOOP(8)  OP_LOOP(9)  OP_LOOP(10)
OP_LOOP(11) OP_LOOP(12) OP_LOOP(13) OP_LOOP(14) OP_LOOP(15) OP_LOOP(16) OP_LOOP(17) OP_LOOP(18) OP_LOOP(19) OP_LOOP(20)
OP_LOOP(21) OP_LOOP(22) OP_LOOP(23) OP_LOOP(24) OP_LOOP(25) OP_LOOP(26) OP_LOOP(27) OP_LOOP(28) OP_LOOP(29) OP_LOOP(30)
OP_LOOP(31) OP_LOOP(32) OP_LOOP(33) OP_LOOP(34) OP_LOOP(35) OP_LOOP(36) OP_LOOP(37) OP_LOOP(38) OP_LOOP(39) OP_LOOP(40)
OP_LOOP(41) OP_LOOP(42)


void op_par_loop_6_per(void (*kernel)(char*, char*, char*, char*, char*, char*), op_set_core * set, op_arg * arg1, op_arg * arg2, op_arg * arg3, op_arg * arg4, op_arg * arg5, op_arg * arg6) { 

  char * p_a[6] = {0, 0, 0, 0, 0, 0};
  op_arg args[6] = {*arg1, *arg2, *arg3, *arg4, *arg5, *arg6}; 
  int halo = 0;
  int n_upper, n, rank, l, i, j;

  op_mpi_rank (&rank);

  op_monitor_map_mpi(arg5->map, 22908);
  fflush (stdout);

//  printf ("before alloc  \n"); 
  if (arg1->idx < -1) { p_a[1 -1] = (char *) malloc (-1*args[1 -1].idx*arg1->dim);} 
  if (arg2->idx < -1) { p_a[2 -1] = (char *) malloc (-1*args[2 -1].idx*arg2->dim);}
  if (arg3->idx < -1) { p_a[3 -1] = (char *) malloc (-1*args[3 -1].idx*arg3->dim);}
  if (arg4->idx < -1) { p_a[4 -1] = (char *) malloc (-1*args[4 -1].idx*arg4->dim);}
  if (arg5->idx < -1) { p_a[5 -1] = (char *) malloc (-1*args[5 -1].idx*arg5->dim);}
  if (arg6->idx < -1) { p_a[6 -1] = (char *) malloc (-1*args[6 -1].idx*arg6->dim);}
//  printf ("before halo exch  \n");
  n_upper = op_mpi_halo_exchanges_seq (set, 6, args);
  op_mpi_wait_all_seq (6,args);

  printf ("rank %d, n_upper = %d set size = %d, and core size = %d\n", rank, n_upper, set->size, set->core_size);
  fflush (stdout);

  for ( n=0; n<n_upper; n++ ) {
//    if ( n==set->core_size )
    if ( n==set->size) halo = 1;

    if (args[1 -1].idx < -1) op_arg_copy_in (n, args[1 -1], (char **) p_a[1 -1]);
    else {op_arg_set(n,args[1 -1],&p_a[1 -1],halo);}

    if (args[2 -1].idx < -1) op_arg_copy_in (n, args[2 -1], (char **) p_a[2 -1]);
    else {op_arg_set(n,args[2 -1],&p_a[2 -1],halo);}

    if (args[3 -1].idx < -1) op_arg_copy_in (n, args[3 -1], (char **) p_a[3 -1]);
    else {op_arg_set(n,args[3 -1],&p_a[3 -1],halo);}

    if (args[4 -1].idx < -1) op_arg_copy_in (n, args[4 -1], (char **) p_a[4 -1]);
    else {op_arg_set(n,args[4 -1],&p_a[4 -1],halo);}

    if (args[5 -1].idx < -1) op_arg_copy_in (n, args[5 -1], (char **) p_a[5 -1]);
    else {op_arg_set(n,args[5 -1],&p_a[5 -1],halo);}

    if (args[6 -1].idx < -1) op_arg_copy_in (n, args[6 -1], (char **) p_a[6 -1]);
    else {op_arg_set(n,args[6 -1],&p_a[6 -1],halo);}

    if ( n == set->size + 3 && rank == 0 ) {

//    if (n == 22980 && rank == 0) {

//      printf ("printing local index\n");

       printf ("it %d: qb %lf %lf %lf %lf %lf %lf\n",
         n,
         ((double **) p_a)[3][0],
         ((double **) p_a)[3][1],
         ((double **) p_a)[3][2],
         ((double **) p_a)[3][3],
         ((double **) p_a)[3][4],
         ((double **) p_a)[3][5]);

         fflush (stdout);

//      printf ("idx = %d dim = %d value is %d\n", arg5->idx, arg5->map->dim,
//        ((arg5->map)->map)[arg5->idx+n*arg5->map->dim]);
//      printf ("map = %d", arg5->map->map[arg5->idx+n*arg5->map->dim]);
    }

    (*kernel)(p_a[1 -1], p_a[2 -1], p_a[3 -1], p_a[4 -1], p_a[5 -1], p_a[6 -1]);

//    if (n == 4262 && rank == 0) {

/*      printf ("it 4262: qo %lf %lf %lf %lf %lf %lf\n", 
        ((double **) p_a)[4][0],
        ((double **) p_a)[4][1],
        ((double **) p_a)[4][2],
        ((double **) p_a)[4][3],
        ((double **) p_a)[4][4],
        ((double **) p_a)[4][5]);
*/

/*        int map = arg5->map->map[arg5->idx + n*arg5->map->dim];
        printf ("Mapping for iteration 4262 is %d and max size of dataset is %d and max import halo size is %d and offset is %d and halo size is %d, local element index is %d\n", 
          map, 
          arg5->dat->set->size,
          arg5->dat->set->size + OP_import_nonexec_list[arg5->dat->set->index]->size,
          291088 - arg5->dat->set->size,
          OP_import_nonexec_list[arg5->dat->set->index]->size,
          OP_import_nonexec_list[arg5->dat->set->index]->list[4633]);

          fflush (stdout);
*/
/*        printf ("From op_dat, size of arg5 is %d, map val %d it = %d, qo: %.12lf, %.12lf, %.12lf, %.12lf, %.12lf, %.12lf\n",
          arg5->size,
          map,
          n,
          (double) (arg5->data[map]),
          (double) (arg5->data[map+1*8]),
          (double) (arg5->data[map+2*8]),
          (double) (arg5->data[map+3*8]),
          (double) (arg5->data[map+4*8]),
          (double) (arg5->data[map+5*8]));
*/
//        printf ("Size of import exec is %d from rank 1 it is %d\n", 
//          OP_import_exec_list[set->index]->size);

/*        for (i = 0; i < OP_import_exec_list[set->index]->ranks_size; i++) {
            printf ("Rank %d, size %d\n", OP_import_exec_list[set->index]->ranks[i], OP_import_exec_list[set->index]->sizes[i]);
          for ( j = 0; j < OP_import_exec_list[set->index]->sizes[i]; j++ ) {
            printf ("value of imported exec from rank %d is %d\n", OP_import_exec_list[set->index]->ranks[i], OP_import_exec_list[set->index]->list[OP_import_exec_list[set->index]->disps[i]+j]);
          }
        }        
*/

/*        for (i = 0; i < OP_export_exec_list[set->index]->size; i++) {
          printf ("value of export exec at i = %d is %d\n", i, OP_export_exec_list[set->index]->list[i]);
        }
*/


/*        for (l = 0; l < OP_import_nonexec_list[arg5->dat->set->index]->ranks_size; l++) {
          printf ("i = %d (max = %d), disps = %d, ranks = %d\n", l,
            OP_import_nonexec_list[arg5->dat->set->index]->ranks_size,
            OP_import_nonexec_list[arg5->dat->set->index]->disps[l],
            OP_import_nonexec_list[arg5->dat->set->index]->ranks[l]);
        }
*/
//        fflush (stdout);
//    }

  }
//  printf ("before set dirty bit  \n");
  op_mpi_set_dirtybit (6, args);
//  printf ("before reduce  \n");
  if (arg1->argtype == 0) {
    if (strncmp (arg1->type, "double", 6) == 0) {
      op_mpi_reduce_double(arg1,(double *)p_a[1 -1]);
    }
    else if (strncmp (arg1->type, "float", 5) == 0) op_mpi_reduce_float(arg1,(float *)p_a[1 -1]);
    else if ( strncmp (arg1->type, "int", 3) == 0 ){op_mpi_reduce_int(arg1,(int *)p_a[1 -1]);}
    else if ( strncmp (arg1->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg1,(_Bool *)p_a[1 -1]);
    else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg1->type); exit (0);}
  }
  if (arg2->argtype == 0) {if (strncmp (arg2->type, "double", 6) == 0) {op_mpi_reduce_double(arg2,(double *)p_a[2 -1]);} else if (strncmp (arg2->type, "float", 5) == 0) op_mpi_reduce_float(arg2,(float *)p_a[2 -1]); else if ( strncmp (arg2->type, "int", 3) == 0 ){op_mpi_reduce_int(arg2,(int *)p_a[2 -1]);} else if ( strncmp (arg2->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg2,(_Bool *)p_a[2 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg2->type); exit (0);}} ; if (arg3->argtype == 0) {if (strncmp (arg3->type, "double", 6) == 0) {op_mpi_reduce_double(arg3,(double *)p_a[3 -1]);} else if (strncmp (arg3->type, "float", 5) == 0) op_mpi_reduce_float(arg3,(float *)p_a[3 -1]); else if ( strncmp (arg3->type, "int", 3) == 0 ){op_mpi_reduce_int(arg3,(int *)p_a[3 -1]);} else if ( strncmp (arg3->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg3,(_Bool *)p_a[3 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg3->type); exit (0);}} ; if (arg4->argtype == 0) {if (strncmp (arg4->type, "double", 6) == 0) {op_mpi_reduce_double(arg4,(double *)p_a[4 -1]);} else if (strncmp (arg4->type, "float", 5) == 0) op_mpi_reduce_float(arg4,(float *)p_a[4 -1]); else if ( strncmp (arg4->type, "int", 3) == 0 ){op_mpi_reduce_int(arg4,(int *)p_a[4 -1]);} else if ( strncmp (arg4->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg4,(_Bool *)p_a[4 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg4->type); exit (0);}} ; if (arg5->argtype == 0) {if (strncmp (arg5->type, "double", 6) == 0) {op_mpi_reduce_double(arg5,(double *)p_a[5 -1]);} else if (strncmp (arg5->type, "float", 5) == 0) op_mpi_reduce_float(arg5,(float *)p_a[5 -1]); else if ( strncmp (arg5->type, "int", 3) == 0 ){op_mpi_reduce_int(arg5,(int *)p_a[5 -1]);} else if ( strncmp (arg5->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg5,(_Bool *)p_a[5 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg5->type); exit (0);}} ; if (arg6->argtype == 0) {if (strncmp (arg6->type, "double", 6) == 0) {op_mpi_reduce_double(arg6,(double *)p_a[6 -1]);} else if (strncmp (arg6->type, "float", 5) == 0) op_mpi_reduce_float(arg6,(float *)p_a[6 -1]); else if ( strncmp (arg6->type, "int", 3) == 0 ){op_mpi_reduce_int(arg6,(int *)p_a[6 -1]);} else if ( strncmp (arg6->type, "bool", 4) == 0 ) op_mpi_reduce_bool(arg6,(_Bool *)p_a[6 -1]); else { printf ("OP2 error: unrecognised type for reduction, type %s\n",arg6->type); exit (0);}}
//  printf ("before free\n");
  if (arg1->idx < -1) {free (p_a[1 -1]);}
  if (arg2->idx < -1) {free (p_a[2 -1]);}
  if (arg3->idx < -1) {free (p_a[3 -1]);}
  if (arg4->idx < -1) {free (p_a[4 -1]);}
  if (arg5->idx < -1) {free (p_a[5 -1]);}
  if (arg6->idx < -1) {free (p_a[6 -1]);} 
}
