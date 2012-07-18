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

#include <op_lib_core.h>


inline void op_arg_set(int n, op_arg arg, char **p_arg){
  *p_arg = arg->data;

  if ( arg->argtype == OP_ARG_DAT ) {

    if ( arg->map == NULL ) // identity mapping
      *p_arg += arg->size*n;
    else                       // standard pointers
      *p_arg += arg->size*arg->map->map[arg->idx+n*arg->map->dim];
  }
}

#define CHARP_LIST(N) COMMA_LIST(N,CHARP)
#define CHARP(x) char*

#define CHARP_LIST_2(N) SEMI_LIST(N,CHARP2)
#define CHARP2(x) char* ptr##x

#define ARG_SET_LIST(N) SEMI_LIST(N,ARGSET)
#define ARGSET(x) op_arg_set(i,arg##x,&ptr##x)

#define PTR_LIST(N) COMMA_LIST(N,PTRL)
#define PTRL(x) ptr##x

#define OP_LOOP(N) \
  void op_par_loop_##N(void (*kernel)(CHARP_LIST(N)), op_set_core * set, ARG_LIST(N)) { \
    int i;                \
    for(i = 0; i<set->size; i++) {          \
      CHARP_LIST_2(N);              \
      ARG_SET_LIST(N);              \
      (*kernel)(PTR_LIST(N));           \
    }                 \
  }


OP_LOOP(1)  OP_LOOP(2)  OP_LOOP(3)  OP_LOOP(4)  OP_LOOP(5)  OP_LOOP(6)  OP_LOOP(7)  OP_LOOP(8)  OP_LOOP(9)  OP_LOOP(10)
OP_LOOP(11) OP_LOOP(12) OP_LOOP(13) OP_LOOP(14) OP_LOOP(15) OP_LOOP(16) OP_LOOP(17) OP_LOOP(18) OP_LOOP(19) OP_LOOP(20)
OP_LOOP(21) OP_LOOP(22) OP_LOOP(23) OP_LOOP(24) OP_LOOP(25) OP_LOOP(26) OP_LOOP(27) OP_LOOP(28) OP_LOOP(29) OP_LOOP(30)
OP_LOOP(31) OP_LOOP(32) OP_LOOP(33) OP_LOOP(34) OP_LOOP(35) OP_LOOP(36) OP_LOOP(37) OP_LOOP(38) OP_LOOP(39) OP_LOOP(40)
