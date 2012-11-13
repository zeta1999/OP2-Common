//
// auto-generated by op2.m on 16-Oct-2012 15:15:09
//

// user function

#include "bres_calc.h"

// host stub function

void op_par_loop_bres_calc(op_kernel_descriptor *desc ){

  char const *name = desc->name;
  op_set set = desc->set;
  op_arg arg0 = desc->args[0];
  op_arg arg1 = desc->args[1];
  op_arg arg2 = desc->args[2];
  op_arg arg3 = desc->args[3];
  op_arg arg4 = desc->args[4];
  op_arg arg5 = desc->args[5];


  int    nargs   = 6;
  op_arg args[6];

  args[0] = arg0;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  args[5] = arg5;

  int    ninds   = 4;
  int    inds[6] = {0,0,1,2,3,-1};

  if (OP_diags>2) {
    printf(" kernel routine with indirection: %s\n", name);
  }
  
  char  *p_a[6] = {0,0,0,0,0,0};
  
  // initialise timers
  double cpu_t1, cpu_t2, wall_t1, wall_t2;
  op_timers_core(&cpu_t1, &wall_t1);
  
  for (int i=0; i<desc->subset->size; i++) {
    int n = desc->subset->elements[i];
    
    op_arg_set(n,args[0], &p_a[0],0);
    op_arg_set(n,args[1], &p_a[1],0);
    op_arg_set(n,args[2], &p_a[2],0);
    op_arg_set(n,args[3], &p_a[3],0);
    op_arg_set(n,args[4], &p_a[4],0);
    op_arg_set(n,args[5], &p_a[5],0);
    
    // call kernel function, passing in pointers to data
    
    bres_calc( (double *)p_a[0],  (double *)p_a[1],  (double *)p_a[2],  (double *)p_a[3],
             (double *)p_a[4],  (int *)p_a[5] );
  }
  
  // update timer record
  op_timers_core(&cpu_t2, &wall_t2);
  op_timing_realloc(3);
  OP_kernels[3].time     += wall_t2 - wall_t1;

}

void op_par_loop_bres_calc_enqueue(char const *name, op_set set,
                                  op_arg arg0,
                                  op_arg arg1,
                                  op_arg arg2,
                                  op_arg arg3,
                                  op_arg arg4,
                                  op_arg arg5 ) {
  op_kernel_descriptor kern;
  kern.name = name;
  kern.set = set;
  kern.nargs = 6;
  kern.args = (op_arg *)malloc(6*sizeof(op_arg));
  kern.args[0] = arg0;
  kern.args[1] = arg1;
  kern.args[2] = arg2;
  kern.args[3] = arg3;
  kern.args[4] = arg4;
  kern.args[5] = arg5;
  kern.function = op_par_loop_bres_calc;
  kernel_list.push_back(kern);
}
