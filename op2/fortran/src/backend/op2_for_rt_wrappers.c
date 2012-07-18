#include <op_lib_core.h>
#include <op_rt_support.h>
#include <op_lib_c.h>

#include "../../include/op2_for_C_wrappers.h"
#include "../../include/op2_for_rt_wrappers.h"

extern op_plan * OP_plans;

#define ERR_INDEX -1


op_plan * FortranPlanCaller (char name[], op_set set,
  int partitionSize, int argsNumber, op_arg args[], 
  int indsNumber, int inds[]) {

  op_plan * generatedPlan = NULL;

  /* copy the name because FORTRAN doesn't allow allocating
     strings */
  int nameLen = strlen (name);
  char * heapName = (char *) calloc (nameLen, sizeof(char));
  strncpy (heapName, name, nameLen);

  /* call the C OP2 function including CUDA movement of data */
  generatedPlan = op_plan_get (heapName, set, partitionSize,
    argsNumber, args, indsNumber, inds);

  return generatedPlan;
}


int getSetSizeFromOpArg (op_arg * arg)
{
  return arg->dat->set->size;
}
