#ifndef __OP2_FOR_RT_WRAPPERS__
#define __OP2_FOR_RT_WRAPPERS__

#ifdef __cplusplus
extern "C" {
#endif

op_plan * FortranPlanCallerOpenMP ( char name[],
                                    int setId,
                                    int argsNumber,
                                    int args[],
                                    int idxs[],
                                    int maps[],
                                    int accs[],
                                    int indsNumber,
                                    int inds[],
                                    int argsType[],
                                    int partitionSize
                                  );

op_plan * FortranPlanCallerCUDA ( char name[],
                                  int setId,
                                  int argsNumber,
                                  int args[],
                                  int idxs[],
                                  int maps[],
                                  int accs[],
                                  int indsNumber,
                                  int inds[],
                                  int argsType[],
                                  int partitionSize
                                );

void FortranToCMapping (op_arg * arg);

void checkCMapping (op_arg arg);

op_plan * checkExistingPlan (char name[], op_set set,
  int partitionSize, int argsNumber, op_arg args[], 
  int indsNumber, int inds[]);

#ifdef __cplusplus
}
#endif

#endif

