#ifndef __OP2_FOR_RT_WRAPPERS__
#define __OP2_FOR_RT_WRAPPERS__

#ifdef __cplusplus
extern "C" {
#endif

<<<<<<< HEAD

void op_partition_wrapper (const char* lib_name, const char* lib_routine,
  op_set prime_set, op_map prime_map, op_dat coords);

void FortranToCMapping (op_arg * arg);

void checkCMapping (op_arg arg);

op_plan * checkExistingPlan (char name[], op_set set,
  int partitionSize, int argsNumber, op_arg args[], 
  int indsNumber, int inds[]);

/*
 *  These functions scan all declared mappings and decrement/increment their
 *  values by one (used for FORTRAN <--> C conversions)
 */

void decrement_all_mappings ();

void increment_all_mappings ();

op_plan * FortranPlanCaller (char name[], op_set set,
  int partitionSize, int argsNumber, op_arg args[], 
  int indsNumber, int inds[]);

=======
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
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6

#ifdef __cplusplus
}
#endif

#endif

