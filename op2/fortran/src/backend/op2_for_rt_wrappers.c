#include <op_lib_core.h>
#include <op_rt_support.h>
#include <op_lib_c.h>

#include "../../include/op2_for_C_wrappers.h"
#include "../../include/op2_for_rt_wrappers.h"

extern int OP_plan_index, OP_plan_max;
extern op_plan * OP_plans;

#define ERR_INDEX -1

void op_partition_wrapper (const char* lib_name, const char* lib_routine,
  op_set prime_set, op_map prime_map, op_dat coords) {

  /* // copying and decrementing map */
  /* op_map newMap = (op_map) calloc (1, sizeof (op_map_core)); */

  /* // warning: the new map is an exact replica of the original map */
  /* // except that it's a 0->N-1 mapping */
  /* newMap->index = prime_map->index; */
  /* newMap->from = prime_map->from; */
  /* newMap->to = prime_map->to; */
  /* newMap->dim = prime_map->dim; */
  /* newMap->name = prime_map->name; */
  /* newMap->user_managed = prime_map->user_managed; */

  /* newMap->map = (int *) calloc (prime_map->from->size * prime_map->dim, sizeof (int)); */

  /* for ( int i = 0; i < prime_map->from->size * prime_map->dim; i++ ) { */
  /*   newMap->map[i] = prime_map->map[i] - 1; */
  /* } */

  op_partition (lib_name, lib_routine, prime_set, prime_map, coords);
}

void FortranToCMapping (op_arg * arg) {
  op_map newMapStruct = (op_map) calloc (1, sizeof(op_map_core));
  int * newMap = (int *) calloc (arg->map->from->size * arg->map->dim, sizeof (int));

  for ( int i = 0; i < arg->map->from->size * arg->map->dim; i++ )
    newMap[i] = arg->map->map[i] -1;

  // do not deallocate the old map, because it could be used elsewhere
  // - only get a new map in this op_arg
  newMapStruct->map = newMap;
  newMapStruct->index = arg->map->index;
  newMapStruct->from = arg->map->from;
  newMapStruct->to = arg->map->to;
  newMapStruct->dim = arg->map->dim;
  newMapStruct->name = arg->map->name;
  newMapStruct->user_managed = arg->map->user_managed;
  arg->map = newMapStruct;
}

void checkCMapping (op_arg arg) {
//  printf ("Now checking mapping %s\n", arg.map->name);
  for ( int i = 0; i < arg.map->from->size * arg.map->dim; i++ ) {
    if ( arg.map->map[i] >= arg.map->to->size ) {
      printf ("Invalid mapping 1\n");
      exit (0);
    }
    if ( arg.map->map[i] < 0 ) {
      printf ("Invalid mapping 2, value is %d\n", arg.map->map[i]);
      exit (0);
    }
  }
//  printf ("Mapping %s is fine\n", arg.map->name);
}

op_plan * checkExistingPlan (char name[], op_set set,
  int partitionSize, int argsNumber, op_arg args[],
  int indsNumber, int inds[]) {

  int match =0, ip = 0;

  while ( match == 0 && ip < OP_plan_index )
  {
    if ( //( strcmp ( name, OP_plans[ip].name ) == 0 )
        ( set == OP_plans[ip].set )
        && ( argsNumber == OP_plans[ip].nargs )
        && ( indsNumber == OP_plans[ip].ninds )
        && ( partitionSize == OP_plans[ip].part_size ) )
    {
      match = 1;
      for ( int m = 0; m < argsNumber; m++ )
      {
        match = match && ( args[m].dat == OP_plans[ip].dats[m] )
//          && ( args[m].map->index == OP_plans[ip].maps[m]->index )
          && ( args[m].idx == OP_plans[ip].idxs[m] )
          && ( args[m].acc == OP_plans[ip].accs[m] );
      }
    }
    ip++;
  }

  if ( match )
  {
    ip--;
    if ( OP_diags > 3 )
      printf ( " old execution plan #%d\n", ip );
    OP_plans[ip].count++;
    return &( OP_plans[ip] );
  } else
    return NULL;
}


op_plan * FortranPlanCaller (char name[], op_set set,
  int partitionSize, int argsNumber, op_arg args[],
  int indsNumber, int inds[]) {

  op_plan * generatedPlan = NULL;

  generatedPlan = checkExistingPlan (name, set,
    partitionSize, argsNumber, args,
    indsNumber, inds);

  if ( generatedPlan != NULL ) return generatedPlan;

  /* copy the name because FORTRAN doesn't allow allocating
     strings */
  int nameLen = strlen (name);
  char * heapName = (char *) calloc (nameLen, sizeof(char));
  strncpy (heapName, name, nameLen);

/*   for ( int i = 0; i < argsNumber; i++ ) { */
/*     if ( inds[i] != -1 ) { */
/*       if ( args[i].map == NULL) { */
/*         printf ("Null map\n"); */
/*         exit (0); */
/*       } */
/* //      printf ("Now checking argument %d\n", i); */
/*       FortranToCMapping (&args[i]); */
/*       checkCMapping (args[i]); */
/*     } */
/*   } */

  /* call the C OP2 function */
  generatedPlan = op_plan_get (heapName, set, partitionSize,
    argsNumber, args, indsNumber, inds);

  return generatedPlan;
}


int getSetSizeFromOpArg (op_arg * arg)
{
  return arg->dat->set->size;
}
