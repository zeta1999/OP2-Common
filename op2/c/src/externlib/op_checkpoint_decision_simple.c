/*
Open source copyright declaration based on BSD open source template:
http://www.opensource.org/licenses/bsd-license.php

* Copyright (c) 2009, Mike Giles
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * The name of Mike Giles may not be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY Mike Giles ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL Mike Giles BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/*
* op_checkpoint_decision_simple.c
*
* Implements a simple decision algorithm
*
* written by: Istvan Reguly, (Started 03-04-2012)
*/

#include "op_lib_core.h"
#include "op_lib_c.h"
#include "op_checkpoint_decision.h"

int *loops = NULL;
int max_loops = 0;
long *data_discarded = NULL;
long best_discarded = 0;
int discarding_loops = 0;

bool is_fully_written(op_arg *args, int nargs, int number) {
  //if it is directly accessed then it is (hopefully) fully written
  if (args[number].map == OP_ID) return true;

  //if the mapping only maps to a part of the target dataset (e.g. loop on the boundary edges, writing boundary nodes won't write all nodes)
  //this line right below is not a very good estimate, so we have to actually iterate through the whole map
  //if (args[number].map->from->size * args[number].map->dim < args[number].map->to->size) return false;
  bool *touched = (bool *)malloc(args[number].map->to->size * sizeof(bool));
  memset(touched, 0, args[number].map->to->size * sizeof(bool));
  for (int i = 0; i<args[number].map->from->size; i++) {
    for (int j = 0; j< args[number].map->dim; j++) {
      touched[args[number].map->map[i*args[number].map->dim + j]] = true;
    }
  }
  for (int i = 0; i<args[number].map->to->size;i++) if (touched[i]==false) return false;
  //it might be wise to set this as a property of the mapping if we don't want to do this over and over again.
  //In that case, this should probably be moved to op_plan_core
  free(touched);

  //check to see if all the indices in the map are accessed (i.e. OP_ALL or 0..max_dim, currently only the latter)
  //it might happen that two maps are used to access the same dataset
  //but it the user shouldnt access the same dat with the same map and index (I don't check for that)
  //if (args[number].idx == OP_ALL) return true;
  int counter = args[number].map->dim;
  for (int i = 0; i<nargs; i++) {
    if (args[i].dat == args[number].dat) {
      counter--;
      if (args[i].map != args[number].map) {
        return false;
      }
    }
  }
  if (counter > 0) return false;

  return true;
}

void gather_statistics(op_arg *args, int nargs, int loop_id) {
  //check if we have seen this one before
  if (loop_id < max_loops && loops[loop_id]>0) return;

  //if we ran out of space, allocate some more
  if (loop_id >= max_loops) {
    max_loops += 10;
    loops = (int *)realloc(loops,max_loops*sizeof(int));
    memset(&loops[max_loops-10],0,max_loops*sizeof(int));
    data_discarded = (long *)realloc(data_discarded,max_loops*sizeof(long));
    memset(&data_discarded[max_loops-10],0,max_loops*sizeof(long));
  }

  //check which datasets are fully written to - i.e. they can be discarded when backing up
  int *done = (int *)malloc(OP_dat_index*sizeof(int));
  memset(done,0, OP_dat_index*sizeof(int));
  for (int i = 0; i < nargs; i++) {
    if (args[i].argtype == OP_ARG_DAT &&
           args[i].dat->status == OP_UNDECIDED &&
           args[i].acc == OP_WRITE &&
           done[args[i].dat->index] == 0) {
        if (is_fully_written(args,nargs,i)) done[args[i].dat->index] = 1;
        else done[args[i].dat->index] = -1;
    }
  }
  for (int i = 0; i< OP_dat_index; i++) {
    if (done[i] == 1) data_discarded[loop_id] += OP_dat_list[i]->size * OP_dat_list[i]->set->size;
  }
  if (data_discarded[loop_id]>0) discarding_loops++;
  free(done);

  //set the rank of the current loop in terms of how much data is discarded
  int rank = 1;
  for (int i = 0; i < max_loops; i++) {
    if (loops[i] > 0 && data_discarded[i] > data_discarded[loop_id]) rank = loops[i];
  }
  for (int i = 0; i < max_loops; i++) {
    if (loops[i] >= rank) loops[i]++;
  }
  loops[loop_id] = rank;
  if (rank == 1) best_discarded = data_discarded[loop_id];
}

bool should_backup(int loop_id, double time_left, double timeout) {
  //if below 10% of the timeout, just start backing up
  if (time_left/timeout < 0.1) {
    return true;
    printf("Decision timing out, triggering checkpoint...\n");
  }
  //Wait 50% of the timeout for the best 10%
  if (time_left/timeout > 0.5 && data_discarded[loop_id] < best_discarded*0.9) return false;
  //Wait another 20% of the timeout for the best 50%
  if (time_left/timeout > 0.3 && data_discarded[loop_id] < best_discarded*0.5) return false;
  //Go ahead with anything that is better than nothing
  if (data_discarded[loop_id] == 0) return false;
  printf("Triggering checkpoint, loop: %d, data discarded: %f kB (%f)\n", loop_id, (double)data_discarded[loop_id]/1024.0, (double)data_discarded[loop_id]/(double)best_discarded);
  return true;
}

void decision_init() {
  max_loops = 10;
  loops = (int *)malloc(max_loops*sizeof(int));
  memset(loops, 0, max_loops*sizeof(int));
  data_discarded = (long *)malloc(max_loops*sizeof(long));
  memset(data_discarded, 0, max_loops*sizeof(long));
}
