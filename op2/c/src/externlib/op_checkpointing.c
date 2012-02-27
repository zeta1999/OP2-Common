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
* op_checkpointing.c
*
* Implements the checkpointing routines for OP2
*
* written by: Istvan Reguly, (Started 02-27-2012)
*/

#include "op_lib_core.h"
#include "op_lib_c.h"
#include <op_checkpointing.h>
#include "hdf5.h"
#include "hdf5_hl.h"

#ifndef backupSize
#define backupSize 1000
#endif

// How backing up op_arg_gbls works:

// since DATA encapsulated by op_arg (created by an op_arg_gbl) has a
// primitive type (unlike op_arg_dats where it is an op_dat) no OP2 data
// persists between subsequent calls to the same op_par_loop. So we either
// store DATA on a per-loop basis, which would result in a triple-level
// indirection (i.e. loop idx, arg idx, data idx) or we only store a
// unique index and use that to access a loop index independent storage
// We do the latter.

int gbl_backup_index = 0, gbl_restore_index = 0, gbl_max = 0;
int *gbl_counter=NULL; //amount of data stored per backed up op_arg_gbl
int *gbl_storage_max=NULL; //max amount of data stored per backed up op_arg_gbl
char **gbl_storage=NULL; //where the data is stored

int loop_max = 0;
int *loop_gbl_max=NULL;
int **loop_gbl_args=NULL;

op_backup_state backup_state = OP_BACKUP_GATHER;
const char* filename;
hid_t file;
herr_t status;

#define check_hdf5_error(err)           __check_hdf5_error      (err, __FILE__, __LINE__)

void __check_hdf5_error(herr_t err, const char *file, const int line) {
  if (err < 0) {
    printf("%s(%i) : OP2_HDF5_error() Runtime API error %d.\n", file, line, (int)err);
      exit(-1);
  }
}

bool file_exists(const char * file_name)
{
    if (FILE * file = fopen(file_name, "r"))
    {
        fclose(file);
        return true;
    }
    return false;
}

void store_gbl(op_arg *arg) {
  if (arg->index == -1) { //if it has not been seen before
    if (gbl_backup_index == gbl_max) { //allocate some more space if we have ran out of it
      gbl_max += 10;
      printf("Allocing more storage for op_arg_gbls: gbl_max = %d\n",gbl_max);
      gbl_counter = (int *)realloc(gbl_counter, gbl_max*sizeof(int));
      for (int i = gbl_backup_index; i < gbl_max; i++) gbl_counter[i] = 0;
      gbl_storage_max = (int *)realloc(gbl_storage_max, gbl_max*sizeof(int));
      for (int i = gbl_backup_index; i < gbl_max; i++) gbl_storage_max[i] = 0;
      gbl_storage = (char **)realloc(gbl_storage, gbl_max*sizeof(char *));
      for (int i = gbl_backup_index; i < gbl_max; i++) {
        gbl_storage[i] = NULL;
      }
    }
    arg->index = gbl_backup_index; //give it the next available index
    gbl_backup_index++;
    gbl_restore_index = gbl_backup_index; //keep them in sync
  }

  if (gbl_counter[arg->index] == gbl_storage_max[arg->index]) { //if we ran out of space for backing up this particular op_arg_gbl
    gbl_storage_max[arg->index] += backupSize * arg->size;
    gbl_storage[arg->index] = (char *)realloc(gbl_storage[arg->index], gbl_storage_max[arg->index]);
    printf("Allocing more storage for op_arg_gbl: %d gbl_storage_max = %d\n", arg->index, gbl_storage_max[arg->index]);
  }

  memcpy(&gbl_storage[arg->index][gbl_counter[arg->index]], arg->data, arg->size); //save it
  gbl_counter[arg->index]+=arg->size;
}

void restore_gbl(op_arg *arg) {
  if (arg->index == -1) { //if it has not been seen before
    arg->index = gbl_restore_index;
    gbl_restore_index++; //only increasing the op_restore_index, gbl_backup_index has the total number of op_arg_gbls backed up
  }
  //sanity checks
  if (arg->index > gbl_backup_index) {
    printf("ERROR: Trying to restore more op_arg_gbls than saved\n");
    exit(-1);
  }
  if (gbl_counter[arg->index] >= gbl_storage_max[arg->index]) {
    printf("ERROR: Trying to restore op_arg_gbls beyond the restore point, total: %d queried: %d\n", gbl_storage_max[arg->index], gbl_counter[arg->index]);
    exit(-1);
  }
  memcpy(arg->data, &gbl_storage[arg->index][gbl_counter[arg->index]], arg->size); //restore it
  gbl_counter[arg->index]+=arg->size;
}

bool op_checkpointing_init(const char *file_name) {
  filename = file_name;
  if (!file_exists(filename)) {
    backup_state = OP_BACKUP_GATHER;
    printf("//\n// Backup mode\n//\n");
    for (int i = 0; i < OP_dat_index; i++) {
      OP_dat_list[i]->status = OP_UNDECIDED;
    }
    return false;
  } else {
    file = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
    backup_state = OP_BACKUP_LEADIN;
    printf("//\n// Restore mode\n//\n");
    //load everyting here, and set dat->status
    for (int i = 0; i < OP_dat_index; i++) {
      if (H5LTfind_dataset(file, OP_dat_list[i]->name)) {
        if (strcmp(OP_dat_list[i]->type,"int")==0) {
          check_hdf5_error(H5LTread_dataset (file,  OP_dat_list[i]->name, H5T_NATIVE_INT, OP_dat_list[i]->data));
        } else if (strcmp(OP_dat_list[i]->type,"float")==0) {
          check_hdf5_error(H5LTread_dataset (file,  OP_dat_list[i]->name, H5T_NATIVE_FLOAT, OP_dat_list[i]->data));
        } else if (strcmp(OP_dat_list[i]->type,"double")==0) {
          check_hdf5_error(H5LTread_dataset (file,  OP_dat_list[i]->name, H5T_NATIVE_DOUBLE, OP_dat_list[i]->data));
        } else {
          printf("Unsupported data type in op_arg_dat() %s\n",  OP_dat_list[i]->name);
          exit(-1);
        }
      }
    }
    //restore control vars
    check_hdf5_error(H5LTread_dataset (file,  "gbl_backup_index", H5T_NATIVE_INT, &gbl_backup_index));
    gbl_restore_index = 0;
    gbl_max = gbl_backup_index;
    printf("Restoring %d op_arg_gbls\n", gbl_backup_index);
    gbl_counter = (int *)malloc(gbl_max*sizeof(int));
    check_hdf5_error(H5LTread_dataset (file,  "gbl_counter", H5T_NATIVE_INT, gbl_counter));
    printf("First op_arg_gbl has %d entries\n", gbl_counter[0]);
    gbl_storage_max = (int *)malloc(gbl_max*sizeof(int));
    gbl_storage = (char **)malloc(gbl_max*sizeof(char*));
    for (int i = 0; i< gbl_max; i++) {
      gbl_storage_max[i] = gbl_counter[i];
      gbl_counter[i] = 0;
      gbl_storage[i] = (char *)malloc(gbl_storage_max[i]*sizeof(char*));
    }
    char buffer[16]; if (gbl_backup_index > 100000) {printf("too many globals... correct me in op_checkpointing.c\n"); exit(-1);}
    memset(buffer,0,16);
    for (int i = 0; i < gbl_backup_index; i++) {
      sprintf(buffer, "gbl_storage%d",i);
      check_hdf5_error(H5LTread_dataset (file,  buffer, H5T_NATIVE_CHAR, gbl_storage[i]));
      printf("Restoring op_arg_gbl%d\n", i);
    }

    check_hdf5_error(H5LTread_dataset (file,  "loop_max", H5T_NATIVE_INT, &loop_max));
    loop_gbl_args = (int **)malloc(loop_max*sizeof(int *));
    loop_gbl_max = (int *)malloc(loop_max*sizeof(int));
    for (int i = 0; i < loop_max; i++) {
      loop_gbl_args[i] = NULL;
      loop_gbl_max[i] = 0;
    }

    printf("restoring %d loops\n", loop_max);
    check_hdf5_error(H5LTread_dataset (file,  "loop_gbl_max", H5T_NATIVE_INT, loop_gbl_max));
    memset(buffer,0,16); if (loop_max > 1000) {printf("too many loops... correct me in op_checkpointing.c\n"); exit(-1);}
    for (int i = 0; i < loop_max; i++) {
      if (loop_gbl_max[i] !=0) {
        sprintf(buffer, "loop_gbl_args%d",i);
        loop_gbl_args[i] = (int *)malloc(loop_gbl_max[i] * sizeof(int));
        check_hdf5_error(H5LTread_dataset (file,  buffer, H5T_NATIVE_INT, loop_gbl_args[i]));
        printf("restoring loop %d with %d op_arg_gbls\n", i, loop_gbl_max[i]);
      } else loop_gbl_args[i] = NULL;
    }
    check_hdf5_error(H5Fclose(file));
    return true;
  }
}

void op_checkpointing_after(op_arg *args, int nargs, int loop_id) {
  if (loop_id >= loop_max) { //if we ran out of space for storing per loop data, allocate some more
    printf("Allocing more storage for loops: loop_max = %d\n",loop_max);
    loop_max += 10;
    loop_gbl_args = (int **)realloc(loop_gbl_args, loop_max*sizeof(int *));
    loop_gbl_max = (int *)realloc(loop_gbl_max, loop_max*sizeof(int));
    for (int i = loop_max-10; i < loop_max; i++) {
      loop_gbl_args[i] = NULL;
      loop_gbl_max[i] = 0;
    }
  }
  int ctr = 0;
  for (int i = 0; i < nargs; i++) { //count the number of op_arg_gbls to be backed up
    if (args[i].argtype == OP_ARG_GBL &&
      args[i].acc != OP_READ) {
      ctr++;
    }
  }
  if (ctr == 0) return;
  else if (loop_gbl_args[loop_id] == NULL) { //if we haven't encountered this loop before, allocate memory to store indices and set them to -1
    loop_gbl_max[loop_id] = ctr;
    loop_gbl_args[loop_id] = (int *)malloc(ctr*sizeof(int));
    for (int i = 0; i < ctr; i++) loop_gbl_args[loop_id][i] = -1;
    printf("New par_loop with op_arg_gbls (%d)\n", ctr);
  }
  if (backup_state == OP_BACKUP_GATHER) { //save control variables (op_arg_gbls)
    ctr = 0;
    for (int i = 0; i < nargs; i++) {
      if (args[i].argtype == OP_ARG_GBL &&
        args[i].acc != OP_READ) {

        args[i].index = loop_gbl_args[loop_id][ctr]; //this is -1 if this op_arg_gbl hasn't been backed up before
        store_gbl(&args[i]);
        loop_gbl_args[loop_id][ctr] = args[i].index; //now it has a proper index
        ctr++;
      }
    }
  } else if (backup_state == OP_BACKUP_LEADIN) { //restore control variables (op_arg_gbls)
    ctr = 0;
    for (int i = 0; i < nargs; i++) {
      if (args[i].argtype == OP_ARG_GBL &&
      args[i].acc != OP_READ) {

        args[i].index = loop_gbl_args[loop_id][ctr];
        restore_gbl(&args[i]);
        loop_gbl_args[loop_id][ctr] = args[i].index; //these should have the same indices as in the backup process provided the control flows are the same (and if they are not, then we are in trouble)
        ctr++;
      }
    }
  }
}

bool op_checkpointing_before(op_arg *args, int nargs) {
  for (int i = 0; i < nargs; i++) { //flag variables that are touched (we do this everytime it is called, may be a little redundant)
    if (args[i].argtype == OP_ARG_DAT && args[i].argtype != OP_READ) args[i].dat->ever_written = true;
  }

  if (backup_state == OP_BACKUP_GATHER) {
    //do something clever here, like gathering statistics. Backup of control variables (i.e. gbls) happens after the loop
  } else  if (backup_state == OP_BACKUP_LEADIN) {
    //??, but restoring control variables (op_arg_gbls) happens after the loop
    return false;
  } else if (backup_state == OP_BACKUP_RESTORE) {
    //this is the point where we do the switch from restore mode to computation mode
    backup_state = OP_BACKUP_GATHER;
    for (int i = 0; i < OP_dat_index; i++) {
      OP_dat_list[i]->status = OP_UNDECIDED;
    }
  } else  if (backup_state == OP_BACKUP_BEGIN) {
    //where we start backing up stuff
    printf("Creating hdf5 file %s\n", filename);
    file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    //write datasets
    for (int i = 0; i < nargs; i++) {
      if (args[i].argtype == OP_ARG_DAT &&
        args[i].dat->ever_written &&
        args[i].dat->status == OP_UNDECIDED &&
        args[i].acc != OP_WRITE) {
        //write it to disk
        args[i].dat->status = OP_SAVED;
        hsize_t dims[1];
        dims[0] = args[i].dat->dim * args[i].dat->set->size;
        if (strcmp(args[i].type,"int")==0) {
          check_hdf5_error(H5LTmake_dataset(file, args[i].dat->name, 1, dims, H5T_NATIVE_INT, args[i].dat->data));
        } else if (strcmp(args[i].type,"float")==0) {
          check_hdf5_error(H5LTmake_dataset(file, args[i].dat->name, 1, dims, H5T_NATIVE_FLOAT, args[i].dat->data));
        } else if (strcmp(args[i].type,"double")==0) {
          check_hdf5_error(H5LTmake_dataset(file, args[i].dat->name, 1, dims, H5T_NATIVE_DOUBLE, args[i].dat->data));
        } else {
          printf("Unsupported data type in op_arg_dat() %s\n", args[i].dat->name);
          exit(-1);
        }
        printf("Backed up %s\n", args[i].dat->name);
      } else if (args[i].argtype == OP_ARG_DAT &&
             args[i].dat->status == OP_UNDECIDED &&
             args[i].acc == OP_WRITE) { //SHOULD ACCOUNT FOR PARTIAL WRITES OF INDIRECT SETS!!
        //if it is written to then we don't have to back it up
        args[i].dat->status = OP_NOT_SAVED;
        printf("Discarding %s\n", args[i].dat->name);
      }
    }
    backup_state = OP_BACKUP_IN_PROCESS;
    bool done = true;
    for (int i = 0; i < OP_dat_index; i++) {
      if (OP_dat_list[i]->status == OP_UNDECIDED && OP_dat_list[i]->ever_written) {
        done = false;
      }
    }
    if (done) backup_state = OP_BACKUP_END;
  } else  if (backup_state == OP_BACKUP_IN_PROCESS) {
    //when we have already begun backing up, but there are a few datasets that are undecided (whether or not they should be backed up)
    for (int i = 0; i < nargs; i++) {
      if (args[i].argtype == OP_ARG_DAT &&
        args[i].dat->ever_written &&
        args[i].dat->status == OP_UNDECIDED &&
        args[i].acc != OP_WRITE) {
        //write it to disk
        args[i].dat->status = OP_SAVED;
        hsize_t dims[1];
        dims[0] = args[i].dat->dim * args[i].dat->set->size;
        if (strcmp(args[i].type,"int")==0) {
          check_hdf5_error(H5LTmake_dataset(file, args[i].dat->name, 1, dims, H5T_NATIVE_INT, args[i].dat->data));
        } else if (strcmp(args[i].type,"float")==0) {
          check_hdf5_error(H5LTmake_dataset(file, args[i].dat->name, 1, dims, H5T_NATIVE_FLOAT, args[i].dat->data));
        } else if (strcmp(args[i].type,"double")==0) {
          check_hdf5_error(H5LTmake_dataset(file, args[i].dat->name, 1, dims, H5T_NATIVE_DOUBLE, args[i].dat->data));
        } else {
          printf("Unsupported data type in op_arg_dat() %s\n", args[i].dat->name);
          exit(-1);
        }
        printf("Backing up %s, delayed\n", args[i].dat->name);
      } else if (args[i].argtype == OP_ARG_DAT &&
             args[i].dat->status == OP_UNDECIDED &&
               args[i].acc == OP_WRITE) { //SHOULD ACCOUNT FOR PARTIAL WRITES OF INDIRECT SETS!!
        //if it is written to then we don't have to back it up
        args[i].dat->status = OP_NOT_SAVED;
        printf("Discarding %s, delayed\n", args[i].dat->name);
      }
    }
    bool done = true;
    for (int i = 0; i < OP_dat_index; i++) {
      if (OP_dat_list[i]->status == OP_UNDECIDED && OP_dat_list[i]->ever_written) {
        done = false;
      }
    }
    if (done) backup_state = OP_BACKUP_END;
  }

  if (backup_state == OP_BACKUP_END) {
    //either timed out or ended, if it's the former, back up everything left
    for (int i = 0; i < OP_dat_index; i++) {
      if (OP_dat_list[i]->status == OP_UNDECIDED && OP_dat_list[i]->ever_written) {
        OP_dat_list[i]->status = OP_SAVED;
        hsize_t dims[1];
        dims[0] = OP_dat_list[i]->dim * OP_dat_list[i]->set->size;
        if (strcmp(OP_dat_list[i]->type,"int")==0) {
          check_hdf5_error(H5LTmake_dataset(file, OP_dat_list[i]->name, 1, dims, H5T_NATIVE_INT, OP_dat_list[i]->data));
        } else if (strcmp(OP_dat_list[i]->type,"float")==0) {
          check_hdf5_error(H5LTmake_dataset(file, OP_dat_list[i]->name, 1, dims, H5T_NATIVE_FLOAT, OP_dat_list[i]->data));
        } else if (strcmp(OP_dat_list[i]->type,"double")==0) {
          check_hdf5_error(H5LTmake_dataset(file, OP_dat_list[i]->name, 1, dims, H5T_NATIVE_DOUBLE, OP_dat_list[i]->data));
        } else {
          printf("Unsupported data type in op_dat %s\n", OP_dat_list[i]->name);
          exit(-1);
        }
        printf("Timeout, force saving %s\n", args[i].dat->name);
      }
    }

    //write control variables
    hsize_t dims[1];
    dims[0] = 1;
    check_hdf5_error(H5LTmake_dataset(file, "gbl_backup_index", 1, dims, H5T_NATIVE_INT, &gbl_backup_index));
    dims[0] = gbl_backup_index;
    check_hdf5_error(H5LTmake_dataset(file, "gbl_counter", 1, dims, H5T_NATIVE_INT, gbl_counter));
    for (int i = 0; i<gbl_backup_index; i++) printf ("gbl_counter[%d] = %d\n",i , gbl_counter[i]);
    char buffer[16]; if (gbl_backup_index > 100000) {printf("too many globals... correct me in op_checkpointing.c\n"); exit(-1);}
    memset(buffer,0,16);
    for (int i = 0; i < gbl_backup_index; i++) {
      dims[0] = gbl_counter[i];
      sprintf(buffer, "gbl_storage%d",i);
      check_hdf5_error(H5LTmake_dataset(file, buffer, 1, dims, H5T_NATIVE_CHAR, gbl_storage[i]));
    }

    dims[0] = 1;
    check_hdf5_error(H5LTmake_dataset(file, "loop_max", 1, dims, H5T_NATIVE_INT, &loop_max));
    dims[0] = loop_max;
    check_hdf5_error(H5LTmake_dataset(file, "loop_gbl_max", 1, dims, H5T_NATIVE_INT, loop_gbl_max));

    memset(buffer,0,16); if (loop_max > 1000) {printf("too many loops... correct me in op_checkpointing.c\n"); exit(-1);}
    for (int i = 0; i < loop_max; i++) {
      if (loop_gbl_max[i] != 0) {
        dims[0] = loop_gbl_max[i];
        sprintf(buffer, "loop_gbl_args%d",i);
        check_hdf5_error(H5LTmake_dataset(file, buffer, 1, dims, H5T_NATIVE_INT, loop_gbl_args[i]));
      }
    }
    printf("Saved control variables\n");
    check_hdf5_error(H5Fclose(file));
    printf("Done backing up\n");
    //finished backing up, reset everything, prepare to be backed up at a later point
    backup_state = OP_BACKUP_GATHER;
    for (int i = 0; i < OP_dat_index; i++) {
      OP_dat_list[i]->status = OP_UNDECIDED;
    }
  }
  return true;
}
