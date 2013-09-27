/*
 * Open source copyright declaration based on BSD open source template:
 * http://www.opensource.org/licenses/bsd-license.php
 *
 * This file is part of the OP2 distribution.
 *
 * Copyright (c) 2011, Mike Giles and others. Please see the AUTHORS file in
 * the main source directory for a full list of copyright holders.
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

#include <op_lib_core.h>
#include <op_lib_cpp.h>
#include <op_util.h>
#include <vector>

#ifdef HAVE_PTSCOTCH
#include <scotch.h>
#endif

#ifdef PARMETIS_VER_4
#include <metis.h>
#endif

typedef struct {
  int a;
  int b;
} map2;

int compare (const void * a, const void * b)
{
  return ( (*(map2*)a).a - (*(map2*)b).a );
}



//propage renumbering based on a map that points to an already reordered set
void propagate_reordering(op_set from, op_set to,
  std::vector< std::vector <int> >& set_permutations) {

  if(to->size == 0) return;

  //find a map that is (to)->(from), reorder (to)
  if (set_permutations[to->index].size()==0) {
    for (int mapidx = 0; mapidx < OP_map_index; mapidx++) {
      op_map map = OP_map_list[mapidx];
      if (map->to == from && map->from == to) {
        std::vector<map2> renum(to->size);
        for (int i = 0; i < to->size; i++) {
          renum[i].a = set_permutations[from->index][map->map[map->dim*i]];
          renum[i].b = i;
        }
        qsort(&renum[0], renum.size(), sizeof(map2), compare);
        set_permutations[to->index].resize(to->size);
        for (int i = 0; i < to->size; i++)
          set_permutations[to->index][renum[i].b] = i;
        break;
      }
    }
  }
  if (set_permutations[to->index].size()==0) {
    printf("Could not find suitable mapping to renumber %s based on %s\n", to->name, from->name);
    return;
  }
  //find any maps that is (*)->to, propagate reordering
  for (int mapidx = 0; mapidx < OP_map_index; mapidx++) {
    op_map map = OP_map_list[mapidx];
    if (map->to == to && set_permutations[map->from->index].size()==0) {
      propagate_reordering(to, map->from, set_permutations);
    }
  }
}

void reorder_set(op_set set,
  int *set_permutations) {

  int set_size = set->size + set->exec_size + set->nonexec_size;
  if (set_size == 0) return;

  for (int mapidx = 0; mapidx < OP_map_index; mapidx++) {
    op_map map = OP_map_list[mapidx];
    if (map->from == set) {
      int *tempmap = (int *)malloc(set_size*sizeof(int)*map->dim);

      for (int i = 0; i < set_size; i++)
        std::copy(map->map + map->dim*i, map->map + map->dim*(i+1),
                  tempmap + map->dim*set_permutations[i]);
      free(map->map);
      map->map = tempmap;

    } else if (map->to == set) {
      for (int i = 0; i < (map->from->size+map->from->exec_size+map->from->nonexec_size)*map->dim; i++)
        map->map[i] = set_permutations[map->map[i]];
    }
  }
  op_dat_entry *item;
  TAILQ_FOREACH(item, &OP_dat_list, entries) {
    op_dat dat = item->dat;
    if (dat->set == set && dat->data != NULL) {
      char *tempdata = (char *)malloc((size_t)set_size*(size_t)dat->size);
      for (unsigned long int i = 0; i < (unsigned long int)set_size; i++)
        std::copy(dat->data + (unsigned long int)dat->size*i,
                  dat->data + (unsigned long int)dat->size*(i+1),
                  tempdata + (unsigned long int)dat->size*
                  (unsigned long int)set_permutations[i]);
      free(dat->data);
      dat->data = tempdata;
    }
  }
}

void create_loopback(std::vector<int>& row_offsets,
                     std::vector<int>& col_indices, op_map base) {
  row_offsets.resize(base->to->size+1);
  if (base->to == base->from) {
    col_indices.resize(base->dim*base->from->size);
    //if map is self-referencing, just create row_offsets with dim stride, and copy over col_indices
    std::copy(base->map, base->map+base->dim*base->from->size, col_indices.begin());
    row_offsets[0] = 0;
    for (int i = 1; i < base->from->size+1; i++) row_offsets[i] = i*base->dim;
  } else {
    //otherwise, construct self-referencing map
    col_indices.resize(base->from->size * (base->dim-1) * (base->dim)); //Worst case memory requirement

    //construct map pointing back
    std::vector<map2> loopback(base->from->size * base->dim);
    for (int i = 0; i < base->from->size*base->dim; i++) {
      loopback[i].a = base->map[i];
      loopback[i].b = i/base->dim;
    }
    qsort(&loopback[0], loopback.size(), sizeof(map2), compare);

    row_offsets[0] = 0;
    row_offsets[1] = 0;
    row_offsets[base->to->size] = 0;
    for (int i = 0; i < base->dim; i++) {
      if (base->map[base->dim*loopback[0].b+i] != 0)
        col_indices[row_offsets[1]++] = base->map[base->dim*loopback[0].b+i];
    }
    int nodectr = 0;
    for (int i = 1; i < base->from->size * base->dim; i++) {
      if (loopback[i].a != loopback[i-1].a) {
        nodectr++; row_offsets[nodectr+1] = row_offsets[nodectr];
      }

      for (int d1 = 0; d1 < base->dim; d1++) {
        int id = base->map[base->dim*loopback[i].b+d1];
        int add = (id != nodectr);
        for (int d2 = row_offsets[nodectr]; (d2 < row_offsets[nodectr+1]) && add; d2++) {
          if (col_indices[d2] == id) add = 0;
        }
        if (add) col_indices[row_offsets[nodectr+1]++] = id;
      }
    }
    if (row_offsets[base->to->size] == 0) {
      printf("Map %s is not an onto map from %s to %s, aborting renumbering...\n",
        base->name, base->from->name, base->to->name);
      return;
    }
    col_indices.resize(row_offsets[base->to->size]);
    printf("Loopback map %s->%s constructed: %d, from set %s (%d)\n",
      base->to->name, base->to->name, (int)col_indices.size(), base->from->name, base->from->size);
  }
}

void op_renumber(op_map base) {
#ifndef HAVE_PTSCOTCH
  printf("OP2 was not compiled with Scotch, no reordering.\n");
#else
  printf("Renumbering using base map %s\n", base->name);
  if (op_is_root() == 0) {
    printf("Renumbering only works with 1 rank\n");
    exit(-1);
  }
  int generated_partvec = 0;

  if (FILE * file = fopen("partvec0001_0001", "r"))
  {
    fclose(file);
    int possible[] = {1,2,4,6,8,12,16,22,24,32,64,128,192,256,512,1024,2048,4096};
    for (int i = 0; i < 18; i++) {
      char buffer[64];
      sprintf(buffer,"partvec0001_%04d",possible[i]);
      if (!(file = fopen(buffer,"r"))) continue;
      printf("Processing partitioning for %d partitions\n", possible[i]);
      int *partvec = (int *)malloc(base->to->size * sizeof(int));
      int *order = (int *)malloc(base->to->size * sizeof(int));
      int total_ctr = 0;
      for (int f = 0; f < possible[i]; f++) {
        if (f>0) {
          fclose(file);
          sprintf(buffer,"partvec%04d_%04d",f+1,possible[i]);
          file = fopen(buffer,"r");
        }
        int counter = 0;
        int id, part;
        while (fscanf(file, "%d %d", &id, &part) != EOF) {
          partvec[id] = part-1;
          order[id] = counter;
          counter++;
          total_ctr++;
        }
      }
      fclose(file);
      sprintf(buffer,"partvec%04d",possible[i]);
      op_decl_dat(base->to, 1, "int", partvec, buffer);
      sprintf(buffer,"ordering%04d",possible[i]);
      op_decl_dat(base->to, 1, "int", order, buffer);
      if (total_ctr != base->to->size)
        printf("Size mismatch %d %d\n", total_ctr, base->to->size);
      generated_partvec = 1;
    }
  }
//-----------------------------------------------------------------------------------------
// Build adjacency list
//-----------------------------------------------------------------------------------------
  std::vector<int> row_offsets;
  std::vector<int> col_indices;
  create_loopback(row_offsets,col_indices,base);

  if (generated_partvec == 0) {
#ifdef PARMETIS_VER_4
    int possible[] = {2,4,6,8,12,16,22,24,32,64,128,192,256,512,1024,2048,4096};
    for (int i = 0; i < 17; i++) {
      int *partvec = (int *)malloc(base->to->size * sizeof(int));
      int nconstr = 1;
      int edgecut;
      int nparts = possible[i];
      idx_t options[METIS_NOPTIONS];
      METIS_SetDefaultOptions(options);
      options[METIS_OPTION_OBJTYPE] = METIS_OBJTYPE_VOL;
      options[METIS_OPTION_NCUTS] = 3;
      options[METIS_OPTION_NSEPS] = 3;
      options[METIS_OPTION_NUMBERING] = 0;
      options[METIS_OPTION_MINCONN] = 1;

      METIS_PartGraphKway(&base->to->size, &nconstr, &row_offsets[0],
      &col_indices[0], NULL, NULL, NULL, &nparts, NULL, NULL, options, &edgecut, partvec);
      printf("Metis partitioning precomputed for %d partitions. Edgecut: %d\n",nparts, edgecut);
      char buffer[50];
      sprintf(buffer,"partvec%04d",possible[i]);
      op_decl_dat(base->to, 1, "int", partvec, buffer);
    }
#endif
  }

  //
  // Using SCOTCH for reordering
  //
  SCOTCH_Num baseval = 0; // start numbering from 0
  SCOTCH_Num vertnbr = base->to->size; // number of vertices in graph = number of cells in mesh
  SCOTCH_Num edgenbr = row_offsets[base->to->size];

  SCOTCH_Graph *graphptr = SCOTCH_graphAlloc();
  SCOTCH_graphInit(graphptr);

  SCOTCH_Num *verttab = &row_offsets[0];

  SCOTCH_Num *vendtab = &verttab[1]; // = NULL; // Used to calculate vertex degree = verttab[i+1] - verttab[i]
  SCOTCH_Num *velotab = NULL; // Vertex load = vertex weight
  SCOTCH_Num *vlbltab = NULL;
  SCOTCH_Num *edgetab = &col_indices[0];

  SCOTCH_Num *edlotab = NULL; // Edge load = edge weight
  SCOTCH_Num *permutation = (SCOTCH_Num*) malloc(base->to->size*sizeof(SCOTCH_Num));
  SCOTCH_Num *ipermutation = (SCOTCH_Num*) malloc(base->to->size*sizeof(SCOTCH_Num));
  SCOTCH_Num *cblkptr = (SCOTCH_Num*) malloc(base->to->size*sizeof(SCOTCH_Num));
  SCOTCH_Num *rangtab = NULL;//(SCOTCH_Num*) malloc(1 + ncell*sizeof(SCOTCH_Num));
  SCOTCH_Num *treetab = NULL;//(SCOTCH_Num*) malloc(ncell*sizeof(SCOTCH_Num));

  int mesg = 0;
  mesg = SCOTCH_graphBuild(graphptr, baseval, vertnbr, verttab, vendtab,
  velotab, vlbltab, edgenbr, edgetab, edlotab);
  if(mesg != 0){
    op_printf("Error during SCOTCH_graphBuild() \n");
    exit(-1);
  }

  SCOTCH_Strat *straptr = SCOTCH_stratAlloc();
  SCOTCH_stratInit(straptr);

  const char * strategyString = "g";
  //    char * strategyString = "(g{pass=100})";
  mesg = SCOTCH_stratGraphOrder(straptr, strategyString);
  if(mesg != 0){
    op_printf("Error during setting strategy string. \n");
    exit(-1);
  }

  mesg = SCOTCH_graphOrder(graphptr, straptr, permutation, ipermutation, cblkptr, rangtab, treetab);
  if(mesg != 0){
    op_printf("Error during SCOTCH_graphOrder() \n");
    exit(-1);
  }
  SCOTCH_graphExit(graphptr);
  SCOTCH_stratExit(straptr);

  std::vector<std::vector<int> > set_permutations(OP_set_index);
  set_permutations[base->to->index].resize(base->to->size);
  std::copy(permutation, permutation+base->to->size, set_permutations[base->to->index].begin());
  propagate_reordering(base->to, base->to, set_permutations);
  for (int i = 0; i < OP_set_index; i++) {
    if (set_permutations[i].size()) reorder_set(OP_set_list[i], &set_permutations[i][0]);
  }
  #endif
}

//propage partitioning based on a map that points to an already partitioned set
void inherit_partitioning(op_set from, op_set to,
  std::vector< std::vector <int> >& set_partitionings) {
  int to_size = to->size + to->exec_size + to->nonexec_size;
  if (to_size == 0) return;

  //find a map that is (to)->(from), partition (to)
  if (set_partitionings[to->index].size()==0) {
    for (int mapidx = 0; mapidx < OP_map_index; mapidx++) {
      op_map map = OP_map_list[mapidx];
      if (map->to == from && map->from == to) {
        set_partitionings[to->index].resize(to_size);
        //if map is 1 or 2 dimensional, just pick the first
        if (map->dim == 1 || map->dim == 2) {
          for (int i = 0; i < to_size; i++) {
            set_partitionings[to->index][i] =
             set_partitionings[from->index][map->map[map->dim*i]];
          }
        } else {
          //otherwise find the partition that an element has the most connections to
          int *destinations = (int *)malloc(map->dim * sizeof(int));
          for (int i = 0; i < to_size; i++) {
            memset(destinations, 0, sizeof(int));
            for (int j = 0; j < map->dim; j++) {
              int found = j;
              for (int k = 0; k < j; j++) {
                if (set_partitionings[from->index][map->map[map->dim*i+j]] ==
                    set_partitionings[from->index][map->map[map->dim*i+k]]) found = k;
              }
              destinations[found]++;
            }
            int target = 0;
            for (int j = 0; j < map->dim; j++) {
              target = destinations[j] > destinations[target] ? j : target;
            }
            set_partitionings[to->index][i] = set_partitionings[from->index][map->map[map->dim*i+target]];
          }
        }
        break;
      }
    }
  }
  if (set_partitionings[to->index].size()==0) {
    return;
  }
  //find any maps that is (*)->to, propagate reordering
  for (int mapidx = 0; mapidx < OP_map_index; mapidx++) {
    op_map map = OP_map_list[mapidx];
    if (map->to == to && set_partitionings[map->from->index].size()==0) {
      inherit_partitioning(to, map->from, set_partitionings);
    }
  }
}

void op_create_partitioned_blocks(op_map base) {
  OP_set_permutations = (op_set_permutation *)malloc(OP_set_index*sizeof(op_set_permutation));
  for (int i = 0; i < OP_set_index; i++) {
    OP_set_permutations[i].set = OP_set_list[i];
    OP_set_permutations[i].permutation = NULL;
    OP_set_permutations[i].num_blocks = 0;
    OP_set_permutations[i].block_offset = NULL;
    OP_set_permutations[i].block_size_owned = NULL;
    OP_set_permutations[i].block_size_full = NULL;
  }

  int set_size = base->to->size + base->to->exec_size + base->to->nonexec_size;
  std::vector<int> row_offsets;
  std::vector<int> col_indices;
  create_loopback(row_offsets,col_indices,base);
  int nparts = MAX((base->from->size+base->from->exec_size+base->from->nonexec_size)/512,60);
  int *partvec = (int *)malloc(set_size * sizeof(int));

  #ifdef HAVE_PTSCOTCH
    SCOTCH_Num baseval = 0; // start numbering from 0
    SCOTCH_Num vertnbr = set_size; // number of vertices in graph = number of cells in mesh
    SCOTCH_Num edgenbr = row_offsets[set_size];
    SCOTCH_Graph *graphptr = SCOTCH_graphAlloc();
    SCOTCH_graphInit(graphptr);
    SCOTCH_Num *verttab = &row_offsets[0];
    SCOTCH_Num *vendtab = &verttab[1]; // Used to calculate vertex degree = verttab[i+1] - verttab[i]
    SCOTCH_Num *velotab = NULL; // Vertex load = vertex weight
    SCOTCH_Num *vlbltab = NULL;
    SCOTCH_Num *edgetab = &col_indices[0];
    SCOTCH_Num *edlotab = NULL; // Edge load = edge weight
    SCOTCH_Num *parttab = (SCOTCH_Num*)partvec;
    int mesg = 0;
    mesg = SCOTCH_graphBuild(graphptr, baseval, vertnbr, verttab, vendtab,
      velotab, vlbltab, edgenbr, edgetab, edlotab);
    if(mesg != 0) {op_printf("Error during SCOTCH_graphBuild() \n");exit(-1);}
    SCOTCH_Strat *straptr = SCOTCH_stratAlloc();
    SCOTCH_stratInit(straptr);

    mesg = SCOTCH_graphPart(graphptr,nparts, straptr, parttab);
    if(mesg != 0) {op_printf("Error during SCOTCH_graphPart() \n");exit(-1);}
    SCOTCH_graphExit(graphptr);
    SCOTCH_stratExit(straptr);
  #elif PARMETIS_VER_4
    int nconstr = 1;
    int edgecut;
    int nparts = possible[i];
    idx_t options[METIS_NOPTIONS];
    METIS_SetDefaultOptions(options);
    options[METIS_OPTION_OBJTYPE] = METIS_OBJTYPE_VOL;
    options[METIS_OPTION_NCUTS] = 3;
    options[METIS_OPTION_NSEPS] = 3;
    options[METIS_OPTION_NUMBERING] = 0;
    options[METIS_OPTION_MINCONN] = 1;

    METIS_PartGraphKway(&set_size, &nconstr, &row_offsets[0],
      &col_indices[0], NULL, NULL, NULL, &nparts, NULL, NULL, options, &edgecut, partvec);
  #else
    printf("No sutiable partitioner found!\n"); return;
  #endif

  //Inherit partitioning to other sets
  std::vector<std::vector<int> > set_partitionings(OP_set_index);
  set_partitionings[base->to->index].resize(set_size);
  std::copy(partvec, partvec+set_size, set_partitionings[base->to->index].begin());
  inherit_partitioning(base->to, base->to, set_partitionings);

  //Create renumbering for each set
  for (int i = 0; i < OP_set_index; i++) {
    if (set_partitionings[i].size() == 0) {printf("Failed to inherit partitioning to set %s\n", OP_set_list[i]->name); continue;}
    op_set set = OP_set_list[i];
    int set_size = set->size + set->exec_size + set->nonexec_size;
    std::vector<map2> ordering(set_size);
    for (int j = 0; j < set_size; j++) {
      ordering[j].a = set_partitionings[i][j];
      ordering[j].b = j;
    }
    qsort(&ordering[0], ordering.size(), sizeof(map2), compare);
    OP_set_permutations[i].permutation = (int *)malloc(set_size*sizeof(int));
    int part_counter = 1;
    for (int j = 0; j < set_size; j++) {
      if (j > 0 && ordering[j].a != ordering[j-1].a) part_counter++;
      OP_set_permutations[i].permutation[j] = ordering[j].b;
    }
    if (set_size / part_counter < 64) {
      printf("Set %s too small for partitioned blocks\n", set->name);
      set_partitionings[i].resize(0);
      free(OP_set_permutations[i].permutation);
      OP_set_permutations[i].permutation = NULL;
      continue;
    }
    OP_set_permutations[i].num_blocks = part_counter;
    OP_set_permutations[i].block_offset = (int*)malloc(part_counter * sizeof(int));
    OP_set_permutations[i].block_size_owned = (int*)malloc(part_counter * sizeof(int));
    OP_set_permutations[i].block_size_full = (int*)malloc(part_counter * sizeof(int));
    part_counter = 0;
    OP_set_permutations[i].block_offset[0] = 0;
    for (int j = 0; j < set_size; j++) {
      if (j > 0 && ordering[j].a != ordering[j-1].a) {
        OP_set_permutations[i].block_size_full[part_counter] = j - OP_set_permutations[i].block_offset[part_counter];
        OP_set_permutations[i].block_size_owned[part_counter] = OP_set_permutations[i].block_size_full[part_counter];
        part_counter++;
        OP_set_permutations[i].block_offset[part_counter] = j;
      }
    }
    OP_set_permutations[i].block_size_full[part_counter] = set_size - OP_set_permutations[i].block_offset[part_counter];
    OP_set_permutations[i].block_size_owned[part_counter] = OP_set_permutations[i].block_size_full[part_counter];

    //Decrement owned size for halo elements
    for (int j = set->size; j < set_size; j++) {
      OP_set_permutations[i].block_size_owned[set_partitionings[i][j]]--;
    }

    //Flag partitions that contain boundary or halo elements
    OP_set_permutations[i].has_boundary = (short *)calloc(part_counter,sizeof(short));
    for (int j = 0; j < part_counter; j++) {
      for (int k = OP_set_permutations[i].block_offset[j]; k < OP_set_permutations[i].block_size_full[j]; k++) {
        if (OP_set_permutations[i].permutation[j] > set->core_size) {
          OP_set_permutations[i].has_boundary[j] = 1;
          break;
        }
      }
    }
  }

  //Reorder sets
  for (int i = 0; i < OP_set_index; i++) {
    reorder_set(OP_set_list[i], OP_set_permutations[i].permutation);
  }
}
