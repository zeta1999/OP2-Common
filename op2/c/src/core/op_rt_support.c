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

/*
 * This file implements the OP2 run-time support used by different
 * OP2 back-ends, like CUDA and OpenMP. It provides and implementation
 * of the plan building function for colouring and partitioning of
 * unstructured meshes.
 */

#include "op_rt_support.h"
#include <algorithm>

/*
 * Global variables
 */

int OP_plan_index = 0, OP_plan_max = 0;
op_plan * OP_plans;

extern op_kernel * OP_kernels;
extern int OP_kern_max;
extern int OP_set_index;
extern int OP_map_index;
extern op_set *OP_set_list;
extern op_map *OP_map_list;
extern Double_linked_list OP_dat_list;

void
op_rt_exit (  )
{
  /* free storage for plans */
  for ( int ip = 0; ip < OP_plan_index; ip++ )
  {
    free ( OP_plans[ip].dats );
    free ( OP_plans[ip].idxs );
    free ( OP_plans[ip].maps );
    free ( OP_plans[ip].accs );
    free ( OP_plans[ip].nthrcol );
    free ( OP_plans[ip].thrcol );
    free ( OP_plans[ip].offset );
    free ( OP_plans[ip].ind_offs );
    free ( OP_plans[ip].ind_sizes );
    free ( OP_plans[ip].nelems );
    free ( OP_plans[ip].blkmap );
    free ( OP_plans[ip].ind_map );
    free ( OP_plans[ip].ind_maps );
    free ( OP_plans[ip].nindirect );
    free ( OP_plans[ip].loc_map );
    free ( OP_plans[ip].loc_maps );
    free ( OP_plans[ip].ncolblk );
    free ( OP_plans[ip].nsharedCol);
  }

  OP_plan_index = 0;
  OP_plan_max = 0;

  free ( OP_plans );
  OP_plans = NULL;
}

/*
 * comparison function for integer quicksort in op_plan
 */

static int
comp ( const void * a2, const void * b2 )
{
  int *a = ( int * ) a2;
  int *b = ( int * ) b2;

  if ( *a == *b )
    return 0;
  else if ( *a < *b )
    return -1;
  else
    return 1;
}

/*
 * plan check routine
 */

void op_plan_check( op_plan OP_plan, int ninds, int * inds)
{
  //compute exec_length - which include the exec halo given certain conditions (MPI)
  int exec_length = OP_plan.set->size;
  for ( int m = 0; m < OP_plan.nargs; m++ )
  {
    if(OP_plan.idxs[m] != -1 && OP_plan.accs[m] != OP_READ ) //if it needs exchaning
    {
      exec_length += OP_plan.set->exec_size;
      break;
    }
  }

  int err, ntot;

  int nblock = 0;
  for ( int col = 0; col < OP_plan.ncolors; col++ )
  {
    nblock += OP_plan.ncolblk[col];
  }

  /*
   * check total size
   */

  int nelem = 0;
  for ( int n = 0; n < nblock; n++ )
    nelem += OP_plan.nelems[n];

  if ( nelem != exec_length )
  {
    printf ( " *** OP_plan_check: nelems error \n" );
  }
  else if ( OP_diags > 6 )
  {
    printf ( " *** OP_plan_check: nelems   OK \n" );
  }

  /*
   * check offset and nelems are consistent
   */

  err = 0;
  ntot = 0;

  for ( int n = 0; n < nblock; n++ )
  {
    err += ( OP_plan.offset[n] != ntot );
    ntot += OP_plan.nelems[n];
  }

  if ( err != 0 )
  {
    printf ( " *** OP_plan_check: offset error \n" );
  }
  else if ( OP_diags > 6 )
  {
    printf ( " *** OP_plan_check: offset   OK \n" );
  }

  /*
   * check blkmap permutation
   */

  int *blkmap = ( int * ) malloc ( nblock * sizeof ( int ) );
  for ( int n = 0; n < nblock; n++ )
    blkmap[n] = OP_plan.blkmap[n];
  qsort ( blkmap, nblock, sizeof ( int ), comp );

  err = 0;
  for ( int n = 0; n < nblock; n++ )
    err += ( blkmap[n] != n );

  free ( blkmap );

  if ( err != 0 )
  {
    printf ( " *** OP_plan_check: blkmap error \n" );
  }
  else if ( OP_diags > 6 )
  {
    printf ( " *** OP_plan_check: blkmap   OK \n" );
  }

  /*
   * check ind_offs and ind_sizes are consistent
   */

  err = 0;

  for ( int i = 0; i < ninds; i++ )
  {
    ntot = 0;

    for ( int n = 0; n < nblock; n++ )
    {
      err += ( OP_plan.ind_offs[i + n * ninds] != ntot );
      ntot += OP_plan.ind_sizes[i + n * ninds];
    }
  }

  if ( err != 0 )
  {
    printf ( " *** OP_plan_check: ind_offs error \n" );
  }
  else if ( OP_diags > 6 )
  {
    printf ( " *** OP_plan_check: ind_offs OK \n" );
  }

  /*
   * check ind_maps correctly ordered within each block
   * and indices within range
   */

  err = 0;


  for ( int m = 0; m < ninds; m++ )
  {
    int m2 = 0;
    while ( inds[m2] != m )
      m2++;

    int halo_size = (OP_plan.maps[m2]->to)->exec_size +
      (OP_plan.maps[m2]->to)->nonexec_size;
    int set_size = OP_plan.maps[m2]->to->size + halo_size;

    ntot = 0;

    for ( int n = 0; n < nblock; n++ )
    {
      int last = -1;
      for ( int e = ntot; e < ntot + OP_plan.ind_sizes[m + n * ninds]; e++ )
      {
        err += ( OP_plan.ind_maps[m][e] <= last );
        last = OP_plan.ind_maps[m][e];
      }
      err += ( last >= set_size );
      ntot += OP_plan.ind_sizes[m + n * ninds];
    }
  }

  if ( err != 0 )
  {
    printf ( " *** OP_plan_check: ind_maps error \n" );
  }
  else if ( OP_diags > 6 )
  {
    printf ( " *** OP_plan_check: ind_maps OK \n" );
  }

  /*
   *check maps (most likely source of errors)
   */



  err = 0;

  for ( int m = 0; m < OP_plan.nargs; m++ )
  {
    if ( OP_plan.maps[m] != NULL )
    {
      op_map map = OP_plan.maps[m];
      int m2 = inds[m];

      ntot = 0;
      for ( int n = 0; n < nblock; n++ )
      {
        for ( int e = ntot; e < ntot + OP_plan.nelems[n]; e++ )
        {
          int p_local = OP_plan.loc_maps[m][e];
          int p_global = OP_plan.ind_maps[m2][p_local + OP_plan.ind_offs[m2 + n * ninds]];
          err += ( p_global != map->map[OP_plan.idxs[m] + e * map->dim] );
        }
        ntot += OP_plan.nelems[n];
      }
    }
  }

  if ( err != 0 )
  {
    printf ( " *** OP_plan_check: maps error \n" );
  }
  else if ( OP_diags > 6 )
  {
    printf ( " *** OP_plan_check: maps     OK \n" );
  }

  /*
   * check thread and block coloring
   */

  return;
}

/*
 * OP plan construction
 */

op_plan *op_plan_core(char const *name, op_set set, int part_size,
                      int nargs, op_arg *args, int ninds, int *inds )
{
  //set exec length
  int exec_length = set->size;
  for(int i = 0; i< nargs; i++)
  {
    if(args[i].idx != -1 && args[i].acc != OP_READ )
    {
      exec_length += set->exec_size;
      break;
    }
  }

  /* first look for an existing execution plan */

  int ip = 0, match = 0;

  while ( match == 0 && ip < OP_plan_index )
  {
    if ( ( strcmp ( name, OP_plans[ip].name ) == 0 )
        && ( set == OP_plans[ip].set )
        && ( nargs == OP_plans[ip].nargs )
        && ( ninds == OP_plans[ip].ninds )
        && ( part_size == OP_plans[ip].part_size ) )
    {
      match = 1;
      for ( int m = 0; m < nargs; m++ )
      {
        if (args[m].dat != NULL && OP_plans[ip].dats[m] != NULL)
          match = match
          && ( args[m].dat->size == OP_plans[ip].dats[m]->size )
          && ( args[m].dat->dim == OP_plans[ip].dats[m]->dim )
          && ( args[m].map == OP_plans[ip].maps[m] )
          && ( args[m].idx == OP_plans[ip].idxs[m] )
          && ( args[m].acc == OP_plans[ip].accs[m] );
        else
          match = match
          && ( args[m].dat == OP_plans[ip].dats[m] )
          && ( args[m].map == OP_plans[ip].maps[m] )
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
  }
  else
  {
    if ( OP_diags > 1 )
      printf ( " new execution plan #%d for kernel %s\n", ip, name );
  }
  double wall_t1, wall_t2, cpu_t1, cpu_t2;
  op_timers_core(&cpu_t1, &wall_t1);
  /* work out worst case shared memory requirement per element */

  int maxbytes = 0;
  for ( int m = 0; m < nargs; m++ )
  {
    if ( inds[m] >= 0 )
      maxbytes += args[m].dat->size;
  }

  /* set blocksize and number of blocks; adaptive size based on 48kB of shared memory */

  int bsize = part_size;        // blocksize
  if ( bsize == 0 )
    bsize = ( 48 * 1024 / ( 64 * maxbytes ) ) * 64;
  //int nblocks = ( exec_length - 1 ) / bsize + 1;
  int nblocks = 0;
  /*if (set->core_size != 0) nblocks += ( set->core_size - 1 ) / bsize + 1;
  if (set->core_size != exec_length) nblocks += ( exec_length - set->core_size - 1 ) / bsize + 1;*/

  int indirect_reduce = 0;
  for ( int m = 0; m < nargs; m++ ) {
    indirect_reduce |= (args[m].acc != OP_READ && args[m].argtype == OP_ARG_GBL);
  }
  indirect_reduce &= (ninds>0);

  int prev_offset = 0;
  int next_offset = 0;

  while (next_offset < exec_length)
  {
    prev_offset = next_offset;
    if (prev_offset + bsize >= set->core_size && prev_offset < set->core_size) {
      next_offset = set->core_size;
    } else if (prev_offset + bsize >= set->size && prev_offset < set->size && indirect_reduce) {
      next_offset = set->size;
    } else if (prev_offset + bsize >= exec_length && prev_offset < exec_length) {
      next_offset = exec_length;
    } else {
      next_offset = prev_offset + bsize;
    }
    nblocks++;
  }

  /* enlarge OP_plans array if needed */

  if ( ip == OP_plan_max )
  {
    //printf("allocating more memory for OP_plans %d\n", OP_plan_max);
    OP_plan_max += 10;
    OP_plans = ( op_plan * ) realloc ( OP_plans, OP_plan_max * sizeof ( op_plan ) );
    if ( OP_plans == NULL )
    {
      printf ( " op_plan error -- error reallocating memory for OP_plans\n" );
      exit ( -1 );
    }
  }

  /* allocate memory for new execution plan and store input arguments */

  OP_plans[ip].dats = ( op_dat * ) malloc ( nargs * sizeof ( op_dat ) );
  OP_plans[ip].idxs = ( int * ) malloc ( nargs * sizeof ( int ) );
  OP_plans[ip].maps = ( op_map * ) malloc ( nargs * sizeof ( op_map ) );
  OP_plans[ip].accs = ( op_access * ) malloc ( nargs * sizeof ( op_access ) );

  OP_plans[ip].nthrcol = ( int * ) malloc ( nblocks * sizeof ( int ) );
  OP_plans[ip].thrcol = ( int * ) malloc ( exec_length * sizeof ( int ) );
  OP_plans[ip].offset = ( int * ) malloc ( nblocks * sizeof ( int ) );
  OP_plans[ip].ind_maps = ( int ** ) malloc ( ninds * sizeof ( int * ) );
  OP_plans[ip].ind_offs = ( int * ) malloc ( nblocks * ninds * sizeof ( int ) );
  OP_plans[ip].ind_sizes = ( int * ) malloc ( nblocks * ninds * sizeof ( int ) );
  OP_plans[ip].nindirect = ( int * ) calloc ( ninds, sizeof ( int ) );
  OP_plans[ip].loc_maps = ( short ** ) malloc ( nargs * sizeof ( short * ) );
  OP_plans[ip].nelems = ( int * ) malloc ( nblocks * sizeof ( int ) );
  OP_plans[ip].ncolblk = ( int * ) calloc ( exec_length, sizeof ( int ) );  /* max possibly needed */
  OP_plans[ip].blkmap = ( int * ) calloc ( nblocks, sizeof ( int ) );

  int *offsets = (int *)malloc((ninds+1)*sizeof(int));
  offsets[0] = 0;
  for ( int m = 0; m < ninds; m++ ) {
    int count = 0;
    for ( int m2 = 0; m2 < nargs; m2++ )
      if ( inds[m2] == m )
        count++;
      offsets[m+1] = offsets[m] + count;
  }
  OP_plans[ip].ind_map = ( int * ) malloc ( offsets[ninds] * exec_length * sizeof ( int ) );
  for ( int m = 0; m < ninds; m++ ) {
    OP_plans[ip].ind_maps[m] = &OP_plans[ip].ind_map[exec_length*offsets[m]];
  }
  free(offsets);

  int counter = 0;
  for ( int m = 0; m < nargs; m++ ) {
    if ( inds[m] >= 0 )
      counter++;
    else
      OP_plans[ip].loc_maps[m] = NULL;

    OP_plans[ip].dats[m] = args[m].dat;
    OP_plans[ip].idxs[m] = args[m].idx;
    OP_plans[ip].maps[m] = args[m].map;
    OP_plans[ip].accs[m] = args[m].acc;
  }

  OP_plans[ip].loc_map = ( short * ) malloc ( counter * exec_length * sizeof ( short ) );
  counter = 0;
  for ( int m = 0; m < nargs; m++ ) {
    if ( inds[m] >= 0 ) {
      OP_plans[ip].loc_maps[m] = &OP_plans[ip].loc_map[exec_length*(counter)];
        counter++;
      }
  }


  OP_plans[ip].name = name;
  OP_plans[ip].set = set;
  OP_plans[ip].nargs = nargs;
  OP_plans[ip].ninds = ninds;
  OP_plans[ip].part_size = part_size;
  OP_plans[ip].nblocks = nblocks;
  OP_plans[ip].ncolors_core = 0;
  OP_plans[ip].ncolors_owned = 0;
  OP_plans[ip].count = 1;

  OP_plan_index++;

  /* define aliases */

  op_dat * dats = OP_plans[ip].dats;
  int * idxs = OP_plans[ip].idxs;
  op_map * maps = OP_plans[ip].maps;
  op_access * accs = OP_plans[ip].accs;

  int * offset = OP_plans[ip].offset;
  int * nelems = OP_plans[ip].nelems;
  int ** ind_maps = OP_plans[ip].ind_maps;
  int * ind_offs = OP_plans[ip].ind_offs;
  int * ind_sizes = OP_plans[ip].ind_sizes;
  int * nindirect = OP_plans[ip].nindirect;

  /* allocate working arrays */
  //printf("ninds = %d\n",ninds);
  uint **work;
  work = (uint **)malloc(ninds * sizeof(uint *));

  for ( int m = 0; m < ninds; m++ )
  {
    int m2 = 0;
    while ( inds[m2] != m )
      m2++;

    int to_size = (maps[m2]->to)->exec_size + (maps[m2]->to)->nonexec_size + (maps[m2]->to)->size;
    work[m] = ( uint * )malloc( to_size * sizeof (uint));
  }

  int *work2;
  work2 = ( int * ) malloc ( nargs * bsize * sizeof ( int ) );  /* max possibly needed */

  /* process set one block at a time */

  float total_colors = 0;

  prev_offset = 0;
  next_offset = 0;

  for ( int b = 0; b < nblocks; b++ )
  {
    prev_offset = next_offset;
    if (prev_offset + bsize >= set->core_size && prev_offset < set->core_size) {
      next_offset = set->core_size;
    } else if (prev_offset + bsize >= set->size && prev_offset < set->size && indirect_reduce) {
      next_offset = set->size;
    } else if (prev_offset + bsize >= exec_length && prev_offset < exec_length) {
      next_offset = exec_length;
    } else {
      next_offset = prev_offset + bsize;
    }
    int bs = next_offset-prev_offset;

    offset[b] = prev_offset;  /* offset for block */
    nelems[b] = bs;   /* size of block */

    /* loop over indirection sets */

    for ( int m = 0; m < ninds; m++ )
    {

      /* build the list of elements indirectly referenced in this block */

      int ne = 0;/* number of elements */
      for ( int m2 = 0; m2 < nargs; m2++ )
      {
        if ( inds[m2] == m )
        {
          for ( int e = prev_offset; e < next_offset; e++ )
            work2[ne++] = maps[m2]->map[idxs[m2] + e * maps[m2]->dim];
        }
      }

      /* sort them, then eliminate duplicates */

      qsort(work2, ne, sizeof(int), comp);

      int nde = 0;
      int p = 0;
      while ( p < ne )
      {
        work2[nde] = work2[p];
        while ( p < ne && work2[p] == work2[nde] )
          p++;
        nde++;
      }
      ne = nde; /* number of distinct elements */

      /*
         if (OP_diags > 5) { printf(" indirection set %d: ",m); for (int e=0; e<ne; e++) printf("
         %d",work2[e]); printf(" \n"); } */


      /* store mapping and renumbered mappings in execution plan */

      for ( int e = 0; e < ne; e++ )
      {
        ind_maps[m][nindirect[m]++] = work2[e];
        work[m][work2[e]] = e;  // inverse mapping
      }

      for ( int m2 = 0; m2 < nargs; m2++ )
      {
        if ( inds[m2] == m )
        {
          for ( int e = prev_offset; e < next_offset; e++ )
            OP_plans[ip].loc_maps[m2][e] = (short)(work[m][maps[m2]->map[idxs[m2] + e * maps[m2]->dim]]);
        }
      }

      if ( b == 0 )
      {
        ind_offs[m + b * ninds] = 0;
        ind_sizes[m + b * ninds] = nindirect[m];
      }
      else
      {
        ind_offs[m + b * ninds] = ind_offs[m + ( b - 1 ) * ninds]
          + ind_sizes[m + ( b - 1 ) * ninds];
        ind_sizes[m + b * ninds] = nindirect[m] - ind_offs[m + b * ninds];
      }
    }

    /* now colour main set elements */

    for ( int e = prev_offset; e < next_offset; e++ )
      OP_plans[ip].thrcol[e] = -1;

    int repeat = 1;
    int ncolor = 0;
    int ncolors = 0;

    while ( repeat )
    {
      repeat = 0;

      for ( int m = 0; m < nargs; m++ )
      {
        if ( inds[m] >= 0 )
          for ( int e = prev_offset; e < next_offset; e++ )
            work[inds[m]][maps[m]->map[idxs[m] + e * maps[m]->dim]] = 0; /* zero out color array */
        //work[inds[m]][maps[m]->map[idxs[m] + e * maps[m]->dim]] = 0; /* zero out color array */
      }

      for ( int e = prev_offset; e < next_offset; e++ )
      {
        if ( OP_plans[ip].thrcol[e] == -1 )
        {
          int mask = 0;
          for ( int m = 0; m < nargs; m++ )
            if ( inds[m] >= 0 && accs[m] == OP_INC )
              mask |= work[inds[m]][maps[m]->map[idxs[m] + e * maps[m]->dim]]; /* set bits of mask
              */

          int color = ffs ( ~mask ) - 1;  /* find first bit not set */
          if ( color == -1 )
          {                     /* run out of colors on this pass */
            repeat = 1;
          }
          else
          {
            OP_plans[ip].thrcol[e] = ncolor + color;
            mask = 1 << color;
            ncolors = MAX ( ncolors, ncolor + color + 1 );

            for ( int m = 0; m < nargs; m++ )
              if ( inds[m] >= 0 && accs[m] == OP_INC )
                work[inds[m]][maps[m]->map[idxs[m] + e * maps[m]->dim]] |= mask; /* set color bit */
          }
        }
      }

      ncolor += 32;             /* increment base level */
    }

    OP_plans[ip].nthrcol[b] = ncolors;  /* number of thread colors in this block */
    total_colors += ncolors;

    //if(ncolors>1) printf(" number of colors in this block = %d \n",ncolors);

    /* reorder elements by color? */
  }


  /* color the blocks, after initialising colors to 0 */

  int * blk_col;

  blk_col = ( int * ) malloc ( nblocks * sizeof ( int ) );
  for ( int b = 0; b < nblocks; b++ )
    blk_col[b] = -1;

  int repeat = 1;
  int ncolor = 0;
  int ncolors = 0;

  while ( repeat )
  {
    repeat = 0;

    for ( int m = 0; m < nargs; m++ )
    {
      if ( inds[m] >= 0 )
      {
        int to_size = (maps[m]->to)->exec_size + (maps[m]->to)->nonexec_size + (maps[m]->to)->size;
        for ( int e = 0; e < to_size; e++ )
          work[inds[m]][e] = 0; // zero out color arrays
      }
    }
    prev_offset = 0;
    next_offset = 0;
    for ( int b = 0; b < nblocks; b++ )
    {
      prev_offset = next_offset;

      if (prev_offset + bsize >= set->core_size && prev_offset < set->core_size) {
        next_offset = set->core_size;
      } else if (prev_offset + bsize >= set->size && prev_offset < set->size && indirect_reduce) {
        next_offset = set->size;
      } else if (prev_offset + bsize >= exec_length && prev_offset < exec_length) {
        next_offset = exec_length;
      } else {
        next_offset = prev_offset + bsize;
      }
      if ( blk_col[b] == -1 )
      { // color not yet assigned to block
        uint mask = 0;
        if (next_offset > set->core_size) { //should not use block colors from the core set when doing the non_core ones
          if (prev_offset <= set->core_size) OP_plans[ip].ncolors_core = ncolors;
          for (int shifter = 0; shifter < OP_plans[ip].ncolors_core; shifter++) mask |= 1<<shifter;
          if (prev_offset == set->size && indirect_reduce) OP_plans[ip].ncolors_owned = ncolors;
          for (int shifter = OP_plans[ip].ncolors_core; indirect_reduce && shifter < OP_plans[ip].ncolors_owned; shifter++) mask |= 1<<shifter;
        }

        for ( int m = 0; m < nargs; m++ )
        {
          if ( inds[m] >= 0 && accs[m] == OP_INC )
            for ( int e = prev_offset; e < next_offset; e++ )
              mask |= work[inds[m]][maps[m]->map[idxs[m] +
                e * maps[m]->dim]]; // set bits of mask
        }

        int color = ffs( ~mask ) - 1; // find first bit not set
        if ( color == -1 )
        { //run out of colors on this pass
          repeat = 1;
        }
        else
        {
          blk_col[b] = ncolor + color;
          mask = 1 << color;
          ncolors = MAX( ncolors, ncolor + color + 1 );

          for ( int m = 0; m < nargs; m++ )
          {
            if ( inds[m] >= 0 && accs[m] == OP_INC )
              for ( int e = prev_offset; e < next_offset; e++ )
                work[inds[m]][maps[m]->map[idxs[m] +
                  e * maps[m]->dim]] |= mask;
          }
        }
      }
    }

    ncolor += 32;               // increment base level
  }

  /* store block mapping and number of blocks per color */

  if (indirect_reduce && OP_plans[ip].ncolors_owned == 0) OP_plans[ip].ncolors_owned = ncolors; //no MPI, so get the reduction arrays after everyting is done
  OP_plans[ip].ncolors = ncolors;

  /*for(int col = 0; col = OP_plans[ip].ncolors;col++) //should initialize to zero because calloc returns garbage!!
    {
    OP_plans[ip].ncolblk[col] = 0;
    }*/

  for ( int b = 0; b < nblocks; b++ )
    OP_plans[ip].ncolblk[blk_col[b]]++; // number of blocks of each color

  for ( int c = 1; c < ncolors; c++ )
    OP_plans[ip].ncolblk[c] += OP_plans[ip].ncolblk[c - 1]; // cumsum

  for ( int c = 0; c < ncolors; c++ )
    work2[c] = 0;

  for ( int b = 0; b < nblocks; b++ )
  {
    int c = blk_col[b];
    int b2 = work2[c]; // number of preceding blocks of this color
    if ( c > 0 )
      b2 += OP_plans[ip].ncolblk[c - 1];  // plus previous colors

    OP_plans[ip].blkmap[b2] = b;

    work2[c]++; //increment counter
  }

  for ( int c = ncolors - 1; c > 0; c-- )
    OP_plans[ip].ncolblk[c] -= OP_plans[ip].ncolblk[c - 1]; // undo cumsum


  /* reorder blocks by color? */

  /* work out shared memory requirements */
  OP_plans[ip].nsharedCol = (int *)malloc(ncolors * sizeof(int));
  float total_shared = 0;
  for (int col = 0; col < ncolors; col++) {
    OP_plans[ip].nsharedCol[col] = 0;
    for ( int b = 0; b < nblocks; b++ ) {
      if (blk_col[b] ==  col) {
        int nbytes = 0;
        for ( int m = 0; m < ninds; m++ )  {
          int m2 = 0;
          while ( inds[m2] != m )
            m2++;

          nbytes += ROUND_UP ( ind_sizes[m + b * ninds] * dats[m2]->size );
        }
        OP_plans[ip].nsharedCol[col] = MAX ( OP_plans[ip].nsharedCol[col], nbytes );
        total_shared += nbytes;
      }
    }
  }

  OP_plans[ip].nshared = 0;
  total_shared = 0;

  for ( int b = 0; b < nblocks; b++ )
  {
    int nbytes = 0;
    for ( int m = 0; m < ninds; m++ )
    {
      int m2 = 0;
      while ( inds[m2] != m )
        m2++;

      nbytes += ROUND_UP ( ind_sizes[m + b * ninds] * dats[m2]->size );
    }
    OP_plans[ip].nshared = MAX ( OP_plans[ip].nshared, nbytes );
    total_shared += nbytes;
  }

  /* work out total bandwidth requirements */

  OP_plans[ip].transfer = 0;
  OP_plans[ip].transfer2 = 0;
  float transfer3 = 0;

  for ( int b = 0; b < nblocks; b++ )
  {
    for ( int m = 0; m < nargs; m++ ) //for each argument
    {
      if ( inds[m] < 0 ) //if it is directly addressed
      {
        float fac = 2.0f;
        if ( accs[m] == OP_READ ) //if you only read it - only write???
          fac = 1.0f;
        if ( dats[m] != NULL )
        {
          OP_plans[ip].transfer += fac * nelems[b] * dats[m]->size; //cost of reading it all
          OP_plans[ip].transfer2 += fac * nelems[b] * dats[m]->size;
          transfer3 += fac * nelems[b] * dats[m]->size;
        }
      }
      else //if it is indirectly addressed: cost of reading the pointer to it
      {
        OP_plans[ip].transfer += nelems[b] * sizeof ( short );
        OP_plans[ip].transfer2 += nelems[b] * sizeof ( short );
        transfer3 += nelems[b] * sizeof ( short );
      }
    }
    for ( int m = 0; m < ninds; m++ ) //for each indirect mapping
    {
      int m2 = 0;
      while ( inds[m2] != m ) //find the first argument that uses this mapping
        m2++;
      float fac = 2.0f;
      if ( accs[m2] == OP_READ ) //only read it (write??)
        fac = 1.0f;
      OP_plans[ip].transfer += fac * ind_sizes[m + b * ninds] * dats[m2]->size; //simply read all data one by one

      /* work out how many cache lines are used by indirect addressing */

      int i_map, l_new, l_old;
      int e0 = ind_offs[m + b * ninds]; //where it starts
      int e1 = e0 + ind_sizes[m + b * ninds]; //where it ends

      l_old = -1;

      for ( int e = e0; e < e1; e++ ) //iterate through every indirectly accessed data element
      {
        i_map = ind_maps[m][e]; //the pointer to the data element
        l_new = ( i_map * dats[m2]->size ) / OP_cache_line_size; //which cache line it is on (full size, dim*sizeof(type))
        if ( l_new > l_old ) //if it is on a further cache line (that is not yet loaded, - i_map is ordered)
          OP_plans[ip].transfer2 += fac * OP_cache_line_size; //load the cache line
        l_old = l_new;
        l_new = ( ( i_map + 1 ) * dats[m2]->size - 1 ) / OP_cache_line_size; //the last byte of the data
        OP_plans[ip].transfer2 += fac * ( l_new - l_old ) * OP_cache_line_size; //again, if not loaded, load it (can be multiple cache lines)
        l_old = l_new;
      }

      l_old = -1;

      for ( int e = e0; e < e1; e++ )
      {
        i_map = ind_maps[m][e]; //pointer to the data element
        l_new = ( i_map * dats[m2]->size ) / ( dats[m2]->dim * OP_cache_line_size ); //which cache line the first dimension of the data is on
        if ( l_new > l_old )
          transfer3 += fac * dats[m2]->dim * OP_cache_line_size; //if not loaded yet, load all cache lines
        l_old = l_new;
        l_new = ( ( i_map + 1 ) * dats[m2]->size - 1 ) / ( dats[m2]->dim * OP_cache_line_size ); //primitve type's last byte
        transfer3 += fac * ( l_new - l_old ) * dats[m2]->dim * OP_cache_line_size; //load it
        l_old = l_new;
      }

      /* also include mappings to load/store data */

      fac = 1.0f;
      if ( accs[m2] == OP_RW )
        fac = 2.0f;
      OP_plans[ip].transfer += fac * ind_sizes[m + b * ninds] * sizeof ( int );
      OP_plans[ip].transfer2 += fac * ind_sizes[m + b * ninds] * sizeof ( int );
      transfer3 += fac * ind_sizes[m + b * ninds] * sizeof ( int );
    }
  }

  /* print out useful information */

  if ( OP_diags > 1 )
  {
    printf( " number of blocks       = %d \n", nblocks );
    printf( " number of block colors = %d \n", OP_plans[ip].ncolors );
    printf( " maximum block size     = %d \n", bsize );
    printf( " average thread colors  = %.2f \n", total_colors / nblocks );
    printf( " shared memory required = ");
    for (int i = 0; i < ncolors-1; i++) printf(" %.2f KB,", OP_plans[ip].nsharedCol[i] / 1024.0f );
    printf(" %.2f KB\n", OP_plans[ip].nsharedCol[ncolors-1] / 1024.0f );
    printf( " average data reuse     = %.2f \n", maxbytes * ( exec_length / total_shared ) );
    printf( " data transfer (used)   = %.2f MB \n",
        OP_plans[ip].transfer / ( 1024.0f * 1024.0f ) );
    printf( " data transfer (total)  = %.2f MB \n",
        OP_plans[ip].transfer2 / ( 1024.0f * 1024.0f ) );
    printf( " SoA/AoS transfer ratio = %.2f \n\n", transfer3 / OP_plans[ip].transfer2 );
  }

  /* validate plan info */

  op_plan_check ( OP_plans[ip], ninds, inds );

  /* free work arrays */

  for ( int m = 0; m < ninds; m++ )
    free ( work[m] );
  free ( work );
  free ( work2 );
  free ( blk_col );

  op_timers_core(&cpu_t2, &wall_t2);
  for (int i = 0; i < OP_kern_max; i++) {
    if (strcmp(name, OP_kernels[i].name)==0) {
      OP_kernels[i].plan_time += wall_t2-wall_t1;
      break;
    }
  }
  /* return pointer to plan */

  return &( OP_plans[ip] );
}

typedef struct
{
  op_set set;
  std::vector<int> elements;
  std::vector<op_dat> dats;
  std::vector<int> written;
  std::vector<op_arg *> args;
  int size;
  int ncolors;
  std::vector<int> color_offsets;
} op_dataset_dependency;

std::vector<op_dataset_dependency> dependencies;
std::vector<op_map> maps;
std::vector<op_dat> dats;
void compute_dependencies(op_kernel_descriptor *kernel);
void do_coloring(int set_index);

void op_end_superloop ( op_subset *subset, op_dat data) {
  if (data->set != subset->set) {
    printf("subset must be a subset of the set the data is on\n");
    exit(-1);
  }
  //Set up initial active dataset
  //TODO: more than one output dataset
  dependencies.clear();
  dependencies.resize(OP_set_index);
  for (int i = 0; i < OP_set_index; i++) {
    dependencies[i].set = OP_set_list[i];
    dependencies[i].elements.resize(dependencies[i].set->size); //TODO: this is way too big
    if (data->set == dependencies[i].set) {
      dependencies[i].size = subset->size;
      std::copy(subset->elements, subset->elements+subset->size, dependencies[i].elements.begin());
      dependencies[i].dats.push_back(data);
      dependencies[i].written.push_back(0);
    } else {
      dependencies[i].size = 0;
    }
  }

  for (int i = 0; i < OP_map_index; i++) {
    //skip reverse maps
    if (OP_map_list[i]->index > OP_map_list[i]->reverse_map->index) continue;
    op_map map = (op_map_core *)malloc(sizeof(op_map_core));
    map->index = OP_map_list[i]->index;
    map->from = OP_map_list[i]->from;
    map->to = OP_map_list[i]->to;
    map->dim = OP_map_list[i]->dim;
    map->name = OP_map_list[i]->name;
    map->map = NULL;
    map->row_offsets = NULL;
    map->reverse_map = NULL;
    map->user_managed = 0;
    map->isSimple = OP_map_list[i]->isSimple;
    maps.push_back(map);
  }
  
  for (unsigned int i = 0; i < kernel_list.size(); i++) {
    compute_dependencies(&kernel_list[kernel_list.size()-1-i]);
  }
  
  for (int i = 0; i < OP_set_index; i++) {
    dependencies[i].elements.resize(dependencies[i].size);
  }
  
  double mem_maps = 0.0;
  double mem_set_lists = 0.0;
  double mem_execution_lists = 0.0;
  double mem_dats = 0.0;
  //populate local maps
  for (unsigned int i = 0; i < maps.size(); i++) {
    if (maps[i]==NULL) continue;
    maps[i]->map = (int *)malloc(dependencies[maps[i]->from->index].size * maps[i]->dim*sizeof(int));
    mem_maps += (double)(dependencies[maps[i]->from->index].size * maps[i]->dim*sizeof(int));
    for (int e = 0; e < dependencies[maps[i]->from->index].size; e++) {
      int el = dependencies[maps[i]->from->index].elements[e];
      for (int d = 0; d < maps[i]->dim; d++) {
        int gbl = OP_map_list[i]->map[el*maps[i]->dim + d];
        maps[i]->map[e*maps[i]->dim + d] = std::find(dependencies[maps[i]->to->index].elements.begin(), dependencies[maps[i]->to->index].elements.end(), gbl) - dependencies[maps[i]->to->index].elements.begin();
        //DEBUG: guaranteed segfault, it is okay if we don't find the global index in the other set, but in that case we must never use that map (i.e. this set element cannot be part of any execution set)
        if (maps[i]->map[e*maps[i]->dim + d] == dependencies[maps[i]->to->index].size) maps[i]->map[e*maps[i]->dim + d] = 0xFFFFFFFF;
      }
    }
  }
  
  //Create scratch memory dats
  for (int i = 0; i < OP_set_index; i++) {
    mem_set_lists += (double)(dependencies[i].size * sizeof(int));
    //TODO: shouldn't be reading in read only ones? would need to use a different map though...
    for (unsigned int j = 0; j < dependencies[i].dats.size(); j ++) {
      op_dat data = (op_dat)malloc(sizeof(op_dat_core));
      data->name = dependencies[i].dats[j]->name;
      data->index = dependencies[i].dats[j]->index;
      data->set = dependencies[i].dats[j]->set;
      data->dim = dependencies[i].dats[j]->dim;
      data->size = dependencies[i].dats[j]->size;
      data->type = dependencies[i].dats[j]->type;
      data->data = (char *)malloc(dependencies[i].size * data->size);
      data->data_d = NULL;
      //TODO: all MPI related stuff
      dats.push_back(data);
      mem_dats += (double)(data->size * dependencies[i].size);
    }
  }
  
  //renumber execution sets, substitute new maps and dats into loop args
  for (unsigned int i = 0; i < kernel_list.size(); i++) {
    for (int e = 0; e < kernel_list[i].subset->size; e++) {
      int el = kernel_list[i].subset->elements[e];
      kernel_list[i].subset->elements[e] = std::find(dependencies[kernel_list[i].subset->set->index].elements.begin(), dependencies[kernel_list[i].subset->set->index].elements.end(), el) - dependencies[kernel_list[i].subset->set->index].elements.begin();
      if (kernel_list[i].subset->elements[e] == dependencies[kernel_list[i].subset->set->index].size) {
        printf("Error, execution set element not found in set dependency list\n");
        exit(-1);
      }
    }
    for (int arg = 0; arg <kernel_list[i].nargs; arg++) {
      if (kernel_list[i].args[arg].argtype==OP_ARG_GBL) continue;
      //find local dat, substitute
      op_dat original = kernel_list[i].args[arg].dat;
      for (unsigned int j = 0; j < dats.size(); j++)
        if (dats[j]->index == original->index) {kernel_list[i].args[arg].dat = dats[j]; kernel_list[i].args[arg].data = dats[j]->data; break;}
      if (kernel_list[i].args[arg].dat == original) {
        printf("Error, local dat not found\n"); exit(-1);}//just some sanity check
      
      //find local map, substitute
      if (kernel_list[i].args[arg].map != NULL) {
        for (unsigned int j = 0; j < maps.size(); j++)
          if (maps[j]->index == kernel_list[i].args[arg].map->index) {kernel_list[i].args[arg].map = maps[j]; break;}
      }
    }
  }
  
  for (int i = 0 ; i < OP_set_index; i++) {
    do_coloring(i);
  }
  
  if (OP_diags>2) {
    printf("Maps sanity checks...\n");
    //Maps sanity check
    for (unsigned int i = 0; i < maps.size(); i++) {
      int from = maps[i]->from->index;
      int to = maps[i]->to->index;
      for (int e = 0; e < dependencies[from].size; e++) {
        int gbl_from = dependencies[from].elements[e];
        for (int d = 0; d < maps[i]->dim; d++) {
          int gbl_to = dependencies[to].elements[maps[i]->map[e*maps[i]->dim + d]];
          if (gbl_to != OP_map_list[maps[i]->index]->map[gbl_from * maps[i]->dim + d]) {
            printf("Map sanity check fail: %d != %d\n", gbl_to, OP_map_list[maps[i]->index]->map[gbl_from * maps[i]->dim + d]);
          }
        }
      }
    }
  }
  
  int ctr = 0;
  //Bring in dats to scratch memory
  for (int i = 0; i < OP_set_index; i++) {
    //TODO: shouldn't be reading in read only ones? would need to use a different map though...
    for (unsigned int j = 0; j < dependencies[i].dats.size(); j ++) {
      op_dat data = dats[ctr];
      if (data->index != dependencies[i].dats[j]->index) printf("Scratch bring in error\n");
      for (int e = 0; e < dependencies[i].size; e++) {
        //printf("%s %d %g -> %d\n", data->name, dependencies[i].elements[e], *((double *)&dependencies[i].dats[j]->data[dependencies[i].elements[e]*data->size]), e);
        memcpy(&data->data[e*data->size], &dependencies[i].dats[j]->data[dependencies[i].elements[e]*data->size], data->size);
      }
      ctr++;
    }
  }
  
  double memuse = mem_set_lists + mem_maps + mem_dats + mem_execution_lists;
  //Execute
  printf("Executing tile, depth %d mem use: %g KB (%.2g KB setlists, %.2g KB maps, %.2g KB dats %.2g KB exec lists)\n", (int)kernel_list.size(), memuse/1024.0, mem_set_lists/1024.0, mem_maps/1024.0, mem_dats/1024.0, mem_execution_lists/1024.0);
  unsigned int i = 0;
  while ( i < kernel_list.size()) {
    kernel_list[i].function(&kernel_list[i]); //save_soln
    i++;
  }
  
  //Put data back
  //TODO: only write out the "owned" data on the top of the tile
  for (int i = 0; i < OP_set_index; i++) {
    for (unsigned int j = 0; j < dependencies[i].dats.size(); j ++) {
      if (dependencies[i].written[j]==0) continue;
      op_dat data = NULL;
      for (unsigned int loc = 0; loc < dats.size(); loc ++) if (dats[loc]->index == dependencies[i].dats[j]->index) data = dats[loc];
      //put
      for (int e = 0; e < dependencies[i].size; e++) {
        memcpy(&dependencies[i].dats[j]->data[dependencies[i].elements[e]*data->size], &data->data[e*data->size], data->size);
      }
    }
  }

}

void add_dependency(op_dat data, int written) {
  //find data in set's data list (or append) and set written flag to 1
  unsigned int idx = std::find(dependencies[data->set->index].dats.begin(), dependencies[data->set->index].dats.end(), data) - dependencies[data->set->index].dats.begin();
  if (idx == dependencies[data->set->index].dats.size()) {
    dependencies[data->set->index].dats.push_back(data);
    dependencies[data->set->index].written.push_back(written);
  } else {
    if (written) dependencies[data->set->index].written[idx] = 1;
  }
}

void add_arg_dependency(op_arg *arg, int set_index) {
  if (arg->argtype == OP_ARG_GBL || arg->map == NULL || arg->acc == OP_READ || arg->acc == OP_WRITE) return; //for these we do not have to do coloring (no indirect OP_WRITE supported)
  //TODO: indirect OP_RW
  unsigned i = 0;
  for (; i < dependencies[set_index].args.size(); i++) {
    if (dependencies[set_index].args[i]->argtype == OP_ARG_GBL) continue;
    if (dependencies[set_index].args[i]->map == arg->map) return;
  }
  dependencies[set_index].args.push_back(arg); //TODO: indexes (multi-dim maps)
}

typedef struct
{
  float key;
  int value;
  int permutation;
} kv_sort2;

inline bool kv_sort2_comp(const kv_sort2 i, const kv_sort2 j) {return i.key < j.key;}


//first, determine what the execution set has to be so that all the already existing data dependencies are computed (OP_WRITE, OP_RW, OP_INC)
//what if we write something that's not an output?? - I think solved by getting ignored (dependencies[idx].size will be 0, nothing will be merged in)
//second, add further dependencies based on what is read (OP_RW, OP_INC, OP_READ)
void compute_dependencies(op_kernel_descriptor *kernel) {
  kernel->subset = (op_subset *)malloc(sizeof(op_subset));
  kernel->subset->set = kernel->set;
  //TODO reuse
  std::vector<int> execution_set(kernel->set->size);
  int execution_size = 0;
  int own_set_added = 0; //For loops that only acess data indirectly, we still need to add execution set to dependency list
  
  std::vector<int> args_done(kernel->nargs, 0);
  //phase one, determine execution set based on existing data dependencies
  for (int m = 0; m < kernel->nargs; m++) {
    if (args_done[m] || kernel->args[m].acc == OP_READ || kernel->args[m].argtype==OP_ARG_GBL) continue; //TODO: handle OP_ARG_GBL
    op_set set = kernel->args[m].dat->set;
    args_done[m] = 1;
    
    
    //see if arg.dat is indirectly accessed
    int indirect = (kernel->args[m].map != NULL);
    
    //if so, find all the indices of the map used to access the dat
    std::vector<int> ind_indices(0);
    op_map ind_map = NULL;
    if (indirect) {
      ind_map = kernel->args[m].map;
      if (kernel->args[m].idx < 0) for (int i = 0; i < -1*kernel->args[m].idx; i++) ind_indices.push_back(i); //Vector maps support
      else {
        ind_indices.push_back(kernel->args[m].idx);
        //Look for other args with the same map, register their idx
        for (int i = m+1; i < kernel->nargs; i++)
          if (kernel->args[i].map == ind_map && kernel->args[i].acc != OP_READ) {
            if (std::find(ind_indices.begin(), ind_indices.end(), kernel->args[i].idx) == ind_indices.end()) {
              ind_indices.push_back(kernel->args[i].idx);
            }
            args_done[i] = 1;
          }
      }
    } else {
      for (int i = m+1; i < kernel->nargs; i++)
        if (kernel->args[i].map == NULL)
          args_done[i] = 1;
    }
    
    for (int i = m; i < kernel->nargs; i++) {
      if (kernel->args[i].map == ind_map && kernel->args[i].argtype != OP_ARG_GBL) add_dependency(kernel->args[i].dat, kernel->args[i].acc != OP_READ);
    }
    
    //find execution set elements required to compute values of the dataset
    if (indirect) { //if indirect
      std::vector<int> set_elements(0);
      op_map rev_map = ind_map->reverse_map; //determine who in the current loop's execution set may contribute
      for (int i = 0; i < dependencies[set->index].size; i++) { //for each element in the output subset
        int element = dependencies[set->index].elements[i];
        int *from_elements;
        int count;
        if (rev_map->isSimple) {
          from_elements = &rev_map->map[rev_map->dim * element];
          count = rev_map->dim;
        } else {
          from_elements = &rev_map->map[rev_map->row_offsets[element]];
          count = rev_map->row_offsets[element+1] - rev_map->row_offsets[element];
        }
        //find out who really does (some indices of the map may not be used
        for (int k = 0; k < count; k++) {
          for (unsigned int j = 0; j < ind_indices.size(); j++)
            if (ind_map->map[from_elements[k]*ind_map->dim + ind_indices[j]] == element) { //TODO: non-simple forward map support
              set_elements.push_back(from_elements[k]);
              break;
            }
        }
      }
      // at this point we have a big unordered list with duplicates
      std::sort(set_elements.begin(), set_elements.end());  //sort
      int num_unique = std::unique(set_elements.begin(), set_elements.end()) - set_elements.begin(); //get rid of duplicates
      std::vector<int> temp(execution_set.begin(), execution_set.begin()+execution_size);
      execution_size = std::set_union(temp.begin(), temp.end(), set_elements.begin(), set_elements.begin()+num_unique, execution_set.begin()) - execution_set.begin();
    } else { //if the output is directly computed by this loop, then we only need to execute the required subset - merge it into existing
      std::vector<int> temp(execution_set.begin(), execution_set.begin()+execution_size);
      execution_size = std::set_union(temp.begin(), temp.end(), dependencies[set->index].elements.begin(), dependencies[set->index].elements.begin()+dependencies[set->index].size, execution_set.begin()) - execution_set.begin();
      own_set_added = 1;
    }
  }
  
  //write execution set
  kernel->subset->size = execution_size;
  kernel->subset->elements = (int *)malloc(execution_size * sizeof(int));
  std::copy(execution_set.begin(), execution_set.begin() + execution_size, kernel->subset->elements);
  
  std::fill(args_done.begin(), args_done.end(), 0);
  //phase two - based on the execution set update data dependencies
  //TODO: merge some of this with first phase?
  for (int arg = 0; arg < kernel->nargs; arg++) {
    if (kernel->args[arg].argtype==OP_ARG_GBL) continue;
    op_dat data = kernel->args[arg].dat;
    op_set set = data->set;
    
    //Add argument to coloring constraints (will only happen for indirect+OP_INC, and if it has't been added before)
    add_arg_dependency(&kernel->args[arg], kernel->subset->set->index);

    //if OP_WRITE or already processed, skip
    //TODO: OP_WRITE skip or not?
    //TODO: indirect writes???
    if (/*kernel->args[arg].acc == OP_WRITE ||*/ args_done[arg]) continue;

    
    args_done[arg] = 1;
    
    
    //see if arg.dat is indirectly accessed
    int indirect = (kernel->args[arg].map != NULL);
    
    std::vector<int> ind_indices(0);
    op_map ind_map = NULL;
    if (indirect) {
      ind_map = kernel->args[arg].map;
      if (kernel->args[arg].idx < 0) for (int i = 0; i < -1*kernel->args[arg].idx; i++) ind_indices.push_back(i); //Vector maps support
      else {
        ind_indices.push_back(kernel->args[arg].idx);
        //Look for other args with the same map, register their idx
        for (int i = arg+1; i < kernel->nargs; i++)
          if (kernel->args[i].map == ind_map /*&& kernel->args[i].acc != OP_WRITE*/) { //TODO: OP_WRITE?
            if (std::find(ind_indices.begin(), ind_indices.end(), kernel->args[i].idx) == ind_indices.end()) {
              ind_indices.push_back(kernel->args[i].idx);
            }
            args_done[i] = 1;
          }
      }
    } else {
      for (int i = arg+1; i < kernel->nargs; i++)
        if (kernel->args[i].map == NULL)
          args_done[i] = 1;
    }
    
    for (int i = arg; i < kernel->nargs; i++) {
      if (kernel->args[i].map == ind_map && kernel->args[i].argtype != OP_ARG_GBL) add_dependency(kernel->args[i].dat, kernel->args[i].acc != OP_READ);
    }
    
    //find dataset elements required to execute the current execution subset
    if (indirect) { //if indirect
      std::vector<int> set_elements(0);
      for (int i = 0; i < kernel->subset->size; i++) { //for each element in the output subset
        int element = kernel->subset->elements[i];
        int *from_elements;
        if (ind_map->isSimple) {
          from_elements = &ind_map->map[ind_map->dim * element];
        } else {
          printf("ERROR: op_arg's map cannot be non-simple\n"); //TODO: non-simple forward map support
          exit(-1);
        }
        //find out who really does (some indices of the map may not be used
        for (unsigned int j = 0; j < ind_indices.size(); j++)
            set_elements.push_back(from_elements[j]);
      }
      // at this point we have a big unordered list with duplicates
      std::sort(set_elements.begin(), set_elements.end());  //sort
      int num_unique = std::unique(set_elements.begin(), set_elements.end()) - set_elements.begin(); //get rid of duplicates
      std::vector<int> temp(dependencies[set->index].elements.begin(), dependencies[set->index].elements.begin()+dependencies[set->index].size);
      dependencies[set->index].size = std::set_union(temp.begin(), temp.end(), set_elements.begin(), set_elements.begin()+num_unique, dependencies[set->index].elements.begin()) - dependencies[set->index].elements.begin();

    } else { //if the dependent dataset is directly accessed by this loop, then we only need to add the execution subset - merge it into existing
      std::vector<int> temp(dependencies[set->index].elements.begin(), dependencies[set->index].elements.begin()+dependencies[set->index].size);
      dependencies[set->index].size = std::set_union(temp.begin(), temp.end(), kernel->subset->elements, kernel->subset->elements+kernel->subset->size, dependencies[set->index].elements.begin()) - dependencies[set->index].elements.begin();
      own_set_added = 1;
    }
  }
  
  //TODO: this may bring in additional data that may not actually be needed
  // i.e. separate indices that we "know about" and ones for which we need data.
  //For loops such as res_calc, no direct access, edges never brought in as a dependency otherwise, couldn't renumber
  if (!own_set_added) {
    std::vector<int> temp(dependencies[kernel->set->index].elements.begin(), dependencies[kernel->set->index].elements.begin()+dependencies[kernel->set->index].size);
    dependencies[kernel->set->index].size = std::set_union(temp.begin(), temp.end(), kernel->subset->elements, kernel->subset->elements+kernel->subset->size, dependencies[kernel->set->index].elements.begin()) - dependencies[kernel->set->index].elements.begin();
  }
//  printf("Kernel %s dependency list: \n", kernel->name);
//  for (int i = 0; i < dependencies[kernel->set->index].size; i++) {
//    printf("%d ", dependencies[kernel->set->index].elements[i]);
//  }
//  printf("\n");
}

void do_coloring(int set_index) {
  int nargs = dependencies[set_index].args.size();
  //do the coloring if necessary
  int color = 0;
  for (int arg = 0; arg < nargs; arg++) {
    if (dependencies[set_index].args[arg]->map != NULL && dependencies[set_index].args[arg]->acc != OP_READ) { //These aren't added to the list anyway, but for clarity this check is here.
      color = 1;
      break;
    }
  }
  
  op_set set = dependencies[set_index].set;
  std::vector<op_kernel_descriptor *> set_kernels(0);
  //Look for kernels executing over this set
  for (unsigned int i = 0; i < kernel_list.size(); i++) {
    if (kernel_list[i].subset->set == set) {
      set_kernels.push_back(&kernel_list[i]);
    }
  }
  
  if (set_kernels.size()==0) return; //We are done here, no need to renumber anything, we never execute on this set

  //set all colors to -1 if we do coloring, otherwise to 0
  std::vector<kv_sort2> colors(dependencies[set_index].size);
  for (int i = 0 ; i < dependencies[set_index].size; i++) {
    colors[i].key = color ? -1 : 0;
    colors[i].value = dependencies[set_index].elements[i]; //global indices
    colors[i].permutation = i;
  }
  int ncolors = 0;
  
  if (color) {
    //
    // Begin execution set coloring
    //

    std::vector<int> inds(nargs, -1);
    std::vector<op_map> maps(nargs, NULL);
    
    int ninds = 0;
    for (int i = 0; i < nargs; i++) {
      if (dependencies[set_index].args[i]->map != NULL && inds[i] == -1) {
        inds[i] = ninds;
        maps[i] = dependencies[set_index].args[i]->map;
        for (int j = i+1; j<nargs; j++) {
          if (dependencies[set_index].args[i]->map == dependencies[set_index].args[j]->map && dependencies[set_index].args[i]->dat == dependencies[set_index].args[j]->dat) {
            inds[j] = ninds;
            maps[j] = dependencies[set_index].args[j]->map;
          }
        }
        ninds++;
      }
    }
    
    uint **work;
    work = (uint **)malloc(ninds * sizeof(uint *));
    
    for ( int m = 0; m < ninds; m++ )
    {
      int m2 = 0;
      while ( inds[m2] != m )
        m2++;
      
      int to_size = dependencies[maps[m2]->to->index].size;
      work[m] = ( uint * )malloc( to_size * sizeof (uint));
    }
    
    int repeat = 1;
    int ncolor = 0;

    while ( repeat )
    {
      repeat = 0;
      //TODO: vector maps support
      for ( int m = 0; m < nargs; m++ )
      {
        if ( inds[m] > 0 )
          for ( int el = 0; el < dependencies[set_index].size; el++ ) {
//            int e = dependencies[set_index].elements[el];
            work[inds[m]][maps[m]->map[dependencies[set_index].args[m]->idx + el * maps[m]->dim]] = 0; // zero out color array
          }
      }
      
      for ( int el = 0; el < dependencies[set_index].size; el++ ) {
//        int e = dependencies[set_index].elements[el];
        if ( colors[el].key == -1 )
        {
          //TODO: vector maps support
          int mask = 0;
          for ( int m = 0; m < nargs; m++ )
            if ( inds[m] >= 0 && dependencies[set_index].args[m]->acc == OP_INC )
              mask |= work[inds[m]][maps[m]->map[dependencies[set_index].args[m]->idx + el * maps[m]->dim]]; // set bits of mask

          
          int color = ffs ( ~mask ) - 1;  // find first bit not set
          if ( color == -1 )
          {                     // run out of colors on this pass
            repeat = 1;
          }
          else
          {
           colors[el].key = ncolor + color;
            mask = 1 << color;
            ncolors = MAX ( ncolors, ncolor + color + 1 );
            
            for ( int m = 0; m < nargs; m++ )
              if ( inds[m] >= 0 && dependencies[set_index].args[m]->acc == OP_INC )
                work[inds[m]][maps[m]->map[dependencies[set_index].args[m]->idx + el * maps[m]->dim]] |= mask; // set color bit
          }
        }
      }
      
      ncolor += 32;             // increment base level
    }
  //
  // End Execution set coloring
  //
  } else {
    ncolors = 1;
  }
  
  //Now we have the number of colors (1 for sets that aren't colored)
  for (unsigned int i = 0; i < set_kernels.size() ; i++) {
    set_kernels[i]->subset->ncolors = ncolors;
    set_kernels[i]->subset->color_offsets = (int *) malloc ((2*ncolors)*sizeof(int));
    for (int j = 0; j < 2*ncolors; j++) {
      set_kernels[i]->subset->color_offsets[j] = 0;
    }
  }
  
  //Work our way backwards to count the number of colors and amend kernel number to element colors
  float increment = 1.0/(set_kernels.size()+1.0);
  for (int i = set_kernels.size()-1; i >= 0 ; i--) {
    for (int e = 0; e < set_kernels[i]->subset->size; e++) {
      int el = set_kernels[i]->subset->elements[e];
      set_kernels[i]->subset->color_offsets[2*(int)floor(colors[el].key)]++; //we have an element with a certain color
      //if it hasn't been flagged as new, flag it with the current backwards loop index*increment
      if (colors[el].key == floor(colors[el].key)) colors[el].key += increment * (set_kernels.size()-i);
    }
  }
  
  //Put the "non-exec" halo to the very end, so as we don't execute on them (this is data elements that are brought in, but only read, i.e. the outermost ring
  int nonexec = 0;
  for (int i = 0; i < colors.size(); i++) {
    if (colors[i].key == floor(colors[i].key)) {
      colors[i].key = ncolors;
      nonexec++;
    }
  }
  
  //At this point we have for each element a color that will order them by the loop they were introduced by, and each loop knows how many elements it has by color
  
  //Now sort it, so we'll have the set ordered by color and within colors by the level at they were introduced, colors[].value is the global index
  std::stable_sort(colors.begin(), colors.end(), kv_sort2_comp);
  dependencies[set_index].ncolors = ncolors;
  dependencies[set_index].color_offsets.resize(ncolors+1);
  dependencies[set_index].color_offsets[0] = 0;
  dependencies[set_index].color_offsets[ncolors] = dependencies[set_index].size-nonexec;
  //printf("name: %s colors: %d\n", dependencies[set_index].set->name, ncolors);
  for (unsigned i = 0; i < colors.size(); i++) {
    dependencies[set_index].elements[i] = colors[i].value; //reorder global indices mapping back from this tile
    //printf("%d ", colors[i].value);
    if (i > 0 && floor(colors[i].key) > floor(colors[i-1].key)) {
      //We shouldn't have empty colors (when looking at the whole set)
      dependencies[set_index].color_offsets[(int)floor(colors[i].key)] = i;
    }
  }
  //printf("\n");
//  printf("Set %s list: \n", set->name);
//  for (int i = 0; i < dependencies[set_index].size; i++) {
//    printf("%d ", dependencies[set_index].elements[i]);
//  }
//  printf("\n");
  
  //Let's update each loop's color offests and free the loop's execution sets
  for (int i = set_kernels.size()-1; i >= 0 ; i--) {
    free(set_kernels[i]->subset->elements);
    for (int j = 0; j < ncolors; j++) {
      int size = set_kernels[i]->subset->color_offsets[2*j];
      set_kernels[i]->subset->color_offsets[2*j] = dependencies[set_index].color_offsets[j];
      set_kernels[i]->subset->color_offsets[2*j+1] = dependencies[set_index].color_offsets[j] + size;
    }
  }
  
  //Get a reverse permutation: where did the item go?
  std::vector<int> reverse_permutation(dependencies[set_index].size);
  for (unsigned int i = 0; i < colors.size(); i++) {
    reverse_permutation[colors[i].permutation] = i;
  }
  
  for (unsigned int i = 0; i < maps.size(); i++) {
    if (maps[i]->from == set) {
      //this map is based on the set we just reordered, reorder it too
      int *new_map = (int *)malloc(dependencies[set_index].size * maps[i]->dim * sizeof(int));
      for (int j = 0; j < dependencies[set_index].size; j++) {
        memcpy(&new_map[maps[i]->dim * reverse_permutation[j]], &maps[i]->map[maps[i]->dim * j], maps[i]->dim * sizeof(int));
      }
      free(maps[i]->map);
      maps[i]->map = new_map;
    } else if (maps[i]->to == set) {
      //this map point to this set, renumber it
      for (int j = 0; j < dependencies[maps[i]->from->index].size * maps[i]->dim; j++) {
        maps[i]->map[j] = reverse_permutation[maps[i]->map[j]];
      }
    }
  }
  
}