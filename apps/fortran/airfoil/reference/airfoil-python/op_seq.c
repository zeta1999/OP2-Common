


//
// header files
//

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "op_seq.h"


//
// global variables
//

int OP_set_index=0,
		OP_map_index=0,
		OP_dat_index=0,
		OP_nplans   =0,
		OP_diags    =0;

op_set  * OP_set_list[MAX_SET_NUM];
op_map  * OP_map_list[MAX_MAP_NUM];
op_dat  * OP_dat_list[MAX_DAT_NUM];
op_plan   OP_plans[MAX_PLAN_NUM];
op_kernel OP_kernels[MAX_KERN_NUM];




void op_decl_set ( int size, op_set * set, char * name ){
  set->size = size;
  set->name = name;

  set->index = OP_set_index;
  OP_set_list[OP_set_index++] = set;
}


void op_decl_map_f ( op_set * from, op_set * to, int dim, int ** map, op_map * mapping, char * name ){
  mapping->from = *from;
  mapping->to   = *to;
  mapping->dim  = dim;
  mapping->map  = *map;
	mapping->name = name;

  mapping->index = OP_map_index;

  OP_map_list[OP_map_index++] = mapping;
}


void op_decl_map ( op_set from, op_set to, int dim, int * map, op_map * mapping, char * name ){
  mapping->from = from;
  mapping->to   = to;
  mapping->dim  = dim;
  mapping->map  = map;
  mapping->name = name;

  mapping->index = OP_map_index;

  OP_map_list[OP_map_index++] = mapping;
}

void op_decl_null_map ( op_map * map )
{
	op_set nullSet = {0,0};

	map->from = nullSet;
	map->to = nullSet;
	map->dim = 0; //set to the proper value in Fortran
	map->index= -1; // no position in OP_map_list
	map->map = NULL;

	// name is not set
}



void op_decl_dat_f ( op_set * set, int dim, int size, void **dat, op_dat *data, char * name ){
  data->set   = *set;
  data->dim   = dim;
  data->dat   = *dat;
  data->size  = dim*size;
	data->name = name;
  data->index = OP_dat_index;
  OP_dat_list[OP_dat_index++] = data;
}


void op_decl_dat ( op_set set, int dim, int size, void *dat, op_dat *data, char * name ){
  data->set   = set;
  data->dim   = dim;
  data->dat   = dat;
  data->size  = dim*size;
	data->name = name;
  data->index = OP_dat_index;
  OP_dat_list[OP_dat_index++] = data;
}


void arg_set ( int displacement,
							 op_dat * arg,
							 int itemSel, // map field to be used
							 op_map * mapIn,
							 char ** p_arg)
{
  int n2;

  if ( mapIn->dim == -1 ) {					 // global variable, no mapping at all
		n2 = 0;
	}

  if ( mapIn->dim == 0 ) {					 // identity mapping
		n2 = displacement;
	}
	if ( mapIn->dim > 0 ) {             // standard pointers
    n2 = mapIn->map[itemSel + displacement * mapIn->dim];
		// in fortran I have incremented all values by one..
	}

  *p_arg = (char *) (arg->dat + n2 * arg->size);
}






void op_par_loop_2 ( void (*subroutineName)(char *, char *), op_set * set,
										 op_dat * dat0, int itemSel0, op_map * map0, op_access access0,
										 op_dat * dat1, int itemSel1, op_map * map1, op_access access1
									 )
{
	int i;

	for ( i = 0; i < set->size; i++ ) {
		char * ptr0, * ptr1;

		arg_set ( i, dat0, itemSel0, map0, &ptr0 );
		arg_set ( i, dat1, itemSel1, map1, &ptr1 );

		(*subroutineName) ( ptr0, ptr1 );
	}
}

void op_par_loop_5 ( void (*subroutineName)(char *, char *, char *, char *, char *), op_set * set,
										op_dat * dat0, int itemSel0, op_map * map0, op_access access0,
										op_dat * dat1, int itemSel1, op_map * map1, op_access access1,
										op_dat * dat2, int itemSel2, op_map * map2, op_access access2,
										op_dat * dat3, int itemSel3, op_map * map3, op_access access3,
										op_dat * dat4, int itemSel4, op_map * map4, op_access access4
									 )
{
	int i;

	for ( i = 0; i < set->size; i++ ) {

		char * ptr0, * ptr1, * ptr2, * ptr3, * ptr4;

		arg_set ( i, dat0, itemSel0, map0, &ptr0 );
		arg_set ( i, dat1, itemSel1, map1, &ptr1 );
		arg_set ( i, dat2, itemSel2, map2, &ptr2 );
		arg_set ( i, dat3, itemSel3, map3, &ptr3 );
		arg_set ( i, dat4, itemSel4, map4, &ptr4 );


		(*subroutineName) ( ptr0, ptr1, ptr2, ptr3, ptr4 );
		//exit ( 0 );
	}
}

void op_par_loop_6 ( void (*subroutineName)(char *, char *, char *, char *, char *, char *), op_set * set,
										op_dat * dat0, int itemSel0, op_map * map0, op_access access0,
										op_dat * dat1, int itemSel1, op_map * map1, op_access access1,
										op_dat * dat2, int itemSel2, op_map * map2, op_access access2,
										op_dat * dat3, int itemSel3, op_map * map3, op_access access3,
										op_dat * dat4, int itemSel4, op_map * map4, op_access access4,
										op_dat * dat5, int itemSel5, op_map * map5, op_access access5
										)
{
	int i;

	for ( i = 0; i < set->size; i++ ) {

		char * ptr0, * ptr1, * ptr2, * ptr3, * ptr4, * ptr5;

		arg_set ( i, dat0, itemSel0, map0, &ptr0 );
		arg_set ( i, dat1, itemSel1, map1, &ptr1 );
		arg_set ( i, dat2, itemSel2, map2, &ptr2 );
		arg_set ( i, dat3, itemSel3, map3, &ptr3 );
		arg_set ( i, dat4, itemSel4, map4, &ptr4 );
		arg_set ( i, dat5, itemSel5, map5, &ptr5 );


		(*subroutineName) ( ptr0, ptr1, ptr2, ptr3, ptr4, ptr5 );

	}
}

void op_par_loop_8 ( void (*subroutineName)(char *, char *, char *, char *, char *, char *, char *, char *), op_set * set,
										op_dat * dat0, int itemSel0, op_map * map0, op_access access0,
										op_dat * dat1, int itemSel1, op_map * map1, op_access access1,
										op_dat * dat2, int itemSel2, op_map * map2, op_access access2,
										op_dat * dat3, int itemSel3, op_map * map3, op_access access3,
										op_dat * dat4, int itemSel4, op_map * map4, op_access access4,
										op_dat * dat5, int itemSel5, op_map * map5, op_access access5,
										op_dat * dat6, int itemSel6, op_map * map6, op_access access6,
										op_dat * dat7, int itemSel7, op_map * map7, op_access access7
										)
{
	int i;

	for ( i = 0; i < set->size; i++ ) {

		char * ptr0, * ptr1, * ptr2, * ptr3, * ptr4, * ptr5, * ptr6, * ptr7;

		arg_set ( i, dat0, itemSel0, map0, &ptr0 );
		arg_set ( i, dat1, itemSel1, map1, &ptr1 );
		arg_set ( i, dat2, itemSel2, map2, &ptr2 );
		arg_set ( i, dat3, itemSel3, map3, &ptr3 );
		arg_set ( i, dat4, itemSel4, map4, &ptr4 );
		arg_set ( i, dat5, itemSel5, map5, &ptr5 );
		arg_set ( i, dat6, itemSel6, map6, &ptr6 );
		arg_set ( i, dat7, itemSel7, map7, &ptr7 );


		(*subroutineName) ( ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7 );

	}
}
