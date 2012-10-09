/*
 * This source file implements all the functions needed
 * to call C core library functions from Fortran
 *
 * It also provides implementation for other
 * Fortran specific core library functions
 */

#include <string.h>

#include <op_lib_c.h>
#include <op_mpi_core.h>
#ifdef NO_MPI

#else
#include <mpi.h>
#endif

#include "../../include/op2_for_C_wrappers.h"

/*
 * Small utility for transforming Fortran OP2 access codes into C OP2 access codes
 */

op_access getAccFromIntCode ( int accCode )
{
  switch ( accCode ) {
  case FOP_READ:
    return OP_READ;
  case FOP_WRITE:
    return OP_WRITE;
  case FOP_RW:
    return OP_RW;
  case FOP_INC:
    return OP_INC;
  case FOP_MIN:
    return OP_MIN;
  case FOP_MAX:
    return OP_MAX;
  default:
    return OP_READ; //default case is treated as READ
  }
}


op_map_core * op_decl_null_map ( )
{
  /* must allocate op_set_core instead of op_set, because the latter is actually a pointer to the former */
  op_set nullSet = NULL;
  op_map map = NULL;

  nullSet = (op_set) calloc ( 1, sizeof ( op_set_core ) );
  map = (op_map) malloc(sizeof(op_map_core));

  nullSet->size = 0;
  nullSet->name = NULL;

  map->from = nullSet;
  map->to = nullSet;
  map->dim = 0; /* set to the proper value in Fortran */
  map->map = NULL;

  return map;
}


op_dat op_decl_gbl_f ( char ** dataIn, int dim, int size, const char * type )
{
  op_dat_core * dataOut = calloc ( 1, sizeof ( op_dat_core ) );

  char * typeName = (char *) calloc ( strlen ( type ), sizeof ( char ) );

  strncpy ( typeName, type, strlen ( type ) );

  dataOut->index = -1;
  dataOut->set = NULL;
  dataOut->dim = dim;
  dataOut->size = size * dim;
  dataOut->data = *dataIn;
  dataOut->data_d = NULL;
  dataOut->type = typeName;
  dataOut->name = NULL;

  return dataOut;
}


/*
 * Utility functions:
 * since op_set/map/dat have become pointers, then from fortran I can't
 * access their fields directly.
 * These routines permit to avoid c_f_pointers in the declaration routines.
 */
int get_set_size (op_set_core * set)
{
  if (set == NULL)
    {
      printf ("Set is NULL\n");
      exit (0);
    }

  return set->size;
}

int get_associated_set_size (op_dat_core * dat)
{
  if (dat == NULL)
    {
      printf ("Dat is NULL\n");
      exit (0);
    }

  if (dat->set == NULL)
    {
      printf ("Set of dat is NULL\n");
      exit (0);
    }

  return dat->set->size;
}


void dumpOpDat (op_dat_core * data, const char * fileName)
{
  int i, j;

  FILE * outfile = fopen (fileName, "w+");

  if (outfile == NULL) exit (0);

  if ( data != NULL )
    {
      // support for old and new names (real should be replaced by double)
      if ( strncmp ( "double", data->type, 6 ) == 0 ||
	   strncmp ( "double", data->type, 6 ) == 0) {
        for ( i = 0; i < data->set->size; i++ )
          for ( j = 0; j < data->dim; j++ )
            fprintf (outfile, "%d, %d -> %e\n", i, j, ((double *) data->data)[i*data->dim + j] );
      }
      else if ( strncmp ( "integer", data->type, 7 ) == 0 )
        for ( i = 0; i < data->dim * data->set->size; i++ )
          fprintf (outfile, "%d\n", ((int *) data->data)[i] );

      else
        {
          printf ( "Unsupported type for dumping %s\n", data->type );
          exit ( 0 );
        }
    }

  i = 291088; j = 2;
  fprintf (outfile, "%d, %d -> %e\n", i, j, ((double *) data->data)[i*data->dim + j] );

  fclose (outfile);
}

/* This function does not specialises w.r.t. a sequence number
 * because of the intrinsic complexity of modifying the
 * LOOP macro
 */
void dumpOpDatSequential(char * kernelName, op_dat_core * dat, op_access access, op_map_core * map)
{
  // OP_GBL or read only
  if (access == OP_READ || map->dim == -1) return;

  char * fileName = calloc (strlen(kernelName) + strlen(dat->name), sizeof (char));
  sprintf (fileName, "%s_%s", kernelName, dat->name);

  dumpOpDat (dat, fileName);
}

void dumpOpDatFromDevice (op_dat_core * data, const char * label, int * sequenceNumber)
{
  op_get_dat (data);

  char * fileName = calloc (strlen(label) + log10(*sequenceNumber) + 1, sizeof (char));

  sprintf (fileName, "%s_%d", label, *sequenceNumber);

  printf ("Dumping %s\n", fileName);

  dumpOpDat (data, fileName);
}

void dumpOpGbl (op_dat_core * data)
{
  int i;
  if ( data != NULL )
    {
      if ( strncmp ( "real", data->type, 4 ) == 0 )
        for ( i = 0; i < data->dim * data->set->size; i++ )
          printf ( "%lf\n", ((double *) data->data)[i] );

      else if ( strncmp ( "integer", data->type, 7 ) == 0 )
        for ( i = 0; i < data->dim * data->set->size; i++ )
          printf ( "%d\n", data->data[i] );
      else
        {
          printf ( "Unsupported type for dumping %s\n", data->type );
          exit ( 0 );
        }
    }
}

void dumpOpMap (op_map_core * map, const char * fileName)
{
  int i, j;

  FILE * outfile = fopen (fileName, "w+");

  if (outfile == NULL) exit (0);

  if ( map != NULL ) {
    for ( i = 0; i < map->from->size; i++ ) {
      for ( j = 0; j < map->dim; j++ ) {
        fprintf (outfile, "%d --> %d", i, ((int *) map->map)[i * map->dim + j] );
      }
      fprintf (outfile, "\n");
    }
  }
  fclose (outfile);
}


op_arg
op_arg_gbl_copy ( char * data, int dim, const char * typ, int size, op_access acc ) {

  int len = strlen (typ);
  char * heapType = (char *) calloc (len, sizeof (char));

  strncpy (heapType, typ, len);

  return op_arg_gbl_char (data, dim, heapType, size, acc);
}

op_arg
op_arg_dat_null (op_dat dat, int idx, op_map map, int dim, const char * typ, op_access acc) {
  op_arg arg;

  arg.argtype = OP_ARG_NULL;

  arg.dat = NULL;

  // forces impossible dimension
  arg.dim = -1;
  arg.idx = -1; //this avoids getting a free in the MPI implementation (see op2_C_reference.c)

  arg.map = NULL;
  arg.acc = OP_ACC_NULL;

  arg.data = NULL;
  arg.data_d = NULL;

  return arg;
}

void op_dump_arg (op_arg * arg)
{
  printf ("index = %d\n", arg->index);
  printf ("dat name = %s, type = %s\n", arg->dat->name, arg->dat->type);

  if ( arg->map != NULL )
    printf ("map name = %s, dim = %d\n", arg->map->name, arg->map->dim);

  printf (" dim and size of op_dat = %d, %d\n", arg->dim, arg->size);
  printf ("type = %s\n", arg->type);
  printf ("access = %d\n", arg->acc);
  printf ("argtype = %d\n", arg->argtype);
  printf ("sent = %d\n", arg->sent);
}

void print_type (op_arg * arg)
{
  printf ("String is %s\n", arg->type);
}

#ifdef NO_MPI

#else
int op_mpi_size () {
  int size;
  MPI_Comm_size (MPI_COMM_WORLD, &size);
  return size;
}

void op_mpi_rank (int * rank) {
  MPI_Comm_rank (MPI_COMM_WORLD, rank);
}

void op_barrier () {
  MPI_Barrier (MPI_COMM_WORLD);
}

void printDat_noGather (op_dat dat) {
  int rank;

  op_mpi_rank (&rank);

  char prefix[13] = "dat_nogather_";
  char filename[14];
  FILE * fileptr;

  sprintf (filename, "%s%d", prefix, rank);

  fileptr = fopen (filename, "a");

  printf ("writing out to %s\n", filename);
  fflush (0);

  // print also the global index of each element
  if ( strncmp ("double", dat->type, 6) == 0 ) {
    for ( int i = 0; i < dat->set->size; i++ ) {
      for ( int j = 0; j < dat->dim; j++ ) {
          fprintf (fileptr, "%lf ", ((double *)dat->data)[i*dat->dim+j]);
//          if ( ((double *)dat->data)[i*dat->dim+j] > 0.0 || ((double *)dat->data)[i*dat->dim+j] < 0.0 ) {
            if ( rank == 2 ) {
              printf ("Rank = %d  --> At (local id = %d, el = %d) original global id = %d --> %.12lf\n", rank, i, j, OP_part_list[dat->set->index]->g_index[i], ((double *)dat->data)[i*dat->dim+j]);
              fflush (0);
            }
//        }
      }
      fprintf (fileptr, "\n");
    }
  }

  fclose (fileptr);
}
#endif

bool isCNullPointer (void * ptr) {
  return (ptr == NULL);
}
