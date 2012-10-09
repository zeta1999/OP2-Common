#
# The following environment variables are required:
#
# CUDA_INSTALL_PATH - CUDA base directory
# OP2_COMPILER      - Compiler toolchain: gnu (gcc, gfortran) or pgi (icc, pgfortran)
# OP2               - OP2 base directory
#
# Note: the CUDA library can only be built with the PGI toolchain

C_OP2 = $(OP2)/c
C_OP2_INC = $(C_OP2)/include
C_OP2_SRC = $(C_OP2)/src

F_OP2 = $(OP2)/fortran
F_OP2_INC = $(F_OP2)/include
F_OP2_SRC = $(F_OP2)/src
F_OP2_OBJ = $(F_OP2)/obj
F_OP2_LIB = $(F_OP2)/lib
F_OP2_MOD = $(F_OP2)/mod

F_OP2_USER = $(F_OP2_SRC)/user
F_OP2_BACKEND = $(F_OP2_SRC)/backend


PARMETIS_INCLUDE_PATH ?= $(PARMETIS_INSTALL_PATH)/include
PARMETIS_LIB_PATH ?= $(PARMETIS_INSTALL_PATH)
PARMETIS_INC = -I$(PARMETIS_INCLUDE_PATH) -DHAVE_PARMETIS
PARMETIS_LIB = -L$(PARMETIS_LIB_PATH) -lparmetis -lmetis

PTSCOTCH_INCLUDE_PATH ?= $(PTSCOTCH_INSTALL_PATH)/include
PTSCOTCH_LIB_PATH ?= $(PTSCOTCH_INSTALL_PATH)/lib
PTSCOTCH_INC = -I$(PTSCOTCH_INCLUDE_PATH) -DHAVE_PTSCOTCH
PTSCOTCH_LIB = -L$(PTSCOTCH_LIB_PATH) -lptscotch -lptscotcherr

<<<<<<< HEAD
HDF5_INCLUDE_PATH ?= $(HDF5_INSTALL_PATH)/include
HDF5_INCLUDE =  -I$(HDF5_INCLUDE_PATH)

HDF5_LIB_PATH ?= $(HDF5_INSTALL_PATH)/lib
HDF5_LIB =  -L$(HDF5_LIB_PATH) -lhdf5
=======
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6

#
# compiler settings
#

# Default to PGI compiler
OP2_COMPILER ?= pgi

ifeq ($(OP2_COMPILER),gnu)
  CC         = gcc
  CFLAGS     = -std=c99 -Wall -Werror -fPIC -pedantic -pipe
  FC         = gfortran
  FFLAGS     = -J$(F_OP2_MOD) -Wall -Werror -pedantic -pipe
  ALLTARGETS = f_reference f_openmp
else
ifeq ($(OP2_COMPILER),pgi)
  F_OP2_MOD_INTEL = $(F_OP2_MOD)/intel
  F_OP2_MOD_PGI   = $(F_OP2_MOD)/pgi
  CC              = icc -g
  CFLAGS          = -std=c99 -Wall -Werror
  FC              = ifort -g
	MPICC						= mpicc -g
	MPIF90					= mpif90 -g
  FFLAGS          = -module $(F_OP2_MOD_INTEL)
<<<<<<< HEAD
  NVCC            = nvcc -g
  PGCC            = pgcc -g
  PGCFLAGS        = -Xcompiler="-g -std=c99 -Wall -fPIC -pedantic -pipe"
  PGFC            = pgfortran -g
=======
  NVCC            = nvcc
  PGCC            = pgcc
  PGCFLAGS        = -Xcompiler="-g -std=c99 -Wall -fPIC -pedantic -pipe"
  PGFC            = pgfortran
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
  PGFFLAGS        = -module $(F_OP2_MOD_PGI) -Mcuda=cuda4.0 -Minform=inform -DOP2_WITH_CUDAFOR
  ALLTARGETS      = f_reference f_openmp f_cuda
else
print:
	@echo "unrecognised value for OP2_COMPILER"
endif
endif

CUDA_INC = $(CUDA_INSTALL_PATH)/include


<<<<<<< HEAD

=======
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
NVCCFLAGS = #-O4

MPI_INC = $(MPI_INSTALL_PATH)/include
MPI_LIB = $(MPI_INSTALL_PATH)/lib

.PHONY: clean directories

# reference is not included in the all directive (missing gfortran on cx1) and
# is treated as a special case
all: $(ALLTARGETS)

directories:
	mkdir -p $(F_OP2_LIB)
	mkdir -p $(F_OP2_MOD) $(F_OP2_MOD_INTEL) $(F_OP2_MOD_PGI)
	mkdir -p $(F_OP2_OBJ)

# does not depend on reference.h because we don't need the op_par_loop templated
# implementation, but our own implementation
f_reference: $(C_OP2_INC)/op_lib_core.h $(C_OP2_SRC)/core/op_lib_core.c \
						 $(C_OP2_SRC)/sequential/op_seq.c \
						 $(C_OP2_SRC)/core/op_dummy_singlenode.c \
						 $(F_OP2_SRC)/backend/op_dummy_wrappers.c \
						 $(F_OP2_SRC)/backend/op2_for_C_wrappers.c \
						 $(F_OP2_SRC)/backend/op2_for_declarations.F90 \
						 $(F_OP2_SRC)/backend/op2_for_reference.F90 \
						 $(F_OP2_SRC)/backend/op2_C_reference.c \
						 directories
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_lib_core.c -o $(F_OP2_OBJ)/op_lib_core.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/sequential/op_seq.c -o $(F_OP2_OBJ)/op_seq.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_dummy_singlenode.c -o $(F_OP2_OBJ)/op_dummy_singlenode.o
	$(CC) $(CFLAGS) -I $(F_OP2_INC) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op_dummy_wrappers.c -o $(F_OP2_OBJ)/op_dummy_wrappers.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
	$(FC) $(FFLAGS) -c $(F_OP2_SRC)/backend/op2_for_declarations.F90 -o $(F_OP2_OBJ)/op2_for_declarations.o
	$(shell cp $(F_OP2_SRC)/backend/op2_for_reference.F90 $(F_OP2_SRC)/backend/op2_for_reference.c)
	$(shell cpp $(F_OP2_SRC)/backend/op2_for_reference.c | sed s/##//g | sed s/\"@\"//g | tr "@" "\\n" > $(F_OP2_SRC)/backend/op2_cpp_reference.F90)
	$(FC) -c $(FFLAGS) $(F_OP2_SRC)/backend/op2_cpp_reference.F90 -o $(F_OP2_OBJ)/op2_for_reference.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_C_reference.c -o $(F_OP2_OBJ)/op2_C_reference.o
	ar -r $(F_OP2_LIB)/libop2_for_reference.a \
				$(F_OP2_OBJ)/op_lib_core.o \
				$(F_OP2_OBJ)/op_seq.o \
				$(F_OP2_OBJ)/op_dummy_singlenode.o \
				$(F_OP2_OBJ)/op_dummy_wrappers.o \
				$(F_OP2_OBJ)/op2_for_reference.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op2_for_declarations.o \
				$(F_OP2_OBJ)/op2_C_reference.o

f_cuda: $(C_OP2_INC)/op_lib_core.h $(C_OP2_SRC)/core/op_lib_core.c \
				$(C_OP2_INC)/op_cuda_rt_support.h $(C_OP2_SRC)/cuda/op_cuda_rt_support.c \
				$(C_OP2_SRC)/cuda/op_cuda_decl.c \
				$(F_OP2_SRC)/backend/op2_for_rt_wrappers.c \
			  $(C_OP2_SRC)/core/op_dummy_singlenode.c \
				$(F_OP2_SRC)/backend/op2_for_rt_support.F90 \
				$(C_OP2_INC)/op_rt_support.h $(C_OP2_SRC)/core/op_rt_support.c \
				$(F_OP2_SRC)/backend/op2_for_rt_wrappers_cuda.c \
				directories
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_lib_core.c -o $(F_OP2_OBJ)/op_lib_core.o
	$(NVCC) $(PGCFLAGS) $(NVCCFLAGS) -I$(C_OP2_INC)  -I$(CUDA_INC) -c $(C_OP2_SRC)/cuda/op_cuda_decl.c -o $(F_OP2_OBJ)/op_cuda_decl.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -I$(CUDA_INC) -c $(C_OP2_SRC)/cuda/op_cuda_rt_support.c -o $(F_OP2_OBJ)/op_cuda_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_dummy_singlenode.c -o $(F_OP2_OBJ)/op_dummy_singlenode.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_declarations.F90 -o $(F_OP2_OBJ)/op2_for_declarations.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_rt_support.F90 -o $(F_OP2_OBJ)/op2_for_rt_support.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/cudaConfigurationParams.F90 -o $(F_OP2_OBJ)/cudaConfigurationParams.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_rt_support.c -o $(F_OP2_OBJ)/op_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -I$(F_OP2_SRC)/backend/ -I$(CUDA_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers_cuda.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o
	ar -r $(F_OP2_LIB)/libop2_for_cuda.a \
				$(F_OP2_OBJ)/op_lib_core.o \
				$(F_OP2_OBJ)/op_rt_support.o \
				$(F_OP2_OBJ)/op_cuda_decl.o \
				$(F_OP2_OBJ)/op_cuda_rt_support.o \
				$(F_OP2_OBJ)/op_dummy_singlenode.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op2_for_declarations.o \
				$(F_OP2_OBJ)/op2_for_rt_support.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o \
				$(F_OP2_OBJ)/cudaConfigurationParams.o

f_openmp: $(C_OP2_INC)/op_lib_core.h $(C_OP2_SRC)/core/op_lib_core.c \
					$(C_OP2_SRC)/openmp/op_openmp_decl.c \
					$(F_OP2_SRC)/backend/op2_for_declarations.F90 \
					$(C_OP2_INC)/op_rt_support.h $(C_OP2_SRC)/core/op_rt_support.c \
					$(F_OP2_SRC)/backend/op2_for_rt_wrappers.c \
					$(F_OP2_SRC)/backend/op2_for_rt_support.F90 \
					$(C_OP2_SRC)/externlib/op_hdf5.c \
				  $(F_OP2_SRC)/backend/op_dummy_wrappers.c \
					$(C_OP2_SRC)/core/op_dummy_singlenode.c \
					$(C_OP2_SRC)/externlib/op_util.c \
					$(F_OP2_SRC)/backend/op2_for_reference.F90 \
					$(F_OP2_SRC)/backend/op2_C_reference.c \
					directories
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_lib_core.c -o $(F_OP2_OBJ)/op_lib_core.o
	$(CC) $(CFLAGS) -DNO_MPI -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_rt_support.c -o $(F_OP2_OBJ)/op_rt_support.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/openmp/op_openmp_decl.c -o $(F_OP2_OBJ)/op_openmp_decl.o
	$(FC) $(FFLAGS) -c $(F_OP2_SRC)/backend/op2_for_declarations.F90 -o $(F_OP2_OBJ)/op2_for_declarations.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers.o
	$(FC) $(FFLAGS) -c $(F_OP2_SRC)/backend/op2_for_rt_support.F90 -o $(F_OP2_OBJ)/op2_for_rt_support.o
	$(CC) $(CFLAGS) $(HDF5_INCLUDE) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/externlib/op_hdf5.c -o $(F_OP2_OBJ)/op_hdf5.o
	$(CC) $(CFLAGS) -I $(F_OP2_INC) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op_dummy_wrappers.c -o $(F_OP2_OBJ)/op_dummy_wrappers.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_dummy_singlenode.c -o $(F_OP2_OBJ)/op_dummy_singlenode.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/externlib/op_util.c -o $(F_OP2_OBJ)/op_util.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_C_reference.c -o $(F_OP2_OBJ)/op2_C_reference.o
	$(shell cp $(F_OP2_SRC)/backend/op2_for_reference.F90 $(F_OP2_SRC)/backend/op2_for_reference.c)
	$(shell cpp $(F_OP2_SRC)/backend/op2_for_reference.c | sed s/##//g | sed s/\"@\"//g | tr "@" "\\n" > $(F_OP2_SRC)/backend/op2_cpp_reference.F90)
	$(FC) -c $(FFLAGS) $(F_OP2_SRC)/backend/op2_cpp_reference.F90 -o $(F_OP2_OBJ)/op2_for_reference.o
	ar -r $(F_OP2_LIB)/libop2_for_openmp.a \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op_lib_core.o \
				$(F_OP2_OBJ)/op_rt_support.o \
				$(F_OP2_OBJ)/op_openmp_decl.o \
				$(F_OP2_OBJ)/op2_for_declarations.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers.o \
				$(F_OP2_OBJ)/op2_for_rt_support.o \
				$(F_OP2_OBJ)/op_dummy_wrappers.o \
				$(F_OP2_OBJ)/op_dummy_singlenode.o \
				$(F_OP2_OBJ)/op_util.o \
				$(F_OP2_OBJ)/op2_for_reference.o \
				$(F_OP2_OBJ)/op2_C_reference.o \
				$(F_OP2_OBJ)/op_hdf5.o

f_rt_support: $(C_OP2_INC)/op_rt_support.h $(C_OP2_SRC)/core/op_rt_support.c \
							$(F_OP2_SRC)/backend/op2_for_rt_wrappers.c $(F_OP2_SRC)/backend/op2_for_rt_support.F90 \
							directories
	$(PGCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_rt_support.c -o $(F_OP2_OBJ)/op_rt_support.o
	$(PGCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_rt_support.F90 -o $(F_OP2_OBJ)/op2_for_rt_support.o
	$(PGCC) $(PGCFLAGS) -I$(C_OP2_INC) -I$(F_OP2_SRC)/backend/ -I$(CUDA_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers_cuda.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o
	ar -r $(F_OP2_LIB)/libop2_for_rt_support.a \
				$(F_OP2_OBJ)/op_rt_support.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers.o \
				$(F_OP2_OBJ)/op2_for_rt_support.o	\
				$(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o

# the following is used when the application mixes openmp loops with reference loops
f_openmp_reference: f_openmp_rt_support $(C_OP2_INC)/op_lib_core.h $(C_OP2_SRC)/core/op_lib_core.c \
										$(C_OP2_SRC)/openmp/op_openmp_decl.c \
										$(F_OP2_SRC)/backend/op2_for_declarations.F90 \
										$(F_OP2_SRC)/backend/op2_for_reference.F90 \
										$(F_OP2_SRC)/backend/op2_C_reference.c \
										directories
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_lib_core.c -o $(F_OP2_OBJ)/op_lib_core.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/openmp/op_openmp_decl.c -o $(F_OP2_OBJ)/op_openmp_decl.o
	$(FC) $(FFLAGS) -c $(F_OP2_SRC)/backend/op2_for_declarations.F90 -o $(F_OP2_OBJ)/op2_for_declarations.o
	$(shell cp $(F_OP2_SRC)/backend/op2_for_reference.F90 $(F_OP2_SRC)/backend/op2_for_reference.c)
	$(shell cpp $(F_OP2_SRC)/backend/op2_for_reference.c | sed s/##//g | sed s/\"@\"//g | tr "@" "\\n" > $(F_OP2_SRC)/backend/op2_cpp_reference.F90)
	$(FC) -c $(FFLAGS) $(F_OP2_SRC)/backend/op2_cpp_reference.F90 -o $(F_OP2_OBJ)/op2_for_reference.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_C_reference.c -o $(F_OP2_OBJ)/op2_C_reference.o
	ar -r $(F_OP2_LIB)/libop2_for_openmp_reference.a \
				$(F_OP2_OBJ)/op2_for_reference.o \
				$(F_OP2_OBJ)/op2_C_reference.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op_lib_core.o \
				$(F_OP2_OBJ)/op_openmp_decl.o \
				$(F_OP2_OBJ)/op2_for_declarations.o

f_mpi_seq: $(C_OP2_INC)/op_lib_core.h $(C_OP2_SRC)/core/op_lib_core.c \
					 $(C_OP2_INC)/op_rt_support.h $(C_OP2_SRC)/core/op_rt_support.c \
					 $(F_OP2_SRC)/backend/op2_for_C_wrappers.c \
					 $(C_OP2_SRC)/externlib/op_util.c \
					 $(F_OP2_SRC)/backend/op2_for_declarations.F90 \
					 $(F_OP2_SRC)/backend/op2_for_rt_support.F90 \
					 $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c \
					 $(F_OP2_SRC)/backend/op2_for_reference.F90 \
					 $(F_OP2_SRC)/backend/op_dummy_wrappers.c \
					 $(F_OP2_SRC)/backend/op2_C_reference.c \
					 $(C_OP2_INC)/op_lib_mpi.h  $(C_OP2_INC)/op_mpi_core.h \
					 $(C_OP2_SRC)/mpi/op_mpi_core.c $(C_OP2_SRC)/mpi/op_mpi_decl.c \
					 $(C_OP2_SRC)/mpi/op_mpi_part_core.c $(C_OP2_SRC)/mpi/op_mpi_rt_support.c \
					 directories
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_lib_core.c -o $(F_OP2_OBJ)/op_lib_core.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_rt_support.c -o $(F_OP2_OBJ)/op_rt_support.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/externlib/op_util.c -o $(F_OP2_OBJ)/op_util.o
	$(CC) $(CFLAGS) -I $(F_OP2_INC) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op_dummy_wrappers.c -o $(F_OP2_OBJ)/op_dummy_wrappers.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
	$(FC) $(FFLAGS) -c $(F_OP2_SRC)/backend/op2_for_declarations.F90 -o $(F_OP2_OBJ)/op2_for_declarations.o
	$(FC) $(FFLAGS) -c $(F_OP2_SRC)/backend/op2_for_rt_support.F90 -o $(F_OP2_OBJ)/op2_for_rt_support.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers.o
	$(shell cp $(F_OP2_SRC)/backend/op2_for_reference.F90 $(F_OP2_SRC)/backend/op2_for_reference.c)
	$(shell cpp $(F_OP2_SRC)/backend/op2_for_reference.c | sed s/##//g | sed s/\"@\"//g | tr "@" "\\n" > $(F_OP2_SRC)/backend/op2_cpp_reference.F90)
	$(FC) -c $(FFLAGS) $(F_OP2_SRC)/backend/op2_cpp_reference.F90 -o $(F_OP2_OBJ)/op2_for_reference.o
	$(CC) $(CFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_C_reference.c -o $(F_OP2_OBJ)/op2_C_reference.o
	$(MPICC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_core.c -o $(F_OP2_OBJ)/op_mpi_core.o
	$(MPICC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_decl.c -o $(F_OP2_OBJ)/op_mpi_decl.o
	$(MPICC) $(CFLAGS) -I$(C_OP2_INC) $(PARMETIS_INC) $(PARMETIS_LIB) -c $(C_OP2_SRC)/mpi/op_mpi_part_core.c -o $(F_OP2_OBJ)/op_mpi_part_core.o
	$(MPICC) $(CFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_rt_support.c -o $(F_OP2_OBJ)/op_mpi_rt_support.o
	ar -r $(F_OP2_LIB)/libop2_for_mpi_seq.a \
				$(F_OP2_OBJ)/op_lib_core.o \
				$(F_OP2_OBJ)/op_rt_support.o \
				$(F_OP2_OBJ)/op_util.o \
				$(F_OP2_OBJ)/op_dummy_wrappers.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op2_for_declarations.o \
				$(F_OP2_OBJ)/op2_for_rt_support.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers.o \
				$(F_OP2_OBJ)/op_mpi_core.o \
				$(F_OP2_OBJ)/op_mpi_decl.o \
				$(F_OP2_OBJ)/op_mpi_part_core.o \
				$(F_OP2_OBJ)/op_mpi_rt_support.o \
				$(F_OP2_OBJ)/op2_C_reference.o \
				$(F_OP2_OBJ)/op2_for_reference.o


f_mpi_cuda: $(C_OP2_INC)/op_lib_core.h $(C_OP2_SRC)/core/op_lib_core.c \
					 $(C_OP2_INC)/op_rt_support.h $(C_OP2_SRC)/core/op_rt_support.c \
					 $(F_OP2_SRC)/backend/op2_for_C_wrappers.c \
					 $(C_OP2_SRC)/externlib/op_util.c \
					 $(F_OP2_SRC)/backend/op2_for_declarations.F90 \
					 $(F_OP2_SRC)/backend/op2_for_rt_support.F90 \
					 $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c \
					 $(F_OP2_SRC)/backend/op2_for_reference.F90 \
					 $(F_OP2_SRC)/backend/op2_C_reference.c \
					 $(C_OP2_INC)/op_lib_mpi.h  $(C_OP2_INC)/op_mpi_core.h \
					 $(C_OP2_SRC)/mpi/op_mpi_core.c $(C_OP2_SRC)/mpi/op_mpi_cuda_decl.c \
					 $(C_OP2_SRC)/mpi/op_mpi_part_core.c $(C_OP2_SRC)/mpi/op_mpi_cuda_rt_support.c \
					 $(C_OP2_SRC)/mpi/op_mpi_cuda_kernels.cu \
					 directories
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_lib_core.c -o $(F_OP2_OBJ)/op_lib_core.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_rt_support.c -o $(F_OP2_OBJ)/op_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/externlib/op_util.c -o $(F_OP2_OBJ)/op_util.o
<<<<<<< HEAD
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -I$(MPI_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
=======
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_declarations.F90 -o $(F_OP2_OBJ)/op2_for_declarations.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_rt_support.F90 -o $(F_OP2_OBJ)/op2_for_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers_cuda.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o
	$(shell cp $(F_OP2_SRC)/backend/op2_for_reference.F90 $(F_OP2_SRC)/backend/op2_for_reference.c)
	$(shell cpp $(F_OP2_SRC)/backend/op2_for_reference.c | sed s/##//g | sed s/\"@\"//g | tr "@" "\\n" > $(F_OP2_SRC)/backend/op2_cpp_reference.F90)
	$(PGFC) -c $(PGFFLAGS) $(F_OP2_SRC)/backend/op2_cpp_reference.F90 -o $(F_OP2_OBJ)/op2_for_reference.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_C_reference.c -o $(F_OP2_OBJ)/op2_C_reference.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_core.c -o $(F_OP2_OBJ)/op_mpi_core.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_cuda_decl.c -o $(F_OP2_OBJ)/op_mpi_cuda_decl.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/cuda/op_cuda_rt_support.c -o $(F_OP2_OBJ)/op_cuda_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) $(PARMETIS_INC) $(PARMETIS_LIB) -c $(C_OP2_SRC)/mpi/op_mpi_part_core.c -o $(F_OP2_OBJ)/op_mpi_part_core.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_cuda_rt_support.c -o $(F_OP2_OBJ)/op_mpi_cuda_rt_support.o
	$(NVCC) $(NVCCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_cuda_kernels.cu -o $(F_OP2_OBJ)/op_mpi_cuda_kernels.o
<<<<<<< HEAD
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) $(NVCCFLAGS) $(HDF5_INCLUDE) -I$(C_OP2_INC)  -I$(CUDA_INC) -c $(C_OP2_SRC)/mpi/op_mpi_hdf5.c -o $(F_OP2_OBJ)/op_mpi_hdf5.o
=======
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
	ar -r $(F_OP2_LIB)/libop2_for_mpi_cuda.a \
				$(F_OP2_OBJ)/op_lib_core.o \
				$(F_OP2_OBJ)/op_rt_support.o \
				$(F_OP2_OBJ)/op_util.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op2_for_declarations.o \
				$(F_OP2_OBJ)/op2_for_rt_support.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers.o \
				$(F_OP2_OBJ)/op_mpi_core.o \
				$(F_OP2_OBJ)/op_mpi_cuda_decl.o \
				$(F_OP2_OBJ)/op_cuda_rt_support.o \
				$(F_OP2_OBJ)/op_mpi_part_core.o \
				$(F_OP2_OBJ)/op_mpi_cuda_rt_support.o \
				$(F_OP2_OBJ)/op2_C_reference.o \
				$(F_OP2_OBJ)/op2_for_reference.o \
<<<<<<< HEAD
				$(F_OP2_OBJ)/op_mpi_hdf5.o \
=======
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
				$(F_OP2_OBJ)/op_mpi_cuda_kernels.o

# Warning: the following directives are specialised for HYDRA while transitioning to full OP2 capability
# Do not use them unless you know what you are doing

# CUDA backend, includes sequential implementation of op_par_loop
f_cuda_hydra: $(C_OP2_INC)/op_lib_core.h $(C_OP2_SRC)/core/op_lib_core.c \
				$(C_OP2_INC)/op_cuda_rt_support.h $(C_OP2_SRC)/cuda/op_cuda_rt_support.c \
				$(C_OP2_SRC)/cuda/op_cuda_decl.c \
				$(F_OP2_SRC)/backend/op2_for_rt_wrappers.c \
				$(C_OP2_SRC)/core/op_dummy_singlenode.c \
				$(F_OP2_SRC)/backend/op2_for_rt_support.F90 \
				$(C_OP2_INC)/op_rt_support.h $(C_OP2_SRC)/core/op_rt_support.c \
				$(F_OP2_SRC)/backend/op2_for_rt_wrappers_cuda.c \
				$(F_OP2_SRC)/backend/op2_for_reference.F90 \
				$(F_OP2_SRC)/backend/op2_C_reference.c \
<<<<<<< HEAD
			  $(C_OP2_SRC)/externlib/op_util.c \
				$(C_OP2_SRC)/externlib/op_hdf5.c \
=======
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
				directories
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_lib_core.c -o $(F_OP2_OBJ)/op_lib_core.o
	$(NVCC) $(PGCFLAGS) $(NVCCFLAGS) -I$(C_OP2_INC)  -I$(CUDA_INC) -c $(C_OP2_SRC)/cuda/op_cuda_decl.c -o $(F_OP2_OBJ)/op_cuda_decl.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -I$(CUDA_INC) -c $(C_OP2_SRC)/cuda/op_cuda_rt_support.c -o $(F_OP2_OBJ)/op_cuda_rt_support.o
<<<<<<< HEAD
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
=======
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_dummy_singlenode.c -o $(F_OP2_OBJ)/op_dummy_singlenode.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_declarations.F90 -o $(F_OP2_OBJ)/op2_for_declarations.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_rt_support.F90 -o $(F_OP2_OBJ)/op2_for_rt_support.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/cudaConfigurationParams.F90 -o $(F_OP2_OBJ)/cudaConfigurationParams.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_rt_support.c -o $(F_OP2_OBJ)/op_rt_support.o
<<<<<<< HEAD
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/externlib/op_util.c -o $(F_OP2_OBJ)/op_util.o
=======
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -I$(F_OP2_SRC)/backend/ -I$(CUDA_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers_cuda.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o
	$(shell cp $(F_OP2_SRC)/backend/op2_for_reference.F90 $(F_OP2_SRC)/backend/op2_for_reference.c)
	$(shell cpp $(F_OP2_SRC)/backend/op2_for_reference.c | sed s/##//g | sed s/\"@\"//g | tr "@" "\\n" > $(F_OP2_SRC)/backend/op2_cpp_reference.F90)
	$(PGFC) -c $(PGFFLAGS) $(F_OP2_SRC)/backend/op2_cpp_reference.F90 -o $(F_OP2_OBJ)/op2_for_reference.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_C_reference.c -o $(F_OP2_OBJ)/op2_C_reference.o
<<<<<<< HEAD
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) $(NVCCFLAGS) $(HDF5_INCLUDE) -I$(C_OP2_INC)  -I$(CUDA_INC) -c $(C_OP2_SRC)/externlib/op_hdf5.c -o $(F_OP2_OBJ)/op_hdf5.o
=======
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6
	ar -r $(F_OP2_LIB)/libop2_for_cuda_hydra.a \
				$(F_OP2_OBJ)/op_lib_core.o \
				$(F_OP2_OBJ)/op_rt_support.o \
				$(F_OP2_OBJ)/op_cuda_decl.o \
				$(F_OP2_OBJ)/op_cuda_rt_support.o \
				$(F_OP2_OBJ)/op_dummy_singlenode.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op2_for_declarations.o \
				$(F_OP2_OBJ)/op2_for_rt_support.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o \
				$(F_OP2_OBJ)/cudaConfigurationParams.o \
				$(F_OP2_OBJ)/op2_for_reference.o \
<<<<<<< HEAD
				$(F_OP2_OBJ)/op2_C_reference.o \
				$(F_OP2_OBJ)/op_util.o \
				$(F_OP2_OBJ)/op_hdf5.o

f_mpi_seq_hydra: $(C_OP2_INC)/op_lib_core.h $(C_OP2_SRC)/core/op_lib_core.c \
					 $(C_OP2_INC)/op_rt_support.h $(C_OP2_SRC)/core/op_rt_support.c \
					 $(F_OP2_SRC)/backend/op2_for_C_wrappers.c \
					 $(C_OP2_SRC)/externlib/op_util.c \
					 $(F_OP2_SRC)/backend/op2_for_declarations.F90 \
					 $(F_OP2_SRC)/backend/op2_for_rt_support.F90 \
					 $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c \
					 $(F_OP2_SRC)/backend/op2_for_reference.F90 \
					 $(F_OP2_SRC)/backend/op_dummy_wrappers.c \
					 $(F_OP2_SRC)/backend/op2_C_reference.c \
					 $(C_OP2_INC)/op_lib_mpi.h  $(C_OP2_INC)/op_mpi_core.h \
					 $(C_OP2_SRC)/mpi/op_mpi_core.c $(C_OP2_SRC)/mpi/op_mpi_decl.c \
					 $(C_OP2_SRC)/mpi/op_mpi_part_core.c $(C_OP2_SRC)/mpi/op_mpi_rt_support.c \
	 				 $(C_OP2_SRC)/mpi/op_mpi_hdf5.c \
					 directories
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_lib_core.c -o $(F_OP2_OBJ)/op_lib_core.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_rt_support.c -o $(F_OP2_OBJ)/op_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/externlib/op_util.c -o $(F_OP2_OBJ)/op_util.o
	$(NVCC) $(PGCFLAGS) -I $(F_OP2_INC) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op_dummy_wrappers.c -o $(F_OP2_OBJ)/op_dummy_wrappers.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_declarations.F90 -o $(F_OP2_OBJ)/op2_for_declarations.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_rt_support.F90 -o $(F_OP2_OBJ)/op2_for_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers.o
	$(shell cp $(F_OP2_SRC)/backend/op2_for_reference.F90 $(F_OP2_SRC)/backend/op2_for_reference.c)
	$(shell cpp $(F_OP2_SRC)/backend/op2_for_reference.c | sed s/##//g | sed s/\"@\"//g | tr "@" "\\n" > $(F_OP2_SRC)/backend/op2_cpp_reference.F90)
	$(PGFC) -c $(PGFFLAGS) $(F_OP2_SRC)/backend/op2_cpp_reference.F90 -o $(F_OP2_OBJ)/op2_for_reference.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_C_reference.c -o $(F_OP2_OBJ)/op2_C_reference.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_core.c -o $(F_OP2_OBJ)/op_mpi_core.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_decl.c -o $(F_OP2_OBJ)/op_mpi_decl.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) $(PARMETIS_INC) -c $(C_OP2_SRC)/mpi/op_mpi_part_core.c -o $(F_OP2_OBJ)/op_mpi_part_core.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_rt_support.c -o $(F_OP2_OBJ)/op_mpi_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) $(NVCCFLAGS) $(HDF5_INCLUDE) -I$(C_OP2_INC)  -I$(CUDA_INC) -c $(C_OP2_SRC)/mpi/op_mpi_hdf5.c -o $(F_OP2_OBJ)/op_mpi_hdf5.o
	ar -r $(F_OP2_LIB)/libop2_for_mpi_seq_hydra.a \
				$(F_OP2_OBJ)/op_lib_core.o \
				$(F_OP2_OBJ)/op_rt_support.o \
				$(F_OP2_OBJ)/op_util.o \
				$(F_OP2_OBJ)/op_dummy_wrappers.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op2_for_declarations.o \
				$(F_OP2_OBJ)/op2_for_rt_support.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers.o \
				$(F_OP2_OBJ)/op_mpi_core.o \
				$(F_OP2_OBJ)/op_mpi_decl.o \
				$(F_OP2_OBJ)/op_mpi_part_core.o \
				$(F_OP2_OBJ)/op_mpi_rt_support.o \
				$(F_OP2_OBJ)/op2_C_reference.o \
				$(F_OP2_OBJ)/op2_for_reference.o \
				$(F_OP2_OBJ)/op_mpi_hdf5.o



f_mpi_cuda_hydra: $(C_OP2_INC)/op_lib_core.h $(C_OP2_SRC)/core/op_lib_core.c \
					 $(C_OP2_INC)/op_rt_support.h $(C_OP2_SRC)/core/op_rt_support.c \
					 $(F_OP2_SRC)/backend/op2_for_C_wrappers.c \
					 $(C_OP2_SRC)/externlib/op_util.c \
					 $(F_OP2_SRC)/backend/op2_for_declarations.F90 \
					 $(F_OP2_SRC)/backend/op2_for_rt_support.F90 \
					 $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c \
					 $(F_OP2_SRC)/backend/op2_for_rt_wrappers_cuda.c \
					 $(F_OP2_SRC)/backend/op2_for_reference.F90 \
					 $(F_OP2_SRC)/backend/op2_C_reference.c \
					 $(C_OP2_INC)/op_lib_mpi.h  $(C_OP2_INC)/op_mpi_core.h \
					 $(C_OP2_SRC)/mpi/op_mpi_core.c $(C_OP2_SRC)/mpi/op_mpi_cuda_decl.c \
					 $(C_OP2_SRC)/mpi/op_mpi_part_core.c $(C_OP2_SRC)/mpi/op_mpi_cuda_rt_support.c \
					 $(C_OP2_SRC)/mpi/op_mpi_cuda_kernels.cu \
	 				 $(C_OP2_SRC)/mpi/op_mpi_hdf5.c \
					 directories
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_lib_core.c -o $(F_OP2_OBJ)/op_lib_core.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/core/op_rt_support.c -o $(F_OP2_OBJ)/op_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(C_OP2_SRC)/externlib/op_util.c -o $(F_OP2_OBJ)/op_util.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_C_wrappers.c -o $(F_OP2_OBJ)/op2_for_C_wrappers.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_declarations.F90 -o $(F_OP2_OBJ)/op2_for_declarations.o
	$(PGFC) $(PGFFLAGS) -c $(F_OP2_SRC)/backend/op2_for_rt_support.F90 -o $(F_OP2_OBJ)/op2_for_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers.o
	$(NVCC) $(PGCFLAGS) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_for_rt_wrappers_cuda.c -o $(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o
	$(shell cp $(F_OP2_SRC)/backend/op2_for_reference.F90 $(F_OP2_SRC)/backend/op2_for_reference.c)
	$(shell cpp $(F_OP2_SRC)/backend/op2_for_reference.c | sed s/##//g | sed s/\"@\"//g | tr "@" "\\n" > $(F_OP2_SRC)/backend/op2_cpp_reference.F90)
	$(PGFC) -c $(PGFFLAGS) $(F_OP2_SRC)/backend/op2_cpp_reference.F90 -o $(F_OP2_OBJ)/op2_for_reference.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(F_OP2_SRC)/backend/op2_C_reference.c -o $(F_OP2_OBJ)/op2_C_reference.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_core.c -o $(F_OP2_OBJ)/op_mpi_core.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_cuda_decl.c -o $(F_OP2_OBJ)/op_mpi_cuda_decl.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/cuda/op_cuda_rt_support.c -o $(F_OP2_OBJ)/op_cuda_rt_support.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) $(PARMETIS_INC) $(PARMETIS_LIB) -c $(C_OP2_SRC)/mpi/op_mpi_part_core.c -o $(F_OP2_OBJ)/op_mpi_part_core.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_cuda_rt_support.c -o $(F_OP2_OBJ)/op_mpi_cuda_rt_support.o
	$(NVCC) $(NVCCFLAGS) -I$(MPI_INC) -I$(C_OP2_INC) -c $(C_OP2_SRC)/mpi/op_mpi_cuda_kernels.cu -o $(F_OP2_OBJ)/op_mpi_cuda_kernels.o
	$(NVCC) $(PGCFLAGS) -I$(MPI_INC) $(NVCCFLAGS) $(HDF5_INCLUDE) -I$(C_OP2_INC)  -I$(CUDA_INC) -c $(C_OP2_SRC)/mpi/op_mpi_hdf5.c -o $(F_OP2_OBJ)/op_mpi_hdf5.o
	ar -r $(F_OP2_LIB)/libop2_for_mpi_cuda_hydra.a \
				$(F_OP2_OBJ)/op_lib_core.o \
				$(F_OP2_OBJ)/op_rt_support.o \
				$(F_OP2_OBJ)/op_util.o \
				$(F_OP2_OBJ)/op2_for_C_wrappers.o \
				$(F_OP2_OBJ)/op2_for_declarations.o \
				$(F_OP2_OBJ)/op2_for_rt_support.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers_cuda.o \
				$(F_OP2_OBJ)/op2_for_rt_wrappers.o \
				$(F_OP2_OBJ)/op_mpi_core.o \
				$(F_OP2_OBJ)/op_mpi_cuda_decl.o \
				$(F_OP2_OBJ)/op_cuda_rt_support.o \
				$(F_OP2_OBJ)/op_mpi_part_core.o \
				$(F_OP2_OBJ)/op_mpi_cuda_rt_support.o \
				$(F_OP2_OBJ)/op2_C_reference.o \
				$(F_OP2_OBJ)/op2_for_reference.o \
				$(F_OP2_OBJ)/op_mpi_cuda_kernels.o \
				$(F_OP2_OBJ)/op_mpi_hdf5.o

=======
				$(F_OP2_OBJ)/op2_C_reference.o
>>>>>>> 91a1b5bf4b61f120d4f599350756a7f5aee527f6


clean:
	-rm -if $(F_OP2_OBJ)/*.o
	-rm -if $(F_OP2_LIB)/*.a
	-rm -if $(F_OP2_SRC)/*~
	-rm -if $(F_OP2_SRC)/backend/*~
	-rm -if $(F_OP2_SRC)/backend/op2_for_reference.c
	-rm -if $(F_OP2_SRC)/backend/op2_cpp_reference.F90
	-rm -if $(F_OP2_MOD)/*.mod
	-rm -if $(F_OP2)/*~

