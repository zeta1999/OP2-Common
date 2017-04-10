#
# The following environment variables should be predefined:
#
# CUDA_INSTALL_PATH
# PARMETIS_INSTALL_PATH
# PTSCOTCH_INSTALL_PATH
# HDF5_INSTALL_PATH
#
# OP2_INSTALL_PATH
# OP2_COMPILER (gnu,intel,etc)
#

#
# set paths for header files and libraries
#
OP2_INC		= -I$(OP2_INSTALL_PATH)/c/include
OP2_LIB		= -L$(OP2_INSTALL_PATH)/c/lib

CUDA_INC	= -I$(CUDA_INSTALL_PATH)/include
CUDA_LIB	= -L$(CUDA_INSTALL_PATH)/lib64


ifeq ($(OP2_COMPILER),gnu)
  CPP		= g++
  CPPFLAGS	= -O2 -fPIC -DUNIX -Wall -O0 -g -Wextra
  OMPFLAGS	= -fopenmp
  MPICPP	= $(MPI_INSTALL_PATH)/bin/mpiCC
  MPIFLAGS	= $(CCFLAGS)
else
ifeq ($(OP2_COMPILER),intel)
  CPP		= icpc
  CCFLAGS	= -O3 -xAVX -DMPICH_IGNORE_CXX_SEEK -restrict -fno-alias -inline-forceinline -qopt-report=5 -parallel -DVECTORIZE #-parallel #-DCOMM_PERF #-DDEBUG #-vec-report
#  CCFLAGS	= -O3 -xAVX -DMPICH_IGNORE_CXX_SEEK -fno-alias -inline-forceinline -qopt-report -parallel -prec-div -DVECTORIZE #-parallel #-DCOMM_PERF #-DDEBUG #-vec-report
  CPPFLAGS 	= $(CCFLAGS)
  OMPFLAGS	= -qopenmp
  MPICPP	= $(MPI_INSTALL_PATH)/bin/mpiicpc
  NVCCFLAGS	= -ccbin=$(MPICPP)
  MPIFLAGS	= $(CPPFLAGS)
else
ifeq ($(OP2_COMPILER),xl)
  CPP		= xlc++
  CCFLAGS	= -O5 -qarch=pwr8 -qtune=pwr8 -qhot
#  CCFLAGS	= -O3 -xAVX -DMPICH_IGNORE_CXX_SEEK -fno-alias -inline-forceinline -qopt-report -parallel -prec-div -DVECTORIZE #-parallel #-DCOMM_PERF #-DDEBUG #-vec-report
  CPPFLAGS 	= $(CCFLAGS)
  OMPFLAGS	= -qsmp=omp -qthreaded
  MPICPP	= $(MPI_INSTALL_PATH)/bin/mpicxx
  MPIFLAGS	= $(CPPFLAGS)
else
ifeq ($(OP2_COMPILER),pgi)
  CPP       	= pgc++
  CCFLAGS  	= -O3
  CPPFLAGS 	= $(CCFLAGS)
  OMPFLAGS 	= -mp
  MPICC   	= $(MPI_INSTALL_PATH)/bin/mpicc
  MPICPP   	= $(MPI_INSTALL_PATH)/bin/mpicxx
  MPIFLAGS 	= $(CPPFLAGS)
  NVCCFLAGS	= -ccbin=$(MPICPP)
  ACCFLAGS      = -acc -Minfo=acc -ta=tesla:cc35 -DOPENACC
else
ifeq ($(OP2_COMPILER),cray)
  CPP           = CC
  CCFLAGS       = -O3 -h fp3 -h ipa5
  CPPFLAGS      = $(CCFLAGS)
  OMPFLAGS      = -h omp
  MPICPP        = CC
  MPIFLAGS      = $(CPPFLAGS)
else
print:
	@echo "unrecognised value for OP2_COMPILER"
endif
endif
endif
endif
endif



#
# set flags for NVCC compilation and linking
#
ifndef NV_ARCH
  MESSAGE=select an NVIDA device to compile in CUDA, e.g. make NV_ARCH=KEPLER
  NV_ARCH=Kepler
endif
ifeq ($(NV_ARCH),Fermi)
  CODE_GEN_CUDA=-gencode arch=compute_20,code=sm_21
else
ifeq ($(NV_ARCH),Kepler)
  CODE_GEN_CUDA=-gencode arch=compute_35,code=sm_35
endif
endif

NVCCFLAGS       := $(NVCCFLAGS) $(CODE_GEN_CUDA) -m64 -Xptxas -dlcm=ca -Xptxas=-v -use_fast_math -O3 #-g -G -O0

VAR		= #-DOP_PART_SIZE_1=160 -DOP_PART_SIZE_2=320 -DOP_PART_SIZE_3=64 #-DOP_BLOCK_SIZE_0=64 -DOP_BLOCK_SIZE_1=64 -DOP_BLOCK_SIZE_2=64 -DOP_BLOCK_SIZE_3=64 -DOP_BLOCK_SIZE_4=64

#
# partitioning software for MPI versions
#
PARMETIS_VER=4
ifeq ($(PARMETIS_VER),4)
  PARMETIS_INC = -I$(PARMETIS_INSTALL_PATH)/include -DHAVE_PARMETIS -DPARMETIS_VER_4
  PARMETIS_LIB = -L$(PARMETIS_INSTALL_PATH)/lib -lparmetis -lmetis
else
  PARMETIS_INC = -I$(PARMETIS_INSTALL_PATH)/ -DHAVE_PARMETIS
  PARMETIS_LIB = -L$(PARMETIS_INSTALL_PATH)/ -lparmetis -lmetis
endif

PTSCOTCH_INC 	= -I$(PTSCOTCH_INSTALL_PATH)/include -DHAVE_PTSCOTCH
PTSCOTCH_LIB 	= -L$(PTSCOTCH_INSTALL_PATH)/lib/ -lptscotch \
                  -L$(PTSCOTCH_INSTALL_PATH)/lib/ -lptscotcherr

HDF5_INC = -I$(HDF5_INSTALL_PATH)/include
HDF5_LIB = -L$(HDF5_INSTALL_PATH)/lib -lhdf5 -lz


#
# master to make all versions
#
ALL_TARGETS = clean airfoil_seq airfoil_openmp_classical airfoil_mpi airfoil_mpi_openmp_classical
ifeq ($(OP2_COMPILER),pgi)
	ALL_TARGETS += airfoil_openacc airfoil_mpi_openacc
endif
ifeq ($(OP2_COMPILER),intel)
	ALL_TARGETS += #airfoil_mpi_vec
endif

all: $(ALL_TARGETS)

#
# simple sequential version
#

airfoil_seq: airfoil.cpp save_soln.h adt_calc.h res_calc.h bres_calc.h
	     $(MPICPP) $(CPPFLAGS) airfoil.cpp $(OP2_INC) $(HDF5_INC) $(OP2_LIB) -lop2_seq -lop2_hdf5 $(HDF5_LIB) -o airfoil_seq

airfoil_openmp_classical: airfoil.cpp save_soln.h adt_calc.h res_calc.h bres_calc.h
	     $(MPICPP) $(CPPFLAGS) -DOPENMP_CLASSICAL -std=c++11 $(OMPFLAGS) airfoil.cpp $(OP2_INC) $(HDF5_INC) $(OP2_LIB) -lop2_seq -lop2_hdf5 $(HDF5_LIB) -o airfoil_openmp_classical

#
# mpi with sequential-nodes version
#

airfoil_mpi: airfoil.cpp save_soln.h adt_calc.h res_calc.h bres_calc.h Makefile
	$(MPICPP) $(MPIFLAGS) airfoil.cpp $(OP2_INC) $(PARMETIS_INC) $(PTSCOTCH_INC) $(HDF5_INC) \
	$(OP2_LIB) -lop2_mpi $(PARMETIS_LIB) $(PTSCOTCH_LIB) $(HDF5_LIB) -o airfoil_mpi

airfoil_mpi_openmp_classical: airfoil.cpp save_soln.h adt_calc.h res_calc.h bres_calc.h Makefile
	$(MPICPP) $(MPIFLAGS) $(OMPFLAGS) -std=c++11 -DOPENMP_CLASSICAL airfoil.cpp $(OP2_INC) $(PARMETIS_INC) $(HDF5_INC) $(PTSCOTCH_INC) \
	$(OP2_LIB) -lop2_mpi $(PARMETIS_LIB) $(PTSCOTCH_LIB) $(HDF5_LIB) -o airfoil_mpi_openmp_classical


#
# cleanup
#

clean:
		rm -f airfoil_seq airfoil_openmp_classical airfoil_mpi airfoil_mpi_openmp_classical *.o *.optrpt
