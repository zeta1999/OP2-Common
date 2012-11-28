IC = icc
FC = ifort


OPENMP = -openmp
PREPROCESSOR = -fpp

EXEC = airfoil_openmp_$(PART_SIZE_ENV)

CPPLINK = -lstdc++
OPT = -DOP_PART_SIZE_1=$(PART_SIZE_ENV) -O2

F_OP2_MOD = $(OP2)/fortran/mod/intel
F_OP2_LIB = $(OP2)/fortran/lib

HDF5_LIB = -L$(HDF5_INSTALL_PATH)/lib -lhdf5
MPI_LIB = -L$(MPI_INSTALL_PATH)/lib -lmpi

FLINK = -L$(F_OP2_LIB)
FMODS = -module $(F_OP2_MOD)

all: cfunctions airfoil_seq link

cfunctions: debug.c
	$(IC) $(OPT) $(DEBUG) $(OPENMP) -c debug.c

airfoil_seq: OP2Profiling.f90 debug_int.f90 input.f90 constvars.f90 rose_openmp_code.F90 airfoil_seqfun.f90 rose_airfoil.F90
	$(FC) $(OPT) $(DEBUG) $(OPENMP) $(FMODS) -c OP2Profiling.f90 debug_int.f90 input.f90 constvars.f90 rose_openmp_code.F90 airfoil_seqfun.f90 rose_airfoil.F90

link: debug.o OP2Profiling.o debug_int.o input.o constvars.o rose_openmp_code.o airfoil_seqfun.o rose_airfoil.o
	$(FC) $(OPT) $(OPENMP) $(FLINK) OP2Profiling.o debug.o debug_int.o input.o constvars.o rose_openmp_code.o airfoil_seqfun.o rose_airfoil.o -o $(EXEC) -lop2_for_openmp $(HDF5_LIB) $(MPI_LIB) -lz

clean:
	rm -f *.o
	rm -f *.mod
	rm -f $(EXEC)
	rm -f *~
