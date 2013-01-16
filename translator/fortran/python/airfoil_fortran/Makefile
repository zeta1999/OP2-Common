IC = icc
FC = /opt/intel/Compiler/11.1/064/bin/intel64/ifort -O2

#IC = pgcc
#FC = pgfortran

OPENMP = -openmp
#OPENMP = -mp
PREPROCESSOR = -fpp

EXEC = airfoil_openmp_$(PART_SIZE_ENV)

CPPLINK = -lstdc++
OPT = -DOP_PART_SIZE_1=$(PART_SIZE_ENV) -O2

OP2 = $(OP2_INSTALL_PATH)

F_OP2_MOD = $(OP2)/fortran/mod/intel
#F_OP2_MOD = $(OP2)/fortran/mod/pgi
F_OP2_LIB = $(OP2)/fortran/lib

HDF5_LIB = -L$(HDF5_INSTALL_PATH)/lib -lhdf5
MPI_LIB = -L$(MPI_INSTALL_PATH)/lib -lmpi

FLINK = -L$(F_OP2_LIB)
FMODS = -I$(F_OP2_MOD)

all: clean cfunctions compile link

cfunctions: debug.c
	$(IC) $(OPT) $(DEBUG) $(OPENMP) -c debug.c

compile:  airfoil_op.F90 save_soln_kernel.F90 adt_calc_kernel.F90 res_calc_kernel.F90 bres_calc_kernel.F90 update_kernel.F90
	$(FC) $(OPT) $(OPENMP) $(FMODS) -c constants.F90 input.F90 save_soln_kernel.F90 update_kernel.F90 bres_calc_kernel.F90 res_calc_kernel.F90 adt_calc_kernel.F90 airfoil_op.F90

link: debug.o input.o constants.o airfoil_op.o
	$(FC) $(OPT) $(OPENMP) $(FLINK) debug.o input.o constants.o save_soln_kernel.o adt_calc_kernel.o res_calc_kernel.o bres_calc_kernel.o update_kernel.o airfoil_op.o -o $(EXEC) -lop2_for_openmp $(HDF5_LIB) $(MPI_LIB) -lz

clean:
	rm -f *.o
	rm -f *.mod
	rm -f $(EXEC)
	rm -f *~
