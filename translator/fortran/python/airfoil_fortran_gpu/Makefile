IC = pgcc
FC = pgfortran


PREPROCESSOR = -fpp

CPPLINK = -lstdc++
OPT = -Mcuda=cuda4.0 -Minform=inform -O2 -fast -Mcuda=cc20
OP2 = $(OP2_INSTALL_PATH)

F_OP2_MOD = $(OP2)/fortran/mod/$(OP2_COMPILER)
F_OP2_LIB = $(OP2)/fortran/lib

HDF5_LIB = -L$(HDF5_INSTALL_PATH)/lib -lhdf5
MPI_LIB = -L$(MPI_INSTALL_PATH)/lib -lmpi
PARMETIS_INC 	= -I$(PARMETIS_INSTALL_PATH) -DHAVE_PARMETIS
PARMETIS_LIB 	= -L$(PARMETIS_INSTALL_PATH) -lparmetis \
		  -L$(PARMETIS_INSTALL_PATH) -lmetis

PTSCOTCH_INC 	= -I$(PTSCOTCH_INSTALL_PATH)/include -DHAVE_PTSCOTCH
PTSCOTCH_LIB 	= -L$(PTSCOTCH_INSTALL_PATH)/lib/ -lptscotch \
                  -L$(PTSCOTCH_INSTALL_PATH)/lib/ -lptscotcherr


FLINK = -L$(F_OP2_LIB)
FMODS = -I$(F_OP2_MOD)

all: clean cfunctions  airfoil_cuda

cfunctions: debug.c
	$(IC) $(OPT) $(DEBUG) -c debug.c

airfoil_cuda: airfoil_op.F90 save_soln_kernel.CUF adt_calc_kernel.CUF \
	res_calc_kernel.CUF bres_calc_kernel.CUF update_kernel.CUF constants.F90 input.F90 cudaConfigurationParams.F95
	$(FC) $(OPT) $(OPENMP) $(FMODS) -c cudaConfigurationParams.F95 constants.F90 input.F90 \
	save_soln_kernel.CUF update_kernel.CUF bres_calc_kernel.CUF \
	res_calc_kernel.CUF adt_calc_kernel.CUF airfoil_op.F90
	$(FC) $(OPT) $(OPENMP) $(FLINK) debug.o cudaConfigurationParams.o constants.o input.o airfoil_seqfun.o \
	save_soln_kernel.o adt_calc_kernel.o res_calc_kernel.o \
	bres_calc_kernel.o update_kernel.o airfoil_op.o -o airfoil_cuda -lop2_for_cuda

clean:
	rm -f *.o
	rm -f *.mod
	rm -f $(EXEC)
	rm -f *~
	rm -f airfoil_cuda
