IC = pgcc
FC = pgfortran

PREPROCESSOR = -fpp

CPPLINK = -lstdc++
OPT = -Mcuda=4.2 -Minform=inform -O0 -g #-fast
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

all: clean cfunctions airfoil_seq airfoil_cuda airfoil_hdf5_seq airfoil_hdf5_cuda airfoil_hdf5_mpi_cuda

cfunctions: debug.c
	$(IC) $(OPT) $(DEBUG) -c debug.c

airfoil_seq: airfoil_seqfun.F90 airfoil.F90 constants.F95 input.F90
	$(FC) $(OPT) $(FMODS) -c constants.F95 input.F90 airfoil_seqfun.F90 airfoil.F90
	$(FC) $(OPT) $(FLINK) constants.o airfoil.o input.o airfoil_seqfun.o -o airfoil_seq -lop2_for_reference

airfoil_hdf5_seq: airfoil_seqfun.F90 airfoil_hdf5.F90 constants.F95 input.F90
	$(FC) $(OPT) $(FMODS) -c constants.F95 input.F90 airfoil_seqfun.F90 airfoil_hdf5.F90
	$(FC) $(OPT) $(FLINK) constants.o airfoil_hdf5.o input.o airfoil_seqfun.o -o airfoil_hdf5_seq -lop2_for_reference_hdf5 $(HDF5_LIB) $(MPI_LIB) -lz


airfoil_cuda: airfoil_op.F90 save_soln_kernel.CUF adt_calc_kernel.CUF input.F90 \
	res_calc_kernel.CUF bres_calc_kernel.CUF update_kernel.CUF constants.F95 input.F90 cudaConfigurationParams.F95
	$(FC) $(OPT) $(OPENMP) $(FMODS) $(FLINK) -DOP2_WITH_CUDAFOR cudaConfigurationParams.F95 constants.F95 input.F90 \
	save_soln_kernel.CUF update_kernel.CUF bres_calc_kernel.CUF \
	res_calc_kernel.CUF adt_calc_kernel.CUF airfoil_op.F90 -o airfoil_cuda -lop2_for_cuda

airfoil_hdf5_cuda: airfoil_hdf5_op.F90 save_soln_kernel.CUF adt_calc_kernel.CUF input.F90 \
        res_calc_kernel.CUF bres_calc_kernel.CUF update_kernel.CUF constants.F95 input.F90 cudaConfigurationParams.F95
	$(FC) $(OPT) $(OPENMP) $(FMODS) $(FLINK) -DOP2_WITH_CUDAFOR cudaConfigurationParams.F95 constants.F95 input.F90 \
        save_soln_kernel.CUF update_kernel.CUF bres_calc_kernel.CUF \
        res_calc_kernel.CUF adt_calc_kernel.CUF airfoil_hdf5_op.F90 -o airfoil_hdf5_cuda -lop2_for_cuda_hydra $(HDF5_LIB) $(MPI_LIB) -lz

airfoil_hdf5_mpi_cuda: airfoil_hdf5_op.F90 save_soln_kernel.CUF adt_calc_kernel.CUF input.F90 \
        res_calc_kernel.CUF bres_calc_kernel.CUF update_kernel.CUF constants.F95 input.F90 cudaConfigurationParams.F95
	$(FC) $(OPT) $(OPENMP) $(FMODS) $(FLINK) $(CPPLINK) $(PARMETIS_INC) $(PTSCOTCH_INC) -DOP2_WITH_CUDAFOR cudaConfigurationParams.F95 constants.F95 input.F90 \
        save_soln_kernel.CUF update_kernel.CUF bres_calc_kernel.CUF \
        res_calc_kernel.CUF adt_calc_kernel.CUF airfoil_hdf5_op.F90 -o airfoil_hdf5_mpi_cuda -lop2_for_mpi_cuda_hydra $(PARMETIS_LIB) $(PTSCOTCH_LIB) $(HDF5_LIB) $(MPI_LIB) -lz

clean:
	rm -f *.o
	rm -f *.mod
	rm -f $(EXEC)
	rm -f *~
	rm -f airfoil_seq airfoil_cuda airfoil_hdf5_seq airfoil_hdf5_cuda airfoil_hdf5_mpi_cuda
