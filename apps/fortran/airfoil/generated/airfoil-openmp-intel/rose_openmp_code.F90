MODULE GENERATED_MODULE
USE OP2_FORTRAN_DECLARATIONS
USE OP2_FORTRAN_RT_SUPPORT
USE ISO_C_BINDING
#ifdef _OPENMP
  USE OMP_LIB
#endif

REAL(kind=8) :: alpha_OP2_CONSTANT
REAL(kind=8) :: cfl_OP2_CONSTANT
REAL(kind=8) :: eps_OP2_CONSTANT
REAL(kind=8) :: gam_OP2_CONSTANT
REAL(kind=8) :: gm1_OP2_CONSTANT
REAL(kind=8) :: mach_OP2_CONSTANT
REAL(kind=8), DIMENSION(4) :: qinf_OP2_CONSTANT

REAL(kind=4) :: loopTimeHostadt_calc_1379395014
REAL(kind=4) :: loopTimeKerneladt_calc_1379395014
INTEGER(kind=4) :: numberCalledadt_calc_1379395014

LOGICAL :: firstTime_adt_calc = .TRUE.
TYPE ( c_ptr )  :: planRet_adt_calc
TYPE ( op_plan ) , POINTER :: actualPlan_adt_calc
TYPE ( c_ptr ) , POINTER, DIMENSION(:) :: ind_maps_adt_calc
TYPE ( c_ptr ) , POINTER, DIMENSION(:) :: mappingArray_adt_calc

INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_maps1_adt_calc

INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray1_adt_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray2_adt_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray3_adt_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray4_adt_calc

INTEGER(kind=4) :: mappingArray1Size_adt_calc
INTEGER(kind=4) :: mappingArray2Size_adt_calc
INTEGER(kind=4) :: mappingArray3Size_adt_calc
INTEGER(kind=4) :: mappingArray4Size_adt_calc

INTEGER(kind=4), POINTER, DIMENSION(:) :: blkmap_adt_calc
INTEGER(kind=4) :: blkmapSize_adt_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_offs_adt_calc
INTEGER(kind=4) :: ind_offsSize_adt_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_sizes_adt_calc
INTEGER(kind=4) :: ind_sizesSize_adt_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: nelems_adt_calc
INTEGER(kind=4) :: nelemsSize_adt_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: nthrcol_adt_calc
INTEGER(kind=4) :: nthrcolSize_adt_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: offset_adt_calc
INTEGER(kind=4) :: offsetSize_adt_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: thrcol_adt_calc
INTEGER(kind=4) :: thrcolSize_adt_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ncolblk_adt_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: pnindirect_adt_calc


REAL(kind=4) :: loopTimeHostbres_calc_3019043301
REAL(kind=4) :: loopTimeKernelbres_calc_3019043301
INTEGER(kind=4) :: numberCalledbres_calc_3019043301
LOGICAL :: firstTime_bres_calc = .TRUE.
TYPE ( c_ptr )  :: planRet_bres_calc
TYPE ( op_plan ) , POINTER :: actualPlan_bres_calc
TYPE ( c_ptr ) , POINTER, DIMENSION(:) :: ind_maps_bres_calc
TYPE ( c_ptr ) , POINTER, DIMENSION(:) :: mappingArray_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_maps1_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_maps3_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_maps4_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_maps5_bres_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray1_bres_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray2_bres_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray3_bres_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray4_bres_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray5_bres_calc
INTEGER(kind=4) :: mappingArray1Size_bres_calc
INTEGER(kind=4) :: mappingArray2Size_bres_calc
INTEGER(kind=4) :: mappingArray3Size_bres_calc
INTEGER(kind=4) :: mappingArray4Size_bres_calc
INTEGER(kind=4) :: mappingArray5Size_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: blkmap_bres_calc
INTEGER(kind=4) :: blkmapSize_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_offs_bres_calc
INTEGER(kind=4) :: ind_offsSize_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_sizes_bres_calc
INTEGER(kind=4) :: ind_sizesSize_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: nelems_bres_calc
INTEGER(kind=4) :: nelemsSize_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: nthrcol_bres_calc
INTEGER(kind=4) :: nthrcolSize_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: offset_bres_calc
INTEGER(kind=4) :: offsetSize_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: thrcol_bres_calc
INTEGER(kind=4) :: thrcolSize_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ncolblk_bres_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: pnindirect_bres_calc


REAL(kind=4) :: loopTimeHostres_calc_3884922247
REAL(kind=4) :: loopTimeKernelres_calc_3884922247
INTEGER(kind=4) :: numberCalledres_calc_3884922247
LOGICAL :: firstTime_res_calc = .TRUE.
TYPE ( c_ptr )  :: planRet_res_calc
TYPE ( op_plan ) , POINTER :: actualPlan_res_calc
TYPE ( c_ptr ) , POINTER, DIMENSION(:) :: ind_maps_res_calc
TYPE ( c_ptr ) , POINTER, DIMENSION(:) :: mappingArray_res_calc

INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_maps1_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_maps3_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_maps5_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_maps7_res_calc

INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray1_res_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray2_res_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray3_res_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray4_res_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray5_res_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray6_res_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray7_res_calc
INTEGER(kind=2), POINTER, DIMENSION(:) :: mappingArray8_res_calc

INTEGER(kind=4) :: mappingArray1Size_res_calc
INTEGER(kind=4) :: mappingArray2Size_res_calc
INTEGER(kind=4) :: mappingArray3Size_res_calc
INTEGER(kind=4) :: mappingArray4Size_res_calc
INTEGER(kind=4) :: mappingArray5Size_res_calc
INTEGER(kind=4) :: mappingArray6Size_res_calc
INTEGER(kind=4) :: mappingArray7Size_res_calc
INTEGER(kind=4) :: mappingArray8Size_res_calc

INTEGER(kind=4), POINTER, DIMENSION(:) :: blkmap_res_calc
INTEGER(kind=4) :: blkmapSize_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_offs_res_calc
INTEGER(kind=4) :: ind_offsSize_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ind_sizes_res_calc
INTEGER(kind=4) :: ind_sizesSize_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: nelems_res_calc
INTEGER(kind=4) :: nelemsSize_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: nthrcol_res_calc
INTEGER(kind=4) :: nthrcolSize_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: offset_res_calc
INTEGER(kind=4) :: offsetSize_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: thrcol_res_calc
INTEGER(kind=4) :: thrcolSize_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: ncolblk_res_calc
INTEGER(kind=4), POINTER, DIMENSION(:) :: pnindirect_res_calc

REAL(kind=4) :: loopTimeHostsave_soln_qdim_2320554256
REAL(kind=4) :: loopTimeKernelsave_soln_qdim_2320554256
INTEGER(kind=4) :: numberCalledsave_soln_qdim_2320554256

REAL(kind=4) :: loopTimeHostupdate_2692987784
REAL(kind=4) :: loopTimeKernelupdate_2692987784
INTEGER(kind=4) :: numberCalledupdate_2692987784
CONTAINS

SUBROUTINE printProfInfo()
print *, 'loopTimeHostadt_calc_1379395014 = ', loopTimeHostadt_calc_1379395014
print *, 'loopTimeKerneladt_calc_1379395014 = ', loopTimeKerneladt_calc_1379395014
print *, 'loopTimeHostadt_calc_1379395014 Average = ', loopTimeHostadt_calc_1379395014/numberCalledadt_calc_1379395014
print *, 'loopTimeKerneladt_calc_1379395014 Average = ', loopTimeKerneladt_calc_1379395014/numberCalledadt_calc_1379395014

print *, 'loopTimeHostbres_calc_3019043301 = ', loopTimeHostbres_calc_3019043301
print *, 'loopTimeKernelbres_calc_3019043301 = ', loopTimeKernelbres_calc_3019043301
print *, 'loopTimeHostbres_calc_3019043301 Average = ', loopTimeHostbres_calc_3019043301/numberCalledbres_calc_3019043301
print *, 'loopTimeKernelbres_calc_3019043301 Average = ', loopTimeKernelbres_calc_3019043301/numberCalledbres_calc_3019043301

print *, 'loopTimeHostres_calc_3884922247 = ', loopTimeHostres_calc_3884922247
print *, 'loopTimeKernelres_calc_3884922247 = ', loopTimeKernelres_calc_3884922247
print *, 'loopTimeHostres_calc_3884922247 Average = ', loopTimeHostres_calc_3884922247/numberCalledres_calc_3884922247
print *, 'loopTimeKernelres_calc_3884922247 Average = ', loopTimeKernelres_calc_3884922247/numberCalledres_calc_3884922247

print *, 'loopTimeHostsave_soln_qdim_2320554256 = ', loopTimeHostsave_soln_qdim_2320554256
print *, 'loopTimeKernelsave_soln_qdim_2320554256 = ', loopTimeKernelsave_soln_qdim_2320554256
print *, 'loopTimeHostsave_soln_qdim_2320554256 Average = ', loopTimeHostsave_soln_qdim_2320554256/numberCalledsave_soln_qdim_2320554256
print *, 'loopTimeKernelsave_soln_qdim_2320554256 Average = ', loopTimeKernelsave_soln_qdim_2320554256/numberCalledsave_soln_qdim_2320554256

print *, 'loopTimeHostupdate_2692987784 = ', loopTimeHostupdate_2692987784
print *, 'loopTimeKernelupdate_2692987784 = ', loopTimeKernelupdate_2692987784
print *, 'loopTimeHostupdate_2692987784 Average = ', loopTimeHostupdate_2692987784/numberCalledupdate_2692987784
print *, 'loopTimeKernelupdate_2692987784 Average = ', loopTimeKernelupdate_2692987784/numberCalledupdate_2692987784
END SUBROUTINE

SUBROUTINE initOP2Constants(alpha,cfl,eps,gam,gm1,mach,qinf)
IMPLICIT NONE
  REAL(kind=8) :: alpha
  REAL(kind=8) :: cfl
  REAL(kind=8) :: eps
  REAL(kind=8) :: gam
  REAL(kind=8) :: gm1
  REAL(kind=8) :: mach
  REAL(kind=8), DIMENSION(4) :: qinf
  INTEGER(kind=4) :: i1
  alpha_OP2_CONSTANT = alpha
  cfl_OP2_CONSTANT = cfl
  eps_OP2_CONSTANT = eps
  gam_OP2_CONSTANT = gam
  gm1_OP2_CONSTANT = gm1
  mach_OP2_CONSTANT = mach
  qinf_OP2_CONSTANT = qinf
END SUBROUTINE

SUBROUTINE adt_calc_modified(x1,x2,x3,x4,q,adt)
IMPLICIT NONE
  REAL(kind=8), DIMENSION(*) :: x1
  REAL(kind=8), DIMENSION(*) :: x2
  REAL(kind=8), DIMENSION(*) :: x3
  REAL(kind=8), DIMENSION(*) :: x4
  REAL(kind=8), DIMENSION(*) :: q
  REAL(kind=8) :: adt
  REAL(kind=8) :: dx,dy,ri,u,v,c

  ri = 1.0 / q(1)
  u = ri * q(2)
  v = ri * q(3)
  c = sqrt(gam_OP2_CONSTANT * gm1_OP2_CONSTANT * (ri * q(4) - 0.5 * (u * u + v * v)))
  dx = x2(1) - x1(1)
  dy = x2(2) - x1(2)
  adt = abs(u * dy - v * dx) + c * sqrt(dx * dx + dy * dy)
  dx = x3(1) - x2(1)
  dy = x3(2) - x2(2)
  adt = adt + abs(u * dy - v * dx) + c * sqrt(dx * dx + dy * dy)
  dx = x4(1) - x3(1)
  dy = x4(2) - x3(2)
  adt = adt + abs(u * dy - v * dx) + c * sqrt(dx * dx + dy * dy)
  dx = x1(1) - x4(1)
  dy = x1(2) - x4(2)
  adt = adt + abs(u * dy - v * dx) + c * sqrt(dx * dx + dy * dy)
  adt = adt / cfl_OP2_CONSTANT
END SUBROUTINE

SUBROUTINE adt_calc_kernel(  &
  &   opDat1,   &
  &   opDat5,   &
  &   opDat6,   &
  &   ind_maps1,&
  &   mappingArray1,  &
  &   mappingArray2,  &
  &   mappingArray3,  &
  &   mappingArray4,  &
  &   ind_sizes,  &
  &   ind_offs, &
  &   blkmap, &
  &   offset, &
  &   nelems, &
  &   nthrcol,  &
  &   thrcol, &
  &   blockOffset,  &
  &   blockID)

  IMPLICIT NONE
  REAL(kind=8), DIMENSION(0:*) :: opDat1
  REAL(kind=8), DIMENSION(0:*) :: opDat5
  REAL(kind=8), DIMENSION(0:*) :: opDat6

  INTEGER(kind=4), DIMENSION(0:), TARGET :: ind_maps1

  INTEGER(kind=2), DIMENSION(0:*) :: mappingArray1
  INTEGER(kind=2), DIMENSION(0:*) :: mappingArray2
  INTEGER(kind=2), DIMENSION(0:*) :: mappingArray3
  INTEGER(kind=2), DIMENSION(0:*) :: mappingArray4

  INTEGER(kind=4), DIMENSION(0:*) :: ind_sizes
  INTEGER(kind=4), DIMENSION(0:*) :: ind_offs
  INTEGER(kind=4), DIMENSION(0:*) :: blkmap
  INTEGER(kind=4), DIMENSION(0:*) :: offset
  INTEGER(kind=4), DIMENSION(0:*) :: nelems
  INTEGER(kind=4), DIMENSION(0:*) :: nthrcol
  INTEGER(kind=4), DIMENSION(0:*) :: thrcol
  INTEGER(kind=4) :: blockOffset
  INTEGER(kind=4) :: blockID
  INTEGER(kind=4) :: threadBlockOffset
  INTEGER(kind=4) :: threadBlockID
  INTEGER(kind=4) :: numberOfActiveThreads
  INTEGER(kind=4) :: i1
  INTEGER(kind=4) :: i2
  REAL(kind=8), DIMENSION(0:128000 - 1), TARGET :: sharedFloat8

  INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat1IndirectionMap
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat1SharedIndirection
  INTEGER(kind=4) :: opDat1nBytes
  INTEGER(kind=4) :: opDat1RoundUp
  INTEGER(kind=4) :: opDat1SharedIndirectionSize

  threadBlockID = blkmap(blockID + blockOffset)
  numberOfActiveThreads = nelems(threadBlockID)
  threadBlockOffset = offset(threadBlockID)

  opDat1SharedIndirectionSize = ind_sizes(0 + threadBlockID * 1)
  opDat1IndirectionMap => ind_maps1(ind_offs(0 + threadBlockID * 1):)
  opDat1nBytes = 0
  opDat1SharedIndirection => sharedFloat8(opDat1nBytes:)

  DO i1 = 0, opDat1SharedIndirectionSize - 1, 1
     DO i2 = 0, 2 - 1, 1
       opDat1SharedIndirection(i2 + i1 * 2 + 1) = opDat1(i2 + opDat1IndirectionMap(i1 + 1) * 2)
     END DO
  END DO

  DO i1 = 0, numberOfActiveThreads - 1, 1
    CALL adt_calc_modified ( &
       &  opDat1SharedIndirection(1 + mappingArray1(i1 + threadBlockOffset) * 2:1 + mappingArray1(i1 + threadBlockOffset) * 2 + 2 - 1), &
       &  opDat1SharedIndirection(1 + mappingArray2(i1 + threadBlockOffset) * 2:1 + mappingArray2(i1 + threadBlockOffset) * 2 + 2 - 1), &
       &  opDat1SharedIndirection(1 + mappingArray3(i1 + threadBlockOffset) * 2:1 + mappingArray3(i1 + threadBlockOffset) * 2 + 2 - 1), &
       &  opDat1SharedIndirection(1 + mappingArray4(i1 + threadBlockOffset) * 2:1 + mappingArray4(i1 + threadBlockOffset) * 2 + 2 - 1), &
       &  opDat5((i1 + threadBlockOffset) * 4:(i1 + threadBlockOffset) * 4 + 4 - 1), &
       &  opDat6((i1 + threadBlockOffset) * 1))
  END DO

END SUBROUTINE

SUBROUTINE adt_calc_host( userSubroutine, set, &
  & opArg1, &
  & opArg2, &
  & opArg3, &
  & opArg4, &
  & opArg5, &
  & opArg6)
  IMPLICIT NONE
  character(len=9), INTENT(IN) :: userSubroutine
  TYPE ( op_set ) , INTENT(IN) :: set

  TYPE ( op_arg ) , INTENT(IN) :: opArg1
  TYPE ( op_arg ) , INTENT(IN) :: opArg2
  TYPE ( op_arg ) , INTENT(IN) :: opArg3
  TYPE ( op_arg ) , INTENT(IN) :: opArg4
  TYPE ( op_arg ) , INTENT(IN) :: opArg5
  TYPE ( op_arg ) , INTENT(IN) :: opArg6

  TYPE ( op_arg ) , DIMENSION(6) :: opArgArray
  INTEGER(kind=4) :: numberOfOpDats
  INTEGER(kind=4) :: returnMPIHaloExchange
  INTEGER(kind=4) :: returnSetKernelTiming
  TYPE ( op_set_core ) , POINTER :: opSetCore

  TYPE ( op_set_core ) , POINTER :: opSet1Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat1Local
  INTEGER(kind=4) :: opDat1Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet5Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat5Local
  INTEGER(kind=4) :: opDat5Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet6Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat6Local
  INTEGER(kind=4) :: opDat6Cardinality

  TYPE ( op_map_core ) , POINTER :: opMap1Core
  TYPE ( op_map_core ) , POINTER :: opMap2Core
  TYPE ( op_map_core ) , POINTER :: opMap3Core
  TYPE ( op_map_core ) , POINTER :: opMap4Core
  TYPE ( op_map_core ) , POINTER :: opMap5Core
  TYPE ( op_map_core ) , POINTER :: opMap6Core

  TYPE ( op_dat_core ) , POINTER :: opDat1Core
  TYPE ( op_dat_core ) , POINTER :: opDat2Core
  TYPE ( op_dat_core ) , POINTER :: opDat3Core
  TYPE ( op_dat_core ) , POINTER :: opDat4Core
  TYPE ( op_dat_core ) , POINTER :: opDat5Core
  TYPE ( op_dat_core ) , POINTER :: opDat6Core

  INTEGER(kind=4) :: threadID
  INTEGER(kind=4) :: numberOfThreads
  INTEGER(kind=4) :: partitionSize
  INTEGER(kind=4), DIMENSION(1:6) :: opDatArray
  INTEGER(kind=4), DIMENSION(1:6) :: mappingIndicesArray
  INTEGER(kind=4), DIMENSION(1:6) :: mappingArray
  INTEGER(kind=4), DIMENSION(1:6) :: accessDescriptorArray
  INTEGER(kind=4), DIMENSION(1:6) :: indirectionDescriptorArray
  INTEGER(kind=4), DIMENSION(1:6) :: opDatTypesArray
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayStart
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayEnd
  INTEGER(kind=4) :: numberOfIndirectOpDats
  INTEGER(kind=4) :: blockOffset
  INTEGER(kind=4) :: nblocks
  REAL(kind=8) :: startTimeHost
  REAL(kind=8) :: endTimeHost
  REAL(kind=8) :: startTimeKernel
  REAL(kind=8) :: endTimeKernel
  REAL(kind=8) :: accumulatorHostTime
  REAL(kind=8) :: accumulatorKernelTime
  INTEGER(kind=4) :: i1
  INTEGER(kind=4) :: i2

  IF (set%setPtr%size .EQ. 0) THEN
    RETURN
  END IF

  numberCalledadt_calc_1379395014 = numberCalledadt_calc_1379395014 + 1

  call date_and_time(values=timeArrayStart)
  startTimeHost = 1.00000 * timeArrayStart(8) + &
  & 1000.00 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

#ifdef OP_PART_SIZE_1
  partitionSize = OP_PART_SIZE_1
#else
  partitionSize = 0
#endif
#ifdef _OPENMP
  numberOfThreads = omp_get_max_threads()
#else
  numberOfThreads = 1
#endif

  numberOfOpDats = 6

  opArgArray(1) = opArg1
  opArgArray(2) = opArg2
  opArgArray(3) = opArg3
  opArgArray(4) = opArg4
  opArgArray(5) = opArg5
  opArgArray(6) = opArg6

  indirectionDescriptorArray(1) = 0
  indirectionDescriptorArray(2) = 0
  indirectionDescriptorArray(3) = 0
  indirectionDescriptorArray(4) = 0
  indirectionDescriptorArray(5) = -1
  indirectionDescriptorArray(6) = -1

  numberOfIndirectOpDats = 1

  planRet_adt_calc = FortranPlanCaller( &
  & userSubroutine, &
  & set%setCPtr, &
  & partitionSize, &
  & numberOfOpDats, &
  & opArgArray, &
  & numberOfIndirectOpDats, &
  & indirectionDescriptorArray)

  CALL c_f_pointer(planRet_adt_calc,actualPlan_adt_calc)
  CALL c_f_pointer(actualPlan_adt_calc%nindirect,pnindirect_adt_calc,(/numberOfIndirectOpDats/))
  CALL c_f_pointer(actualPlan_adt_calc%ind_maps,ind_maps_adt_calc,(/numberOfIndirectOpDats/))
  CALL c_f_pointer(actualPlan_adt_calc%maps,mappingArray_adt_calc,(/numberOfOpDats/))
  CALL c_f_pointer(actualPlan_adt_calc%ncolblk,ncolblk_adt_calc,(/set%setPtr%size/))
  CALL c_f_pointer(actualPlan_adt_calc%ind_sizes,ind_sizes_adt_calc,(/actualPlan_adt_calc%nblocks * numberOfIndirectOpDats/))
  CALL c_f_pointer(actualPlan_adt_calc%ind_offs,ind_offs_adt_calc,(/actualPlan_adt_calc%nblocks * numberOfIndirectOpDats/))
  CALL c_f_pointer(actualPlan_adt_calc%blkmap,blkmap_adt_calc,(/actualPlan_adt_calc%nblocks/))
  CALL c_f_pointer(actualPlan_adt_calc%offset,offset_adt_calc,(/actualPlan_adt_calc%nblocks/))
  CALL c_f_pointer(actualPlan_adt_calc%nelems,nelems_adt_calc,(/actualPlan_adt_calc%nblocks/))
  CALL c_f_pointer(actualPlan_adt_calc%nthrcol,nthrcol_adt_calc,(/actualPlan_adt_calc%nblocks/))
  CALL c_f_pointer(actualPlan_adt_calc%thrcol,thrcol_adt_calc,(/set%setPtr%size/))

  CALL c_f_pointer(ind_maps_adt_calc(1),ind_maps1_adt_calc,(/pnindirect_adt_calc(1)/))

  IF (indirectionDescriptorArray(1) >= 0) THEN
    CALL c_f_pointer(mappingArray_adt_calc(1),mappingArray1_adt_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(2) >= 0) THEN
    CALL c_f_pointer(mappingArray_adt_calc(2),mappingArray2_adt_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(3) >= 0) THEN
    CALL c_f_pointer(mappingArray_adt_calc(3),mappingArray3_adt_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(4) >= 0) THEN
    CALL c_f_pointer(mappingArray_adt_calc(4),mappingArray4_adt_calc,(/set%setPtr%size/))
  END IF

  opSetCore => set%setPtr
  opDat1Cardinality = opArg1%dim * getSetSizeFromOpArg(opArg1)
  opDat5Cardinality = opArg5%dim * getSetSizeFromOpArg(opArg5)
  opDat6Cardinality = opArg6%dim * getSetSizeFromOpArg(opArg6)

  CALL c_f_pointer(opArg1%data,opDat1Local,(/opDat1Cardinality/))
  CALL c_f_pointer(opArg5%data,opDat5Local,(/opDat5Cardinality/))
  CALL c_f_pointer(opArg6%data,opDat6Local,(/opDat6Cardinality/))

  call date_and_time(values=timeArrayEnd)
  endTimeHost = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7) + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorHostTime = endTimeHost - startTimeHost
  loopTimeHostadt_calc_1379395014 = loopTimeHostadt_calc_1379395014 + accumulatorHostTime

  call date_and_time(values=timeArrayStart)
  startTimeKernel = 1.00000 * timeArrayStart(8) + &
  & 1000 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

  blockOffset = 0

  DO i1 = 0, actualPlan_adt_calc%ncolors - 1, 1
    nblocks = ncolblk_adt_calc(i1 + 1)

    !$OMP PARALLEL DO private (threadID)
    DO i2 = 0, nblocks - 1, 1
      threadID = omp_get_thread_num()
      CALL adt_calc_kernel( &
           & opDat1Local, &
           & opDat5Local, &
           & opDat6Local, &
           & ind_maps1_adt_calc, &
           & mappingArray1_adt_calc, &
           & mappingArray2_adt_calc, &
           & mappingArray3_adt_calc, &
           & mappingArray4_adt_calc, &
           & ind_sizes_adt_calc, &
           & ind_offs_adt_calc, &
           & blkmap_adt_calc, &
           & offset_adt_calc, &
           & nelems_adt_calc, &
           & nthrcol_adt_calc, &
           & thrcol_adt_calc, &
           & blockOffset,i2)
      END DO
      !$OMP END PARALLEL DO
      blockOffset = blockOffset + nblocks
    END DO

    call date_and_time(values=timeArrayEnd)
    endTimeKernel = 1.00000 * timeArrayEnd(8) + &
    & 1000 * timeArrayEnd(7) + &
    & 60000 * timeArrayEnd(6) + &
    & 3600000 * timeArrayEnd(5)

    accumulatorKernelTime = endTimeKernel - startTimeKernel
    loopTimeKerneladt_calc_1379395014 = loopTimeKerneladt_calc_1379395014 + accumulatorKernelTime

    call date_and_time(values=timeArrayStart)
    startTimeHost = 1.00000 * timeArrayStart(8) + 1000.00 * timeArrayStart(7) + 60000 * timeArrayStart(6) + 3600000 * timeArrayStart(5)

    call date_and_time(values=timeArrayEnd)
    endTimeHost = 1.00000 * timeArrayEnd(8) + &
    & 1000 * timeArrayEnd(7) + &
    & 60000 * timeArrayEnd(6) + &
    & 3600000 * timeArrayEnd(5)

    accumulatorHostTime = endTimeHost - startTimeHost
    loopTimeHostadt_calc_1379395014 = loopTimeHostadt_calc_1379395014 + accumulatorHostTime
    returnSetKernelTiming = setKernelTime(0, userSubroutine, &
    & accumulatorKernelTime / 1000.00,actualPlan_adt_calc%transfer,actualPlan_adt_calc%transfer2)
END SUBROUTINE




SUBROUTINE bres_calc_modified(x1,x2,q1,adt1,res1,bound)
  IMPLICIT NONE
  REAL(kind=8), DIMENSION(2) :: x1
  REAL(kind=8), DIMENSION(2) :: x2
  REAL(kind=8), DIMENSION(4) :: q1
  REAL(kind=8) :: adt1
  REAL(kind=8), DIMENSION(4) :: res1
  INTEGER(kind=4), DIMENSION(1) :: bound
  REAL(kind=8) :: dx,dy,mu,ri,p1,vol1,p2,vol2,f

  dx = x1(1) - x2(1)
  dy = x1(2) - x2(2)
  ri = 1.0 / q1(1)
  p1 = gm1_OP2_CONSTANT * (q1(4) - 0.5 * ri * (q1(2) * q1(2) + q1(3) * q1(3)))

  IF (bound(1) .EQ. 1) THEN
    res1(2) = res1(2) + p1 * dy
    res1(3) = res1(3) + -(p1 * dx)
  ELSE
    vol1 = ri * (q1(2) * dy - q1(3) * dx)
    ri = 1.0 / qinf_OP2_CONSTANT(1)
    p2 = gm1_OP2_CONSTANT * (qinf_OP2_CONSTANT(4) - 0.5 * ri * (qinf_OP2_CONSTANT(2) * qinf_OP2_CONSTANT(2) + qinf_OP2_CONSTANT(3) * qinf_OP2_CONSTANT(3)))
    vol2 = ri * (qinf_OP2_CONSTANT(2) * dy - qinf_OP2_CONSTANT(3) * dx)
    mu = adt1 * eps_OP2_CONSTANT
    f = 0.5 * (vol1 * q1(1) + vol2 * qinf_OP2_CONSTANT(1)) + mu * (q1(1) - qinf_OP2_CONSTANT(1))
    res1(1) = res1(1) + f
    f = 0.5 * (vol1 * q1(2) + p1 * dy + vol2 * qinf_OP2_CONSTANT(2) + p2 * dy) + mu * (q1(2) - qinf_OP2_CONSTANT(2))
    res1(2) = res1(2) + f
    f = 0.5 * (vol1 * q1(3) - p1 * dx + vol2 * qinf_OP2_CONSTANT(3) - p2 * dx) + mu * (q1(3) - qinf_OP2_CONSTANT(3))
    res1(3) = res1(3) + f
    f = 0.5 * (vol1 * (q1(4) + p1) + vol2 * (qinf_OP2_CONSTANT(4) + p2)) + mu * (q1(4) - qinf_OP2_CONSTANT(4))
    res1(4) = res1(4) + f
  END IF
END SUBROUTINE

SUBROUTINE bres_calc_kernel( &
  & opDat1, &
  & opDat3, &
  & opDat4, &
  & opDat5, &
  & opDat6, &
  & ind_maps1, &
  & ind_maps3, &
  & ind_maps4, &
  & ind_maps5, &
  & mappingArray1, &
  & mappingArray2, &
  & mappingArray3, &
  & mappingArray4, &
  & mappingArray5, &
  & ind_sizes, &
  & ind_offs, &
  & blkmap, &
  & offset, &
  & nelems, &
  & nthrcol, &
  & thrcol, &
  & blockOffset, &
  & blockID)

  IMPLICIT NONE

  REAL(kind=8), DIMENSION(0:*) :: opDat1
  REAL(kind=8), DIMENSION(0:*) :: opDat3
  REAL(kind=8), DIMENSION(0:*) :: opDat4
  REAL(kind=8), DIMENSION(0:*) :: opDat5
  INTEGER(kind=4), DIMENSION(0:*) :: opDat6
  INTEGER(kind=4), DIMENSION(0:), TARGET :: ind_maps1
  INTEGER(kind=4), DIMENSION(0:), TARGET :: ind_maps3
  INTEGER(kind=4), DIMENSION(0:), TARGET :: ind_maps4
  INTEGER(kind=4), DIMENSION(0:), TARGET :: ind_maps5
  INTEGER(kind=2), DIMENSION(0:*) :: mappingArray1
  INTEGER(kind=2), DIMENSION(0:*) :: mappingArray2
  INTEGER(kind=2), DIMENSION(0:*) :: mappingArray3
  INTEGER(kind=2), DIMENSION(0:*) :: mappingArray4
  INTEGER(kind=2), DIMENSION(0:*) :: mappingArray5
  INTEGER(kind=4), DIMENSION(0:*) :: ind_sizes
  INTEGER(kind=4), DIMENSION(0:*) :: ind_offs
  INTEGER(kind=4), DIMENSION(0:*) :: blkmap
  INTEGER(kind=4), DIMENSION(0:*) :: offset
  INTEGER(kind=4), DIMENSION(0:*) :: nelems
  INTEGER(kind=4), DIMENSION(0:*) :: nthrcol
  INTEGER(kind=4), DIMENSION(0:*) :: thrcol
  INTEGER(kind=4) :: blockOffset
  INTEGER(kind=4) :: blockID
  INTEGER(kind=4) :: threadBlockOffset
  INTEGER(kind=4) :: threadBlockID
  INTEGER(kind=4) :: numberOfActiveThreads
  INTEGER(kind=4) :: i1
  INTEGER(kind=4) :: i2
  INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat1IndirectionMap
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat1SharedIndirection
  REAL(kind=8), DIMENSION(0:128000 - 1), TARGET :: sharedFloat8
  INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat3IndirectionMap
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat3SharedIndirection
  INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat4IndirectionMap
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat4SharedIndirection
  INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat5IndirectionMap
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat5SharedIndirection
  INTEGER(kind=4) :: opDat1nBytes
  INTEGER(kind=4) :: opDat3nBytes
  INTEGER(kind=4) :: opDat4nBytes
  INTEGER(kind=4) :: opDat5nBytes
  INTEGER(kind=4) :: opDat1RoundUp
  INTEGER(kind=4) :: opDat3RoundUp
  INTEGER(kind=4) :: opDat4RoundUp
  INTEGER(kind=4) :: opDat5RoundUp
  INTEGER(kind=4) :: opDat1SharedIndirectionSize
  INTEGER(kind=4) :: opDat3SharedIndirectionSize
  INTEGER(kind=4) :: opDat4SharedIndirectionSize
  INTEGER(kind=4) :: opDat5SharedIndirectionSize
  REAL(kind=8), DIMENSION(0:3) :: opDat5Local
  INTEGER(kind=4) :: opDat5Map
  INTEGER(kind=4) :: numOfColours
  INTEGER(kind=4) :: numberOfActiveThreadsCeiling
  INTEGER(kind=4) :: colour1
  INTEGER(kind=4) :: colour2

  threadBlockID = blkmap(blockID + blockOffset)
  numberOfActiveThreads = nelems(threadBlockID)
  threadBlockOffset = offset(threadBlockID)
  numberOfActiveThreadsCeiling = numberOfActiveThreads
  numOfColours = nthrcol(threadBlockID)

  opDat1SharedIndirectionSize = ind_sizes(0 + threadBlockID * 4)
  opDat3SharedIndirectionSize = ind_sizes(1 + threadBlockID * 4)
  opDat4SharedIndirectionSize = ind_sizes(2 + threadBlockID * 4)
  opDat5SharedIndirectionSize = ind_sizes(3 + threadBlockID * 4)
  opDat1IndirectionMap => ind_maps1(ind_offs(0 + threadBlockID * 4):)
  opDat3IndirectionMap => ind_maps3(ind_offs(1 + threadBlockID * 4):)
  opDat4IndirectionMap => ind_maps4(ind_offs(2 + threadBlockID * 4):)
  opDat5IndirectionMap => ind_maps5(ind_offs(3 + threadBlockID * 4):)
  opDat3RoundUp = opDat1SharedIndirectionSize * 2
  opDat4RoundUp = opDat3SharedIndirectionSize * 4
  opDat5RoundUp = opDat4SharedIndirectionSize * 1

  opDat1nBytes = 0
  opDat3nBytes = opDat1nBytes + opDat3RoundUp
  opDat4nBytes = opDat3nBytes + opDat4RoundUp
  opDat5nBytes = opDat4nBytes + opDat5RoundUp
  opDat1SharedIndirection => sharedFloat8(opDat1nBytes:)
  opDat3SharedIndirection => sharedFloat8(opDat3nBytes:)
  opDat4SharedIndirection => sharedFloat8(opDat4nBytes:)
  opDat5SharedIndirection => sharedFloat8(opDat5nBytes:)

  DO i1 = 0, opDat1SharedIndirectionSize - 1, 1
    DO i2 = 0, 2 - 1, 1
      opDat1SharedIndirection(i2 + i1 * 2 + 1) = opDat1(i2 + opDat1IndirectionMap(i1 + 1) * 2)
    END DO
  END DO

  DO i1 = 0, opDat3SharedIndirectionSize - 1, 1
    DO i2 = 0, 4 - 1, 1
      opDat3SharedIndirection(i2 + i1 * 4 + 1) = opDat3(i2 + opDat3IndirectionMap(i1 + 1) * 4)
    END DO
  END DO

  DO i1 = 0, opDat4SharedIndirectionSize - 1, 1
    DO i2 = 0, 1 - 1, 1
      opDat4SharedIndirection(i2 + i1 * 1 + 1) = opDat4(i2 + opDat4IndirectionMap(i1 + 1) * 1)
    END DO
  END DO

  DO i1 = 0, opDat5SharedIndirectionSize - 1, 1
    DO i2 = 0, 4 - 1, 1
      opDat5SharedIndirection(i2 + i1 * 4 + 1) = 0
    END DO
  END DO

  DO i1 = 0, numberOfActiveThreadsCeiling - 1, 1
    colour2 = -1
    IF (i1 < numberOfActiveThreads) THEN
      DO i2 = 0, 4 - 1, 1
        opDat5Local(i2) = 0
      END DO
      CALL bres_calc_modified( &
      & opDat1SharedIndirection(1 + mappingArray1(i1 + threadBlockOffset) * 2:1 + mappingArray1(i1 + threadBlockOffset) * 2 + 2 - 1), &
      & opDat1SharedIndirection(1 + mappingArray2(i1 + threadBlockOffset) * 2:1 + mappingArray2(i1 + threadBlockOffset) * 2 + 2 - 1), &
      & opDat3SharedIndirection(1 + mappingArray3(i1 + threadBlockOffset) * 4:1 + mappingArray3(i1 + threadBlockOffset) * 4 + 4 - 1), &
      & opDat4SharedIndirection(1 + mappingArray4(i1 + threadBlockOffset) * 1), &
      & opDat5Local,&
      & opDat6((i1 + threadBlockOffset) * 1))
      colour2 = thrcol(i1 + threadBlockOffset)
    END IF

    opDat5Map = mappingArray5(i1 + threadBlockOffset)

    DO colour1 = 0, numOfColours - 1, 1
      IF (colour2 .EQ. colour1) THEN
         DO i2 = 0, 4 - 1, 1
          opDat5SharedIndirection(1 + (i2 + opDat5Map * 4)) = opDat5SharedIndirection(1 + (i2 + opDat5Map * 4)) + opDat5Local(i2)
        END DO
      END IF
    END DO
  END DO

  DO i1 = 0, opDat5SharedIndirectionSize - 1, 1
    DO i2 = 0, 4 - 1, 1
      opDat5(i2 + opDat5IndirectionMap(i1 + 1) * 4) = opDat5(i2 + opDat5IndirectionMap(i1 + 1) * 4) + opDat5SharedIndirection(1 + (i2 + i1 * 4))
    END DO
  END DO
END SUBROUTINE

SUBROUTINE bres_calc_host( userSubroutine, set, &
  & opArg1, &
  & opArg2, &
  & opArg3, &
  & opArg4, &
  & opArg5, &
  & opArg6)

  IMPLICIT NONE
  character(len=10), INTENT(IN) :: userSubroutine
  TYPE ( op_set ) , INTENT(IN) :: set

  TYPE ( op_arg ) , INTENT(IN) :: opArg1
  TYPE ( op_arg ) , INTENT(IN) :: opArg2
  TYPE ( op_arg ) , INTENT(IN) :: opArg3
  TYPE ( op_arg ) , INTENT(IN) :: opArg4
  TYPE ( op_arg ) , INTENT(IN) :: opArg5
  TYPE ( op_arg ) , INTENT(IN) :: opArg6

  TYPE ( op_arg ) , DIMENSION(6) :: opArgArray
  INTEGER(kind=4) :: numberOfOpDats
  INTEGER(kind=4) :: returnMPIHaloExchange
  INTEGER(kind=4) :: returnSetKernelTiming
  TYPE ( op_set_core ) , POINTER :: opSetCore

  TYPE ( op_set_core ) , POINTER :: opSet1Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat1Local
  INTEGER(kind=4) :: opDat1Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet3Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat3Local
  INTEGER(kind=4) :: opDat3Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet4Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat4Local
  INTEGER(kind=4) :: opDat4Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet5Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat5Local
  INTEGER(kind=4) :: opDat5Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet6Core
  INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat6Local
  INTEGER(kind=4) :: opDat6Cardinality

  TYPE ( op_map_core ) , POINTER :: opMap1Core
  TYPE ( op_map_core ) , POINTER :: opMap2Core
  TYPE ( op_map_core ) , POINTER :: opMap3Core
  TYPE ( op_map_core ) , POINTER :: opMap4Core
  TYPE ( op_map_core ) , POINTER :: opMap5Core
  TYPE ( op_map_core ) , POINTER :: opMap6Core

  TYPE ( op_dat_core ) , POINTER :: opDat1Core
  TYPE ( op_dat_core ) , POINTER :: opDat2Core
  TYPE ( op_dat_core ) , POINTER :: opDat3Core
  TYPE ( op_dat_core ) , POINTER :: opDat4Core
  TYPE ( op_dat_core ) , POINTER :: opDat5Core
  TYPE ( op_dat_core ) , POINTER :: opDat6Core


  INTEGER(kind=4) :: threadID
  INTEGER(kind=4) :: numberOfThreads
  INTEGER(kind=4) :: partitionSize
  INTEGER(kind=4), DIMENSION(1:6) :: opDatArray
  INTEGER(kind=4), DIMENSION(1:6) :: mappingIndicesArray
  INTEGER(kind=4), DIMENSION(1:6) :: mappingArray
  INTEGER(kind=4), DIMENSION(1:6) :: accessDescriptorArray
  INTEGER(kind=4), DIMENSION(1:6) :: indirectionDescriptorArray
  INTEGER(kind=4), DIMENSION(1:6) :: opDatTypesArray
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayStart
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayEnd
  INTEGER(kind=4) :: numberOfIndirectOpDats
  INTEGER(kind=4) :: blockOffset
  INTEGER(kind=4) :: nblocks
  REAL(kind=8) :: startTimeHost
  REAL(kind=8) :: endTimeHost
  REAL(kind=8) :: startTimeKernel
  REAL(kind=8) :: endTimeKernel
  REAL(kind=8) :: accumulatorHostTime
  REAL(kind=8) :: accumulatorKernelTime
  INTEGER(kind=4) :: i1
  INTEGER(kind=4) :: i2


  IF (set%setPtr%size .EQ. 0) THEN
    RETURN
  END IF

  numberCalledbres_calc_3019043301 = numberCalledbres_calc_3019043301 + 1

  call date_and_time(values=timeArrayStart)
  startTimeHost = 1.00000 * timeArrayStart(8) + &
  & 1000.00 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

#ifdef OP_PART_SIZE_1
  partitionSize = OP_PART_SIZE_1
#else
  partitionSize = 0
#endif

#ifdef _OPENMP
  numberOfThreads = omp_get_max_threads()
#else
  numberOfThreads = 1
#endif

  numberOfOpDats = 6

  opArgArray(1) = opArg1
  opArgArray(2) = opArg2
  opArgArray(3) = opArg3
  opArgArray(4) = opArg4
  opArgArray(5) = opArg5
  opArgArray(6) = opArg6

  indirectionDescriptorArray(1) = 0
  indirectionDescriptorArray(2) = 0
  indirectionDescriptorArray(3) = 1
  indirectionDescriptorArray(4) = 2
  indirectionDescriptorArray(5) = 3
  indirectionDescriptorArray(6) = -1

  numberOfIndirectOpDats = 4

  planRet_bres_calc = FortranPlanCaller( &
                      & userSubroutine, &
                      & set%setCPtr, &
                      & partitionSize, &
                      & numberOfOpDats, &
                      & opArgArray, &
                      & numberOfIndirectOpDats, &
                      & indirectionDescriptorArray)

   CALL c_f_pointer(planRet_bres_calc,actualPlan_bres_calc)
   CALL c_f_pointer(actualPlan_bres_calc%nindirect,pnindirect_bres_calc,(/numberOfIndirectOpDats/))
   CALL c_f_pointer(actualPlan_bres_calc%ind_maps,ind_maps_bres_calc,(/numberOfIndirectOpDats/))
   CALL c_f_pointer(actualPlan_bres_calc%maps,mappingArray_bres_calc,(/numberOfOpDats/))
   CALL c_f_pointer(actualPlan_bres_calc%ncolblk,ncolblk_bres_calc,(/set%setPtr%size/))
   CALL c_f_pointer(actualPlan_bres_calc%ind_sizes,ind_sizes_bres_calc,(/actualPlan_bres_calc%nblocks * numberOfIndirectOpDats/))
   CALL c_f_pointer(actualPlan_bres_calc%ind_offs,ind_offs_bres_calc,(/actualPlan_bres_calc%nblocks * numberOfIndirectOpDats/))
   CALL c_f_pointer(actualPlan_bres_calc%blkmap,blkmap_bres_calc,(/actualPlan_bres_calc%nblocks/))
   CALL c_f_pointer(actualPlan_bres_calc%offset,offset_bres_calc,(/actualPlan_bres_calc%nblocks/))
   CALL c_f_pointer(actualPlan_bres_calc%nelems,nelems_bres_calc,(/actualPlan_bres_calc%nblocks/))
   CALL c_f_pointer(actualPlan_bres_calc%nthrcol,nthrcol_bres_calc,(/actualPlan_bres_calc%nblocks/))
   CALL c_f_pointer(actualPlan_bres_calc%thrcol,thrcol_bres_calc,(/set%setPtr%size/))

   CALL c_f_pointer(ind_maps_bres_calc(1),ind_maps1_bres_calc,(/pnindirect_bres_calc(1)/))
   CALL c_f_pointer(ind_maps_bres_calc(2),ind_maps3_bres_calc,(/pnindirect_bres_calc(2)/))
   CALL c_f_pointer(ind_maps_bres_calc(3),ind_maps4_bres_calc,(/pnindirect_bres_calc(3)/))
   CALL c_f_pointer(ind_maps_bres_calc(4),ind_maps5_bres_calc,(/pnindirect_bres_calc(4)/))

   IF (indirectionDescriptorArray(1) >= 0) THEN
     CALL c_f_pointer(mappingArray_bres_calc(1),mappingArray1_bres_calc,(/set%setPtr%size/))
   END IF

   IF (indirectionDescriptorArray(2) >= 0) THEN
     CALL c_f_pointer(mappingArray_bres_calc(2),mappingArray2_bres_calc,(/set%setPtr%size/))
   END IF

  IF (indirectionDescriptorArray(3) >= 0) THEN
    CALL c_f_pointer(mappingArray_bres_calc(3),mappingArray3_bres_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(4) >= 0) THEN
    CALL c_f_pointer(mappingArray_bres_calc(4),mappingArray4_bres_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(5) >= 0) THEN
    CALL c_f_pointer(mappingArray_bres_calc(5),mappingArray5_bres_calc,(/set%setPtr%size/))
  END IF

  opSetCore => set%setPtr

  opDat1Cardinality = opArg1%dim * getSetSizeFromOpArg(opArg1)
  opDat3Cardinality = opArg3%dim * getSetSizeFromOpArg(opArg3)
  opDat4Cardinality = opArg4%dim * getSetSizeFromOpArg(opArg4)
  opDat5Cardinality = opArg5%dim * getSetSizeFromOpArg(opArg5)
  opDat6Cardinality = opArg6%dim * getSetSizeFromOpArg(opArg6)

  CALL c_f_pointer(opArg1%data,opDat1Local,(/opDat1Cardinality/))
  CALL c_f_pointer(opArg3%data,opDat3Local,(/opDat3Cardinality/))
  CALL c_f_pointer(opArg4%data,opDat4Local,(/opDat4Cardinality/))
  CALL c_f_pointer(opArg5%data,opDat5Local,(/opDat5Cardinality/))
  CALL c_f_pointer(opArg6%data,opDat6Local,(/opDat6Cardinality/))

  call date_and_time(values=timeArrayEnd)
  endTimeHost = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7) + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorHostTime = endTimeHost - startTimeHost
  loopTimeHostbres_calc_3019043301 = loopTimeHostbres_calc_3019043301 + accumulatorHostTime

  call date_and_time(values=timeArrayStart)
  startTimeKernel = 1.00000 * timeArrayStart(8) + &
  & 1000 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

  blockOffset = 0

  DO i1 = 0, actualPlan_bres_calc%ncolors - 1, 1
    nblocks = ncolblk_bres_calc(i1 + 1)
    !$OMP PARALLEL DO private (threadID)
    DO i2 = 0, nblocks - 1, 1
      threadID = omp_get_thread_num()
      CALL bres_calc_kernel( &
           & opDat1Local, &
           & opDat3Local, &
           & opDat4Local, &
           & opDat5Local, &
           & opDat6Local, &
           & ind_maps1_bres_calc, &
           & ind_maps3_bres_calc, &
           & ind_maps4_bres_calc, &
           & ind_maps5_bres_calc, &
           & mappingArray1_bres_calc, &
           & mappingArray2_bres_calc, &
           & mappingArray3_bres_calc, &
           & mappingArray4_bres_calc, &
           & mappingArray5_bres_calc, &
           & ind_sizes_bres_calc, &
           & ind_offs_bres_calc, &
           & blkmap_bres_calc, &
           & offset_bres_calc, &
           & nelems_bres_calc, &
           & nthrcol_bres_calc, &
           & thrcol_bres_calc, &
           & blockOffset, &
           & i2)
    END DO
    !$OMP END PARALLEL DO

    blockOffset = blockOffset + nblocks
  END DO

  call date_and_time(values=timeArrayEnd)
  endTimeKernel = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7) + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorKernelTime = endTimeKernel - startTimeKernel
  loopTimeKernelbres_calc_3019043301 = loopTimeKernelbres_calc_3019043301 + accumulatorKernelTime

  call date_and_time(values=timeArrayStart)
  startTimeHost = 1.00000 * timeArrayStart(8) + &
  & 1000.00 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

  call date_and_time(values=timeArrayEnd)
  endTimeHost = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7) + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorHostTime = endTimeHost - startTimeHost
  loopTimeHostbres_calc_3019043301 = loopTimeHostbres_calc_3019043301 + accumulatorHostTime
  returnSetKernelTiming = setKernelTime(2,userSubroutine, &
  & accumulatorKernelTime / 1000.00,actualPlan_bres_calc%transfer,actualPlan_bres_calc%transfer2)
END SUBROUTINE

SUBROUTINE res_calc_modified(x1,x2,q1,q2,adt1,adt2,res1,res2)
IMPLICIT NONE
REAL(kind=8), DIMENSION(2) :: x1
REAL(kind=8), DIMENSION(2) :: x2
REAL(kind=8), DIMENSION(4) :: q1
REAL(kind=8), DIMENSION(4) :: q2
REAL(kind=8) :: adt1
REAL(kind=8) :: adt2
REAL(kind=8), DIMENSION(4) :: res1
REAL(kind=8), DIMENSION(4) :: res2
REAL(kind=8) :: dx,dy,mu,ri,p1,vol1,p2,vol2,f
dx = x1(1) - x2(1)
dy = x1(2) - x2(2)
ri = 1.0 / q1(1)
p1 = gm1_OP2_CONSTANT * (q1(4) - 0.5 * ri * (q1(2) * q1(2) + q1(3) * q1(3)))
vol1 = ri * (q1(2) * dy - q1(3) * dx)
ri = 1.0 / q2(1)
p2 = gm1_OP2_CONSTANT * (q2(4) - 0.5 * ri * (q2(2) * q2(2) + q2(3) * q2(3)))
vol2 = ri * (q2(2) * dy - q2(3) * dx)
mu = 0.5 * (adt1 + adt2) * eps_OP2_CONSTANT
f = 0.5 * (vol1 * q1(1) + vol2 * q2(1)) + mu * (q1(1) - q2(1))
res1(1) = res1(1) + f
res2(1) = res2(1) - f
f = 0.5 * (vol1 * q1(2) + p1 * dy + vol2 * q2(2) + p2 * dy) + mu * (q1(2) - q2(2))
res1(2) = res1(2) + f
res2(2) = res2(2) - f
f = 0.5 * (vol1 * q1(3) - p1 * dx + vol2 * q2(3) - p2 * dx) + mu * (q1(3) - q2(3))
res1(3) = res1(3) + f
res2(3) = res2(3) - f
f = 0.5 * (vol1 * (q1(4) + p1) + vol2 * (q2(4) + p2)) + mu * (q1(4) - q2(4))
res1(4) = res1(4) + f
res2(4) = res2(4) - f
END SUBROUTINE

SUBROUTINE res_calc_kernel( &
    & opDat1, &
    & opDat3, &
    & opDat5, &
    & opDat7, &
    & ind_maps1, &
    & ind_maps3, &
    & ind_maps5, &
    & ind_maps7, &
    & mappingArray1, &
    & mappingArray2, &
    & mappingArray3, &
    & mappingArray4, &
    & mappingArray5, &
    & mappingArray6, &
    & mappingArray7, &
    & mappingArray8, &
    & ind_sizes, &
    & ind_offs, &
    & blkmap,offset, &
    & nelems, &
    & nthrcol, &
    & thrcol, &
    & blockOffset, &
    & blockID)

     IMPLICIT NONE

     REAL(kind=8), DIMENSION(0:*) :: opDat1
     REAL(kind=8), DIMENSION(0:*) :: opDat3
     REAL(kind=8), DIMENSION(0:*) :: opDat5
     REAL(kind=8), DIMENSION(0:*) :: opDat7
     INTEGER(kind=4), DIMENSION(0:), TARGET :: ind_maps1
     INTEGER(kind=4), DIMENSION(0:), TARGET :: ind_maps3
     INTEGER(kind=4), DIMENSION(0:), TARGET :: ind_maps5
     INTEGER(kind=4), DIMENSION(0:), TARGET :: ind_maps7
     INTEGER(kind=2), DIMENSION(0:*) :: mappingArray1
     INTEGER(kind=2), DIMENSION(0:*) :: mappingArray2
     INTEGER(kind=2), DIMENSION(0:*) :: mappingArray3
     INTEGER(kind=2), DIMENSION(0:*) :: mappingArray4
     INTEGER(kind=2), DIMENSION(0:*) :: mappingArray5
     INTEGER(kind=2), DIMENSION(0:*) :: mappingArray6
     INTEGER(kind=2), DIMENSION(0:*) :: mappingArray7
     INTEGER(kind=2), DIMENSION(0:*) :: mappingArray8

     INTEGER(kind=4), DIMENSION(0:*) :: ind_sizes
     INTEGER(kind=4), DIMENSION(0:*) :: ind_offs
     INTEGER(kind=4), DIMENSION(0:*) :: blkmap
     INTEGER(kind=4), DIMENSION(0:*) :: offset
     INTEGER(kind=4), DIMENSION(0:*) :: nelems
     INTEGER(kind=4), DIMENSION(0:*) :: nthrcol
     INTEGER(kind=4), DIMENSION(0:*) :: thrcol
     INTEGER(kind=4) :: blockOffset
     INTEGER(kind=4) :: blockID
     INTEGER(kind=4) :: threadBlockOffset
     INTEGER(kind=4) :: threadBlockID
     INTEGER(kind=4) :: numberOfActiveThreads
     INTEGER(kind=4) :: i1
     INTEGER(kind=4) :: i2
     REAL(kind=8), DIMENSION(0:128000 - 1), TARGET :: sharedFloat8

     INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat1IndirectionMap
     REAL(kind=8), POINTER, DIMENSION(:) :: opDat1SharedIndirection
     INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat3IndirectionMap
     REAL(kind=8), POINTER, DIMENSION(:) :: opDat3SharedIndirection
     INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat5IndirectionMap
     REAL(kind=8), POINTER, DIMENSION(:) :: opDat5SharedIndirection
     INTEGER(kind=4), POINTER, DIMENSION(:) :: opDat7IndirectionMap
     REAL(kind=8), POINTER, DIMENSION(:) :: opDat7SharedIndirection
     INTEGER(kind=4) :: opDat1nBytes
     INTEGER(kind=4) :: opDat3nBytes
     INTEGER(kind=4) :: opDat5nBytes
     INTEGER(kind=4) :: opDat7nBytes
     INTEGER(kind=4) :: opDat1RoundUp
     INTEGER(kind=4) :: opDat3RoundUp
     INTEGER(kind=4) :: opDat5RoundUp
     INTEGER(kind=4) :: opDat7RoundUp
     INTEGER(kind=4) :: opDat1SharedIndirectionSize
     INTEGER(kind=4) :: opDat3SharedIndirectionSize
     INTEGER(kind=4) :: opDat5SharedIndirectionSize
     INTEGER(kind=4) :: opDat7SharedIndirectionSize
     REAL(kind=8), DIMENSION(0:3) :: opDat7Local
     INTEGER(kind=4) :: opDat7Map
     REAL(kind=8), DIMENSION(0:3) :: opDat8Local
     INTEGER(kind=4) :: opDat8Map
     INTEGER(kind=4) :: numOfColours
     INTEGER(kind=4) :: numberOfActiveThreadsCeiling
     INTEGER(kind=4) :: colour1
     INTEGER(kind=4) :: colour2

     threadBlockID = blkmap(blockID + blockOffset)
     numberOfActiveThreads = nelems(threadBlockID)
     threadBlockOffset = offset(threadBlockID)
     numberOfActiveThreadsCeiling = numberOfActiveThreads
     numOfColours = nthrcol(threadBlockID)

     opDat1SharedIndirectionSize = ind_sizes(0 + threadBlockID * 4)
     opDat3SharedIndirectionSize = ind_sizes(1 + threadBlockID * 4)
     opDat5SharedIndirectionSize = ind_sizes(2 + threadBlockID * 4)
     opDat7SharedIndirectionSize = ind_sizes(3 + threadBlockID * 4)

     opDat1IndirectionMap => ind_maps1(ind_offs(0 + threadBlockID * 4):)
     opDat3IndirectionMap => ind_maps3(ind_offs(1 + threadBlockID * 4):)
     opDat5IndirectionMap => ind_maps5(ind_offs(2 + threadBlockID * 4):)
     opDat7IndirectionMap => ind_maps7(ind_offs(3 + threadBlockID * 4):)

     opDat3RoundUp = opDat1SharedIndirectionSize * 2
     opDat5RoundUp = opDat3SharedIndirectionSize * 4
     opDat7RoundUp = opDat5SharedIndirectionSize * 1

     opDat1nBytes = 0

     opDat3nBytes = opDat1nBytes + opDat3RoundUp
     opDat5nBytes = opDat3nBytes + opDat5RoundUp
     opDat7nBytes = opDat5nBytes + opDat7RoundUp

     opDat1SharedIndirection => sharedFloat8(opDat1nBytes:)
     opDat3SharedIndirection => sharedFloat8(opDat3nBytes:)
     opDat5SharedIndirection => sharedFloat8(opDat5nBytes:)
     opDat7SharedIndirection => sharedFloat8(opDat7nBytes:)

     DO i1 = 0, opDat1SharedIndirectionSize - 1, 1
       DO i2 = 0, 2 - 1, 1
         opDat1SharedIndirection(i2 + i1 * 2 + 1) = opDat1(i2 + opDat1IndirectionMap(i1 + 1) * 2)
       END DO
     END DO

     DO i1 = 0, opDat3SharedIndirectionSize - 1, 1
       DO i2 = 0, 4 - 1, 1
         opDat3SharedIndirection(i2 + i1 * 4 + 1) = opDat3(i2 + opDat3IndirectionMap(i1 + 1) * 4)
       END DO
     END DO

     DO i1 = 0, opDat5SharedIndirectionSize - 1, 1
       DO i2 = 0, 1 - 1, 1
         opDat5SharedIndirection(i2 + i1 * 1 + 1) = opDat5(i2 + opDat5IndirectionMap(i1 + 1) * 1)
       END DO
     END DO

     DO i1 = 0, opDat7SharedIndirectionSize - 1, 1
       DO i2 = 0, 4 - 1, 1
         opDat7SharedIndirection(i2 + i1 * 4 + 1) = 0
       END DO
     END DO

     DO i1 = 0, numberOfActiveThreadsCeiling - 1, 1
       colour2 = -1
       IF (i1 < numberOfActiveThreads) THEN
         DO i2 = 0, 4 - 1, 1
           opDat7Local(i2) = 0
         END DO

         DO i2 = 0, 4 - 1, 1
           opDat8Local(i2) = 0
         END DO

         CALL res_calc_modified( &
         & opDat1SharedIndirection(1 + mappingArray1(i1 + threadBlockOffset) * 2:1 + mappingArray1(i1 + threadBlockOffset) * 2 + 2 - 1), &
         & opDat1SharedIndirection(1 + mappingArray2(i1 + threadBlockOffset) * 2:1 + mappingArray2(i1 + threadBlockOffset) * 2 + 2 - 1), &
         & opDat3SharedIndirection(1 + mappingArray3(i1 + threadBlockOffset) * 4:1 + mappingArray3(i1 + threadBlockOffset) * 4 + 4 - 1), &
         & opDat3SharedIndirection(1 + mappingArray4(i1 + threadBlockOffset) * 4:1 + mappingArray4(i1 + threadBlockOffset) * 4 + 4 - 1), &
         & opDat5SharedIndirection(1 + mappingArray5(i1 + threadBlockOffset) * 1), &
         & opDat5SharedIndirection(1 + mappingArray6(i1 + threadBlockOffset) * 1), &
         & opDat7Local, &
         & opDat8Local)
         colour2 = thrcol(i1 + threadBlockOffset)
       END IF

       opDat7Map = mappingArray7(i1 + threadBlockOffset)
       opDat8Map = mappingArray8(i1 + threadBlockOffset)

       DO colour1 = 0, numOfColours - 1, 1
         IF (colour2 .EQ. colour1) THEN
           DO i2 = 0, 4 - 1, 1
             opDat7SharedIndirection(1 + (i2 + opDat7Map * 4)) = opDat7SharedIndirection(1 + (i2 + opDat7Map * 4)) + opDat7Local(i2)
           END DO

           DO i2 = 0, 4 - 1, 1
             opDat7SharedIndirection(1 + (i2 + opDat8Map * 4)) = opDat7SharedIndirection(1 + (i2 + opDat8Map * 4)) + opDat8Local(i2)
           END DO
         END IF
       END DO
     END DO

     DO i1 = 0, opDat7SharedIndirectionSize - 1, 1
       DO i2 = 0, 4 - 1, 1
         opDat7(i2 + opDat7IndirectionMap(i1 + 1) * 4) = opDat7(i2 + opDat7IndirectionMap(i1 + 1) * 4) + opDat7SharedIndirection(1 + (i2 + i1 * 4))
       END DO
     END DO
END SUBROUTINE

SUBROUTINE res_calc_host( userSubroutine, set, &
  & opArg1, &
  & opArg2, &
  & opArg3, &
  & opArg4, &
  & opArg5, &
  & opArg6, &
  & opArg7, &
  & opArg8)

  IMPLICIT NONE
  character(len=9), INTENT(IN) :: userSubroutine
  TYPE ( op_set ) , INTENT(IN) :: set

  TYPE ( op_arg ) , INTENT(IN) :: opArg1
  TYPE ( op_arg ) , INTENT(IN) :: opArg2
  TYPE ( op_arg ) , INTENT(IN) :: opArg3
  TYPE ( op_arg ) , INTENT(IN) :: opArg4
  TYPE ( op_arg ) , INTENT(IN) :: opArg5
  TYPE ( op_arg ) , INTENT(IN) :: opArg6
  TYPE ( op_arg ) , INTENT(IN) :: opArg7
  TYPE ( op_arg ) , INTENT(IN) :: opArg8

  TYPE ( op_arg ) , DIMENSION(8) :: opArgArray
  INTEGER(kind=4) :: numberOfOpDats
  INTEGER(kind=4) :: returnMPIHaloExchange
  INTEGER(kind=4) :: returnSetKernelTiming
  TYPE ( op_set_core ) , POINTER :: opSetCore

  TYPE ( op_set_core ) , POINTER :: opSet1Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat1Local
  INTEGER(kind=4) :: opDat1Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet3Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat3Local
  INTEGER(kind=4) :: opDat3Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet5Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat5Local
  INTEGER(kind=4) :: opDat5Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet7Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat7Local
  INTEGER(kind=4) :: opDat7Cardinality

  TYPE ( op_map_core ) , POINTER :: opMap1Core
  TYPE ( op_map_core ) , POINTER :: opMap2Core
  TYPE ( op_map_core ) , POINTER :: opMap3Core
  TYPE ( op_map_core ) , POINTER :: opMap4Core
  TYPE ( op_map_core ) , POINTER :: opMap5Core
  TYPE ( op_map_core ) , POINTER :: opMap6Core
  TYPE ( op_map_core ) , POINTER :: opMap7Core
  TYPE ( op_map_core ) , POINTER :: opMap8Core

  TYPE ( op_dat_core ) , POINTER :: opDat1Core
  TYPE ( op_dat_core ) , POINTER :: opDat2Core
  TYPE ( op_dat_core ) , POINTER :: opDat3Core
  TYPE ( op_dat_core ) , POINTER :: opDat4Core
  TYPE ( op_dat_core ) , POINTER :: opDat5Core
  TYPE ( op_dat_core ) , POINTER :: opDat6Core
  TYPE ( op_dat_core ) , POINTER :: opDat7Core
  TYPE ( op_dat_core ) , POINTER :: opDat8Core


  INTEGER(kind=4) :: threadID
  INTEGER(kind=4) :: numberOfThreads
  INTEGER(kind=4) :: partitionSize
  INTEGER(kind=4), DIMENSION(1:8) :: opDatArray
  INTEGER(kind=4), DIMENSION(1:8) :: mappingIndicesArray
  INTEGER(kind=4), DIMENSION(1:8) :: mappingArray
  INTEGER(kind=4), DIMENSION(1:8) :: accessDescriptorArray
  INTEGER(kind=4), DIMENSION(1:8) :: indirectionDescriptorArray
  INTEGER(kind=4), DIMENSION(1:8) :: opDatTypesArray
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayStart
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayEnd
  INTEGER(kind=4) :: numberOfIndirectOpDats
  INTEGER(kind=4) :: blockOffset
  INTEGER(kind=4) :: nblocks
  REAL(kind=8) :: startTimeHost
  REAL(kind=8) :: endTimeHost
  REAL(kind=8) :: startTimeKernel
  REAL(kind=8) :: endTimeKernel
  REAL(kind=8) :: accumulatorHostTime
  REAL(kind=8) :: accumulatorKernelTime
  INTEGER(kind=4) :: i1
  INTEGER(kind=4) :: i2

  IF (set%setPtr%size .EQ. 0) THEN
    RETURN
  END IF

  numberCalledres_calc_3884922247 = numberCalledres_calc_3884922247 + 1

  call date_and_time(values=timeArrayStart)
  startTimeHost = 1.00000 * timeArrayStart(8) + 1000.00 * timeArrayStart(7) + &
                  & 60000 * timeArrayStart(6) + 3600000 * timeArrayStart(5)

#ifdef OP_PART_SIZE_1
    partitionSize = OP_PART_SIZE_1
#else
    partitionSize = 0
#endif

#ifdef _OPENMP
    numberOfThreads = omp_get_max_threads()
#else
    numberOfThreads = 1
#endif

  numberOfOpDats = 8

  opArgArray(1) = opArg1
  opArgArray(2) = opArg2
  opArgArray(3) = opArg3
  opArgArray(4) = opArg4
  opArgArray(5) = opArg5
  opArgArray(6) = opArg6
  opArgArray(7) = opArg7
  opArgArray(8) = opArg8

  indirectionDescriptorArray(1) = 0
  indirectionDescriptorArray(2) = 0
  indirectionDescriptorArray(3) = 1
  indirectionDescriptorArray(4) = 1
  indirectionDescriptorArray(5) = 2
  indirectionDescriptorArray(6) = 2
  indirectionDescriptorArray(7) = 3
  indirectionDescriptorArray(8) = 3

  numberOfIndirectOpDats = 4

  planRet_res_calc = FortranPlanCaller( &
  & userSubroutine, &
  & set%setCPtr, &
  & partitionSize, &
  & numberOfOpDats, &
  & opArgArray, &
  & numberOfIndirectOpDats, &
  & indirectionDescriptorArray)

  CALL c_f_pointer(planRet_res_calc,actualPlan_res_calc)

  CALL c_f_pointer(actualPlan_res_calc%nindirect,pnindirect_res_calc,(/numberOfIndirectOpDats/))
  CALL c_f_pointer(actualPlan_res_calc%ind_maps,ind_maps_res_calc,(/numberOfIndirectOpDats/))
  CALL c_f_pointer(actualPlan_res_calc%maps,mappingArray_res_calc,(/numberOfOpDats/))
  CALL c_f_pointer(actualPlan_res_calc%ncolblk,ncolblk_res_calc,(/set%setPtr%size/))
  CALL c_f_pointer(actualPlan_res_calc%ind_sizes,ind_sizes_res_calc,(/actualPlan_res_calc%nblocks * numberOfIndirectOpDats/))
  CALL c_f_pointer(actualPlan_res_calc%ind_offs,ind_offs_res_calc,(/actualPlan_res_calc%nblocks * numberOfIndirectOpDats/))
  CALL c_f_pointer(actualPlan_res_calc%blkmap,blkmap_res_calc,(/actualPlan_res_calc%nblocks/))
  CALL c_f_pointer(actualPlan_res_calc%offset,offset_res_calc,(/actualPlan_res_calc%nblocks/))
  CALL c_f_pointer(actualPlan_res_calc%nelems,nelems_res_calc,(/actualPlan_res_calc%nblocks/))
  CALL c_f_pointer(actualPlan_res_calc%nthrcol,nthrcol_res_calc,(/actualPlan_res_calc%nblocks/))
  CALL c_f_pointer(actualPlan_res_calc%thrcol,thrcol_res_calc,(/set%setPtr%size/))

  CALL c_f_pointer(ind_maps_res_calc(1),ind_maps1_res_calc,(/pnindirect_res_calc(1)/))
  CALL c_f_pointer(ind_maps_res_calc(2),ind_maps3_res_calc,(/pnindirect_res_calc(2)/))
  CALL c_f_pointer(ind_maps_res_calc(3),ind_maps5_res_calc,(/pnindirect_res_calc(3)/))
  CALL c_f_pointer(ind_maps_res_calc(4),ind_maps7_res_calc,(/pnindirect_res_calc(4)/))

  IF (indirectionDescriptorArray(1) >= 0) THEN
    CALL c_f_pointer(mappingArray_res_calc(1),mappingArray1_res_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(2) >= 0) THEN
    CALL c_f_pointer(mappingArray_res_calc(2),mappingArray2_res_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(3) >= 0) THEN
    CALL c_f_pointer(mappingArray_res_calc(3),mappingArray3_res_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(4) >= 0) THEN
    CALL c_f_pointer(mappingArray_res_calc(4),mappingArray4_res_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(5) >= 0) THEN
    CALL c_f_pointer(mappingArray_res_calc(5),mappingArray5_res_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(6) >= 0) THEN
    CALL c_f_pointer(mappingArray_res_calc(6),mappingArray6_res_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(7) >= 0) THEN
    CALL c_f_pointer(mappingArray_res_calc(7),mappingArray7_res_calc,(/set%setPtr%size/))
  END IF

  IF (indirectionDescriptorArray(8) >= 0) THEN
    CALL c_f_pointer(mappingArray_res_calc(8),mappingArray8_res_calc,(/set%setPtr%size/))
  END IF

  opSetCore => set%setPtr

  opDat1Cardinality = opArg1%dim * getSetSizeFromOpArg(opArg1)
  opDat3Cardinality = opArg3%dim * getSetSizeFromOpArg(opArg3)
  opDat5Cardinality = opArg5%dim * getSetSizeFromOpArg(opArg5)
  opDat7Cardinality = opArg7%dim * getSetSizeFromOpArg(opArg7)

  CALL c_f_pointer(opArg1%data,opDat1Local,(/opDat1Cardinality/))
  CALL c_f_pointer(opArg3%data,opDat3Local,(/opDat3Cardinality/))
  CALL c_f_pointer(opArg5%data,opDat5Local,(/opDat5Cardinality/))
  CALL c_f_pointer(opArg7%data,opDat7Local,(/opDat7Cardinality/))

  call date_and_time(values=timeArrayEnd)
  endTimeHost = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7)  + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorHostTime = endTimeHost - startTimeHost
  loopTimeHostres_calc_3884922247 = loopTimeHostres_calc_3884922247 + accumulatorHostTime

  call date_and_time(values=timeArrayStart)
  startTimeKernel = 1.00000 * timeArrayStart(8) + &
  & 1000 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

  blockOffset = 0

  DO i1 = 0, actualPlan_res_calc%ncolors - 1, 1
    nblocks = ncolblk_res_calc(i1 + 1)
    !$OMP PARALLEL DO private (threadID)
      DO i2 = 0, nblocks - 1, 1
        threadID = omp_get_thread_num()
          CALL res_calc_kernel( &
               & opDat1Local, &
               & opDat3Local, &
               & opDat5Local, &
               & opDat7Local, &
               & ind_maps1_res_calc, &
               & ind_maps3_res_calc, &
               & ind_maps5_res_calc, &
               & ind_maps7_res_calc, &
               & mappingArray1_res_calc, &
               & mappingArray2_res_calc, &
               & mappingArray3_res_calc, &
               & mappingArray4_res_calc, &
               & mappingArray5_res_calc, &
               & mappingArray6_res_calc, &
               & mappingArray7_res_calc, &
               & mappingArray8_res_calc, &
               & ind_sizes_res_calc, &
               & ind_offs_res_calc, &
               & blkmap_res_calc, &
               & offset_res_calc, &
               & nelems_res_calc, &
               & nthrcol_res_calc, &
               & thrcol_res_calc, &
               & blockOffset,i2)
      END DO
    !$OMP END PARALLEL DO
    blockOffset = blockOffset + nblocks
  END DO

  call date_and_time(values=timeArrayEnd)
  endTimeKernel = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7) + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorKernelTime = endTimeKernel - startTimeKernel
  loopTimeKernelres_calc_3884922247 = loopTimeKernelres_calc_3884922247 + accumulatorKernelTime

  call date_and_time(values=timeArrayStart)
  startTimeHost = 1.00000 * timeArrayStart(8) + &
  & 1000.00 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

  call date_and_time(values=timeArrayEnd)
  endTimeHost = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7) + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorHostTime = endTimeHost - startTimeHost
  loopTimeHostres_calc_3884922247 = loopTimeHostres_calc_3884922247 + accumulatorHostTime
  returnSetKernelTiming = setKernelTime(1, userSubroutine,&
  & accumulatorKernelTime / 1000.00,actualPlan_res_calc%transfer,actualPlan_res_calc%transfer2)

END SUBROUTINE

SUBROUTINE s2456222878(qdim,q,qold)
  IMPLICIT NONE
  INTEGER(kind=4) :: qdim
  REAL(kind=8), DIMENSION(qdim) :: q
  REAL(kind=8), DIMENSION(*) :: qold
  INTEGER(kind=4) :: i

  DO i = 1, qdim
    qold(i) = q(i)
  END DO
END SUBROUTINE

SUBROUTINE s2882256832(opDat1,opDat2,opDat3,sliceStart,sliceEnd)
  IMPLICIT NONE
  INTEGER(kind=4) :: opDat1
  DOUBLE PRECISION, DIMENSION(0:*) :: opDat2
  DOUBLE PRECISION, DIMENSION(0:*) :: opDat3
  INTEGER(kind=4) :: sliceStart
  INTEGER(kind=4) :: sliceEnd
  INTEGER(kind=4) :: i1

  DO i1 = sliceStart, sliceEnd - 1, 1
    CALL s2456222878(opDat1,opDat2(i1 * 4:i1 * 4 + 4 - 1),opDat3(i1 * 4:i1 * 4 + 4 - 1))
  END DO
END SUBROUTINE

SUBROUTINE save_soln_qdim_host(userSubroutine,set,opArg1,opArg2,opArg3)
  IMPLICIT NONE
  character(len=15), INTENT(IN) :: userSubroutine
  TYPE ( op_set ) , INTENT(IN) :: set
  TYPE ( op_arg ) , INTENT(IN) :: opArg1
  TYPE ( op_arg ) , INTENT(IN) :: opArg2
  TYPE ( op_arg ) , INTENT(IN) :: opArg3
  TYPE ( op_set_core ) , POINTER :: opSetCore
  TYPE ( op_dat_core ) , POINTER :: opDat1Core
  INTEGER(kind=4), POINTER :: opDat1Local
  INTEGER(kind=4) :: opDat1Cardinality
  TYPE ( op_set_core ) , POINTER :: opSet1Core
  TYPE ( op_dat_core ) , POINTER :: opDat2Core
  DOUBLE PRECISION, POINTER, DIMENSION(:) :: opDat2Local
  INTEGER(kind=4) :: opDat2Cardinality
  TYPE ( op_set_core ) , POINTER :: opSet2Core
  TYPE ( op_dat_core ) , POINTER :: opDat3Core
  DOUBLE PRECISION, POINTER, DIMENSION(:) :: opDat3Local
  INTEGER(kind=4) :: opDat3Cardinality
  TYPE ( op_set_core ) , POINTER :: opSet3Core
  INTEGER(kind=4) :: threadID
  INTEGER(kind=4) :: i1
  INTEGER(kind=4) :: numberOfThreads
  INTEGER(kind=4) :: sliceStart
  INTEGER(kind=4) :: sliceEnd
  INTEGER(kind=4) :: partitionSize
  TYPE ( op_arg ) , DIMENSION(3) :: opArgArray
  INTEGER(kind=4) :: numberOfOpDats
  INTEGER(kind=4) :: returnMPIHaloExchange
  INTEGER(kind=4) :: returnSetKernelTiming
  REAL(kind=8) :: startTimeHost
  REAL(kind=8) :: endTimeHost
  REAL(kind=8) :: startTimeKernel
  REAL(kind=8) :: endTimeKernel
  REAL(kind=8) :: accumulatorHostTime
  REAL(kind=8) :: accumulatorKernelTime
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayStart
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayEnd

  IF (set%setPtr%size .EQ. 0) THEN
    RETURN
  END IF

  numberCalledsave_soln_qdim_2320554256 = numberCalledsave_soln_qdim_2320554256 + 1
  call date_and_time(values=timeArrayStart)
  startTimeHost = 1.00000 * timeArrayStart(8) + 1000.00 * timeArrayStart(7) + 60000 * timeArrayStart(6) + 3600000 * timeArrayStart(5)

#ifdef _OPENMP
    numberOfThreads = omp_get_max_threads()
#else
    numberOfThreads = 1
#endif

  opSetCore => set%setPtr
  opDat1Cardinality = opArg1%dim
  opDat2Cardinality = opArg2%dim * getSetSizeFromOpArg(opArg2)
  opDat3Cardinality = opArg3%dim * getSetSizeFromOpArg(opArg3)

  CALL c_f_pointer(opArg1%data,opDat1Local)
  CALL c_f_pointer(opArg2%data,opDat2Local,(/opDat2Cardinality/))
  CALL c_f_pointer(opArg3%data,opDat3Local,(/opDat3Cardinality/))

  call date_and_time(values=timeArrayEnd)
  endTimeHost = 1.00000 * timeArrayEnd(8) + 1000 * timeArrayEnd(7) + 60000 * timeArrayEnd(6) + 3600000 * timeArrayEnd(5)
  accumulatorHostTime = endTimeHost - startTimeHost
  loopTimeHostsave_soln_qdim_2320554256 = loopTimeHostsave_soln_qdim_2320554256 + accumulatorHostTime
  call date_and_time(values=timeArrayStart)
  startTimeKernel = 1.00000 * timeArrayStart(8) + 1000 * timeArrayStart(7) + 60000 * timeArrayStart(6) + 3600000 * timeArrayStart(5)

  !$OMP PARALLEL DO private (sliceStart,sliceEnd,i1,threadID)
  DO i1 = 0, numberOfThreads - 1, 1
    sliceStart = opSetCore%size * i1 / numberOfThreads
    sliceEnd = opSetCore%size * (i1 + 1) / numberOfThreads
    threadID = omp_get_thread_num()
    CALL s2882256832(opDat1Local,opDat2Local,opDat3Local,sliceStart,sliceEnd)
  END DO
  !$OMP END PARALLEL DO

  call date_and_time(values=timeArrayEnd)
  endTimeKernel = 1.00000 * timeArrayEnd(8) + 1000 * timeArrayEnd(7) + 60000 * timeArrayEnd(6) + 3600000 * timeArrayEnd(5)
  accumulatorKernelTime = endTimeKernel - startTimeKernel
  loopTimeKernelsave_soln_qdim_2320554256 = loopTimeKernelsave_soln_qdim_2320554256 + accumulatorKernelTime

  call date_and_time(values=timeArrayStart)
  startTimeHost = 1.00000 * timeArrayStart(8) + 1000.00 * timeArrayStart(7) + 60000 * timeArrayStart(6) + 3600000 * timeArrayStart(5)

  call date_and_time(values=timeArrayEnd)
  endTimeHost = 1.00000 * timeArrayEnd(8) + 1000 * timeArrayEnd(7) + 60000 * timeArrayEnd(6) + 3600000 * timeArrayEnd(5)

  accumulatorHostTime = endTimeHost - startTimeHost
  loopTimeHostsave_soln_qdim_2320554256 = loopTimeHostsave_soln_qdim_2320554256 + accumulatorHostTime
  returnSetKernelTiming = setKernelTime(4,userSubroutine,accumulatorKernelTime / 1000.00,0.00000,0.00000)
END SUBROUTINE

SUBROUTINE update_modified(qold,q,res,adt,rms)
IMPLICIT NONE
REAL(kind=8), DIMENSION(*) :: qold
REAL(kind=8), DIMENSION(*) :: q
REAL(kind=8), DIMENSION(*) :: res
REAL(kind=8), DIMENSION(*) :: adt
REAL(kind=8), DIMENSION(1) :: rms
REAL(kind=8) :: del,adti
INTEGER(kind=4) :: i
adti = 1.0 / adt(1)

DO i = 1, 4
del = adti * res(i)
q(i) = qold(i) - del
res(i) = 0.0
rms(1) = rms(1) + del * del
END DO

END SUBROUTINE

SUBROUTINE update_kernel( &
  & opDat1, &
  & opDat2, &
  & opDat3, &
  & opDat4, &
  & opDat5, &
  & sliceStart, &
  & sliceEnd)

  IMPLICIT NONE
  REAL(kind=8), DIMENSION(0:*) :: opDat1
  REAL(kind=8), DIMENSION(0:*) :: opDat2
  REAL(kind=8), DIMENSION(0:*) :: opDat3
  REAL(kind=8), DIMENSION(0:*) :: opDat4
  REAL(kind=8) :: opDat5
  INTEGER(kind=4) :: sliceStart
  INTEGER(kind=4) :: sliceEnd
  INTEGER(kind=4) :: i1

  DO i1 = sliceStart, sliceEnd - 1, 1
    CALL update_modified( &
    & opDat1(i1 * 4:i1 * 4 + 4 - 1), &
    & opDat2(i1 * 4:i1 * 4 + 4 - 1), &
    & opDat3(i1 * 4:i1 * 4 + 4 - 1), &
    & opDat4(i1 * 1), &
    & opDat5)
  END DO
END SUBROUTINE

SUBROUTINE update_host( userSubroutine, set, &
  & opArg1, &
  & opArg2, &
  & opArg3, &
  & opArg4, &
  & opArg5 )

  IMPLICIT NONE
  character(len=7), INTENT(IN) :: userSubroutine
  TYPE ( op_set ) , INTENT(IN) :: set

  TYPE ( op_arg ) , INTENT(IN) :: opArg1
  TYPE ( op_arg ) , INTENT(IN) :: opArg2
  TYPE ( op_arg ) , INTENT(IN) :: opArg3
  TYPE ( op_arg ) , INTENT(IN) :: opArg4
  TYPE ( op_arg ) , INTENT(IN) :: opArg5

  TYPE ( op_arg ) , DIMENSION(5) :: opArgArray
  INTEGER(kind=4) :: numberOfOpDats
  INTEGER(kind=4) :: returnMPIHaloExchange
  INTEGER(kind=4) :: returnSetKernelTiming
  TYPE ( op_set_core ) , POINTER :: opSetCore

  TYPE ( op_set_core ) , POINTER :: opSet1Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat1Local
  INTEGER(kind=4) :: opDat1Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet2Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat2Local
  INTEGER(kind=4) :: opDat2Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet3Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat3Local
  INTEGER(kind=4) :: opDat3Cardinality

  TYPE ( op_set_core ) , POINTER :: opSet4Core
  REAL(kind=8), POINTER, DIMENSION(:) :: opDat4Local
  INTEGER(kind=4) :: opDat4Cardinality

  REAL(kind=8), POINTER :: opDat5Local
  INTEGER(kind=4) :: opDat5Cardinality

  TYPE ( op_dat_core ) , POINTER :: opDat1Core
  TYPE ( op_dat_core ) , POINTER :: opDat2Core
  TYPE ( op_dat_core ) , POINTER :: opDat3Core
  TYPE ( op_dat_core ) , POINTER :: opDat4Core
  TYPE ( op_dat_core ) , POINTER :: opDat5Core

  INTEGER(kind=4) :: threadID
  INTEGER(kind=4) :: numberOfThreads
  INTEGER(kind=4) :: sliceStart
  INTEGER(kind=4) :: sliceEnd
  INTEGER(kind=4) :: partitionSize
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayStart
  INTEGER(kind=4), DIMENSION(1:8) :: timeArrayEnd
  REAL(kind=8) :: startTimeHost
  REAL(kind=8) :: endTimeHost
  REAL(kind=8) :: startTimeKernel
  REAL(kind=8) :: endTimeKernel
  REAL(kind=8) :: accumulatorHostTime
  REAL(kind=8) :: accumulatorKernelTime
  INTEGER(kind=4) :: i1
  INTEGER(kind=4) :: i10
  INTEGER(kind=4) :: i11

  REAL(kind=8), DIMENSION(:), ALLOCATABLE :: reductionArrayHost5

  IF (set%setPtr%size .EQ. 0) THEN
    RETURN
  END IF

  numberCalledupdate_2692987784 = numberCalledupdate_2692987784 + 1
  call date_and_time(values=timeArrayStart)
  startTimeHost = 1.00000 * timeArrayStart(8) + &
  & 1000.00 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

#ifdef _OPENMP
    numberOfThreads = omp_get_max_threads()
#else
    numberOfThreads = 1
#endif

  opSetCore => set%setPtr

  opDat1Cardinality = opArg1%dim * getSetSizeFromOpArg(opArg1)
  opDat2Cardinality = opArg2%dim * getSetSizeFromOpArg(opArg2)
  opDat3Cardinality = opArg3%dim * getSetSizeFromOpArg(opArg3)
  opDat4Cardinality = opArg4%dim * getSetSizeFromOpArg(opArg4)
  opDat5Cardinality = opArg5%dim

  CALL c_f_pointer(opArg1%data,opDat1Local,(/opDat1Cardinality/))
  CALL c_f_pointer(opArg2%data,opDat2Local,(/opDat2Cardinality/))
  CALL c_f_pointer(opArg3%data,opDat3Local,(/opDat3Cardinality/))
  CALL c_f_pointer(opArg4%data,opDat4Local,(/opDat4Cardinality/))
  CALL c_f_pointer(opArg5%data,opDat5Local)

  allocate( reductionArrayHost5(numberOfThreads * 1) )
  DO i10 = 1, numberOfThreads, 1
    DO i11 = 1, 1, 1
      reductionArrayHost5((i10 - 1) * 1 + i11) = 0
    END DO
  END DO

  call date_and_time(values=timeArrayEnd)
  endTimeHost = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7) + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorHostTime = endTimeHost - startTimeHost
  loopTimeHostupdate_2692987784 = loopTimeHostupdate_2692987784 + accumulatorHostTime

  call date_and_time(values=timeArrayStart)
  startTimeKernel = 1.00000 * timeArrayStart(8) + &
  & 1000 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

  !$OMP PARALLEL DO private (sliceStart,sliceEnd,i1,threadID)
  DO i1 = 0, numberOfThreads - 1, 1
    sliceStart = opSetCore%size * i1 / numberOfThreads
    sliceEnd = opSetCore%size * (i1 + 1) / numberOfThreads
    threadID = omp_get_thread_num()
    CALL update_kernel( &
    & opDat1Local, &
    & opDat2Local, &
    & opDat3Local, &
    & opDat4Local, &
    & reductionArrayHost5(threadID * 1 + 1), &
    & sliceStart, &
    & sliceEnd)
  END DO
  !$OMP END PARALLEL DO

  call date_and_time(values=timeArrayEnd)
  endTimeKernel = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7) + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorKernelTime = endTimeKernel - startTimeKernel
  loopTimeKernelupdate_2692987784 = loopTimeKernelupdate_2692987784 + accumulatorKernelTime

  call date_and_time(values=timeArrayStart)
  startTimeHost = 1.00000 * timeArrayStart(8) + &
  & 1000.00 * timeArrayStart(7) + &
  & 60000 * timeArrayStart(6) + &
  & 3600000 * timeArrayStart(5)

  DO i10 = 1, numberOfThreads, 1
    DO i11 = 1, 1, 1
      opDat5Local = opDat5Local + reductionArrayHost5((i10 - 1) * 1 + i11)
    END DO
  END DO

  deallocate( reductionArrayHost5 )

  call date_and_time(values=timeArrayEnd)
  endTimeHost = 1.00000 * timeArrayEnd(8) + &
  & 1000 * timeArrayEnd(7) + &
  & 60000 * timeArrayEnd(6) + &
  & 3600000 * timeArrayEnd(5)

  accumulatorHostTime = endTimeHost - startTimeHost
  loopTimeHostupdate_2692987784 = loopTimeHostupdate_2692987784 + accumulatorHostTime
  returnSetKernelTiming = setKernelTime(3,userSubroutine, &
  & accumulatorKernelTime / 1000.00,0.00000,0.00000)
END SUBROUTINE

END MODULE GENERATED_MODULE
