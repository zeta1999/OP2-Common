##########################################################################
#
# OpenMP code generator
#
# This routine is called by op2 which parses the input files
#
# It produces a file xxx_kernel.cpp for each kernel,
# plus a master kernel file
#
##########################################################################

import re
import datetime

def comm(line):
  global file_text, FORTRAN, CPP
  global depth
  prefix = ' '*depth
  if len(line) == 0:
    file_text +='\n'
  elif FORTRAN:
    file_text +='!  '+line+'\n'
  elif CPP:
    file_text +=prefix+'//'+line+'\n'

def rep(line,m):
  global dims, idxs, typs, indtyps, inddims

  if FORTRAN:
    if m < len(inddims):
      line = re.sub('INDDIM',str(inddims[m]),line)
      line = re.sub('INDTYP',str(indtyps[m]),line)

    line = re.sub('INDARG','ind_arg'+str(m+1),line)
    line = re.sub('DIM',str(dims[m]),line)
    line = re.sub('ARG','arg'+str(m+1),line)
    line = re.sub('TYP',typs[m],line)
    line = re.sub('IDX',str(int(idxs[m])),line)
  elif CPP:
    line = re.sub('INDDIM',str(inddims[m]),line)
    line = re.sub('INDTYP',str(indtyps[m]),line)

    line = re.sub('INDARG','ind_arg'+str(m),line)
    line = re.sub('DIM',str(dims[m]),line)
    line = re.sub('ARG','arg'+str(m),line)
    line = re.sub('TYP',typs[m],line)
    line = re.sub('IDX',str(int(idxs[m])),line)
  return line

def code(text):
  global file_text, FORTRAN, CPP, g_m
  global depth
  prefix = ' '*depth
  if FORTRAN:
    file_text += prefix+rep(text,g_m)+'\n'
  elif CPP:
    file_text += prefix+rep(text,g_m)+'\n'

def FOR(i,start,finish):
  global file_text, FORTRAN, CPP, g_m
  global depth
  if FORTRAN:
    code('do '+i+' = '+start+', '+finish+'-1')
  elif CPP:
    code('for ( int '+i+'='+start+'; '+i+'<'+finish+'; '+i+'++ ){')
  depth += 2

def ENDFOR():
  global file_text, FORTRAN, CPP, g_m
  global depth
  depth -= 2
  if FORTRAN:
    code('enddo')
  elif CPP:
    code('}')

def IF(line):
  global file_text, FORTRAN, CPP, g_m
  global depth
  if FORTRAN:
    code('if ('+line+') then')
  elif CPP:
    code('if ('+ line + ') {')
  depth += 2

def ENDIF():
  global file_text, FORTRAN, CPP, g_m
  global depth
  depth -= 2
  if FORTRAN:
    code('endif')
  elif CPP:
    code('}')


def op2_gen_openmp(master, date, consts, kernels):

  global dims, idxs, typs, indtyps, inddims
  global FORTRAN, CPP, g_m, file_text, depth

  OP_ID   = 1;  OP_GBL   = 2;  OP_MAP = 3;

  OP_READ = 1;  OP_WRITE = 2;  OP_RW  = 3;
  OP_INC  = 4;  OP_MAX   = 5;  OP_MIN = 6;

  accsstring = ['OP_READ','OP_WRITE','OP_RW','OP_INC','OP_MAX','OP_MIN' ]

  any_soa = 0
  for nk in range (0,len(kernels)):
    any_soa = any_soa or sum(kernels[nk]['soaflags'])

##########################################################################
#  create new kernel file
##########################################################################

  for nk in range (0,len(kernels)):
    name  = kernels[nk]['name']
    nargs = kernels[nk]['nargs']
    dims  = kernels[nk]['dims']
    maps  = kernels[nk]['maps']
    var   = kernels[nk]['var']
    typs  = kernels[nk]['typs']
    accs  = kernels[nk]['accs']
    idxs  = kernels[nk]['idxs']
    inds  = kernels[nk]['inds']
    soaflags = kernels[nk]['soaflags']
    ninds   = kernels[nk]['ninds']
    inddims = kernels[nk]['inddims']
    indaccs = kernels[nk]['indaccs']
    indtyps = kernels[nk]['indtyps']
    invinds = kernels[nk]['invinds']

#
# set two logicals
#
    j = 0
    for i in range(0,nargs):
      if maps[i] == OP_MAP and accs[i] == OP_INC:
        j = i
    ind_inc = j > 0

    j = 0
    for i in range(0,nargs):
      if maps[i] == OP_GBL and accs[i] <> OP_READ:
        j = i
    reduct = j > 0


    FORTRAN = 1;
    CPP     = 0;
    g_m = 0;
    file_text = ''
    depth = 0

##########################################################################
#  Generate Header
##########################################################################

    code('MODULE GENERATED MODULE')
    code('USE OP2_FORTRAN_DECLARATIONS')
    code('USE OP2_FORTRAN_RT_SUPPORT')
    code('USE ISO_C_BINDING')
    code('')
    code('#ifdef _OPENMP'); depth = depth + 2
    code('USE OMP_LIB'); depth = depth - 2
    code('#endif')

##########################################################################
#  Inline user kernel function
##########################################################################
    code('')
    comm('user function')
    code('include '+name+'.inc')
    code('')
    code('')

##########################################################################
#  Generate OpenMP kernel function
##########################################################################
    comm('x86 kernel function')
    code('SUBROUTINE op_x86_'+name+'( &'); depth = depth + 2

    for g_m in range(0,ninds):
      code('&  opDat'+str(invinds[g_m]+1)+',   &')

    for g_m in range(0,nargs):
      if maps[g_m] == OP_ID:
        code('&  opDat'+str(g_m+1)+',   &')

    for g_m in range(0,ninds):
      code('&  ind_maps'+str(invinds[g_m]+1)+', &')

    for g_m in range(0,nargs):
      if maps[g_m] <> OP_ID:
        code('&  mappingArray'+str(g_m+1)+', &')


    code('&  ind_sizes, &')
    code('&  ind_offs,  & ')
    code('&  block_offset,  & ')
    code('&  blkmap,        & ')
    code('&  offset,        & ')
    code('&  nelems,        & ')
    code('&  nthrcol,       & ')
    code('&  thrcol,        & ')
    code('&  blockOffset,   & ')
    code('&  blockID        ) ')
    code('')

    code('IMPLICIT NONE')

    for g_m in range(0,ninds):
      if typs[g_m] == 'double':
        code('REAL(kind=8), dimension(0:*) :: opDat'+str(invinds[g_m]+1))
      elif typs[g_m] == 'int':
        code('INTEGER(kind=4), dimension(0:*) :: opDat'+str(invinds[g_m]+1))

    for g_m in range(0,ninds):
      code('INTEGER(kind=4), dimension(0:), target :: ind_maps'+str(invinds[g_m]+1))

    for g_m in range(0,nargs):
      code('INTEGER(kind=2), dimension(0:*) :: mappingArray'+str(g_m+1))

    code('')

    code('INTEGER(kind=4), dimension(0:*) :: ind_arg_sizes')
    code('INTEGER(kind=4), dimension(0:*) :: ind_arg_offs')
    code('INTEGER(kind=4), dimension(0:*) :: blkmap')
    code('INTEGER(kind=4), dimension(0:*) :: offset')
    code('INTEGER(kind=4), dimension(0:*) :: nelems')
    code('INTEGER(kind=4), dimension(0:*) :: ncolors')
    code('INTEGER(kind=4), dimension(0:*) :: colors')
    code('INTEGER(kind=4) :: blockOffset')
    code('INTEGER(kind=4) :: blockID')
    code('INTEGER(kind=4) :: threadBlockOffset')
    code('INTEGER(kind=4) :: threadBlockID')
    code('INTEGER(kind=4) :: numberOfActiveThreads')
    code('INTEGER(kind=4) :: i1')
    code('INTEGER(kind=4) :: i2')
    code('REAL(kind=8), dimension(0:128000 - 1), target :: sharedFloat8')
    code('')

    for g_m in range(0,ninds):
      code('INTEGER(kind=4), POINTER, dimension(:) :: opDat'+str(invinds[g_m]+1)+'IndirectionMap')
      if typs[g_m] == 'double':
        code('REAL(kind=8), POINTER, dimension(:) :: opDat'+str(invinds[g_m]+1)+'SharedIndirection')

    for g_m in range(0,ninds):
      code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1)+'nBytes')

    for g_m in range(0,ninds):
      code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1)+'RoundUp')
    for g_m in range(0,ninds):
      code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1)+'SharedIndirectionSize ')

    for g_m in range(0,ninds):
      if accs[invinds[g_m]] == OP_INC:
        for m in range (0,int(idxs[g_m])):
          code('REAL(kind=8), dimension(0:3) :: opDat'+str(invinds[g_m]+1+m)+'Local')
          code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1+m)+'Map')


    code('INTEGER(kind=4) :: numOfColours ')
    code('INTEGER(kind=4) :: numberOfActiveThreadsCeiling')
    code('INTEGER(kind=4) :: colour1')
    code('INTEGER(kind=4) :: colour2')
    code('')
    code('threadBlockID = blkmap(blockID + blockOffset)')
    code('numberOfActiveThreads = nelems(threadBlockID)')
    code('threadBlockOffset = offset(threadBlockID)')
    code('numberOfActiveThreadsCeiling = numberOfActiveThreads')
    code('numOfColours = nthrcol(threadBlockID)')
    code('')

    for g_m in range(0,ninds):
      code('opDat'+str(invinds[g_m]+1)+'SharedIndirectionSize = ind_sizes('+str(g_m)+' + threadBlockID * '+inddims[g_m]+')')

    for g_m in range(0,ninds):
      code('opDat'+str(invinds[g_m]+1)+'IndirectionMap => ind_maps1('+str(g_m)+' + threadBlockID * 4):')

    for g_m in range(1,ninds):
      code('opDat'+str(invinds[g_m]+1)+'RoundUp = opDat1SharedIndirectionSize * '+inddims[g_m-1])

    for g_m in range(0,ninds):
      if g_m == 0:
        code('opDat'+str(invinds[g_m]+1)+'nBytes = 0')
      else:
        code('opDat'+str(invinds[g_m]+1)+'nBytes = opDat'+str(invinds[g_m]-1)+'nBytes + opDat'+str(invinds[g_m]+1)+'RoundUp')

    for g_m in range(0,ninds):
      code('opDat'+str(invinds[g_m]+1)+'SharedIndirection => sharedFloat8(opDat'+str(invinds[g_m]+1)+'nBytes:)')
    code('')

    for g_m in range(0,ninds):
      code('DO i1 = 0, opDat'+str(invinds[g_m]+1)+'SharedIndirectionSize - 1, 1')
      code('  DO i2 = 0, '+inddims[g_m]+' - 1, 1')
      if accs[invinds[g_m]] == OP_READ:
        code('    opDat'+str(invinds[g_m]+1)+'SharedIndirection(i2 + i1 * '+inddims[g_m]+\
        ' + 1) = opDat'+str(invinds[g_m]+1)+'(i2 + opDat'+str(invinds[g_m]+1)+\
        'IndirectionMap(i1 + 1) * '+inddims[g_m]+')')
      elif accs[invinds[g_m]] == OP_INC:
        code('    opDat'+str(invinds[g_m]+1)+'SharedIndirection(i2 + i1 * '+inddims[g_m]+\
        ' + 1) = 0')
      code('  END DO')
      code('END DO')
      code('')

    code('DO i1 = 0, numberOfActiveThreadsCeiling - 1, 1')
    code('  colour2 = -1')
    code('  IF (i1 < numberOfActiveThreads) THEN')
    for g_m in range(0,ninds):
      if accs[invinds[g_m]] == OP_INC:
        for m in range (0,int(idxs[g_m])):
          code('    DO i2 = 0, '+inddims[g_m]+' - 1, 1')
          code('      opDat'+str(invinds[g_m]+1+m)+'(i2) = 0')
          code('    END DO')

    #kernel call

    code('')
    code('    CALL '+name+'( &')
    for g_m in range(0,nargs):
      if maps[g_m] == OP_ID:
        if int(dims[g_m]) > 1:
          code('    & opDat'+str(g_m+1)+'((i1 + threadBlockOffset) * '+dims[g_m]+':(i1 + threadBlockOffset) * '+dims[g_m]+' + '+dims[g_m]+' - 1), &')
        else:
          code('    & opDat'+str(g_m+1)+'((i1 + threadBlockOffset) * 1)) &')
      if maps[g_m] == OP_MAP and accs[g_m] == OP_READ:
        if int(dims[g_m]) > 1:
          code('    & opDat'+str(invinds[inds[g_m]-1]+1)+'SharedIndirection(1 + mappingArray'+str(g_m+1)+'(i1 + threadBlockOffset) * '+dims[g_m]+':1 + mappingArray'+str(g_m+1)+'(i1 + threadBlockOffset) * '+dims[g_m]+' + '+dims[g_m]+' - 1), &')
        else:
          code('    & opDat'+str(invinds[inds[g_m]-1]+1)+'SharedIndirection(1 + mappingArray'+str(g_m+1)+'(i1 + threadBlockOffset) * 1), &')
      elif maps[g_m] == OP_MAP and (accs[g_m] == OP_INC or accs[g_m] == OP_RW):
        code('    & opDat'+str(g_m+1)+'Local, &')
    code('    & )')
    code('    colour2 = thrcol(i1 + threadBlockOffset)')
    code('  END IF')

    code('')
    for g_m in range(0,ninds):
      if accs[invinds[g_m]] == OP_INC:
        for m in range (0,int(idxs[g_m])):
          code('  opDat'+str(invinds[g_m]+1+m)+'Map = mappingArray'+str(invinds[g_m]+1+m)+'(i1 + threadBlockOffset)')

    code('')
    code('  DO colour1 = 0, numOfColours - 1, 1')
    code('    IF (colour2 .EQ. colour1) THEN')

    for g_m in range(0,ninds):
      if accs[invinds[g_m]] == OP_INC:
        for m in range (0,int(idxs[g_m])):
          code('      DO i2 = 0, '+dims[g_m]+' - 1, 1')
          code('        opDat'+str(invinds[g_m]+1)+'SharedIndirection(1 + (i2 + opDat'+str(invinds[g_m]+1+m)+'Map * '+dims[g_m]+')) = opDat7SharedIndirection(1 + (i2 + opDat'+str(invinds[g_m]+1+m)+'Map * '+dims[g_m]+')) + opDat'+str(invinds[g_m]+1+m)+'Local(i2)')
          code('      END DO')
          code('')

    code('    END IF')
    code('  END DO')
    code('END DO')
    code('')
    for g_m in range(0,ninds):
      if accs[invinds[g_m]] == OP_INC:
        code('DO i1 = 0, opDat7SharedIndirectionSize - 1, 1')
        code('  DO i2 = 0, '+dims[g_m]+' - 1, 1')
        code('    opDat'+str(invinds[g_m]+1)+'(i2 + opDat'+str(invinds[g_m]+1)+'IndirectionMap(i1 + 1) * '+dims[g_m]+') = opDat'+str(invinds[g_m]+1)+'(i2 + opDat7IndirectionMap(i1 + 1) * '+dims[g_m]+') + opDat'+str(invinds[g_m]+1)+'SharedIndirection(1 + (i2 + i1 * '+dims[g_m]+'))')
        code('  END DO')
        code('END DO')
    depth = depth - 2
    code('END SUBROUTINE')
    code('')

##########################################################################
#  Generate OpenMP hust stub
##########################################################################
    code('SUBROUTINE '+name+'_host( userSubroutine, set, &'); depth = depth + 2
    for g_m in range(0,nargs):
      if g_m == nargs-1:
        code('& opArg'+str(g_m+1)+' )')
      else:
        code('& opArg'+str(g_m+1)+', &')

    code('')
    code('IMPLICIT NONE')
    code('character(len=9), INTENT(IN) :: userSubroutine')
    code('type ( op_set ) , INTENT(IN) :: set')
    code('')

    for g_m in range(0,nargs):
      code('type ( op_arg ) , INTENT(IN) :: opArg'+str(g_m+1))
    code('')

    code('type ( op_arg ) , dimension('+str(nargs)+') :: opArgArray')
    code('INTEGER(kind=4) :: numberOfOpDats')
    code('INTEGER(kind=4) :: returnMPIHaloExchange')
    code('INTEGER(kind=4) :: returnSetKernelTiming')
    code('type ( op_set_core ) , POINTER :: opSetCore')
    code('')

    for g_m in range(0,ninds):
      code('type ( op_set_core ) , POINTER :: opSet'+str(invinds[g_m]+1)+'Core')
      code('REAL(kind=8), POINTER, DIMENSION(:) :: opDat'+str(invinds[g_m]+1)+'Local')
      code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1)+'Cardinality')
      code('')

    for g_m in range(0,nargs):
      code('type ( op_map_core ) , POINTER :: opMap'+str(g_m+1)+'Core')
    code('')
    for g_m in range(0,nargs):
      code('type ( op_dat_core ) , POINTER :: opDat'+str(g_m+1)+'Core')
    code('')

    code('INTEGER(kind=4) :: threadID')
    code('INTEGER(kind=4) :: numberOfThreads')
    code('INTEGER(kind=4) :: partitionSize')
    code('INTEGER(kind=4), dimension(1:'+str(nargs)+') :: opDatArray')
    code('INTEGER(kind=4), dimension(1:'+str(nargs)+') :: mappingIndicesArray')
    code('INTEGER(kind=4), dimension(1:'+str(nargs)+') :: mappingArray')
    code('INTEGER(kind=4), dimension(1:'+str(nargs)+') :: accessDescriptorArray')
    code('INTEGER(kind=4), dimension(1:'+str(nargs)+') :: indirectionDescriptorArray')
    code('INTEGER(kind=4), dimension(1:'+str(nargs)+') :: opDatTypesArray')
    code('INTEGER(kind=4), dimension(1:'+str(nargs)+') :: timeArrayStart')
    code('INTEGER(kind=4), dimension(1:'+str(nargs)+') :: timeArrayEnd')
    code('INTEGER(kind=4) :: numberOfIndirectOpDats')
    code('INTEGER(kind=4) :: blockOffset')
    code('INTEGER(kind=4) :: nblocks')
    code('REAL(kind=8) :: startTimeHost')
    code('REAL(kind=8) :: endTimeHost')
    code('REAL(kind=8) :: startTimeKernel')
    code('REAL(kind=8) :: endTimeKernel')
    code('REAL(kind=8) :: accumulatorHostTime')
    code('REAL(kind=8) :: accumulatorKernelTime')
    code('INTEGER(kind=4) :: i1')
    code('INTEGER(kind=4) :: i2')
    code('')
    code('IF (set%setPtr%size .EQ. 0) THEN')
    code('  RETURN')
    code('END IF')
    code('')

    code('numberCalled'+name+' = numberCalled'+name+'+ 1')
    code('')
    code('call date_and_time(values=timeArrayStart)')
    code('startTimeHost = 1.00000 * timeArrayStart(8) + ')
    code('              & 1000.00 * timeArrayStart(7) + ')
    code('              & 60000 * timeArrayStart(6) + ')
    code('              & 3600000 * timeArrayStart(5)')
    code('')
    depth = depth - 2
    code('#ifdef OP_PART_SIZE_1')
    code('  partitionSize = OP_PART_SIZE_1')
    code('#else')
    code('  partitionSize = 0')
    code('#endif')
    code('')
    code('#ifdef _OPENMP')
    code('  numberOfThreads = omp_get_max_threads()')
    code('#else')
    code('  numberOfThreads = 1')
    code('#endif')
    depth = depth + 2

    code('')
    code('numberOfOpDats = '+str(nargs))
    code('')

    for g_m in range(0,nargs):
      code('opArgArray(1) = opArg'+str(g_m+1)+')')
    code('')
    for g_m in range(0,nargs):
      code('indirectionDescriptorArray('+str(g_m+1)+') = 0')
    code('')

    if ninds > 0:
      code('numberOfIndirectOpDats = '+str(ninds))
    code('')

    code('planRet_'+name+' = FortranPlanCaller( &')
    code('& userSubroutine, &')
    code('& set%setCPtr, &')
    code('& partitionSize, &')
    code('& numberOfOpDats, &')
    code('& opArgArray, &')
    code('& numberOfIndirectOpDats, &')
    code('& indirectionDescriptorArray)')
    code('')
    code('CALL c_f_pointer(planRet_res_calc,actualPlan_'+name+')')
    code('CALL c_f_pointer(actualPlan_'+name+'%nindirect,pnindirect_'+name+',(/numberOfIndirectOpDats/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%ind_maps,ind_maps_'+name+',(/numberOfIndirectOpDats/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%maps,mappingArray_'+name+',(/numberOfOpDats/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%ncolblk,ncolblk_'+name+',(/set%setPtr%size/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%ind_sizes,ind_sizes_'+name+',(/actualPlan_'+name+'%nblocks * numberOfIndirectOpDats/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%ind_offs,ind_offs_'+name+',(/actualPlan_'+name+'%nblocks * numberOfIndirectOpDats/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%blkmap,blkmap_'+name+',(/actualPlan_'+name+'%nblocks/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%offset,offset_'+name+',(/actualPlan_'+name+'%nblocks/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%nelems,nelems_'+name+',(/actualPlan_'+name+'%nblocks/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%nthrcol,nthrcol_'+name+',(/actualPlan_'+name+'%nblocks/))')
    code('CALL c_f_pointer(actualPlan_'+name+'%thrcol,thrcol_'+name+',(/set%setPtr%size/))')
    code('')
    for g_m in range(0,ninds):
      code('CALL c_f_pointer(ind_maps_'+name+'('+str(g_m+1)+'),ind_maps'+str(invinds[g_m]+1)+'_'+name+',(/pnindirect_res_calc('+str(g_m+1)+')/))')
    code('')
    for g_m in range(0,nargs):
      code('IF (indirectionDescriptorArray('+str(g_m+1)+') >= 0) THEN')
      code('  CALL c_f_pointer(mappingArray_res_calc('+str(g_m+1)+'),mappingArray'+str(g_m+1)+'_'+name+',(/set%setPtr%size/))')
      code('END IF')
      code('')
    code('')
    code('opSetCore => set%setPtr')
    code('')
    for g_m in range(0,ninds):
      code('opDat'+str(invinds[g_m]+1)+'Cardinality = opArg'+str(invinds[g_m]+1)+'%dim * getSetSizeFromOpArg(opArg'+str(invinds[g_m]+1)+')')
    code('')
    for g_m in range(0,ninds):
      code('CALL c_f_pointer(opArg'+str(invinds[g_m]+1)+'%data,opDat'+str(invinds[g_m]+1)+'Local,(/opDat1Cardinality/))')
    code('')

    code('call date_and_time(values=timeArrayEnd)')
    code('endTimeHost = 1.00000 * timeArrayEnd(8) + &')
    code('& 1000 * timeArrayEnd(7)  + &')
    code('& 60000 * timeArrayEnd(6) + &')
    code('& 3600000 * timeArrayEnd(5)')
    code('')
    code('accumulatorHostTime = endTimeHost - startTimeHost')
    code('loopTimeHostres_calc_3884922247 = loopTimeHostres_calc_3884922247 + accumulatorHostTime')
    code('')
    code('call date_and_time(values=timeArrayStart)')
    code('startTimeKernel = 1.00000 * timeArrayStart(8) + &')
    code('& 1000 * timeArrayStart(7) + ')
    code('& 60000 * timeArrayStart(6) + &')
    code('& 3600000 * timeArrayStart(5)')
    code('')
    code('blockOffset = 0')
    code('')

    code('DO i1 = 0, actualPlan_'+name+'%ncolors - 1, 1')
    code('  nblocks = ncolblk_'+name+'(i1 + 1)')
    code('  !$OMP PARALLEL DO private (threadID)')
    code('  DO i2 = 0, nblocks - 1, 1')
    code('    threadID = omp_get_thread_num()')
    code('    CALL res_calc_kernel( &')

    for g_m in range(0,ninds):
      code('    & opDat'+str(invinds[g_m]+1)+'Local, &')
    for g_m in range(0,ninds):
      code('    & ind_maps'+str(invinds[g_m]+1)+'_'+name+', &')
    for g_m in range(0,nargs):
      code('    & mappingArray'+str(g_m+1)+'_'+name+', &')

    code('    & ind_sizes_res_calc, &')
    code('    & ind_offs_res_calc, &')
    code('    & blkmap_res_calc, &')
    code('    & offset_res_calc, &')
    code('    & nelems_res_calc, &')
    code('    & nthrcol_res_calc, &')
    code('    & thrcol_res_calc, &')
    code('    & blockOffset,i2)')

    code('  END DO')
    code('  !$OMP END PARALLEL DO')
    code('  blockOffset = blockOffset + nblocks')
    code('END DO')
    code('')

    code('call date_and_time(values=timeArrayEnd)')
    code('endTimeKernel = 1.00000 * timeArrayEnd(8) + &')
    code('& 1000 * timeArrayEnd(7) + &')
    code('& 60000 * timeArrayEnd(6) + &')
    code('& 3600000 * timeArrayEnd(5)')
    code('')
    code('accumulatorKernelTime = endTimeKernel - startTimeKernel')
    code('loopTimeKernel'+name+' = loopTimeKernel'+name+' + accumulatorKernelTime')
    code('')
    code('call date_and_time(values=timeArrayStart)')
    code('startTimeHost = 1.00000 * timeArrayStart(8) + &')
    code('& 1000.00 * timeArrayStart(7) + &')
    code('& 60000 * timeArrayStart(6) + &')
    code('& 3600000 * timeArrayStart(5)')
    code('')
    code('call date_and_time(values=timeArrayEnd)')
    code('endTimeHost = 1.00000 * timeArrayEnd(8) + &')
    code('1000 * timeArrayEnd(7) + &')
    code('60000 * timeArrayEnd(6) + &')
    code('3600000 * timeArrayEnd(5)')
    code('')
    code('accumulatorHostTime = endTimeHost - startTimeHost')
    code('loopTimeHost'+name+' = loopTimeHost'+name+' + accumulatorHostTime')
    code('returnSetKernelTiming = setKernelTime(1 , userSubroutine, &')
    code('& accumulatorKernelTime / 1000.00,actualPlan_'+name+'%transfer,actualPlan_'+name+'%transfer2)')

    depth = depth - 2
    code('END SUBROUTINE')

##########################################################################
#  output individual kernel file
##########################################################################
    fid = open(name+'_kernel.F90','w')
    date = datetime.datetime.now()
    fid.write('//\n// auto-generated by op2.py on '+date.strftime("%Y-%m-%d %H:%M")+'\n//\n\n')
    fid.write(file_text)
    fid.close()
