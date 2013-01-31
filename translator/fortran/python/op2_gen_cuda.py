##########################################################################
#
# CUDA code generator
#
# This routine is called by op2 which parses the input files
#
# It produces a file xxx_kernel.CUF for each kernel,
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
    file_text +='! '+line+'\n'
  elif CPP:
    file_text +=prefix+'//'+line+'\n'

def rep(line,m):
  global dims, idxs, typs, indtyps, inddims

  if FORTRAN:
    if m < len(inddims):
      line = re.sub('INDDIM',str(inddims[m]),line)
      line = re.sub('INDTYP',str(indtyps[m]),line)

    line = re.sub('INDARG','ind_arg'+str(m+1),line)
    line = re.sub('DIMS',str(dims[m]),line)
    line = re.sub('ARG','arg'+str(m+1),line)
    line = re.sub('TYPS',typs[m],line)
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

def code_pre(text):
  global file_text, FORTRAN, CPP, g_m
  if FORTRAN:
    file_text += rep(text,g_m)+'\n'
  elif CPP:
    file_text += rep(text,g_m)+'\n'

def DO(i,start,finish):
  global file_text, FORTRAN, CPP, g_m
  global depth
  if FORTRAN:
    code('DO '+i+' = '+start+', '+finish+' - 1, 1')
  elif CPP:
    code('for ( int '+i+'='+start+'; '+i+'<'+finish+'; '+i+'++ ){')
  depth += 2

def DOWHILE(line):
  global file_text, FORTRAN, CPP, g_m
  global depth
  if FORTRAN:
    code('DO WHILE '+line)
  elif CPP:
    code('while ('+ line+ ' )')
  depth += 2

def FOR(i,start,finish):
  global file_text, FORTRAN, CPP, g_m
  global depth
  if FORTRAN:
    code('DO '+i+' = '+start+', '+finish+' - 1')
  elif CPP:
    code('for ( int '+i+'='+start+'; '+i+'<'+finish+'; '+i+'++ ){')
  depth += 2

def ENDDO():
  global file_text, FORTRAN, CPP, g_m
  global depth
  depth -= 2
  if FORTRAN:
    code('END DO')
  elif CPP:
    code('}')

def ENDFOR():
  global file_text, FORTRAN, CPP, g_m
  global depth
  depth -= 2
  if FORTRAN:
    code('END DO')
  elif CPP:
    code('}')

def IF(line):
  global file_text, FORTRAN, CPP, g_m
  global depth
  if FORTRAN:
    code('IF ('+line+') THEN')
  elif CPP:
    code('if ('+ line + ') {')
  depth += 2

def ENDIF():
  global file_text, FORTRAN, CPP, g_m
  global depth
  depth -= 2
  if FORTRAN:
    code('END IF')
  elif CPP:
    code('}')


def op2_gen_cuda(master, date, consts, kernels):

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

    code('MODULE '+name.upper()+'_MODULE')
    code('USE OP2_FORTRAN_DECLARATIONS')
    code('USE OP2_FORTRAN_RT_SUPPORT')
    code('USE ISO_C_BINDING')
    code('USE OP2_CONSTANTS')
    code('USE CUDAFOR')
    code('USE CUDACONFIGURATIONPARAMS')
    code('')

##########################################################################
#  Variable declarations
##########################################################################



##########################################################################
#  Inline user kernel function
##########################################################################
    code('')
    code('CONTAINS')
    code('')
    comm('user function')
    code('include "'+name+'.inc"')
    code('')
    code('')

##########################################################################
#  Generate CUDA kernel function
##########################################################################
    comm('CUDA kernel function')
    code('attributes (global) SUBROUTINE op_cuda_'+name+'( &'); depth = depth + 2
    code('& opDatDimensions, &')
    code('& opDatCardinalities, &')
    code('& pindSizes, &')
    code('& pindOffs, &')
    code('& pblkMap, &')
    code('& poffset, &')
    code('& pnelems, &')
    code('& pnthrcol, &')
    code('& pthrcol, &')
    code('& blockOffset)')
    code('')
    code('IMPLICIT NONE')
    code('')
    code('TYPE ( adt_calc_opDatDimensions ) , DEVICE :: opDatDimensions')
    code('TYPE ( adt_calc_opDatCardinalities ) , DEVICE :: opDatCardinalities')
    code('INTEGER(kind=4), DIMENSION(0:opDatCardinalities%pindSizesSize - 1), DEVICE :: pindSizes')
    code('INTEGER(kind=4), DIMENSION(0:opDatCardinalities%pindOffsSize - 1), DEVICE :: pindOffs')
    code('INTEGER(kind=4), DIMENSION(0:opDatCardinalities%pblkMapSize - 1), DEVICE :: pblkMap')
    code('INTEGER(kind=4), DIMENSION(0:opDatCardinalities%poffsetSize - 1), DEVICE :: poffset')
    code('INTEGER(kind=4), DIMENSION(0:opDatCardinalities%pnelemsSize - 1), DEVICE :: pnelems')
    code('INTEGER(kind=4), DIMENSION(0:opDatCardinalities%pnthrcolSize - 1), DEVICE :: pnthrcol')
    code('INTEGER(kind=4), DIMENSION(0:opDatCardinalities%pthrcolSize - 1), DEVICE :: pthrcol')
    code('INTEGER(kind=4), VALUE :: blockOffset')
    code('')
    for g_m in range(0,ninds):
      if accs[invinds[g_m]] == OP_INC:
        for m in range (0,int(idxs[g_m])):
          code('REAL(kind=8), DIMENSION(0:3) :: opDat'+str(invinds[g_m]+1+m)+'Local')
          code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1+m)+'Map')
    code('')
    for g_m in range(0,ninds):
      code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1)+'nBytes')
    code('')
    for g_m in range(0,ninds):
      code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1)+'RoundUp')
    code('')
    for g_m in range(0,ninds):
      code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1)+'SharedIndirectionSize')
    code('')
    code('REAL(kind=8), DIMENSION(0:*), SHARED :: sharedFloat8')
    code('INTEGER(kind=4) :: sharedOffsetFloat8')
    code('INTEGER(kind=4), SHARED :: numOfColours')
    code('INTEGER(kind=4), SHARED :: numberOfActiveThreadsCeiling')
    code('INTEGER(kind=4), SHARED :: sharedMemoryOffset')
    code('INTEGER(kind=4), SHARED :: blockID')
    code('INTEGER(kind=4), SHARED :: numberOfActiveThreads')
    code('INTEGER(kind=4) :: moduloResult')
    code('INTEGER(kind=4) :: nbytes')
    code('INTEGER(kind=4) :: colour1')
    code('INTEGER(kind=4) :: colour2')
    code('INTEGER(kind=4) :: n1')
    code('INTEGER(kind=4) :: i1')
    code('INTEGER(kind=4) :: i2')

    IF('threadIdx%x - 1 .EQ. 0')
    code('blockID = pblkMap(blockIdx%x - 1 + blockOffset)')
    code('numberOfActiveThreads = pnelems(blockID)')
    code('numberOfActiveThreadsCeiling = blockDim%x * (1 + (numberOfActiveThreads - 1) / blockDim%x)')
    code('numOfColours = pnthrcol(blockID)')
    code('sharedMemoryOffset = poffset(blockID)')
    code('')
    for g_m in range(0,ninds):
      code('opDat'+str(invinds[g_m]+1)+'SharedIndirectionSize = pindSizes('+str(g_m)+' + blockID * '+str(ninds)+')')
    ENDIF()
    code('')
    code('CALL syncthreads()')
    code('')
    for g_m in range(0,ninds):
      code('opDat'+str(invinds[g_m]+1)+'RoundUp = opDat'+str(invinds[g_m]+1)+'SharedIndirectionSize * opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension')
    code('')
    for g_m in range(0,ninds):
      if g_m == 0:
        code('opDat'+str(invinds[g_m]+1)+'nBytes = 0')
      else:
        code('opDat'+str(invinds[g_m]+1)+'nBytes = opDat'+str(invinds[g_m-1]+1)+'nBytes * 8 / 8 + opDat'+str(invinds[g_m]+1)+'RoundUp * 8 / 8')
    code('')

    for g_m in range(0,ninds):
      code('i1 = threadIdx%x - 1')
      code('n1 = opDat'+str(invinds[g_m]+1)+'SharedIndirectionSize * opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension')
      if accs[invinds[g_m]] == OP_READ:
        DOWHILE('i1 < n1')
        code('moduloResult = mod(i1,opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension)')
        code('sharedFloat8(opDat'+str(invinds[g_m]+1)+'nBytes + i1) = opDat'+str(invinds[g_m]+1)+'Device'+name+'( &')
        code('& moduloResult + ind_maps'+str(invinds[g_m]+1)+'_'+name+'(0 + (pindOffs(1 + blockID * 4) + i1 / &')
        code('& opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension) + 1) * &')
        code('& opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension + 1)')
        code('i1 = i1 + blockDim%x')
        ENDDO()
      elif accs[invinds[g_m]] == OP_INC:
        DOWHILE('i1 < n1')
        code('sharedFloat8(opDat'+str(invinds[g_m]+1)+'nBytes + i1) = 0')
        code('i1 = i1 + blockDim%x')
        ENDDO()
      code('')

    code('CALL syncthreads()')
    code('i1 = threadIdx%x - 1')
    code('')
    DO('i1','0','numberOfActiveThreadsCeiling')
    code('colour2 = -1')
    IF('i1 < numberOfActiveThreads')
    for g_m in range(0,ninds):
      if accs[invinds[g_m]] == OP_INC:
        for m in range (0,int(idxs[g_m])):
          DO('i2','0','opDatDimensions%opDat'+str(invinds[g_m]+1+m)+'Dimension ')
          code('opDat'+str(invinds[g_m]+1+m)+'Local(i2) = 0')
          ENDDO()

##########################################################################
#  CUDA kernel call
##########################################################################
    if ninds > 0: #indirect kernel call
      code('')
      comm('kernel call')
      line = 'CALL '+name+'( &'
      indent = '\n'+' '*depth
      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID:
          line = line + indent + '& sharedFloat8(opDat'+str(invinds[inds[g_m]-1]+1)+'nBytes + mappingArray'+str(g_m+1)+name+'(i1 + sharedMemoryOffset + 1) * opDatDimensions%opDat'+str(g_m+1)+'Dimension)'
        if maps[g_m] == OP_MAP and accs[g_m] == OP_READ:
          line = line + indent + '& sharedFloat8(opDat'+str(invinds[inds[g_m]-1]+1)+'nBytes + mappingArray'+str(g_m+1)+name+'(i1 + sharedMemoryOffset + 1) * opDatDimensions%opDat'+str(g_m+1)+'Dimension)'
        elif maps[g_m] == OP_MAP and (accs[g_m] == OP_INC or accs[g_m] == OP_RW):
          line = line +indent + '& opDat'+str(g_m+1)+'Local'
        if g_m < nargs-1:
          line = line +', &'
        else:
           line = line +' &'
      depth = depth - 2
      code(line + indent + '& )')
      depth = depth + 2
      code('colour2 = pthrcol(i1 + sharedMemoryOffset)')
      ENDIF()

      code('')
      for g_m in range(0,ninds):
        if accs[invinds[g_m]] == OP_INC:
          for m in range (0,int(idxs[g_m])):
            code('opDat'+str(invinds[g_m]+1+m)+'Map = mappingArray'+str(invinds[g_m]+1+m)+'_'+name+'(i1 + sharedMemoryOffset + 1)')
      code('')

      DO('colour1','0','numOfColours')
      IF('colour2 .EQ. colour1')
      for g_m in range(0,ninds):
        if accs[invinds[g_m]] == OP_INC:
          for m in range (0,int(idxs[g_m])):
            DO('i2','0', 'opDatDimensions%opDatinddims'+str(invinds[g_m]+1+m)+'Dimension')
            code('sharedFloat8(opDat'+str(invinds[g_m]+1)+'nBytes + (i2 + opDat'+str(invinds[g_m]+1+m)+'Map * opDatDimensions%opDat'+str(invinds[g_m]+1+m)+'Dimension)) = &')
            code('& sharedFloat8(opDat'+str(invinds[g_m]+1)+'nBytes + (i2 + opDat'+str(invinds[g_m]+1+m)+'Map * opDatDimensions%opDat'+str(invinds[g_m]+1+m)+'Dimension)) + opDat'+str(invinds[g_m]+1+m)+'Local(i2)')
            ENDDO()
            code('')
      ENDIF()
      code('CALL syncthreads()')
      ENDDO()
      code('i1 = i1 + blockDim%x')
      ENDDO()
      code('')
      code('CALL syncthreads()')
      code('i1 = threadIdx%x - 1')
      code('')
      for g_m in range(0,ninds):
        if accs[invinds[g_m]] == OP_INC:
          DOWHILE('i1 < opDat'+str(invinds[g_m]+1)+'SharedIndirectionSize * opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension')
          code('moduloResult = mod(i1,opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension)')
          code('opDat'+str(invinds[g_m]+1)+'Deviceres_calc(moduloResult + ind_maps'+str(invinds[g_m]+1)+'_res_calc &')
          code('& (0 + (pindOffs(3 + blockID * 4) + i1 / opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension) + 1) * &')
          code('& opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension + 1) = &')
          code('& opDat'+str(invinds[g_m]+1)+'Deviceres_calc(moduloResult + ind_maps'+str(invinds[g_m]+1)+'_res_calc &')
          code('& (0 + (pindOffs(3 + blockID * 4) + i1 / opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension) + 1) * &')
          code('& opDatDimensions%opDat'+str(invinds[g_m]+1)+'Dimension + 1) + &')
          code('& sharedFloat8(opDat'+str(invinds[g_m]+1)+'nBytes + i1)')
          ENDDO()

    else: #direct kernel call
      code('')
      comm('kernel call')

    depth = depth - 2
    code('END SUBROUTINE')
    code('')

##########################################################################
#  Generate CUP hust stub
##########################################################################
    code('attributes (host) SUBROUTINE '+name+'_host( userSubroutine, set, &'); depth = depth + 2
    for g_m in range(0,nargs):
      if g_m == nargs-1:
        code('& opArg'+str(g_m+1)+' )')
      else:
        code('& opArg'+str(g_m+1)+', &')

    code('')
    code('IMPLICIT NONE')
    code('character(len='+str(len(name)+1)+'), INTENT(IN) :: userSubroutine')
    code('type ( op_set ) , INTENT(IN) :: set')
    code('')

    for g_m in range(0,nargs):
      code('type ( op_arg ) , INTENT(IN) :: opArg'+str(g_m+1))
    code('')
    code('type ( op_arg ) , DIMENSION('+str(nargs)+') :: opArgArray')
    code('INTEGER(kind=4) :: numberOfOpDats')
    code('INTEGER(kind=4) :: returnMPIHaloExchange')
    code('INTEGER(kind=4) :: returnSetKernelTiming')
    code('')
    code('TYPE ( '+name+'_opDatDimensions ) , DEVICE :: opDatDimensions')
    code('TYPE ( '+name+'_opDatCardinalities ) , DEVICE :: opDatCardinalities')
    code('')
    for g_m in range(0,ninds):
      code('INTEGER(kind=4) :: opDat'+str(invinds[g_m]+1)+'Cardinality')
    code('')
    code('TYPE ( op_plan ) , POINTER :: actualPlan_res_calc')
    code('TYPE ( c_devptr ) , POINTER, DIMENSION(:) :: pindMaps')
    code('TYPE ( c_devptr ) , POINTER, DIMENSION(:) :: pmaps')
    code('')
    code('INTEGER(kind=4) :: pindMapsSize')
    code('INTEGER(kind=4) :: blocksPerGrid')
    code('INTEGER(kind=4) :: threadsPerBlock')
    code('INTEGER(kind=4) :: dynamicSharedMemorySize')
    code('INTEGER(kind=4) :: threadSynchRet')
    code('INTEGER(kind=4), DIMENSION(1:'+str(nargs)+') :: opDatArray')
    code('INTEGER(kind=4), DIMENSION(1:'+str(nargs)+') :: mappingIndicesArray')
    code('INTEGER(kind=4), DIMENSION(1:'+str(nargs)+') :: mappingArray')
    code('INTEGER(kind=4), DIMENSION(1:'+str(nargs)+') :: accessDescriptorArray')
    code('INTEGER(kind=4), DIMENSION(1:'+str(nargs)+') :: indirectionDescriptorArray')
    code('')
    for g_m in range(0,nargs):
      code('INTEGER(kind=4) :: mappingArray'+str(g_m+1)+'Size')
    code('')

    code('INTEGER(kind=4) :: numberOfIndirectOpDats')
    code('INTEGER(kind=4) :: blockOffset')
    code('INTEGER(kind=4) :: pindSizesSize')
    code('INTEGER(kind=4) :: pindOffsSize')
    code('INTEGER(kind=4) :: pblkMapSize')
    code('INTEGER(kind=4) :: poffsetSize')
    code('INTEGER(kind=4) :: pnelemsSize')
    code('INTEGER(kind=4) :: pnthrcolSize')
    code('INTEGER(kind=4) :: pthrcolSize')
    code('INTEGER(kind=4), POINTER, DIMENSION(:) :: ncolblk')
    code('INTEGER(kind=4), POINTER, DIMENSION(:) :: pnindirect')
    code('INTEGER(kind=4), DIMENSION(:), DEVICE, ALLOCATABLE :: pindSizes')
    code('INTEGER(kind=4), DIMENSION(:), DEVICE, ALLOCATABLE :: pindOffs')
    code('INTEGER(kind=4), DIMENSION(:), DEVICE, ALLOCATABLE :: pblkMap')
    code('INTEGER(kind=4), DIMENSION(:), DEVICE, ALLOCATABLE :: poffset')
    code('INTEGER(kind=4), DIMENSION(:), DEVICE, ALLOCATABLE :: pnelems')
    code('INTEGER(kind=4), DIMENSION(:), DEVICE, ALLOCATABLE :: pnthrcol')
    code('INTEGER(kind=4), DIMENSION(:), DEVICE, ALLOCATABLE :: pthrcol')
    code('INTEGER(kind=4) :: partitionSize')
    code('INTEGER(kind=4) :: blockSize')
    code('INTEGER(kind=4) :: i1')
    code('INTEGER(kind=4) :: i2')
    code('INTEGER(kind=4), SAVE :: calledTimes')
    code('INTEGER(kind=4) :: returnDumpOpDat')
    code('TYPE ( cudaEvent )  :: startTimeHost')
    code('TYPE ( cudaEvent )  :: endTimeHost')
    code('TYPE ( cudaEvent )  :: startTimeKernel')
    code('TYPE ( cudaEvent )  :: endTimeKernel')
    code('INTEGER(kind=4) :: istat')
    code('REAL(kind=4) :: accumulatorHostTime')
    code('REAL(kind=4) :: accumulatorKernelTime')
    code('')

    code('numberOfOpDats = '+str(nargs))
    code('')

    for g_m in range(0,nargs):
      code('opArgArray('+str(g_m+1)+') = opArg'+str(g_m+1))
    code('')

    IF('set%setPtr%size .EQ. 0')
    code('RETURN')
    ENDIF()
    code('')

    code('istat = cudaEventCreate(startTimeHost)')
    code('istat = cudaEventCreate(endTimeHost)')
    code('istat = cudaEventCreate(startTimeKernel)')
    code('istat = cudaEventCreate(endTimeKernel)')
    code('')
    code('numberCalled'+name+' = numberCalled'+name+' + 1')
    code('istat = cudaEventRecord(startTimeHost,0)')
    code('')

    for g_m in range(0,nargs):
      code('indirectionDescriptorArray('+str(g_m+1)+') = '+str(inds[g_m]-1))
    code('')

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


    for g_m in range(0,ninds):
      code('opDatCardinalities%opDat'+str(invinds[g_m]+1)+'Cardinality = opArg'+str(invinds[g_m]+1)+'%dim * getSetSizeFromOpArg(opArg'+str(invinds[g_m]+1)+')')
    for g_m in range(0,nargs):
      if maps[g_m] == OP_ID:
        code('opDatCardinalities%opDat'+str(g_m+1)+'Cardinality = opArg'+str(g_m+1)+'%dim * getSetSizeFromOpArg(opArg'+str(g_m+1)+')')
      elif maps[g_m] == OP_GBL:
        code('opDatCardinalities%opDat'+str(g_m+1)+'Cardinality = opArg'+str(g_m+1)+'%dim')

    code('')
    for g_m in range(0,ninds):
      code('opDat'+str(invinds[g_m]+1)+'Cardinality = opArg'+str(invinds[g_m]+1)+'%dim * getSetSizeFromOpArg(opArg'+str(invinds[g_m]+1)+')')
    for g_m in range(0,nargs):
      if maps[g_m] == OP_ID:
        code('opDat'+str(g_m+1)+'Cardinality = opArg'+str(g_m+1)+'%dim * getSetSizeFromOpArg(opArg'+str(g_m+1)+')')
      elif maps[g_m] == OP_GBL:
        code('opDat'+str(g_m+1)+'Cardinality = opArg'+str(g_m+1)+'%dim')

    code('')
    for g_m in range(0,ninds):
      code('CALL c_f_pointer(opArg'+str(invinds[g_m]+1)+'%data_d,opDat'+str(invinds[g_m]+1)+'Device'+name+',(/opDat'+str(invinds[g_m]+1)+'Cardinality/))')
    for g_m in range(0,nargs):
      if maps[g_m] == OP_ID:
        code('CALL c_f_pointer(opArg'+str(g_m+1)+'%data_d,opDat'+str(g_m+1)+'Device'+name+',(/opDat'+str(g_m+1)+'Cardinality/))')
      elif maps[g_m] == OP_GBL:
        code('CALL c_f_pointer(opArg'+str(g_m+1)+'%data_d,opDat'+str(g_m+1)+'Device'+name+')')
    code('')

    code('CALL c_f_pointer(planRet_res_calc,actualPlan_res_calc)')
    code('CALL c_f_pointer(actualPlan_res_calc%ind_maps,pindMaps,(/numberOfIndirectOpDats/))')
    code('CALL c_f_pointer(actualPlan_res_calc%ncolblk,ncolblk,(/set%setPtr%size/))')
    code('')
    code('pindSizesSize = actualPlan_res_calc%nblocks * numberOfIndirectOpDats')
    code('CALL c_f_pointer(actualPlan_res_calc%ind_sizes,pindSizes,(/pindSizesSize/))')
    code('')
    code('pindOffsSize = pindSizesSize')
    code('CALL c_f_pointer(actualPlan_res_calc%ind_offs,pindOffs,(/pindOffsSize/))')
    code('')
    code('pblkMapSize = actualPlan_res_calc%nblocks')
    code('CALL c_f_pointer(actualPlan_res_calc%blkmap,pblkMap,(/pblkMapSize/))')
    code('')
    code('poffsetSize = actualPlan_res_calc%nblocks')
    code('CALL c_f_pointer(actualPlan_res_calc%offset,poffset,(/poffsetSize/))')
    code('')
    code('pnelemsSize = actualPlan_res_calc%nblocks')
    code('CALL c_f_pointer(actualPlan_res_calc%nelems,pnelems,(/pnelemsSize/))')
    code('')
    code('pnthrcolSize = actualPlan_res_calc%nblocks')
    code('CALL c_f_pointer(actualPlan_res_calc%nthrcol,pnthrcol,(/pnthrcolSize/))')
    code('')
    code('pthrcolSize = set%setPtr%size')
    code('CALL c_f_pointer(actualPlan_res_calc%thrcol,pthrcol,(/pthrcolSize/))')
    code('CALL c_f_pointer(actualPlan_res_calc%nindirect,pnindirect,(/numberOfIndirectOpDats/))')
    code('')
    for g_m in range(0,ninds):
      code('CALL c_f_pointer(pindMaps('+str(g_m+1)+'),ind_maps'+str(invinds[g_m]+1)+'_'+name+',pnindirect('+str(g_m+1)+'))')
    code('')

    for g_m in range(0,nargs):
      if maps[g_m] == OP_MAP:
        IF('indirectionDescriptorArray('+str(g_m+1)+') >= 0')
        code('mappingArray'+str(g_m+1)+'Size = set%setPtr%size')
        code('CALL c_f_pointer(pmaps('+str(g_m+1)+'),mappingArray'+str(g_m+1)+'_'+name+',(/mappingArray'+str(g_m+1)+'Size/))')
        ENDIF()
        code('')

    for g_m in range(0,ninds):
      code('opDatCardinalities%ind_maps'+str(invinds[g_m]+1)+'Size = pnindirect('+str(g_m+1)+')')
    code('')
    for g_m in range(0,nargs):
      code('opDatCardinalities%mappingArray'+str(g_m+1)+'Size = mappingArray'+str(g_m+1)+'Size')
    code('')

    code('opDatCardinalities%pblkMapSize = pblkMapSize')
    code('opDatCardinalities%pindOffsSize = pindOffsSize')
    code('opDatCardinalities%pindSizesSize = pindSizesSize')
    code('opDatCardinalities%pnelemsSize = pnelemsSize')
    code('opDatCardinalities%pnthrcolSize = pnthrcolSize')
    code('opDatCardinalities%poffsetSize = poffsetSize')
    code('opDatCardinalities%pthrcolSize = pthrcolSize')
    code('')
    code('istat = cudaEventRecord(endTimeHost,0)')
    code('istat = cudaEventSynchronize(endTimeHost)')
    code('istat = cudaEventElapsedTime(accumulatorHostTime,startTimeHost,endTimeHost)')
    code('')
    code('loopTimeHost'+name+' = loopTimeHost'+name+' + accumulatorHostTime')
    code('istat = cudaEventRecord(startTimeKernel,0)')

    #indirect loop host stub call
    code('blockOffset = 0')
    code('threadsPerBlock = getBlockSize(userSubroutine,set%setPtr%size)')

    DO('i2','0','actualPlan_'+name+'%ncolors')
    code('blocksPerGrid = ncolblk(i2 + 1)')
    code('dynamicSharedMemorySize = actualPlan_'+name+'%nshared')
    code('')
    code('CALL '+name+'_kernel <<<blocksPerGrid,threadsPerBlock,dynamicSharedMemorySize>>> &')
    code('& (opDatDimensions,opDatCardinalities,pindSizes,pindOffs,pblkMap, &')
    code('& poffset,pnelems,pnthrcol,pthrcol,blockOffset)')
    code('')
    code('threadSynchRet = cudaThreadSynchronize()')
    code('blockOffset = blockOffset + blocksPerGrid')
    ENDDO()
    code('')
    code('istat = cudaEventRecord(endTimeKernel,0)')
    code('istat = cudaEventSynchronize(endTimeKernel)')
    code('istat = cudaEventElapsedTime(accumulatorKernelTime,startTimeKernel,endTimeKernel)')
    code('loopTimeKernel'+name+' = loopTimeKernel'+name+' + accumulatorKernelTime')
    code('')
    code('istat = cudaEventRecord(startTimeHost,0)')
    code('istat = cudaEventRecord(endTimeHost,0)')
    code('istat = cudaEventSynchronize(endTimeHost)')
    code('istat = cudaEventElapsedTime(accumulatorHostTime,startTimeHost,endTimeHost)')
    code('loopTimeHost'+name+' = loopTimeHost'+name+' + accumulatorHostTime')
    code('')

    depth = depth - 2
    code('END SUBROUTINE')
    code('END MODULE '+name.upper()+'_MODULE')
##########################################################################
#  output individual kernel file
##########################################################################
    fid = open(name+'_kernel.CUF','w')
    date = datetime.datetime.now()
    fid.write('!\n! auto-generated by op2.py on '+date.strftime("%Y-%m-%d %H:%M")+'\n!\n\n')
    fid.write(file_text)
    fid.close()
