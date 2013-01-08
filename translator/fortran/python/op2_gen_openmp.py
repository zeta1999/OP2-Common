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
      code('opDat'+str(invinds[g_m]+1)+'SharedIndirectionSize = ind_sizes('+str(g_m)+' + threadBlockID * 4)')
    code('')

    for g_m in range(0,ninds):
      code('opDat'+str(invinds[g_m]+1)+'IndirectionMap => ind_maps1('+str(g_m)+' + threadBlockID * 4):')
    code('')

    for g_m in range(1,ninds):
      code('opDat'+str(invinds[g_m]+1)+'RoundUp = opDat1SharedIndirectionSize * '+inddims[g_m-1])
    code('')

    for g_m in range(0,ninds):
      if g_m == 0:
        code('opDat'+str(invinds[g_m]+1)+'nBytes = 0')
      else:
        code('opDat'+str(invinds[g_m]+1)+'nBytes = opDat'+str(invinds[g_m]-1)+'nBytes + opDat'+str(invinds[g_m]+1)+'RoundUp')
    code('')

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
    comm('----------------kernel call---------------------')
    code('    CALL '+name+'( &')
    for g_m in range(0,ninds):
      for m in range (0,int(idxs[invinds[g_m]+1])):
        if accs[invinds[g_m]] == OP_READ:
          if int(dims[invinds[g_m]+1]) > 1:
            code('    & opDat'+str(invinds[g_m]+1)+'SharedIndirection(1 +  mappingArray'+str(invinds[g_m]+m+1)+'(i1 + threadBlockOffset) *'+inddims[g_m]+
            ':1 mappingArray'+str(invinds[g_m]+m+1)+'(i1 + threadBlockOffset) * '+dims[invinds[g_m]+1]+' + '+dims[invinds[g_m]+1]+' - 1), &')
          else:
            code('    & opDat'+str(invinds[g_m]+1)+'SharedIndirection(1 +  mappingArray'+str(invinds[g_m]+m+1)+'(i1 + threadBlockOffset) *'+inddims[g_m]+', &')
      if accs[invinds[g_m]] == OP_INC:
        for m in range (0,int(idxs[g_m])):
          code('    & opDat'+str(invinds[g_m]+1+m)+'Local, &')

    for g_m in range(0,nargs):
      if maps[g_m] == OP_ID:
        code('    & opDat'+str(g_m+1)+'((i1 + threadBlockOffset) * 1)) &')

    code('    & )')
    code('    colour2 = thrcol(i1 + threadBlockOffset)')
    code('  END IF')


    code('')
    comm('----------------kernel call alternate---------------------')
    code('    CALL '+name+'( &')
    code('    indaccs[]  '+str(indaccs))
    code('    idx[]  '+str(idxs))
    code('    inds[]  '+str(inds))
    code('    dims[]  '+str(dims))
    code('    invinds[]  '+str(invinds))


    for g_m in range(0,ninds):
      if int(idxs[g_m])>0:
        code('    & opDat'+str(inds[g_m])+'SharedIndirection(1 +  mappingArray'+str(invinds[g_m]+m+1)+'(i1 + threadBlockOffset) *'+dims[g_m]+', &')

    for g_m in range(0,nargs):
      if maps[g_m] == OP_ID:
        if int(dims[g_m]) > 1:
          code('    & opDat'+str(g_m+1)+'((i1 + threadBlockOffset) * '+dims[g_m]+':(i1 + threadBlockOffset) * '+dims[g_m]+' + '+dims[g_m]+' - 1), &')
        else:
          code('    & opDat'+str(g_m+1)+'((i1 + threadBlockOffset) * 1)) &')


    code('')
    for g_m in range(0,ninds):
      if accs[invinds[g_m]] == OP_INC:
        for m in range (0,int(idxs[g_m])):
          code('  opDat'+str(invinds[g_m]+1+m)+'Map = mappingArray'+str(invinds[g_m]+1+m)+'(i1 + threadBlockOffset)')

    code('')
    code('  DO colour1 = 0, numOfColours - 1, 1')
    code('    IF (colour2 .EQ. colour1) THEN')
    code('      ! More content to be generated here')
    code('    END IF')
    code('  END DO')
    code('END DO')
    code('')
    comm('More content to be generated here')
    depth = depth - 2
    code('END SUBROUTINE')
    code('')

##########################################################################
#  Generate OpenMP hust stub
##########################################################################
    code('SUBROUTINE '+name+'_host( userSubroutine, set, &'); depth = depth + 2
    for g_m in range(0,ninds):
      code('opArg'+str(g_m)+', &')
    code('& ) ')


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
