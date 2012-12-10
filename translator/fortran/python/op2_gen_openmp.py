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
			code('&  INDTYP *ind_ARG,   &')
			code('&  int *ind_ARG_maps, &')

		for g_m in range (0,nargs):
			if maps[g_m] == OP_GBL and accs[g_m] == OP_READ:
				# declared const for performance
				code('&  ARG, &')
			elif maps[g_m] == OP_ID and ninds>0:
				code('&  ARG, &')
			elif maps[g_m] == OP_GBL or maps[g_m] == OP_ID:
				code('&  ARG, &')

		code('!-- mappingArray*s -- goes here, ')
		code('&  ind_arg_sizes, &')
		code('&  ind_arg_offs,  & ')
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
		for g_m in range (0,nargs):
			if maps[g_m] == OP_GBL and accs[g_m] == OP_READ:
				# declared const for performance
				code('REAL(kind=8), dimension(0:*) :: ARG, ')
			elif maps[g_m] == OP_ID and ninds>0:
				code('REAL(kind=8), dimension(0:*) :: ARG, ')
			elif maps[g_m] == OP_GBL or maps[g_m] == OP_ID:
				code('REAL(kind=8), dimension(0:*) :: ARG, ')

		for g_m in range(0,ninds):
			code('INTEGER(kind=4), dimension(0:), target :: ind_mapsARG')

		code('!-- INTEGER(kind=2), dimension(0:*) :: mappingArray*s -- goes here, ')
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
			code('INTEGER(kind=4), POINTER, dimension(:) :: opDatARGIndirectionMap')
		for g_m in range(0,ninds):
			code('REAL(kind=8), POINTER, dimension(:) :: opDatARGSharedIndirection')
		for g_m in range(0,ninds):
			code('INTEGER(kind=4) :: opDatARGnBytes')
		for g_m in range(0,ninds):
			code('INTEGER(kind=4) :: opDatARGRoundUp')
		for g_m in range(0,ninds):
			code('INTEGER(kind=4) :: opDatARGSharedIndirectionSize ')

		code('!-- REAL(kind=8), dimension(0:3) :: opDat**Local -- goes here')
		code('!-- INTEGER(kind=4) :: opDat7Map -- goes here')

		code('INTEGER(kind=4) :: numOfColours ')
		code('INTEGER(kind=4) :: numberOfActiveThreadsCeiling')
		code('!-- INTEGER(kind=4) :: colour1 -- goes here')

		code('')
		comm('more declarations here')
		code('')

		code('threadBlockID = blkmap(blockID + blockOffset)')
		code('numberOfActiveThreads = nelems(threadBlockID)')
		code('threadBlockOffset = offset(threadBlockID)')
		code('numberOfActiveThreadsCeiling = numberOfActiveThreads')
		code('numOfColours = nthrcol(threadBlockID)')

		code('')
		#kernel call
		comm('user-supplied kernel call')


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
