#!/usr/bin/env python
#######################################################################
#                                                                     #
#       This Python routine generates the header file op_seq.h        #
#                                                                     #
#######################################################################


#
# this sets the max number of arguments in op_par_loop
#
maxargs = 10

#open/create file
f = open('./op_openmp_classical.h','w')

#
#first the top bit
#

top =  """
//
// header for sequential and MPI+sequentional execution
//

#include "op_lib_cpp.h"
#include <type_traits>
#include <typeinfo>

static int op2_stride = 1;
#define OP2_STRIDE(arr, idx) arr[idx]
#define COMMA ,
#define TEM(N) T##N
#define TYPE(N) typename std::conditional<std::is_same<TEM(N) COMMA double>::value || std::is_same< TEM(N)  COMMA const double>::value COMMA  double COMMA  typename std::conditional<std::is_same< TEM(N)  COMMA  float>::value || std::is_same< TEM(N)  COMMA const float>::value  COMMA  float COMMA  int>::type>::type

inline void op_args_check(op_set set, int nargs, op_arg *args,
                                      int *ninds, const char *name) {
  for (int n=0; n<nargs; n++)
    op_arg_check(set,n,args[n],ninds,name);
}

struct op_arg_dat_inderect_mapping{
    op_dat dat;
    int index;
    op_map map;
};

extern int nKernels=0;

bool presentInsideInds(op_arg_dat_inderect_mapping *inds,op_arg args, int nargs){

    for (int i = 0; i < nargs; ++i) {
        if(inds[i].dat == args.dat && inds[i].map == args.map)
            return true;
    }

    return false;

}

bool insideConteiner(int *conteiner,int currentsize,int inds){
    for (int i = 0; i < currentsize; ++i) {
        if(conteiner[i] == inds)
            return true;
    }

    return false;

}

"""

f.write(top)

#
# now for op_par_loop defns
#


for nargs in range (1,maxargs+1):
    f.write('//\n')
    f.write('//op_par_loop routine for '+str(nargs)+' arguments\n')
    f.write('//\n')

    n_per_line = 4

    f.write('template <')
    for n in range (0, nargs):
        f.write('class T'+str(n))
        if nargs <> 1 and n != nargs-1:
            f.write(',')
        else:
            f.write('>\n')
        if n%n_per_line == 3 and n <> nargs-1:
            f.write('\n          ')

    f.write('void op_par_loop_omp(void (*kernel)(')
    for n in range (0, nargs):
        f.write('T'+str(n)+'*')
        if nargs <> 1 and n != nargs-1:
            f.write(', ')
        else:
            f.write('),\n')
        if n%n_per_line == 3 and n <> nargs-1:
            f.write('\n                                ')

    f.write('    char const * name, op_set set,\n    ')
    for n in range (0, nargs):
        f.write('op_arg arg'+str(n))
        if nargs <> 1 and n != nargs-1:
            f.write(', ')
        else:
            f.write('){\n')
        if n%n_per_line == 3 and n <> nargs-1:
            f.write('\n    ')

    f.write('  op_arg args['+str(nargs)+'] = {')
    for n in range (0, nargs):
        f.write('arg'+str(n))
        if nargs <> 1 and n != nargs-1:
            f.write(', ')
        else:
            f.write('};\n')
        if n%n_per_line == 3 and n <> nargs-1:
            f.write('\n                    ')



    for n in range (0, nargs):
        f.write('char * arg'+str(n)+'h;\n')
        f.write('if(args['+str(n)+'].argtype == OP_ARG_GBL && args['+str(n)+'].acc != OP_READ)\n')
        f.write('arg'+str(n)+'h = (char *)args['+str(n)+'].data;\n')

    f.write('int nargs = '+str(nargs)+';\n')
    f.write('int ninds = 0; \n')
    f.write('int inds['+str(nargs)+'] = {')
    for n in range (0, nargs):
        f.write('0')
        if nargs <> 1 and n != nargs-1:
            f.write(',')
        else:
            f.write('};\n')


    f.write('int count = 0;\n')
    f.write('int n = -1;\n')
    f.write('op_arg_dat_inderect_mapping indmap[nargs];\n')
    f.write('int indmapSize=0;\n')
    f.write('for (int i = 0; i < '+str(nargs)+'; ++i) {\n')
    f.write(' if(args[i].map == OP_ID){\n')
    f.write('  	inds[count] = -1;\n')
    f.write('   count++;\n')
    f.write(' }else\n')
    f.write('  	if(args[i].map != NULL && args[i].idx != -1)\n')
    f.write('   {\n')
    f.write('  		if(!presentInsideInds(indmap, args[i],indmapSize)){\n')
    f.write('    	n++;\n')
    f.write('   	inds[count] = n;\n')
    f.write('   	indmap[indmapSize].dat = args[i].dat;\n')
    f.write('   	indmap[indmapSize].index = args[i].idx;\n')
    f.write('   	indmap[indmapSize].map = args[i].map; \n')
    f.write('           indmapSize++; \n')
    f.write('   	count++;\n')
    f.write(' 	}else\n')
    f.write(' 		{\n')
    f.write('    	inds[count] = n;\n')
    f.write('    	count++;\n')
    f.write(' 		}\n')
    f.write('	}\n')
    f.write('}\n')

    f.write('int conteiner['+str(nargs)+'];\n')
    f.write('int currentsize=0;\n')
    f.write('for (int i = 0; i < '+str(nargs)+'; ++i)\n')
    f.write('	if(inds[i] != -1)\n')
    f.write('		if(!insideConteiner(conteiner, currentsize, inds[i]))\n')
    f.write(' 		{\n')
    f.write('    	conteiner[currentsize]=inds[i];\n')
    f.write('     	currentsize++;\n')
    f.write('    	ninds++;\n')
    f.write('  		}\n')

    f.write('// check if there is some reduction to do\n')
    f.write('bool reduct = false;\n')
    f.write('for (int i = 0; i < '+str(nargs)+'; ++i)\n')
    f.write(' 	if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)\n')
    f.write('		reduct = true;\n')

    f.write('int nthreads = 0;\n')
    f.write('int set_size;\n')
    f.write('int part_size;\n')
    f.write('if(ninds > 0){\n')
    f.write('	if (OP_diags > 2) {\n')
    f.write('		printf(" kernel routine w/o indirection:  update");\n')
    f.write('	}\n')
    f.write('	set_size = op_mpi_halo_exchanges(set, nargs, args);\n')
    f.write('#ifdef OP_PART_SIZE\n')
    f.write('	part_size = OP_PART_SIZE\n')
    f.write('#else\n')
    f.write('	part_size = OP_part_size;\n')
    f.write('#endif\n')
    f.write('}\n')
    f.write('if(reduct || ninds == 0){\n')
    f.write('#ifdef _OPENMP\n')
    f.write('	nthreads = omp_get_max_threads();\n')
    f.write('#else\n')
    f.write('	nthreads = 1;\n')
    f.write('#endif\n')
    f.write('}\n')

    for n in range (0, nargs):
        f.write('TYPE('+str(n)+')* arg'+str(n)+'_l;\n')
    f.write('if(reduct){\n')
    for n in range (0, nargs):
        f.write('if(args['+str(n)+'].argtype == OP_ARG_GBL && args['+str(n)+'].acc != OP_READ)\n')
        f.write('{\n')
        f.write('arg'+str(n)+'_l= new TYPE('+str(n)+')[nthreads * 64];\n')
        f.write('for (int thr = 0; thr < nthreads; thr++)\n')
        f.write('   for (int d = 0; d < args['+str(n)+'].dim; d++){\n')
        f.write('       if(args['+str(n)+'].acc != OP_INC){\n')
        f.write('           arg'+str(n)+'_l[d + thr * 64] = ZERO_float;\n')
        f.write('       }\n')
        f.write('       else{\n')
        f.write('           arg'+str(n)+'_l[d + thr * 64] = arg'+str(n)+'h[d];\n')
        f.write('       }\n')
        f.write('   }\n')
        f.write('}\n')
    f.write('}\n')
    f.write('if(set->size > 0){\n')
    f.write('	if(ninds > 0)\n')
    f.write('	{\n')
    f.write('		op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);\n')
    f.write('		// execute plan\n')
    f.write('		int block_offset = 0;\n')
    f.write('		for (int col = 0; col < Plan->ncolors; col++) {\n')
    f.write('   		if (col == Plan->ncolors_core) {\n')
    f.write('       			op_mpi_wait_all(nargs, args);\n')
    f.write('   		}\n')
    f.write('	int nblocks = Plan->ncolblk[col];\n')
    f.write('#pragma omp parallel for\n')
    f.write('		for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {\n')
    f.write('  			int blockId = Plan->blkmap[blockIdx + block_offset];\n')
    f.write(' 			int nelem = Plan->nelems[blockId];\n')
    f.write('  			int offset_b = Plan->offset[blockId];\n')
    f.write('  			for (int n = offset_b; n < offset_b + nelem; n++) {\n')
    for n in range (0, nargs):
        f.write('  			T'+str(n)+' *argIndexMap'+str(n)+';\n')
        f.write('   				if(args['+str(n)+'].argtype == OP_ARG_GBL){\n')
        f.write('      					 if(args['+str(n)+'].acc != OP_READ){\n')
#        f.write('          if(omp_get_thread_num() == 0) \n')
#        f.write('          std::cout << " OP_GBL && !OP_READ " << 64*omp_get_thread_num() << std::endl;\n')
        f.write('            					argIndexMap'+str(n)+'= &arg'+str(n)+'_l[64*omp_get_thread_num()];\n')
        f.write('       			}else{\n')
#        f.write('          if(omp_get_thread_num() == 0) \n')
#        f.write('std::cout << " OP_GBL && OP_READ " << '+str(n)+' << std::endl;\n')
        f.write('           				argIndexMap'+str(n)+'= (T'+str(n)+' *)(args['+str(n)+'].data);\n')
        f.write('       			     }\n')
        f.write('   				}else\n')
        f.write('      					if(args['+str(n)+'].map == OP_ID){\n')
#        f.write('          if(omp_get_thread_num() == 0) \n')
#        f.write('std::cout << "OP_ID " << args['+str(n)+'].dim * n << std::endl;\n')
        f.write('               				argIndexMap'+str(n)+'=&((T'+str(n)+' *)arg'+str(n)+'.data)[args['+str(n)+'].dim * n];\n')
        f.write('   					}else\n')
        f.write('           					if(args['+str(n)+'].map != NULL)\n')
        f.write('   						{\n')
#        f.write(' if(omp_get_thread_num() == 0) \n')
#        f.write('std::cout << " !OP_GBL " << args['+str(n)+'].dim * args['+str(n)+'].map_data[(n * args['+str(n)+'].map->dim + args['+str(n)+'].idx)] << std::endl;\n')
        f.write('              						 argIndexMap'+str(n)+'=&((T'+str(n)+' *)arg'+str(n)+'.data)[args['+str(n)+'].dim * args['+str(n)+'].map_data[(n * args['+str(n)+'].map->dim + args['+str(n)+'].idx)]];\n')
        f.write('  						 }\n')
#    f.write(' if(omp_get_thread_num() == 0) \n')
#    f.write('exit(0);\n')
    f.write('				kernel(\n')
    for n in range (0, nargs):
        f.write('					argIndexMap'+str(n))
        if nargs <> 1 and n != nargs-1:
            f.write(',\n')
        else:
            f.write(');\n')

    f.write('       }\n')
    f.write('   }\n')

    f.write('block_offset += nblocks;\n')

    f.write('	if(reduct)\n')
    f.write('	{\n')
    f.write('		if (col == Plan->ncolors_owned-1) {\n')

    for n in range (0, nargs):
        f.write('		if(args['+str(n)+'].argtype == OP_ARG_GBL && args['+str(n)+'].acc != OP_READ)\n')
        f.write('		{\n')
        f.write('   			for (int thr = 0; thr < nthreads; thr++)\n')
        f.write('           			if(args['+str(n)+'].acc == OP_INC)\n')
        f.write('               			for (int d = 0; d < args['+str(n)+'].dim; d++) {\n')
	f.write(' 						TYPE('+str(n)+')* tmp1 =&((TYPE('+str(n)+')*)arg'+str(n)+'h)[d];\n')
        f.write(' 						TYPE('+str(n)+')* tmp2 =&arg'+str(n)+'_l[d+thr*64];\n')
	f.write(' 						*tmp1 += *tmp2;\n')
	f.write(' 					}\n')
#        f.write('                   				arg'+str(n)+'h[d] += arg'+str(n)+'_l[d+thr*64];\n')
        f.write('           		else\n')
        f.write('               		if(args['+str(n)+'].acc == OP_MIN)\n')
        f.write('                  			for (int d = 0; d < args['+str(n)+'].dim; d++){\n')
#        f.write('                       			arg'+str(n)+'h[d]=MIN(arg'+str(n)+'h[d],arg'+str(n)+'_l[d+thr*64]);\n')
	f.write(' 						TYPE('+str(n)+')* tmp1 =&((TYPE('+str(n)+')*)arg'+str(n)+'h)[d];\n')
        f.write(' 						TYPE('+str(n)+')* tmp2 =&arg'+str(n)+'_l[d+thr*64];\n')
	f.write('  						arg'+str(n)+'h[d]= (char) MIN(*tmp1,*tmp2);\n')
	f.write(' 					}\n')
        f.write('               	else\n')
        f.write('                   		if(args['+str(n)+'].acc == OP_MAX)\n')
        f.write('                       		for (int d = 0; d < args['+str(n)+'].dim; d++){\n')
#        f.write('                           			arg'+str(n)+'h[d]=MAX(arg'+str(n)+'h[d],arg'+str(n)+'_l[d+thr*64]);\n')
	f.write(' 						TYPE('+str(n)+')* tmp1 =&((TYPE('+str(n)+')*)arg'+str(n)+'h)[d];\n')
        f.write(' 						TYPE('+str(n)+')* tmp2 =&arg'+str(n)+'_l[d+thr*64];\n')
	f.write(' 						arg'+str(n)+'h[d]= (char) MAX(*tmp1,*tmp2);\n')
	f.write(' 					}\n')
        f.write('                   	else\n')
        f.write('                       	perror("internal error: invalid reduction option");\n')
        f.write('}\n')


    f.write('            }\n')
    f.write('         }//reduct\n')

    f.write('    }\n')

    f.write('	if(set_size==0 || set_size == set->core_size){\n')
    f.write('		op_mpi_wait_all(nargs, args);\n')
    f.write('   }\n')

    f.write('} // ninds > 0\n')
    f.write('else\n')
    f.write('{\n')
    f.write('	// execute plan\n')
    f.write('#pragma omp parallel for\n')
    f.write('	for (int thr = 0; thr < nthreads; thr++) {\n')
    f.write('   	int start = (set->size * thr) / nthreads;\n')
    f.write('   	int finish = (set->size * (thr + 1)) / nthreads;\n')
    f.write('   		for (int n = start; n < finish; n++) {\n')
    for n in range (0, nargs):
        f.write(' 			T'+str(n)+' *argIndexMap'+str(n)+';\n')
        f.write('				if(args['+str(n)+'].argtype == OP_ARG_GBL){\n')
        f.write('  					if(args['+str(n)+'].acc != OP_READ){\n')
        f.write('      						argIndexMap'+str(n)+'= &((T'+str(n)+' *)arg'+str(n)+'_l)[64*omp_get_thread_num()];\n')
        f.write('  				}else{\n')
        f.write('      					argIndexMap'+str(n)+'= (T'+str(n)+' *)(args['+str(n)+'].data);\n')
        f.write('  				     }\n')
        f.write('				}else\n')
        f.write('					if(args['+str(n)+'].map == OP_ID){\n')
        f.write('  						argIndexMap'+str(n)+'=&((T'+str(n)+' *)arg'+str(n)+'.data)[args['+str(n)+'].dim * n];\n')
        f.write('					}else\n')
        f.write('   						if(args['+str(n)+'].map != NULL)\n')
        f.write('						{\n')
        f.write('  							argIndexMap'+str(n)+'=&((T'+str(n)+' *)arg'+str(n)+'.data)[args['+str(n)+'].dim * args['+str(n)+'].map_data[(n * args['+str(n)+'].map->dim + args['+str(n)+'].idx)]];\n')
        f.write('						}\n')

    f.write('			kernel(\n')
    for n in range (0, nargs):
        f.write('				argIndexMap'+str(n))
        if nargs <> 1 and n != nargs-1:
            f.write(',\n')
        else:
            f.write(');\n')

    f.write('       }\n')
    f.write('   }\n')

    for n in range (0, nargs):
        f.write('		if(args['+str(n)+'].argtype == OP_ARG_GBL && args['+str(n)+'].acc != OP_READ)\n')
        f.write('		{\n')
        f.write('   			for (int thr = 0; thr < nthreads; thr++)\n')
        f.write('           			if(args['+str(n)+'].acc == OP_INC)\n')
        f.write('               			for (int d = 0; d < args['+str(n)+'].dim; d++) {\n')
	f.write(' 						TYPE('+str(n)+')* tmp1 =&((TYPE('+str(n)+')*)arg'+str(n)+'h)[d];\n')
	f.write(' 						TYPE('+str(n)+')* tmp2 =&((TYPE('+str(n)+')*)arg'+str(n)+'_l)[d+thr*64];\n')
	f.write(' 						*tmp1 += *tmp2;\n')
	f.write(' 					}\n')
#        f.write('                   				arg'+str(n)+'h[d] += arg'+str(n)+'_l[d+thr*64];\n')
        f.write('           		else\n')
        f.write('               		if(args['+str(n)+'].acc == OP_MIN)\n')
        f.write('                  			for (int d = 0; d < args['+str(n)+'].dim; d++){\n')
#        f.write('                       			arg'+str(n)+'h[d]=MIN(arg'+str(n)+'h[d],arg'+str(n)+'_l[d+thr*64]);\n')
	f.write(' 						TYPE('+str(n)+')* tmp1 =&((TYPE('+str(n)+')*)arg'+str(n)+'h)[d];\n')
	f.write(' 						TYPE('+str(n)+')* tmp2 =&((TYPE('+str(n)+')*)arg'+str(n)+'_l)[d+thr*64];\n')
	f.write('  						arg'+str(n)+'h[d]= (char) MIN(*tmp1,*tmp2);\n')
	f.write(' 					}\n')
        f.write('               	else\n')
        f.write('                   		if(args['+str(n)+'].acc == OP_MAX)\n')
        f.write('                       		for (int d = 0; d < args['+str(n)+'].dim; d++){\n')
#        f.write('                           			arg'+str(n)+'h[d]=MAX(arg'+str(n)+'h[d],arg'+str(n)+'_l[d+thr*64]);\n')
	f.write(' 						TYPE('+str(n)+')* tmp1 =&((TYPE('+str(n)+')*)arg'+str(n)+'h)[d];\n')
	f.write(' 						TYPE('+str(n)+')* tmp2 =&((TYPE('+str(n)+')*)arg'+str(n)+'_l)[d+thr*64];\n')	
	f.write(' 						arg'+str(n)+'h[d]= (char) MAX(*tmp1,*tmp2);\n')
	f.write(' 					}\n')
        f.write('                   	else\n')
        f.write('                       	perror("internal error: invalid reduction option");\n')
        f.write('}\n')


    f.write('}// else of ninds > 0\n')

    f.write('op_mpi_set_dirtybit(nargs, args);\n')
    f.write('}// set->size > 0\n')
    for n in range (0, nargs):
        f.write('if(args['+str(n)+'].argtype == OP_ARG_GBL && args['+str(n)+'].acc != OP_READ){\n')
        f.write('	free(arg'+str(n)+'_l);\n');
        f.write('	}\n');
    f.write('}\n')


f.close()
