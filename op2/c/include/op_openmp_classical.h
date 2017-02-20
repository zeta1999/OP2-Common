
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

//
//op_par_loop routine for 1 arguments
//
template <class T0>
void op_par_loop(void (*kernel)(T0*),
                 char const * name, op_set set,
                 op_arg arg0){
    op_arg args[1] = {arg0};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    int nargs = 1;
    int ninds = 0;
    int inds[1] = {0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 1; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[1];
    int currentsize=0;
    for (int i = 0; i < 1; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 1; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];
#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        kernel(
                                    argIndexMap0);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    kernel(
                                argIndexMap0);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
}
//
//op_par_loop routine for 2 arguments
//
template <class T0,class T1>
void op_par_loop(void (*kernel)(T0*, T1*),
                 char const * name, op_set set,
                 op_arg arg0, op_arg arg1){
    op_arg args[2] = {arg0, arg1};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    char * arg1h;
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        arg1h = (char *)args[1].data;
    int nargs = 2;
    int ninds = 0;
    int inds[2] = {0,0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 2; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[2];
    int currentsize=0;
    for (int i = 0; i < 2; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 2; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    TYPE(1)* arg1_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        {
            arg1_l= new TYPE(1)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[1].dim; d++){
                    if(args[1].acc != OP_INC){
                        arg1_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg1_l[d + thr * 64] = arg1h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];
#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        T1 *argIndexMap1;
                        if(args[1].argtype == OP_ARG_GBL){
                            if(args[1].acc != OP_READ){
                                argIndexMap1= &arg1_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap1= (T1 *)(args[1].data);
                            }
                        }else
                            if(args[1].map == OP_ID){
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                            }else
                                if(args[1].map != NULL)
                                {
                                    argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                                }
                        kernel(
                                    argIndexMap0,
                                    argIndexMap1);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[1].acc == OP_INC)
                                    for (int d = 0; d < args[1].dim; d++) {
                                        TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                        TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[1].acc == OP_MIN)
                                        for (int d = 0; d < args[1].dim; d++){
                                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                            TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                            arg1h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[1].acc == OP_MAX)
                                            for (int d = 0; d < args[1].dim; d++){
                                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                                TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                                arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    T1 *argIndexMap1;
                    if(args[1].argtype == OP_ARG_GBL){
                        if(args[1].acc != OP_READ){
                            argIndexMap1= &((T1 *)arg1_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap1= (T1 *)(args[1].data);
                        }
                    }else
                        if(args[1].map == OP_ID){
                            argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                        }else
                            if(args[1].map != NULL)
                            {
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                            }
                    kernel(
                                argIndexMap0,
                                argIndexMap1);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[1].acc == OP_INC)
                        for (int d = 0; d < args[1].dim; d++) {
                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                            TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[1].acc == OP_MIN)
                            for (int d = 0; d < args[1].dim; d++){
                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                arg1h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[1].acc == OP_MAX)
                                for (int d = 0; d < args[1].dim; d++){
                                    TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                    TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                    arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ){
        free(arg1_l);
    }
}
//
//op_par_loop routine for 3 arguments
//
template <class T0,class T1,class T2>
void op_par_loop(void (*kernel)(T0*, T1*, T2*),
                 char const * name, op_set set,
                 op_arg arg0, op_arg arg1, op_arg arg2){
    op_arg args[3] = {arg0, arg1, arg2};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    char * arg1h;
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        arg1h = (char *)args[1].data;
    char * arg2h;
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        arg2h = (char *)args[2].data;
    int nargs = 3;
    int ninds = 0;
    int inds[3] = {0,0,0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 3; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[3];
    int currentsize=0;
    for (int i = 0; i < 3; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 3; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    TYPE(1)* arg1_l;
    TYPE(2)* arg2_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        {
            arg1_l= new TYPE(1)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[1].dim; d++){
                    if(args[1].acc != OP_INC){
                        arg1_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg1_l[d + thr * 64] = arg1h[d];
                    }
                }
        }
        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        {
            arg2_l= new TYPE(2)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[2].dim; d++){
                    if(args[2].acc != OP_INC){
                        arg2_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg2_l[d + thr * 64] = arg2h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];
#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        T1 *argIndexMap1;
                        if(args[1].argtype == OP_ARG_GBL){
                            if(args[1].acc != OP_READ){
                                argIndexMap1= &arg1_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap1= (T1 *)(args[1].data);
                            }
                        }else
                            if(args[1].map == OP_ID){
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                            }else
                                if(args[1].map != NULL)
                                {
                                    argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                                }
                        T2 *argIndexMap2;
                        if(args[2].argtype == OP_ARG_GBL){
                            if(args[2].acc != OP_READ){
                                argIndexMap2= &arg2_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap2= (T2 *)(args[2].data);
                            }
                        }else
                            if(args[2].map == OP_ID){
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                            }else
                                if(args[2].map != NULL)
                                {
                                    argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                                }
                        kernel(
                                    argIndexMap0,
                                    argIndexMap1,
                                    argIndexMap2);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[1].acc == OP_INC)
                                    for (int d = 0; d < args[1].dim; d++) {
                                        TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                        TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[1].acc == OP_MIN)
                                        for (int d = 0; d < args[1].dim; d++){
                                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                            TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                            arg1h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[1].acc == OP_MAX)
                                            for (int d = 0; d < args[1].dim; d++){
                                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                                TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                                arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[2].acc == OP_INC)
                                    for (int d = 0; d < args[2].dim; d++) {
                                        TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                        TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[2].acc == OP_MIN)
                                        for (int d = 0; d < args[2].dim; d++){
                                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                            TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                            arg2h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[2].acc == OP_MAX)
                                            for (int d = 0; d < args[2].dim; d++){
                                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                                TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                                arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    T1 *argIndexMap1;
                    if(args[1].argtype == OP_ARG_GBL){
                        if(args[1].acc != OP_READ){
                            argIndexMap1= &((T1 *)arg1_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap1= (T1 *)(args[1].data);
                        }
                    }else
                        if(args[1].map == OP_ID){
                            argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                        }else
                            if(args[1].map != NULL)
                            {
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                            }
                    T2 *argIndexMap2;
                    if(args[2].argtype == OP_ARG_GBL){
                        if(args[2].acc != OP_READ){
                            argIndexMap2= &((T2 *)arg2_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap2= (T2 *)(args[2].data);
                        }
                    }else
                        if(args[2].map == OP_ID){
                            argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                        }else
                            if(args[2].map != NULL)
                            {
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                            }
                    kernel(
                                argIndexMap0,
                                argIndexMap1,
                                argIndexMap2);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[1].acc == OP_INC)
                        for (int d = 0; d < args[1].dim; d++) {
                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                            TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[1].acc == OP_MIN)
                            for (int d = 0; d < args[1].dim; d++){
                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                arg1h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[1].acc == OP_MAX)
                                for (int d = 0; d < args[1].dim; d++){
                                    TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                    TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                    arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[2].acc == OP_INC)
                        for (int d = 0; d < args[2].dim; d++) {
                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                            TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[2].acc == OP_MIN)
                            for (int d = 0; d < args[2].dim; d++){
                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                arg2h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[2].acc == OP_MAX)
                                for (int d = 0; d < args[2].dim; d++){
                                    TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                    TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                    arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ){
        free(arg1_l);
    }
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ){
        free(arg2_l);
    }
}
//
//op_par_loop routine for 4 arguments
//
template <class T0,class T1,class T2,class T3>
void op_par_loop(void (*kernel)(T0*, T1*, T2*, T3*),
                 char const * name, op_set set,
                 op_arg arg0, op_arg arg1, op_arg arg2, op_arg arg3){
    op_arg args[4] = {arg0, arg1, arg2, arg3};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    char * arg1h;
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        arg1h = (char *)args[1].data;
    char * arg2h;
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        arg2h = (char *)args[2].data;
    char * arg3h;
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        arg3h = (char *)args[3].data;
    int nargs = 4;
    int ninds = 0;
    int inds[4] = {0,0,0,0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 4; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[4];
    int currentsize=0;
    for (int i = 0; i < 4; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 4; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    TYPE(1)* arg1_l;
    TYPE(2)* arg2_l;
    TYPE(3)* arg3_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        {
            arg1_l= new TYPE(1)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[1].dim; d++){
                    if(args[1].acc != OP_INC){
                        arg1_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg1_l[d + thr * 64] = arg1h[d];
                    }
                }
        }
        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        {
            arg2_l= new TYPE(2)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[2].dim; d++){
                    if(args[2].acc != OP_INC){
                        arg2_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg2_l[d + thr * 64] = arg2h[d];
                    }
                }
        }
        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        {
            arg3_l= new TYPE(3)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[3].dim; d++){
                    if(args[3].acc != OP_INC){
                        arg3_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg3_l[d + thr * 64] = arg3h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];
#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        T1 *argIndexMap1;
                        if(args[1].argtype == OP_ARG_GBL){
                            if(args[1].acc != OP_READ){
                                argIndexMap1= &arg1_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap1= (T1 *)(args[1].data);
                            }
                        }else
                            if(args[1].map == OP_ID){
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                            }else
                                if(args[1].map != NULL)
                                {
                                    argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                                }
                        T2 *argIndexMap2;
                        if(args[2].argtype == OP_ARG_GBL){
                            if(args[2].acc != OP_READ){
                                argIndexMap2= &arg2_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap2= (T2 *)(args[2].data);
                            }
                        }else
                            if(args[2].map == OP_ID){
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                            }else
                                if(args[2].map != NULL)
                                {
                                    argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                                }
                        T3 *argIndexMap3;
                        if(args[3].argtype == OP_ARG_GBL){
                            if(args[3].acc != OP_READ){
                                argIndexMap3= &arg3_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap3= (T3 *)(args[3].data);
                            }
                        }else
                            if(args[3].map == OP_ID){
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                            }else
                                if(args[3].map != NULL)
                                {
                                    argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                                }
                        kernel(
                                    argIndexMap0,
                                    argIndexMap1,
                                    argIndexMap2,
                                    argIndexMap3);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[1].acc == OP_INC)
                                    for (int d = 0; d < args[1].dim; d++) {
                                        TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                        TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[1].acc == OP_MIN)
                                        for (int d = 0; d < args[1].dim; d++){
                                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                            TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                            arg1h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[1].acc == OP_MAX)
                                            for (int d = 0; d < args[1].dim; d++){
                                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                                TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                                arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[2].acc == OP_INC)
                                    for (int d = 0; d < args[2].dim; d++) {
                                        TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                        TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[2].acc == OP_MIN)
                                        for (int d = 0; d < args[2].dim; d++){
                                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                            TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                            arg2h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[2].acc == OP_MAX)
                                            for (int d = 0; d < args[2].dim; d++){
                                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                                TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                                arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[3].acc == OP_INC)
                                    for (int d = 0; d < args[3].dim; d++) {
                                        TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                        TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[3].acc == OP_MIN)
                                        for (int d = 0; d < args[3].dim; d++){
                                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                            TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                            arg3h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[3].acc == OP_MAX)
                                            for (int d = 0; d < args[3].dim; d++){
                                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                                TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                                arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    T1 *argIndexMap1;
                    if(args[1].argtype == OP_ARG_GBL){
                        if(args[1].acc != OP_READ){
                            argIndexMap1= &((T1 *)arg1_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap1= (T1 *)(args[1].data);
                        }
                    }else
                        if(args[1].map == OP_ID){
                            argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                        }else
                            if(args[1].map != NULL)
                            {
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                            }
                    T2 *argIndexMap2;
                    if(args[2].argtype == OP_ARG_GBL){
                        if(args[2].acc != OP_READ){
                            argIndexMap2= &((T2 *)arg2_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap2= (T2 *)(args[2].data);
                        }
                    }else
                        if(args[2].map == OP_ID){
                            argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                        }else
                            if(args[2].map != NULL)
                            {
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                            }
                    T3 *argIndexMap3;
                    if(args[3].argtype == OP_ARG_GBL){
                        if(args[3].acc != OP_READ){
                            argIndexMap3= &((T3 *)arg3_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap3= (T3 *)(args[3].data);
                        }
                    }else
                        if(args[3].map == OP_ID){
                            argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                        }else
                            if(args[3].map != NULL)
                            {
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                            }
                    kernel(
                                argIndexMap0,
                                argIndexMap1,
                                argIndexMap2,
                                argIndexMap3);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[1].acc == OP_INC)
                        for (int d = 0; d < args[1].dim; d++) {
                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                            TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[1].acc == OP_MIN)
                            for (int d = 0; d < args[1].dim; d++){
                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                arg1h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[1].acc == OP_MAX)
                                for (int d = 0; d < args[1].dim; d++){
                                    TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                    TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                    arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[2].acc == OP_INC)
                        for (int d = 0; d < args[2].dim; d++) {
                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                            TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[2].acc == OP_MIN)
                            for (int d = 0; d < args[2].dim; d++){
                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                arg2h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[2].acc == OP_MAX)
                                for (int d = 0; d < args[2].dim; d++){
                                    TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                    TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                    arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[3].acc == OP_INC)
                        for (int d = 0; d < args[3].dim; d++) {
                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                            TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[3].acc == OP_MIN)
                            for (int d = 0; d < args[3].dim; d++){
                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                arg3h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[3].acc == OP_MAX)
                                for (int d = 0; d < args[3].dim; d++){
                                    TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                    TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                    arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ){
        free(arg1_l);
    }
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ){
        free(arg2_l);
    }
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ){
        free(arg3_l);
    }
}
//
//op_par_loop routine for 5 arguments
//
template <class T0,class T1,class T2,class T3,
          class T4>
void op_par_loop(void (*kernel)(T0*, T1*, T2*, T3*, 
                                T4*),
                 char const * name, op_set set,
                 op_arg arg0, op_arg arg1, op_arg arg2, op_arg arg3,
                 op_arg arg4){
    op_arg args[5] = {arg0, arg1, arg2, arg3,
                      arg4};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    char * arg1h;
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        arg1h = (char *)args[1].data;
    char * arg2h;
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        arg2h = (char *)args[2].data;
    char * arg3h;
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        arg3h = (char *)args[3].data;
    char * arg4h;
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        arg4h = (char *)args[4].data;
    int nargs = 5;
    int ninds = 0;
    int inds[5] = {0,0,0,0,0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 5; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[5];
    int currentsize=0;
    for (int i = 0; i < 5; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 5; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    TYPE(1)* arg1_l;
    TYPE(2)* arg2_l;
    TYPE(3)* arg3_l;
    TYPE(4)* arg4_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        {
            arg1_l= new TYPE(1)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[1].dim; d++){
                    if(args[1].acc != OP_INC){
                        arg1_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg1_l[d + thr * 64] = arg1h[d];
                    }
                }
        }
        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        {
            arg2_l= new TYPE(2)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[2].dim; d++){
                    if(args[2].acc != OP_INC){
                        arg2_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg2_l[d + thr * 64] = arg2h[d];
                    }
                }
        }
        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        {
            arg3_l= new TYPE(3)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[3].dim; d++){
                    if(args[3].acc != OP_INC){
                        arg3_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg3_l[d + thr * 64] = arg3h[d];
                    }
                }
        }
        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        {
            arg4_l= new TYPE(4)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[4].dim; d++){
                    if(args[4].acc != OP_INC){
                        arg4_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg4_l[d + thr * 64] = arg4h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];
#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        T1 *argIndexMap1;
                        if(args[1].argtype == OP_ARG_GBL){
                            if(args[1].acc != OP_READ){
                                argIndexMap1= &arg1_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap1= (T1 *)(args[1].data);
                            }
                        }else
                            if(args[1].map == OP_ID){
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                            }else
                                if(args[1].map != NULL)
                                {
                                    argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                                }
                        T2 *argIndexMap2;
                        if(args[2].argtype == OP_ARG_GBL){
                            if(args[2].acc != OP_READ){
                                argIndexMap2= &arg2_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap2= (T2 *)(args[2].data);
                            }
                        }else
                            if(args[2].map == OP_ID){
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                            }else
                                if(args[2].map != NULL)
                                {
                                    argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                                }
                        T3 *argIndexMap3;
                        if(args[3].argtype == OP_ARG_GBL){
                            if(args[3].acc != OP_READ){
                                argIndexMap3= &arg3_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap3= (T3 *)(args[3].data);
                            }
                        }else
                            if(args[3].map == OP_ID){
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                            }else
                                if(args[3].map != NULL)
                                {
                                    argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                                }
                        T4 *argIndexMap4;
                        if(args[4].argtype == OP_ARG_GBL){
                            if(args[4].acc != OP_READ){
                                argIndexMap4= &arg4_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap4= (T4 *)(args[4].data);
                            }
                        }else
                            if(args[4].map == OP_ID){
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                            }else
                                if(args[4].map != NULL)
                                {
                                    argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                                }
                        kernel(
                                    argIndexMap0,
                                    argIndexMap1,
                                    argIndexMap2,
                                    argIndexMap3,
                                    argIndexMap4);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[1].acc == OP_INC)
                                    for (int d = 0; d < args[1].dim; d++) {
                                        TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                        TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[1].acc == OP_MIN)
                                        for (int d = 0; d < args[1].dim; d++){
                                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                            TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                            arg1h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[1].acc == OP_MAX)
                                            for (int d = 0; d < args[1].dim; d++){
                                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                                TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                                arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[2].acc == OP_INC)
                                    for (int d = 0; d < args[2].dim; d++) {
                                        TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                        TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[2].acc == OP_MIN)
                                        for (int d = 0; d < args[2].dim; d++){
                                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                            TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                            arg2h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[2].acc == OP_MAX)
                                            for (int d = 0; d < args[2].dim; d++){
                                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                                TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                                arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[3].acc == OP_INC)
                                    for (int d = 0; d < args[3].dim; d++) {
                                        TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                        TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[3].acc == OP_MIN)
                                        for (int d = 0; d < args[3].dim; d++){
                                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                            TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                            arg3h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[3].acc == OP_MAX)
                                            for (int d = 0; d < args[3].dim; d++){
                                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                                TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                                arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[4].acc == OP_INC)
                                    for (int d = 0; d < args[4].dim; d++) {
                                        TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                        TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[4].acc == OP_MIN)
                                        for (int d = 0; d < args[4].dim; d++){
                                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                            TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                            arg4h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[4].acc == OP_MAX)
                                            for (int d = 0; d < args[4].dim; d++){
                                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                                TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                                arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    T1 *argIndexMap1;
                    if(args[1].argtype == OP_ARG_GBL){
                        if(args[1].acc != OP_READ){
                            argIndexMap1= &((T1 *)arg1_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap1= (T1 *)(args[1].data);
                        }
                    }else
                        if(args[1].map == OP_ID){
                            argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                        }else
                            if(args[1].map != NULL)
                            {
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                            }
                    T2 *argIndexMap2;
                    if(args[2].argtype == OP_ARG_GBL){
                        if(args[2].acc != OP_READ){
                            argIndexMap2= &((T2 *)arg2_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap2= (T2 *)(args[2].data);
                        }
                    }else
                        if(args[2].map == OP_ID){
                            argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                        }else
                            if(args[2].map != NULL)
                            {
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                            }
                    T3 *argIndexMap3;
                    if(args[3].argtype == OP_ARG_GBL){
                        if(args[3].acc != OP_READ){
                            argIndexMap3= &((T3 *)arg3_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap3= (T3 *)(args[3].data);
                        }
                    }else
                        if(args[3].map == OP_ID){
                            argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                        }else
                            if(args[3].map != NULL)
                            {
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                            }
                    T4 *argIndexMap4;
                    if(args[4].argtype == OP_ARG_GBL){
                        if(args[4].acc != OP_READ){
                            argIndexMap4= &((T4 *)arg4_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap4= (T4 *)(args[4].data);
                        }
                    }else
                        if(args[4].map == OP_ID){
                            argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                        }else
                            if(args[4].map != NULL)
                            {
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                            }
                    kernel(
                                argIndexMap0,
                                argIndexMap1,
                                argIndexMap2,
                                argIndexMap3,
                                argIndexMap4);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[1].acc == OP_INC)
                        for (int d = 0; d < args[1].dim; d++) {
                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                            TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[1].acc == OP_MIN)
                            for (int d = 0; d < args[1].dim; d++){
                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                arg1h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[1].acc == OP_MAX)
                                for (int d = 0; d < args[1].dim; d++){
                                    TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                    TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                    arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[2].acc == OP_INC)
                        for (int d = 0; d < args[2].dim; d++) {
                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                            TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[2].acc == OP_MIN)
                            for (int d = 0; d < args[2].dim; d++){
                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                arg2h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[2].acc == OP_MAX)
                                for (int d = 0; d < args[2].dim; d++){
                                    TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                    TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                    arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[3].acc == OP_INC)
                        for (int d = 0; d < args[3].dim; d++) {
                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                            TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[3].acc == OP_MIN)
                            for (int d = 0; d < args[3].dim; d++){
                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                arg3h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[3].acc == OP_MAX)
                                for (int d = 0; d < args[3].dim; d++){
                                    TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                    TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                    arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[4].acc == OP_INC)
                        for (int d = 0; d < args[4].dim; d++) {
                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                            TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[4].acc == OP_MIN)
                            for (int d = 0; d < args[4].dim; d++){
                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                arg4h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[4].acc == OP_MAX)
                                for (int d = 0; d < args[4].dim; d++){
                                    TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                    TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                    arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ){
        free(arg1_l);
    }
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ){
        free(arg2_l);
    }
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ){
        free(arg3_l);
    }
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ){
        free(arg4_l);
    }
}
//
//op_par_loop routine for 6 arguments
//
template <class T0,class T1,class T2,class T3,
          class T4,class T5>
void op_par_loop(void (*kernel)(T0*, T1*, T2*, T3*, 
                                T4*, T5*),
                 char const * name, op_set set,
                 op_arg arg0, op_arg arg1, op_arg arg2, op_arg arg3,
                 op_arg arg4, op_arg arg5){
    op_arg args[6] = {arg0, arg1, arg2, arg3,
                      arg4, arg5};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    char * arg1h;
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        arg1h = (char *)args[1].data;
    char * arg2h;
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        arg2h = (char *)args[2].data;
    char * arg3h;
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        arg3h = (char *)args[3].data;
    char * arg4h;
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        arg4h = (char *)args[4].data;
    char * arg5h;
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        arg5h = (char *)args[5].data;
    int nargs = 6;
    int ninds = 0;
    int inds[6] = {0,0,0,0,0,0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 6; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[6];
    int currentsize=0;
    for (int i = 0; i < 6; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 6; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    TYPE(1)* arg1_l;
    TYPE(2)* arg2_l;
    TYPE(3)* arg3_l;
    TYPE(4)* arg4_l;
    TYPE(5)* arg5_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        {
            arg1_l= new TYPE(1)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[1].dim; d++){
                    if(args[1].acc != OP_INC){
                        arg1_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg1_l[d + thr * 64] = arg1h[d];
                    }
                }
        }
        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        {
            arg2_l= new TYPE(2)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[2].dim; d++){
                    if(args[2].acc != OP_INC){
                        arg2_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg2_l[d + thr * 64] = arg2h[d];
                    }
                }
        }
        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        {
            arg3_l= new TYPE(3)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[3].dim; d++){
                    if(args[3].acc != OP_INC){
                        arg3_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg3_l[d + thr * 64] = arg3h[d];
                    }
                }
        }
        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        {
            arg4_l= new TYPE(4)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[4].dim; d++){
                    if(args[4].acc != OP_INC){
                        arg4_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg4_l[d + thr * 64] = arg4h[d];
                    }
                }
        }
        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        {
            arg5_l= new TYPE(5)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[5].dim; d++){
                    if(args[5].acc != OP_INC){
                        arg5_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg5_l[d + thr * 64] = arg5h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];

#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        T1 *argIndexMap1;
                        if(args[1].argtype == OP_ARG_GBL){
                            if(args[1].acc != OP_READ){
                                argIndexMap1= &arg1_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap1= (T1 *)(args[1].data);
                            }
                        }else
                            if(args[1].map == OP_ID){
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                            }else
                                if(args[1].map != NULL)
                                {
                                    argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                                }
                        T2 *argIndexMap2;
                        if(args[2].argtype == OP_ARG_GBL){
                            if(args[2].acc != OP_READ){
                                argIndexMap2= &arg2_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap2= (T2 *)(args[2].data);
                            }
                        }else
                            if(args[2].map == OP_ID){
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                            }else
                                if(args[2].map != NULL)
                                {
                                    argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                                }
                        T3 *argIndexMap3;
                        if(args[3].argtype == OP_ARG_GBL){
                            if(args[3].acc != OP_READ){
                                argIndexMap3= &arg3_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap3= (T3 *)(args[3].data);
                            }
                        }else
                            if(args[3].map == OP_ID){
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                            }else
                                if(args[3].map != NULL)
                                {
                                    argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                                }
                        T4 *argIndexMap4;
                        if(args[4].argtype == OP_ARG_GBL){
                            if(args[4].acc != OP_READ){
                                argIndexMap4= &arg4_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap4= (T4 *)(args[4].data);
                            }
                        }else
                            if(args[4].map == OP_ID){
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                            }else
                                if(args[4].map != NULL)
                                {
                                    argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                                }
                        T5 *argIndexMap5;
                        if(args[5].argtype == OP_ARG_GBL){
                            if(args[5].acc != OP_READ){
                                argIndexMap5= &arg5_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap5= (T5 *)(args[5].data);
                            }
                        }else
                            if(args[5].map == OP_ID){
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                            }else
                                if(args[5].map != NULL)
                                {
                                    argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                                }



                        kernel(
                                    argIndexMap0,
                                    argIndexMap1,
                                    argIndexMap2,
                                    argIndexMap3,
                                    argIndexMap4,
                                    argIndexMap5);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[1].acc == OP_INC)
                                    for (int d = 0; d < args[1].dim; d++) {
                                        TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                        TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[1].acc == OP_MIN)
                                        for (int d = 0; d < args[1].dim; d++){
                                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                            TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                            arg1h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[1].acc == OP_MAX)
                                            for (int d = 0; d < args[1].dim; d++){
                                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                                TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                                arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[2].acc == OP_INC)
                                    for (int d = 0; d < args[2].dim; d++) {
                                        TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                        TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[2].acc == OP_MIN)
                                        for (int d = 0; d < args[2].dim; d++){
                                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                            TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                            arg2h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[2].acc == OP_MAX)
                                            for (int d = 0; d < args[2].dim; d++){
                                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                                TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                                arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[3].acc == OP_INC)
                                    for (int d = 0; d < args[3].dim; d++) {
                                        TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                        TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[3].acc == OP_MIN)
                                        for (int d = 0; d < args[3].dim; d++){
                                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                            TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                            arg3h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[3].acc == OP_MAX)
                                            for (int d = 0; d < args[3].dim; d++){
                                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                                TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                                arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[4].acc == OP_INC)
                                    for (int d = 0; d < args[4].dim; d++) {
                                        TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                        TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[4].acc == OP_MIN)
                                        for (int d = 0; d < args[4].dim; d++){
                                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                            TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                            arg4h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[4].acc == OP_MAX)
                                            for (int d = 0; d < args[4].dim; d++){
                                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                                TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                                arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[5].acc == OP_INC)
                                    for (int d = 0; d < args[5].dim; d++) {
                                        TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                        TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[5].acc == OP_MIN)
                                        for (int d = 0; d < args[5].dim; d++){
                                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                            TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                            arg5h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[5].acc == OP_MAX)
                                            for (int d = 0; d < args[5].dim; d++){
                                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                                TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                                arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    T1 *argIndexMap1;
                    if(args[1].argtype == OP_ARG_GBL){
                        if(args[1].acc != OP_READ){
                            argIndexMap1= &((T1 *)arg1_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap1= (T1 *)(args[1].data);
                        }
                    }else
                        if(args[1].map == OP_ID){
                            argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                        }else
                            if(args[1].map != NULL)
                            {
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                            }
                    T2 *argIndexMap2;
                    if(args[2].argtype == OP_ARG_GBL){
                        if(args[2].acc != OP_READ){
                            argIndexMap2= &((T2 *)arg2_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap2= (T2 *)(args[2].data);
                        }
                    }else
                        if(args[2].map == OP_ID){
                            argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                        }else
                            if(args[2].map != NULL)
                            {
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                            }
                    T3 *argIndexMap3;
                    if(args[3].argtype == OP_ARG_GBL){
                        if(args[3].acc != OP_READ){
                            argIndexMap3= &((T3 *)arg3_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap3= (T3 *)(args[3].data);
                        }
                    }else
                        if(args[3].map == OP_ID){
                            argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                        }else
                            if(args[3].map != NULL)
                            {
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                            }
                    T4 *argIndexMap4;
                    if(args[4].argtype == OP_ARG_GBL){
                        if(args[4].acc != OP_READ){
                            argIndexMap4= &((T4 *)arg4_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap4= (T4 *)(args[4].data);
                        }
                    }else
                        if(args[4].map == OP_ID){
                            argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                        }else
                            if(args[4].map != NULL)
                            {
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                            }
                    T5 *argIndexMap5;
                    if(args[5].argtype == OP_ARG_GBL){
                        if(args[5].acc != OP_READ){
                            argIndexMap5= &((T5 *)arg5_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap5= (T5 *)(args[5].data);
                        }
                    }else
                        if(args[5].map == OP_ID){
                            argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                        }else
                            if(args[5].map != NULL)
                            {
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                            }
                    kernel(
                                argIndexMap0,
                                argIndexMap1,
                                argIndexMap2,
                                argIndexMap3,
                                argIndexMap4,
                                argIndexMap5);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[1].acc == OP_INC)
                        for (int d = 0; d < args[1].dim; d++) {
                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                            TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[1].acc == OP_MIN)
                            for (int d = 0; d < args[1].dim; d++){
                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                arg1h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[1].acc == OP_MAX)
                                for (int d = 0; d < args[1].dim; d++){
                                    TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                    TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                    arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[2].acc == OP_INC)
                        for (int d = 0; d < args[2].dim; d++) {
                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                            TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[2].acc == OP_MIN)
                            for (int d = 0; d < args[2].dim; d++){
                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                arg2h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[2].acc == OP_MAX)
                                for (int d = 0; d < args[2].dim; d++){
                                    TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                    TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                    arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[3].acc == OP_INC)
                        for (int d = 0; d < args[3].dim; d++) {
                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                            TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[3].acc == OP_MIN)
                            for (int d = 0; d < args[3].dim; d++){
                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                arg3h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[3].acc == OP_MAX)
                                for (int d = 0; d < args[3].dim; d++){
                                    TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                    TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                    arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[4].acc == OP_INC)
                        for (int d = 0; d < args[4].dim; d++) {
                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                            TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[4].acc == OP_MIN)
                            for (int d = 0; d < args[4].dim; d++){
                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                arg4h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[4].acc == OP_MAX)
                                for (int d = 0; d < args[4].dim; d++){
                                    TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                    TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                    arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[5].acc == OP_INC)
                        for (int d = 0; d < args[5].dim; d++) {
                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                            TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[5].acc == OP_MIN)
                            for (int d = 0; d < args[5].dim; d++){
                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                arg5h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[5].acc == OP_MAX)
                                for (int d = 0; d < args[5].dim; d++){
                                    TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                    TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                    arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ){
        free(arg1_l);
    }
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ){
        free(arg2_l);
    }
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ){
        free(arg3_l);
    }
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ){
        free(arg4_l);
    }
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ){
        free(arg5_l);
    }
}
//
//op_par_loop routine for 7 arguments
//
template <class T0,class T1,class T2,class T3,
          class T4,class T5,class T6>
void op_par_loop(void (*kernel)(T0*, T1*, T2*, T3*, 
                                T4*, T5*, T6*),
                 char const * name, op_set set,
                 op_arg arg0, op_arg arg1, op_arg arg2, op_arg arg3,
                 op_arg arg4, op_arg arg5, op_arg arg6){
    op_arg args[7] = {arg0, arg1, arg2, arg3,
                      arg4, arg5, arg6};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    char * arg1h;
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        arg1h = (char *)args[1].data;
    char * arg2h;
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        arg2h = (char *)args[2].data;
    char * arg3h;
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        arg3h = (char *)args[3].data;
    char * arg4h;
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        arg4h = (char *)args[4].data;
    char * arg5h;
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        arg5h = (char *)args[5].data;
    char * arg6h;
    if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
        arg6h = (char *)args[6].data;
    int nargs = 7;
    int ninds = 0;
    int inds[7] = {0,0,0,0,0,0,0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 7; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[7];
    int currentsize=0;
    for (int i = 0; i < 7; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 7; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    TYPE(1)* arg1_l;
    TYPE(2)* arg2_l;
    TYPE(3)* arg3_l;
    TYPE(4)* arg4_l;
    TYPE(5)* arg5_l;
    TYPE(6)* arg6_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        {
            arg1_l= new TYPE(1)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[1].dim; d++){
                    if(args[1].acc != OP_INC){
                        arg1_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg1_l[d + thr * 64] = arg1h[d];
                    }
                }
        }
        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        {
            arg2_l= new TYPE(2)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[2].dim; d++){
                    if(args[2].acc != OP_INC){
                        arg2_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg2_l[d + thr * 64] = arg2h[d];
                    }
                }
        }
        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        {
            arg3_l= new TYPE(3)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[3].dim; d++){
                    if(args[3].acc != OP_INC){
                        arg3_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg3_l[d + thr * 64] = arg3h[d];
                    }
                }
        }
        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        {
            arg4_l= new TYPE(4)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[4].dim; d++){
                    if(args[4].acc != OP_INC){
                        arg4_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg4_l[d + thr * 64] = arg4h[d];
                    }
                }
        }
        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        {
            arg5_l= new TYPE(5)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[5].dim; d++){
                    if(args[5].acc != OP_INC){
                        arg5_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg5_l[d + thr * 64] = arg5h[d];
                    }
                }
        }
        if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
        {
            arg6_l= new TYPE(6)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[6].dim; d++){
                    if(args[6].acc != OP_INC){
                        arg6_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg6_l[d + thr * 64] = arg6h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];
#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        T1 *argIndexMap1;
                        if(args[1].argtype == OP_ARG_GBL){
                            if(args[1].acc != OP_READ){
                                argIndexMap1= &arg1_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap1= (T1 *)(args[1].data);
                            }
                        }else
                            if(args[1].map == OP_ID){
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                            }else
                                if(args[1].map != NULL)
                                {
                                    argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                                }
                        T2 *argIndexMap2;
                        if(args[2].argtype == OP_ARG_GBL){
                            if(args[2].acc != OP_READ){
                                argIndexMap2= &arg2_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap2= (T2 *)(args[2].data);
                            }
                        }else
                            if(args[2].map == OP_ID){
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                            }else
                                if(args[2].map != NULL)
                                {
                                    argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                                }
                        T3 *argIndexMap3;
                        if(args[3].argtype == OP_ARG_GBL){
                            if(args[3].acc != OP_READ){
                                argIndexMap3= &arg3_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap3= (T3 *)(args[3].data);
                            }
                        }else
                            if(args[3].map == OP_ID){
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                            }else
                                if(args[3].map != NULL)
                                {
                                    argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                                }
                        T4 *argIndexMap4;
                        if(args[4].argtype == OP_ARG_GBL){
                            if(args[4].acc != OP_READ){
                                argIndexMap4= &arg4_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap4= (T4 *)(args[4].data);
                            }
                        }else
                            if(args[4].map == OP_ID){
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                            }else
                                if(args[4].map != NULL)
                                {
                                    argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                                }
                        T5 *argIndexMap5;
                        if(args[5].argtype == OP_ARG_GBL){
                            if(args[5].acc != OP_READ){
                                argIndexMap5= &arg5_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap5= (T5 *)(args[5].data);
                            }
                        }else
                            if(args[5].map == OP_ID){
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                            }else
                                if(args[5].map != NULL)
                                {
                                    argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                                }
                        T6 *argIndexMap6;
                        if(args[6].argtype == OP_ARG_GBL){
                            if(args[6].acc != OP_READ){
                                argIndexMap6= &arg6_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap6= (T6 *)(args[6].data);
                            }
                        }else
                            if(args[6].map == OP_ID){
                                argIndexMap6=&((T6 *)arg6.data)[args[6].dim * n];
                            }else
                                if(args[6].map != NULL)
                                {
                                    argIndexMap6=&((T6 *)arg6.data)[args[6].dim * args[6].map_data[(n * args[6].map->dim + args[6].idx)]];
                                }
                        kernel(
                                    argIndexMap0,
                                    argIndexMap1,
                                    argIndexMap2,
                                    argIndexMap3,
                                    argIndexMap4,
                                    argIndexMap5,
                                    argIndexMap6);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[1].acc == OP_INC)
                                    for (int d = 0; d < args[1].dim; d++) {
                                        TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                        TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[1].acc == OP_MIN)
                                        for (int d = 0; d < args[1].dim; d++){
                                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                            TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                            arg1h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[1].acc == OP_MAX)
                                            for (int d = 0; d < args[1].dim; d++){
                                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                                TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                                arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[2].acc == OP_INC)
                                    for (int d = 0; d < args[2].dim; d++) {
                                        TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                        TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[2].acc == OP_MIN)
                                        for (int d = 0; d < args[2].dim; d++){
                                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                            TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                            arg2h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[2].acc == OP_MAX)
                                            for (int d = 0; d < args[2].dim; d++){
                                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                                TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                                arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[3].acc == OP_INC)
                                    for (int d = 0; d < args[3].dim; d++) {
                                        TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                        TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[3].acc == OP_MIN)
                                        for (int d = 0; d < args[3].dim; d++){
                                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                            TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                            arg3h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[3].acc == OP_MAX)
                                            for (int d = 0; d < args[3].dim; d++){
                                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                                TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                                arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[4].acc == OP_INC)
                                    for (int d = 0; d < args[4].dim; d++) {
                                        TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                        TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[4].acc == OP_MIN)
                                        for (int d = 0; d < args[4].dim; d++){
                                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                            TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                            arg4h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[4].acc == OP_MAX)
                                            for (int d = 0; d < args[4].dim; d++){
                                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                                TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                                arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[5].acc == OP_INC)
                                    for (int d = 0; d < args[5].dim; d++) {
                                        TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                        TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[5].acc == OP_MIN)
                                        for (int d = 0; d < args[5].dim; d++){
                                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                            TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                            arg5h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[5].acc == OP_MAX)
                                            for (int d = 0; d < args[5].dim; d++){
                                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                                TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                                arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[6].acc == OP_INC)
                                    for (int d = 0; d < args[6].dim; d++) {
                                        TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                        TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[6].acc == OP_MIN)
                                        for (int d = 0; d < args[6].dim; d++){
                                            TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                            TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                            arg6h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[6].acc == OP_MAX)
                                            for (int d = 0; d < args[6].dim; d++){
                                                TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                                TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                                arg6h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    T1 *argIndexMap1;
                    if(args[1].argtype == OP_ARG_GBL){
                        if(args[1].acc != OP_READ){
                            argIndexMap1= &((T1 *)arg1_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap1= (T1 *)(args[1].data);
                        }
                    }else
                        if(args[1].map == OP_ID){
                            argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                        }else
                            if(args[1].map != NULL)
                            {
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                            }
                    T2 *argIndexMap2;
                    if(args[2].argtype == OP_ARG_GBL){
                        if(args[2].acc != OP_READ){
                            argIndexMap2= &((T2 *)arg2_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap2= (T2 *)(args[2].data);
                        }
                    }else
                        if(args[2].map == OP_ID){
                            argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                        }else
                            if(args[2].map != NULL)
                            {
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                            }
                    T3 *argIndexMap3;
                    if(args[3].argtype == OP_ARG_GBL){
                        if(args[3].acc != OP_READ){
                            argIndexMap3= &((T3 *)arg3_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap3= (T3 *)(args[3].data);
                        }
                    }else
                        if(args[3].map == OP_ID){
                            argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                        }else
                            if(args[3].map != NULL)
                            {
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                            }
                    T4 *argIndexMap4;
                    if(args[4].argtype == OP_ARG_GBL){
                        if(args[4].acc != OP_READ){
                            argIndexMap4= &((T4 *)arg4_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap4= (T4 *)(args[4].data);
                        }
                    }else
                        if(args[4].map == OP_ID){
                            argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                        }else
                            if(args[4].map != NULL)
                            {
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                            }
                    T5 *argIndexMap5;
                    if(args[5].argtype == OP_ARG_GBL){
                        if(args[5].acc != OP_READ){
                            argIndexMap5= &((T5 *)arg5_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap5= (T5 *)(args[5].data);
                        }
                    }else
                        if(args[5].map == OP_ID){
                            argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                        }else
                            if(args[5].map != NULL)
                            {
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                            }
                    T6 *argIndexMap6;
                    if(args[6].argtype == OP_ARG_GBL){
                        if(args[6].acc != OP_READ){
                            argIndexMap6= &((T6 *)arg6_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap6= (T6 *)(args[6].data);
                        }
                    }else
                        if(args[6].map == OP_ID){
                            argIndexMap6=&((T6 *)arg6.data)[args[6].dim * n];
                        }else
                            if(args[6].map != NULL)
                            {
                                argIndexMap6=&((T6 *)arg6.data)[args[6].dim * args[6].map_data[(n * args[6].map->dim + args[6].idx)]];
                            }
                    kernel(
                                argIndexMap0,
                                argIndexMap1,
                                argIndexMap2,
                                argIndexMap3,
                                argIndexMap4,
                                argIndexMap5,
                                argIndexMap6);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[1].acc == OP_INC)
                        for (int d = 0; d < args[1].dim; d++) {
                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                            TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[1].acc == OP_MIN)
                            for (int d = 0; d < args[1].dim; d++){
                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                arg1h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[1].acc == OP_MAX)
                                for (int d = 0; d < args[1].dim; d++){
                                    TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                    TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                    arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[2].acc == OP_INC)
                        for (int d = 0; d < args[2].dim; d++) {
                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                            TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[2].acc == OP_MIN)
                            for (int d = 0; d < args[2].dim; d++){
                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                arg2h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[2].acc == OP_MAX)
                                for (int d = 0; d < args[2].dim; d++){
                                    TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                    TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                    arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[3].acc == OP_INC)
                        for (int d = 0; d < args[3].dim; d++) {
                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                            TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[3].acc == OP_MIN)
                            for (int d = 0; d < args[3].dim; d++){
                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                arg3h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[3].acc == OP_MAX)
                                for (int d = 0; d < args[3].dim; d++){
                                    TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                    TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                    arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[4].acc == OP_INC)
                        for (int d = 0; d < args[4].dim; d++) {
                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                            TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[4].acc == OP_MIN)
                            for (int d = 0; d < args[4].dim; d++){
                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                arg4h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[4].acc == OP_MAX)
                                for (int d = 0; d < args[4].dim; d++){
                                    TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                    TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                    arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[5].acc == OP_INC)
                        for (int d = 0; d < args[5].dim; d++) {
                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                            TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[5].acc == OP_MIN)
                            for (int d = 0; d < args[5].dim; d++){
                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                arg5h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[5].acc == OP_MAX)
                                for (int d = 0; d < args[5].dim; d++){
                                    TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                    TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                    arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[6].acc == OP_INC)
                        for (int d = 0; d < args[6].dim; d++) {
                            TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                            TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[6].acc == OP_MIN)
                            for (int d = 0; d < args[6].dim; d++){
                                TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                                arg6h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[6].acc == OP_MAX)
                                for (int d = 0; d < args[6].dim; d++){
                                    TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                    TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                                    arg6h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ){
        free(arg1_l);
    }
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ){
        free(arg2_l);
    }
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ){
        free(arg3_l);
    }
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ){
        free(arg4_l);
    }
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ){
        free(arg5_l);
    }
    if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ){
        free(arg6_l);
    }
}
//
//op_par_loop routine for 8 arguments
//
template <class T0,class T1,class T2,class T3,
          class T4,class T5,class T6,class T7>
void op_par_loop(void (*kernel)(T0*, T1*, T2*, T3*, 
                                T4*, T5*, T6*, T7*),
                 char const * name, op_set set,
                 op_arg arg0, op_arg arg1, op_arg arg2, op_arg arg3,
                 op_arg arg4, op_arg arg5, op_arg arg6, op_arg arg7){
    op_arg args[8] = {arg0, arg1, arg2, arg3,
                      arg4, arg5, arg6, arg7};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    char * arg1h;
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        arg1h = (char *)args[1].data;
    char * arg2h;
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        arg2h = (char *)args[2].data;
    char * arg3h;
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        arg3h = (char *)args[3].data;
    char * arg4h;
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        arg4h = (char *)args[4].data;
    char * arg5h;
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        arg5h = (char *)args[5].data;
    char * arg6h;
    if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
        arg6h = (char *)args[6].data;
    char * arg7h;
    if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
        arg7h = (char *)args[7].data;
    int nargs = 8;
    int ninds = 0;
    int inds[8] = {0,0,0,0,0,0,0,0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 8; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[8];
    int currentsize=0;
    for (int i = 0; i < 8; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 8; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    TYPE(1)* arg1_l;
    TYPE(2)* arg2_l;
    TYPE(3)* arg3_l;
    TYPE(4)* arg4_l;
    TYPE(5)* arg5_l;
    TYPE(6)* arg6_l;
    TYPE(7)* arg7_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        {
            arg1_l= new TYPE(1)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[1].dim; d++){
                    if(args[1].acc != OP_INC){
                        arg1_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg1_l[d + thr * 64] = arg1h[d];
                    }
                }
        }
        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        {
            arg2_l= new TYPE(2)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[2].dim; d++){
                    if(args[2].acc != OP_INC){
                        arg2_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg2_l[d + thr * 64] = arg2h[d];
                    }
                }
        }
        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        {
            arg3_l= new TYPE(3)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[3].dim; d++){
                    if(args[3].acc != OP_INC){
                        arg3_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg3_l[d + thr * 64] = arg3h[d];
                    }
                }
        }
        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        {
            arg4_l= new TYPE(4)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[4].dim; d++){
                    if(args[4].acc != OP_INC){
                        arg4_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg4_l[d + thr * 64] = arg4h[d];
                    }
                }
        }
        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        {
            arg5_l= new TYPE(5)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[5].dim; d++){
                    if(args[5].acc != OP_INC){
                        arg5_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg5_l[d + thr * 64] = arg5h[d];
                    }
                }
        }
        if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
        {
            arg6_l= new TYPE(6)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[6].dim; d++){
                    if(args[6].acc != OP_INC){
                        arg6_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg6_l[d + thr * 64] = arg6h[d];
                    }
                }
        }
        if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
        {
            arg7_l= new TYPE(7)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[7].dim; d++){
                    if(args[7].acc != OP_INC){
                        arg7_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg7_l[d + thr * 64] = arg7h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];
#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        T1 *argIndexMap1;
                        if(args[1].argtype == OP_ARG_GBL){
                            if(args[1].acc != OP_READ){
                                argIndexMap1= &arg1_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap1= (T1 *)(args[1].data);
                            }
                        }else
                            if(args[1].map == OP_ID){
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                            }else
                                if(args[1].map != NULL)
                                {
                                    argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                                }
                        T2 *argIndexMap2;
                        if(args[2].argtype == OP_ARG_GBL){
                            if(args[2].acc != OP_READ){
                                argIndexMap2= &arg2_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap2= (T2 *)(args[2].data);
                            }
                        }else
                            if(args[2].map == OP_ID){
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                            }else
                                if(args[2].map != NULL)
                                {
                                    argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                                }
                        T3 *argIndexMap3;
                        if(args[3].argtype == OP_ARG_GBL){
                            if(args[3].acc != OP_READ){
                                argIndexMap3= &arg3_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap3= (T3 *)(args[3].data);
                            }
                        }else
                            if(args[3].map == OP_ID){
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                            }else
                                if(args[3].map != NULL)
                                {
                                    argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                                }
                        T4 *argIndexMap4;
                        if(args[4].argtype == OP_ARG_GBL){
                            if(args[4].acc != OP_READ){
                                argIndexMap4= &arg4_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap4= (T4 *)(args[4].data);
                            }
                        }else
                            if(args[4].map == OP_ID){
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                            }else
                                if(args[4].map != NULL)
                                {
                                    argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                                }
                        T5 *argIndexMap5;
                        if(args[5].argtype == OP_ARG_GBL){
                            if(args[5].acc != OP_READ){
                                argIndexMap5= &arg5_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap5= (T5 *)(args[5].data);
                            }
                        }else
                            if(args[5].map == OP_ID){
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                            }else
                                if(args[5].map != NULL)
                                {
                                    argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                                }
                        T6 *argIndexMap6;
                        if(args[6].argtype == OP_ARG_GBL){
                            if(args[6].acc != OP_READ){
                                argIndexMap6= &arg6_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap6= (T6 *)(args[6].data);
                            }
                        }else
                            if(args[6].map == OP_ID){
                                argIndexMap6=&((T6 *)arg6.data)[args[6].dim * n];
                            }else
                                if(args[6].map != NULL)
                                {
                                    argIndexMap6=&((T6 *)arg6.data)[args[6].dim * args[6].map_data[(n * args[6].map->dim + args[6].idx)]];
                                }
                        T7 *argIndexMap7;
                        if(args[7].argtype == OP_ARG_GBL){
                            if(args[7].acc != OP_READ){
                                argIndexMap7= &arg7_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap7= (T7 *)(args[7].data);
                            }
                        }else
                            if(args[7].map == OP_ID){
                                argIndexMap7=&((T7 *)arg7.data)[args[7].dim * n];
                            }else
                                if(args[7].map != NULL)
                                {
                                    argIndexMap7=&((T7 *)arg7.data)[args[7].dim * args[7].map_data[(n * args[7].map->dim + args[7].idx)]];
                                }
                        kernel(
                                    argIndexMap0,
                                    argIndexMap1,
                                    argIndexMap2,
                                    argIndexMap3,
                                    argIndexMap4,
                                    argIndexMap5,
                                    argIndexMap6,
                                    argIndexMap7);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[1].acc == OP_INC)
                                    for (int d = 0; d < args[1].dim; d++) {
                                        TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                        TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[1].acc == OP_MIN)
                                        for (int d = 0; d < args[1].dim; d++){
                                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                            TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                            arg1h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[1].acc == OP_MAX)
                                            for (int d = 0; d < args[1].dim; d++){
                                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                                TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                                arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[2].acc == OP_INC)
                                    for (int d = 0; d < args[2].dim; d++) {
                                        TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                        TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[2].acc == OP_MIN)
                                        for (int d = 0; d < args[2].dim; d++){
                                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                            TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                            arg2h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[2].acc == OP_MAX)
                                            for (int d = 0; d < args[2].dim; d++){
                                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                                TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                                arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[3].acc == OP_INC)
                                    for (int d = 0; d < args[3].dim; d++) {
                                        TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                        TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[3].acc == OP_MIN)
                                        for (int d = 0; d < args[3].dim; d++){
                                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                            TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                            arg3h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[3].acc == OP_MAX)
                                            for (int d = 0; d < args[3].dim; d++){
                                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                                TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                                arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[4].acc == OP_INC)
                                    for (int d = 0; d < args[4].dim; d++) {
                                        TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                        TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[4].acc == OP_MIN)
                                        for (int d = 0; d < args[4].dim; d++){
                                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                            TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                            arg4h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[4].acc == OP_MAX)
                                            for (int d = 0; d < args[4].dim; d++){
                                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                                TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                                arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[5].acc == OP_INC)
                                    for (int d = 0; d < args[5].dim; d++) {
                                        TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                        TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[5].acc == OP_MIN)
                                        for (int d = 0; d < args[5].dim; d++){
                                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                            TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                            arg5h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[5].acc == OP_MAX)
                                            for (int d = 0; d < args[5].dim; d++){
                                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                                TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                                arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[6].acc == OP_INC)
                                    for (int d = 0; d < args[6].dim; d++) {
                                        TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                        TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[6].acc == OP_MIN)
                                        for (int d = 0; d < args[6].dim; d++){
                                            TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                            TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                            arg6h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[6].acc == OP_MAX)
                                            for (int d = 0; d < args[6].dim; d++){
                                                TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                                TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                                arg6h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[7].acc == OP_INC)
                                    for (int d = 0; d < args[7].dim; d++) {
                                        TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                        TYPE(7)* tmp2 =&arg7_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[7].acc == OP_MIN)
                                        for (int d = 0; d < args[7].dim; d++){
                                            TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                            TYPE(7)* tmp2 =&arg7_l[d+thr*64];
                                            arg7h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[7].acc == OP_MAX)
                                            for (int d = 0; d < args[7].dim; d++){
                                                TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                                TYPE(7)* tmp2 =&arg7_l[d+thr*64];
                                                arg7h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    T1 *argIndexMap1;
                    if(args[1].argtype == OP_ARG_GBL){
                        if(args[1].acc != OP_READ){
                            argIndexMap1= &((T1 *)arg1_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap1= (T1 *)(args[1].data);
                        }
                    }else
                        if(args[1].map == OP_ID){
                            argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                        }else
                            if(args[1].map != NULL)
                            {
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                            }
                    T2 *argIndexMap2;
                    if(args[2].argtype == OP_ARG_GBL){
                        if(args[2].acc != OP_READ){
                            argIndexMap2= &((T2 *)arg2_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap2= (T2 *)(args[2].data);
                        }
                    }else
                        if(args[2].map == OP_ID){
                            argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                        }else
                            if(args[2].map != NULL)
                            {
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                            }
                    T3 *argIndexMap3;
                    if(args[3].argtype == OP_ARG_GBL){
                        if(args[3].acc != OP_READ){
                            argIndexMap3= &((T3 *)arg3_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap3= (T3 *)(args[3].data);
                        }
                    }else
                        if(args[3].map == OP_ID){
                            argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                        }else
                            if(args[3].map != NULL)
                            {
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                            }
                    T4 *argIndexMap4;
                    if(args[4].argtype == OP_ARG_GBL){
                        if(args[4].acc != OP_READ){
                            argIndexMap4= &((T4 *)arg4_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap4= (T4 *)(args[4].data);
                        }
                    }else
                        if(args[4].map == OP_ID){
                            argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                        }else
                            if(args[4].map != NULL)
                            {
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                            }
                    T5 *argIndexMap5;
                    if(args[5].argtype == OP_ARG_GBL){
                        if(args[5].acc != OP_READ){
                            argIndexMap5= &((T5 *)arg5_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap5= (T5 *)(args[5].data);
                        }
                    }else
                        if(args[5].map == OP_ID){
                            argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                        }else
                            if(args[5].map != NULL)
                            {
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                            }
                    T6 *argIndexMap6;
                    if(args[6].argtype == OP_ARG_GBL){
                        if(args[6].acc != OP_READ){
                            argIndexMap6= &((T6 *)arg6_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap6= (T6 *)(args[6].data);
                        }
                    }else
                        if(args[6].map == OP_ID){
                            argIndexMap6=&((T6 *)arg6.data)[args[6].dim * n];
                        }else
                            if(args[6].map != NULL)
                            {
                                argIndexMap6=&((T6 *)arg6.data)[args[6].dim * args[6].map_data[(n * args[6].map->dim + args[6].idx)]];
                            }
                    T7 *argIndexMap7;
                    if(args[7].argtype == OP_ARG_GBL){
                        if(args[7].acc != OP_READ){
                            argIndexMap7= &((T7 *)arg7_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap7= (T7 *)(args[7].data);
                        }
                    }else
                        if(args[7].map == OP_ID){
                            argIndexMap7=&((T7 *)arg7.data)[args[7].dim * n];
                        }else
                            if(args[7].map != NULL)
                            {
                                argIndexMap7=&((T7 *)arg7.data)[args[7].dim * args[7].map_data[(n * args[7].map->dim + args[7].idx)]];
                            }
                    kernel(
                                argIndexMap0,
                                argIndexMap1,
                                argIndexMap2,
                                argIndexMap3,
                                argIndexMap4,
                                argIndexMap5,
                                argIndexMap6,
                                argIndexMap7);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[1].acc == OP_INC)
                        for (int d = 0; d < args[1].dim; d++) {
                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                            TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[1].acc == OP_MIN)
                            for (int d = 0; d < args[1].dim; d++){
                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                arg1h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[1].acc == OP_MAX)
                                for (int d = 0; d < args[1].dim; d++){
                                    TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                    TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                    arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[2].acc == OP_INC)
                        for (int d = 0; d < args[2].dim; d++) {
                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                            TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[2].acc == OP_MIN)
                            for (int d = 0; d < args[2].dim; d++){
                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                arg2h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[2].acc == OP_MAX)
                                for (int d = 0; d < args[2].dim; d++){
                                    TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                    TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                    arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[3].acc == OP_INC)
                        for (int d = 0; d < args[3].dim; d++) {
                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                            TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[3].acc == OP_MIN)
                            for (int d = 0; d < args[3].dim; d++){
                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                arg3h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[3].acc == OP_MAX)
                                for (int d = 0; d < args[3].dim; d++){
                                    TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                    TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                    arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[4].acc == OP_INC)
                        for (int d = 0; d < args[4].dim; d++) {
                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                            TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[4].acc == OP_MIN)
                            for (int d = 0; d < args[4].dim; d++){
                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                arg4h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[4].acc == OP_MAX)
                                for (int d = 0; d < args[4].dim; d++){
                                    TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                    TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                    arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[5].acc == OP_INC)
                        for (int d = 0; d < args[5].dim; d++) {
                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                            TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[5].acc == OP_MIN)
                            for (int d = 0; d < args[5].dim; d++){
                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                arg5h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[5].acc == OP_MAX)
                                for (int d = 0; d < args[5].dim; d++){
                                    TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                    TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                    arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[6].acc == OP_INC)
                        for (int d = 0; d < args[6].dim; d++) {
                            TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                            TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[6].acc == OP_MIN)
                            for (int d = 0; d < args[6].dim; d++){
                                TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                                arg6h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[6].acc == OP_MAX)
                                for (int d = 0; d < args[6].dim; d++){
                                    TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                    TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                                    arg6h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[7].acc == OP_INC)
                        for (int d = 0; d < args[7].dim; d++) {
                            TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                            TYPE(7)* tmp2 =&((TYPE(7)*)arg7_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[7].acc == OP_MIN)
                            for (int d = 0; d < args[7].dim; d++){
                                TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                TYPE(7)* tmp2 =&((TYPE(7)*)arg7_l)[d+thr*64];
                                arg7h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[7].acc == OP_MAX)
                                for (int d = 0; d < args[7].dim; d++){
                                    TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                    TYPE(7)* tmp2 =&((TYPE(7)*)arg7_l)[d+thr*64];
                                    arg7h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ){
        free(arg1_l);
    }
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ){
        free(arg2_l);
    }
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ){
        free(arg3_l);
    }
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ){
        free(arg4_l);
    }
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ){
        free(arg5_l);
    }
    if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ){
        free(arg6_l);
    }
    if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ){
        free(arg7_l);
    }
}
//
//op_par_loop routine for 9 arguments
//
template <class T0,class T1,class T2,class T3,
          class T4,class T5,class T6,class T7,
          class T8>
void op_par_loop(void (*kernel)(T0*, T1*, T2*, T3*, 
                                T4*, T5*, T6*, T7*,
                                T8*),
                 char const * name, op_set set,
                 op_arg arg0, op_arg arg1, op_arg arg2, op_arg arg3,
                 op_arg arg4, op_arg arg5, op_arg arg6, op_arg arg7,
                 op_arg arg8){
    op_arg args[9] = {arg0, arg1, arg2, arg3,
                      arg4, arg5, arg6, arg7,
                      arg8};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    char * arg1h;
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        arg1h = (char *)args[1].data;
    char * arg2h;
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        arg2h = (char *)args[2].data;
    char * arg3h;
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        arg3h = (char *)args[3].data;
    char * arg4h;
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        arg4h = (char *)args[4].data;
    char * arg5h;
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        arg5h = (char *)args[5].data;
    char * arg6h;
    if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
        arg6h = (char *)args[6].data;
    char * arg7h;
    if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
        arg7h = (char *)args[7].data;
    char * arg8h;
    if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ)
        arg8h = (char *)args[8].data;
    int nargs = 9;
    int ninds = 0;
    int inds[9] = {0,0,0,0,0,0,0,0,0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 9; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[9];
    int currentsize=0;
    for (int i = 0; i < 9; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 9; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    TYPE(1)* arg1_l;
    TYPE(2)* arg2_l;
    TYPE(3)* arg3_l;
    TYPE(4)* arg4_l;
    TYPE(5)* arg5_l;
    TYPE(6)* arg6_l;
    TYPE(7)* arg7_l;
    TYPE(8)* arg8_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        {
            arg1_l= new TYPE(1)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[1].dim; d++){
                    if(args[1].acc != OP_INC){
                        arg1_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg1_l[d + thr * 64] = arg1h[d];
                    }
                }
        }
        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        {
            arg2_l= new TYPE(2)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[2].dim; d++){
                    if(args[2].acc != OP_INC){
                        arg2_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg2_l[d + thr * 64] = arg2h[d];
                    }
                }
        }
        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        {
            arg3_l= new TYPE(3)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[3].dim; d++){
                    if(args[3].acc != OP_INC){
                        arg3_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg3_l[d + thr * 64] = arg3h[d];
                    }
                }
        }
        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        {
            arg4_l= new TYPE(4)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[4].dim; d++){
                    if(args[4].acc != OP_INC){
                        arg4_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg4_l[d + thr * 64] = arg4h[d];
                    }
                }
        }
        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        {
            arg5_l= new TYPE(5)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[5].dim; d++){
                    if(args[5].acc != OP_INC){
                        arg5_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg5_l[d + thr * 64] = arg5h[d];
                    }
                }
        }
        if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
        {
            arg6_l= new TYPE(6)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[6].dim; d++){
                    if(args[6].acc != OP_INC){
                        arg6_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg6_l[d + thr * 64] = arg6h[d];
                    }
                }
        }
        if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
        {
            arg7_l= new TYPE(7)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[7].dim; d++){
                    if(args[7].acc != OP_INC){
                        arg7_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg7_l[d + thr * 64] = arg7h[d];
                    }
                }
        }
        if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ)
        {
            arg8_l= new TYPE(8)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[8].dim; d++){
                    if(args[8].acc != OP_INC){
                        arg8_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg8_l[d + thr * 64] = arg8h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];
#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        T1 *argIndexMap1;
                        if(args[1].argtype == OP_ARG_GBL){
                            if(args[1].acc != OP_READ){
                                argIndexMap1= &arg1_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap1= (T1 *)(args[1].data);
                            }
                        }else
                            if(args[1].map == OP_ID){
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                            }else
                                if(args[1].map != NULL)
                                {
                                    argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                                }
                        T2 *argIndexMap2;
                        if(args[2].argtype == OP_ARG_GBL){
                            if(args[2].acc != OP_READ){
                                argIndexMap2= &arg2_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap2= (T2 *)(args[2].data);
                            }
                        }else
                            if(args[2].map == OP_ID){
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                            }else
                                if(args[2].map != NULL)
                                {
                                    argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                                }
                        T3 *argIndexMap3;
                        if(args[3].argtype == OP_ARG_GBL){
                            if(args[3].acc != OP_READ){
                                argIndexMap3= &arg3_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap3= (T3 *)(args[3].data);
                            }
                        }else
                            if(args[3].map == OP_ID){
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                            }else
                                if(args[3].map != NULL)
                                {
                                    argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                                }
                        T4 *argIndexMap4;
                        if(args[4].argtype == OP_ARG_GBL){
                            if(args[4].acc != OP_READ){
                                argIndexMap4= &arg4_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap4= (T4 *)(args[4].data);
                            }
                        }else
                            if(args[4].map == OP_ID){
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                            }else
                                if(args[4].map != NULL)
                                {
                                    argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                                }
                        T5 *argIndexMap5;
                        if(args[5].argtype == OP_ARG_GBL){
                            if(args[5].acc != OP_READ){
                                argIndexMap5= &arg5_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap5= (T5 *)(args[5].data);
                            }
                        }else
                            if(args[5].map == OP_ID){
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                            }else
                                if(args[5].map != NULL)
                                {
                                    argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                                }
                        T6 *argIndexMap6;
                        if(args[6].argtype == OP_ARG_GBL){
                            if(args[6].acc != OP_READ){
                                argIndexMap6= &arg6_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap6= (T6 *)(args[6].data);
                            }
                        }else
                            if(args[6].map == OP_ID){
                                argIndexMap6=&((T6 *)arg6.data)[args[6].dim * n];
                            }else
                                if(args[6].map != NULL)
                                {
                                    argIndexMap6=&((T6 *)arg6.data)[args[6].dim * args[6].map_data[(n * args[6].map->dim + args[6].idx)]];
                                }
                        T7 *argIndexMap7;
                        if(args[7].argtype == OP_ARG_GBL){
                            if(args[7].acc != OP_READ){
                                argIndexMap7= &arg7_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap7= (T7 *)(args[7].data);
                            }
                        }else
                            if(args[7].map == OP_ID){
                                argIndexMap7=&((T7 *)arg7.data)[args[7].dim * n];
                            }else
                                if(args[7].map != NULL)
                                {
                                    argIndexMap7=&((T7 *)arg7.data)[args[7].dim * args[7].map_data[(n * args[7].map->dim + args[7].idx)]];
                                }
                        T8 *argIndexMap8;
                        if(args[8].argtype == OP_ARG_GBL){
                            if(args[8].acc != OP_READ){
                                argIndexMap8= &arg8_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap8= (T8 *)(args[8].data);
                            }
                        }else
                            if(args[8].map == OP_ID){
                                argIndexMap8=&((T8 *)arg8.data)[args[8].dim * n];
                            }else
                                if(args[8].map != NULL)
                                {
                                    argIndexMap8=&((T8 *)arg8.data)[args[8].dim * args[8].map_data[(n * args[8].map->dim + args[8].idx)]];
                                }
                        kernel(
                                    argIndexMap0,
                                    argIndexMap1,
                                    argIndexMap2,
                                    argIndexMap3,
                                    argIndexMap4,
                                    argIndexMap5,
                                    argIndexMap6,
                                    argIndexMap7,
                                    argIndexMap8);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[1].acc == OP_INC)
                                    for (int d = 0; d < args[1].dim; d++) {
                                        TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                        TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[1].acc == OP_MIN)
                                        for (int d = 0; d < args[1].dim; d++){
                                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                            TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                            arg1h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[1].acc == OP_MAX)
                                            for (int d = 0; d < args[1].dim; d++){
                                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                                TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                                arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[2].acc == OP_INC)
                                    for (int d = 0; d < args[2].dim; d++) {
                                        TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                        TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[2].acc == OP_MIN)
                                        for (int d = 0; d < args[2].dim; d++){
                                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                            TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                            arg2h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[2].acc == OP_MAX)
                                            for (int d = 0; d < args[2].dim; d++){
                                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                                TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                                arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[3].acc == OP_INC)
                                    for (int d = 0; d < args[3].dim; d++) {
                                        TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                        TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[3].acc == OP_MIN)
                                        for (int d = 0; d < args[3].dim; d++){
                                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                            TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                            arg3h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[3].acc == OP_MAX)
                                            for (int d = 0; d < args[3].dim; d++){
                                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                                TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                                arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[4].acc == OP_INC)
                                    for (int d = 0; d < args[4].dim; d++) {
                                        TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                        TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[4].acc == OP_MIN)
                                        for (int d = 0; d < args[4].dim; d++){
                                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                            TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                            arg4h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[4].acc == OP_MAX)
                                            for (int d = 0; d < args[4].dim; d++){
                                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                                TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                                arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[5].acc == OP_INC)
                                    for (int d = 0; d < args[5].dim; d++) {
                                        TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                        TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[5].acc == OP_MIN)
                                        for (int d = 0; d < args[5].dim; d++){
                                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                            TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                            arg5h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[5].acc == OP_MAX)
                                            for (int d = 0; d < args[5].dim; d++){
                                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                                TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                                arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[6].acc == OP_INC)
                                    for (int d = 0; d < args[6].dim; d++) {
                                        TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                        TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[6].acc == OP_MIN)
                                        for (int d = 0; d < args[6].dim; d++){
                                            TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                            TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                            arg6h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[6].acc == OP_MAX)
                                            for (int d = 0; d < args[6].dim; d++){
                                                TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                                TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                                arg6h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[7].acc == OP_INC)
                                    for (int d = 0; d < args[7].dim; d++) {
                                        TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                        TYPE(7)* tmp2 =&arg7_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[7].acc == OP_MIN)
                                        for (int d = 0; d < args[7].dim; d++){
                                            TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                            TYPE(7)* tmp2 =&arg7_l[d+thr*64];
                                            arg7h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[7].acc == OP_MAX)
                                            for (int d = 0; d < args[7].dim; d++){
                                                TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                                TYPE(7)* tmp2 =&arg7_l[d+thr*64];
                                                arg7h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[8].acc == OP_INC)
                                    for (int d = 0; d < args[8].dim; d++) {
                                        TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                        TYPE(8)* tmp2 =&arg8_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[8].acc == OP_MIN)
                                        for (int d = 0; d < args[8].dim; d++){
                                            TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                            TYPE(8)* tmp2 =&arg8_l[d+thr*64];
                                            arg8h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[8].acc == OP_MAX)
                                            for (int d = 0; d < args[8].dim; d++){
                                                TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                                TYPE(8)* tmp2 =&arg8_l[d+thr*64];
                                                arg8h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    T1 *argIndexMap1;
                    if(args[1].argtype == OP_ARG_GBL){
                        if(args[1].acc != OP_READ){
                            argIndexMap1= &((T1 *)arg1_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap1= (T1 *)(args[1].data);
                        }
                    }else
                        if(args[1].map == OP_ID){
                            argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                        }else
                            if(args[1].map != NULL)
                            {
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                            }
                    T2 *argIndexMap2;
                    if(args[2].argtype == OP_ARG_GBL){
                        if(args[2].acc != OP_READ){
                            argIndexMap2= &((T2 *)arg2_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap2= (T2 *)(args[2].data);
                        }
                    }else
                        if(args[2].map == OP_ID){
                            argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                        }else
                            if(args[2].map != NULL)
                            {
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                            }
                    T3 *argIndexMap3;
                    if(args[3].argtype == OP_ARG_GBL){
                        if(args[3].acc != OP_READ){
                            argIndexMap3= &((T3 *)arg3_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap3= (T3 *)(args[3].data);
                        }
                    }else
                        if(args[3].map == OP_ID){
                            argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                        }else
                            if(args[3].map != NULL)
                            {
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                            }
                    T4 *argIndexMap4;
                    if(args[4].argtype == OP_ARG_GBL){
                        if(args[4].acc != OP_READ){
                            argIndexMap4= &((T4 *)arg4_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap4= (T4 *)(args[4].data);
                        }
                    }else
                        if(args[4].map == OP_ID){
                            argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                        }else
                            if(args[4].map != NULL)
                            {
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                            }
                    T5 *argIndexMap5;
                    if(args[5].argtype == OP_ARG_GBL){
                        if(args[5].acc != OP_READ){
                            argIndexMap5= &((T5 *)arg5_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap5= (T5 *)(args[5].data);
                        }
                    }else
                        if(args[5].map == OP_ID){
                            argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                        }else
                            if(args[5].map != NULL)
                            {
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                            }
                    T6 *argIndexMap6;
                    if(args[6].argtype == OP_ARG_GBL){
                        if(args[6].acc != OP_READ){
                            argIndexMap6= &((T6 *)arg6_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap6= (T6 *)(args[6].data);
                        }
                    }else
                        if(args[6].map == OP_ID){
                            argIndexMap6=&((T6 *)arg6.data)[args[6].dim * n];
                        }else
                            if(args[6].map != NULL)
                            {
                                argIndexMap6=&((T6 *)arg6.data)[args[6].dim * args[6].map_data[(n * args[6].map->dim + args[6].idx)]];
                            }
                    T7 *argIndexMap7;
                    if(args[7].argtype == OP_ARG_GBL){
                        if(args[7].acc != OP_READ){
                            argIndexMap7= &((T7 *)arg7_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap7= (T7 *)(args[7].data);
                        }
                    }else
                        if(args[7].map == OP_ID){
                            argIndexMap7=&((T7 *)arg7.data)[args[7].dim * n];
                        }else
                            if(args[7].map != NULL)
                            {
                                argIndexMap7=&((T7 *)arg7.data)[args[7].dim * args[7].map_data[(n * args[7].map->dim + args[7].idx)]];
                            }
                    T8 *argIndexMap8;
                    if(args[8].argtype == OP_ARG_GBL){
                        if(args[8].acc != OP_READ){
                            argIndexMap8= &((T8 *)arg8_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap8= (T8 *)(args[8].data);
                        }
                    }else
                        if(args[8].map == OP_ID){
                            argIndexMap8=&((T8 *)arg8.data)[args[8].dim * n];
                        }else
                            if(args[8].map != NULL)
                            {
                                argIndexMap8=&((T8 *)arg8.data)[args[8].dim * args[8].map_data[(n * args[8].map->dim + args[8].idx)]];
                            }
                    kernel(
                                argIndexMap0,
                                argIndexMap1,
                                argIndexMap2,
                                argIndexMap3,
                                argIndexMap4,
                                argIndexMap5,
                                argIndexMap6,
                                argIndexMap7,
                                argIndexMap8);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[1].acc == OP_INC)
                        for (int d = 0; d < args[1].dim; d++) {
                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                            TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[1].acc == OP_MIN)
                            for (int d = 0; d < args[1].dim; d++){
                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                arg1h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[1].acc == OP_MAX)
                                for (int d = 0; d < args[1].dim; d++){
                                    TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                    TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                    arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[2].acc == OP_INC)
                        for (int d = 0; d < args[2].dim; d++) {
                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                            TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[2].acc == OP_MIN)
                            for (int d = 0; d < args[2].dim; d++){
                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                arg2h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[2].acc == OP_MAX)
                                for (int d = 0; d < args[2].dim; d++){
                                    TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                    TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                    arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[3].acc == OP_INC)
                        for (int d = 0; d < args[3].dim; d++) {
                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                            TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[3].acc == OP_MIN)
                            for (int d = 0; d < args[3].dim; d++){
                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                arg3h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[3].acc == OP_MAX)
                                for (int d = 0; d < args[3].dim; d++){
                                    TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                    TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                    arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[4].acc == OP_INC)
                        for (int d = 0; d < args[4].dim; d++) {
                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                            TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[4].acc == OP_MIN)
                            for (int d = 0; d < args[4].dim; d++){
                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                arg4h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[4].acc == OP_MAX)
                                for (int d = 0; d < args[4].dim; d++){
                                    TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                    TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                    arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[5].acc == OP_INC)
                        for (int d = 0; d < args[5].dim; d++) {
                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                            TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[5].acc == OP_MIN)
                            for (int d = 0; d < args[5].dim; d++){
                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                arg5h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[5].acc == OP_MAX)
                                for (int d = 0; d < args[5].dim; d++){
                                    TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                    TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                    arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[6].acc == OP_INC)
                        for (int d = 0; d < args[6].dim; d++) {
                            TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                            TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[6].acc == OP_MIN)
                            for (int d = 0; d < args[6].dim; d++){
                                TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                                arg6h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[6].acc == OP_MAX)
                                for (int d = 0; d < args[6].dim; d++){
                                    TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                    TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                                    arg6h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[7].acc == OP_INC)
                        for (int d = 0; d < args[7].dim; d++) {
                            TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                            TYPE(7)* tmp2 =&((TYPE(7)*)arg7_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[7].acc == OP_MIN)
                            for (int d = 0; d < args[7].dim; d++){
                                TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                TYPE(7)* tmp2 =&((TYPE(7)*)arg7_l)[d+thr*64];
                                arg7h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[7].acc == OP_MAX)
                                for (int d = 0; d < args[7].dim; d++){
                                    TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                    TYPE(7)* tmp2 =&((TYPE(7)*)arg7_l)[d+thr*64];
                                    arg7h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[8].acc == OP_INC)
                        for (int d = 0; d < args[8].dim; d++) {
                            TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                            TYPE(8)* tmp2 =&((TYPE(8)*)arg8_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[8].acc == OP_MIN)
                            for (int d = 0; d < args[8].dim; d++){
                                TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                TYPE(8)* tmp2 =&((TYPE(8)*)arg8_l)[d+thr*64];
                                arg8h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[8].acc == OP_MAX)
                                for (int d = 0; d < args[8].dim; d++){
                                    TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                    TYPE(8)* tmp2 =&((TYPE(8)*)arg8_l)[d+thr*64];
                                    arg8h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ){
        free(arg1_l);
    }
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ){
        free(arg2_l);
    }
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ){
        free(arg3_l);
    }
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ){
        free(arg4_l);
    }
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ){
        free(arg5_l);
    }
    if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ){
        free(arg6_l);
    }
    if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ){
        free(arg7_l);
    }
    if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ){
        free(arg8_l);
    }
}
//
//op_par_loop routine for 10 arguments
//
template <class T0,class T1,class T2,class T3,
          class T4,class T5,class T6,class T7,
          class T8,class T9>
void op_par_loop(void (*kernel)(T0*, T1*, T2*, T3*, 
                                T4*, T5*, T6*, T7*,
                                T8*, T9*),
                 char const * name, op_set set,
                 op_arg arg0, op_arg arg1, op_arg arg2, op_arg arg3,
                 op_arg arg4, op_arg arg5, op_arg arg6, op_arg arg7,
                 op_arg arg8, op_arg arg9){
    op_arg args[10] = {arg0, arg1, arg2, arg3,
                       arg4, arg5, arg6, arg7,
                       arg8, arg9};
    char * arg0h;
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        arg0h = (char *)args[0].data;
    char * arg1h;
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        arg1h = (char *)args[1].data;
    char * arg2h;
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        arg2h = (char *)args[2].data;
    char * arg3h;
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        arg3h = (char *)args[3].data;
    char * arg4h;
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        arg4h = (char *)args[4].data;
    char * arg5h;
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        arg5h = (char *)args[5].data;
    char * arg6h;
    if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
        arg6h = (char *)args[6].data;
    char * arg7h;
    if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
        arg7h = (char *)args[7].data;
    char * arg8h;
    if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ)
        arg8h = (char *)args[8].data;
    char * arg9h;
    if(args[9].argtype == OP_ARG_GBL && args[9].acc != OP_READ)
        arg9h = (char *)args[9].data;
    int nargs = 10;
    int ninds = 0;
    int inds[10] = {0,0,0,0,0,0,0,0,0,0};
    int count = 0;
    int n = -1;
    op_arg_dat_inderect_mapping indmap[nargs];
    int indmapSize=0;
    for (int i = 0; i < 10; ++i) {
        if(args[i].map == OP_ID){
            inds[count] = -1;
            count++;
        }else
            if(args[i].map != NULL && args[i].idx != -1)
            {
                if(!presentInsideInds(indmap, args[i],indmapSize)){
                    n++;
                    inds[count] = n;
                    indmap[indmapSize].dat = args[i].dat;
                    indmap[indmapSize].index = args[i].idx;
                    indmap[indmapSize].map = args[i].map;
                    indmapSize++;
                    count++;
                }else
                {
                    inds[count] = n;
                    count++;
                }
            }
    }
    int conteiner[10];
    int currentsize=0;
    for (int i = 0; i < 10; ++i)
        if(inds[i] != -1)
            if(!insideConteiner(conteiner, currentsize, inds[i]))
            {
                conteiner[currentsize]=inds[i];
                currentsize++;
                ninds++;
            }
    // check if there is some reduction to do
    bool reduct = false;
    for (int i = 0; i < 10; ++i)
        if(args[i].argtype == OP_ARG_GBL && args[i].acc != OP_READ)
            reduct = true;
    int nthreads = 0;
    int set_size;
    int part_size;
    if(ninds > 0){
        if (OP_diags > 2) {
            printf(" kernel routine w/o indirection:  update");
        }
        set_size = op_mpi_halo_exchanges(set, nargs, args);
#ifdef OP_PART_SIZE
        part_size = OP_PART_SIZE
        #else
        part_size = OP_part_size;
#endif
    }
    if(reduct || ninds == 0){
#ifdef _OPENMP
        nthreads = omp_get_max_threads();
#else
        nthreads = 1;
#endif
    }
    TYPE(0)* arg0_l;
    TYPE(1)* arg1_l;
    TYPE(2)* arg2_l;
    TYPE(3)* arg3_l;
    TYPE(4)* arg4_l;
    TYPE(5)* arg5_l;
    TYPE(6)* arg6_l;
    TYPE(7)* arg7_l;
    TYPE(8)* arg8_l;
    TYPE(9)* arg9_l;
    if(reduct){
        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
        {
            arg0_l= new TYPE(0)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[0].dim; d++){
                    if(args[0].acc != OP_INC){
                        arg0_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg0_l[d + thr * 64] = arg0h[d];
                    }
                }
        }
        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
        {
            arg1_l= new TYPE(1)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[1].dim; d++){
                    if(args[1].acc != OP_INC){
                        arg1_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg1_l[d + thr * 64] = arg1h[d];
                    }
                }
        }
        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
        {
            arg2_l= new TYPE(2)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[2].dim; d++){
                    if(args[2].acc != OP_INC){
                        arg2_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg2_l[d + thr * 64] = arg2h[d];
                    }
                }
        }
        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
        {
            arg3_l= new TYPE(3)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[3].dim; d++){
                    if(args[3].acc != OP_INC){
                        arg3_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg3_l[d + thr * 64] = arg3h[d];
                    }
                }
        }
        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
        {
            arg4_l= new TYPE(4)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[4].dim; d++){
                    if(args[4].acc != OP_INC){
                        arg4_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg4_l[d + thr * 64] = arg4h[d];
                    }
                }
        }
        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
        {
            arg5_l= new TYPE(5)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[5].dim; d++){
                    if(args[5].acc != OP_INC){
                        arg5_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg5_l[d + thr * 64] = arg5h[d];
                    }
                }
        }
        if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
        {
            arg6_l= new TYPE(6)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[6].dim; d++){
                    if(args[6].acc != OP_INC){
                        arg6_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg6_l[d + thr * 64] = arg6h[d];
                    }
                }
        }
        if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
        {
            arg7_l= new TYPE(7)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[7].dim; d++){
                    if(args[7].acc != OP_INC){
                        arg7_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg7_l[d + thr * 64] = arg7h[d];
                    }
                }
        }
        if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ)
        {
            arg8_l= new TYPE(8)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[8].dim; d++){
                    if(args[8].acc != OP_INC){
                        arg8_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg8_l[d + thr * 64] = arg8h[d];
                    }
                }
        }
        if(args[9].argtype == OP_ARG_GBL && args[9].acc != OP_READ)
        {
            arg9_l= new TYPE(9)[nthreads * 64];
            for (int thr = 0; thr < nthreads; thr++)
                for (int d = 0; d < args[9].dim; d++){
                    if(args[9].acc != OP_INC){
                        arg9_l[d + thr * 64] = ZERO_float;
                    }
                    else{
                        arg9_l[d + thr * 64] = arg9h[d];
                    }
                }
        }
    }
    if(set->size > 0){
        if(ninds > 0)
        {
            op_plan *Plan = op_plan_get(name, set, part_size, nargs, args, ninds, inds);
            // execute plan
            int block_offset = 0;
            for (int col = 0; col < Plan->ncolors; col++) {
                if (col == Plan->ncolors_core) {
                    op_mpi_wait_all(nargs, args);
                }
                int nblocks = Plan->ncolblk[col];
#pragma omp parallel for
                for (int blockIdx = 0; blockIdx < nblocks; blockIdx++) {
                    int blockId = Plan->blkmap[blockIdx + block_offset];
                    int nelem = Plan->nelems[blockId];
                    int offset_b = Plan->offset[blockId];
                    for (int n = offset_b; n < offset_b + nelem; n++) {
                        T0 *argIndexMap0;
                        if(args[0].argtype == OP_ARG_GBL){
                            if(args[0].acc != OP_READ){
                                argIndexMap0= &arg0_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap0= (T0 *)(args[0].data);
                            }
                        }else
                            if(args[0].map == OP_ID){
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                            }else
                                if(args[0].map != NULL)
                                {
                                    argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                                }
                        T1 *argIndexMap1;
                        if(args[1].argtype == OP_ARG_GBL){
                            if(args[1].acc != OP_READ){
                                argIndexMap1= &arg1_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap1= (T1 *)(args[1].data);
                            }
                        }else
                            if(args[1].map == OP_ID){
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                            }else
                                if(args[1].map != NULL)
                                {
                                    argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                                }
                        T2 *argIndexMap2;
                        if(args[2].argtype == OP_ARG_GBL){
                            if(args[2].acc != OP_READ){
                                argIndexMap2= &arg2_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap2= (T2 *)(args[2].data);
                            }
                        }else
                            if(args[2].map == OP_ID){
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                            }else
                                if(args[2].map != NULL)
                                {
                                    argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                                }
                        T3 *argIndexMap3;
                        if(args[3].argtype == OP_ARG_GBL){
                            if(args[3].acc != OP_READ){
                                argIndexMap3= &arg3_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap3= (T3 *)(args[3].data);
                            }
                        }else
                            if(args[3].map == OP_ID){
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                            }else
                                if(args[3].map != NULL)
                                {
                                    argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                                }
                        T4 *argIndexMap4;
                        if(args[4].argtype == OP_ARG_GBL){
                            if(args[4].acc != OP_READ){
                                argIndexMap4= &arg4_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap4= (T4 *)(args[4].data);
                            }
                        }else
                            if(args[4].map == OP_ID){
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                            }else
                                if(args[4].map != NULL)
                                {
                                    argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                                }
                        T5 *argIndexMap5;
                        if(args[5].argtype == OP_ARG_GBL){
                            if(args[5].acc != OP_READ){
                                argIndexMap5= &arg5_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap5= (T5 *)(args[5].data);
                            }
                        }else
                            if(args[5].map == OP_ID){
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                            }else
                                if(args[5].map != NULL)
                                {
                                    argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                                }
                        T6 *argIndexMap6;
                        if(args[6].argtype == OP_ARG_GBL){
                            if(args[6].acc != OP_READ){
                                argIndexMap6= &arg6_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap6= (T6 *)(args[6].data);
                            }
                        }else
                            if(args[6].map == OP_ID){
                                argIndexMap6=&((T6 *)arg6.data)[args[6].dim * n];
                            }else
                                if(args[6].map != NULL)
                                {
                                    argIndexMap6=&((T6 *)arg6.data)[args[6].dim * args[6].map_data[(n * args[6].map->dim + args[6].idx)]];
                                }
                        T7 *argIndexMap7;
                        if(args[7].argtype == OP_ARG_GBL){
                            if(args[7].acc != OP_READ){
                                argIndexMap7= &arg7_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap7= (T7 *)(args[7].data);
                            }
                        }else
                            if(args[7].map == OP_ID){
                                argIndexMap7=&((T7 *)arg7.data)[args[7].dim * n];
                            }else
                                if(args[7].map != NULL)
                                {
                                    argIndexMap7=&((T7 *)arg7.data)[args[7].dim * args[7].map_data[(n * args[7].map->dim + args[7].idx)]];
                                }
                        T8 *argIndexMap8;
                        if(args[8].argtype == OP_ARG_GBL){
                            if(args[8].acc != OP_READ){
                                argIndexMap8= &arg8_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap8= (T8 *)(args[8].data);
                            }
                        }else
                            if(args[8].map == OP_ID){
                                argIndexMap8=&((T8 *)arg8.data)[args[8].dim * n];
                            }else
                                if(args[8].map != NULL)
                                {
                                    argIndexMap8=&((T8 *)arg8.data)[args[8].dim * args[8].map_data[(n * args[8].map->dim + args[8].idx)]];
                                }
                        T9 *argIndexMap9;
                        if(args[9].argtype == OP_ARG_GBL){
                            if(args[9].acc != OP_READ){
                                argIndexMap9= &arg9_l[64*omp_get_thread_num()];
                            }else{
                                argIndexMap9= (T9 *)(args[9].data);
                            }
                        }else
                            if(args[9].map == OP_ID){
                                argIndexMap9=&((T9 *)arg9.data)[args[9].dim * n];
                            }else
                                if(args[9].map != NULL)
                                {
                                    argIndexMap9=&((T9 *)arg9.data)[args[9].dim * args[9].map_data[(n * args[9].map->dim + args[9].idx)]];
                                }
                        kernel(
                                    argIndexMap0,
                                    argIndexMap1,
                                    argIndexMap2,
                                    argIndexMap3,
                                    argIndexMap4,
                                    argIndexMap5,
                                    argIndexMap6,
                                    argIndexMap7,
                                    argIndexMap8,
                                    argIndexMap9);
                    }
                }
                block_offset += nblocks;
                if(reduct)
                {
                    if (col == Plan->ncolors_owned-1) {
                        if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[0].acc == OP_INC)
                                    for (int d = 0; d < args[0].dim; d++) {
                                        TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                        TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[0].acc == OP_MIN)
                                        for (int d = 0; d < args[0].dim; d++){
                                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                            TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                            arg0h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[0].acc == OP_MAX)
                                            for (int d = 0; d < args[0].dim; d++){
                                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                                TYPE(0)* tmp2 =&arg0_l[d+thr*64];
                                                arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[1].acc == OP_INC)
                                    for (int d = 0; d < args[1].dim; d++) {
                                        TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                        TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[1].acc == OP_MIN)
                                        for (int d = 0; d < args[1].dim; d++){
                                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                            TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                            arg1h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[1].acc == OP_MAX)
                                            for (int d = 0; d < args[1].dim; d++){
                                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                                TYPE(1)* tmp2 =&arg1_l[d+thr*64];
                                                arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[2].acc == OP_INC)
                                    for (int d = 0; d < args[2].dim; d++) {
                                        TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                        TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[2].acc == OP_MIN)
                                        for (int d = 0; d < args[2].dim; d++){
                                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                            TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                            arg2h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[2].acc == OP_MAX)
                                            for (int d = 0; d < args[2].dim; d++){
                                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                                TYPE(2)* tmp2 =&arg2_l[d+thr*64];
                                                arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[3].acc == OP_INC)
                                    for (int d = 0; d < args[3].dim; d++) {
                                        TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                        TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[3].acc == OP_MIN)
                                        for (int d = 0; d < args[3].dim; d++){
                                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                            TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                            arg3h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[3].acc == OP_MAX)
                                            for (int d = 0; d < args[3].dim; d++){
                                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                                TYPE(3)* tmp2 =&arg3_l[d+thr*64];
                                                arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[4].acc == OP_INC)
                                    for (int d = 0; d < args[4].dim; d++) {
                                        TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                        TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[4].acc == OP_MIN)
                                        for (int d = 0; d < args[4].dim; d++){
                                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                            TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                            arg4h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[4].acc == OP_MAX)
                                            for (int d = 0; d < args[4].dim; d++){
                                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                                TYPE(4)* tmp2 =&arg4_l[d+thr*64];
                                                arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[5].acc == OP_INC)
                                    for (int d = 0; d < args[5].dim; d++) {
                                        TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                        TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[5].acc == OP_MIN)
                                        for (int d = 0; d < args[5].dim; d++){
                                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                            TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                            arg5h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[5].acc == OP_MAX)
                                            for (int d = 0; d < args[5].dim; d++){
                                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                                TYPE(5)* tmp2 =&arg5_l[d+thr*64];
                                                arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[6].acc == OP_INC)
                                    for (int d = 0; d < args[6].dim; d++) {
                                        TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                        TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[6].acc == OP_MIN)
                                        for (int d = 0; d < args[6].dim; d++){
                                            TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                            TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                            arg6h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[6].acc == OP_MAX)
                                            for (int d = 0; d < args[6].dim; d++){
                                                TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                                TYPE(6)* tmp2 =&arg6_l[d+thr*64];
                                                arg6h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[7].acc == OP_INC)
                                    for (int d = 0; d < args[7].dim; d++) {
                                        TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                        TYPE(7)* tmp2 =&arg7_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[7].acc == OP_MIN)
                                        for (int d = 0; d < args[7].dim; d++){
                                            TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                            TYPE(7)* tmp2 =&arg7_l[d+thr*64];
                                            arg7h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[7].acc == OP_MAX)
                                            for (int d = 0; d < args[7].dim; d++){
                                                TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                                TYPE(7)* tmp2 =&arg7_l[d+thr*64];
                                                arg7h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[8].acc == OP_INC)
                                    for (int d = 0; d < args[8].dim; d++) {
                                        TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                        TYPE(8)* tmp2 =&arg8_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[8].acc == OP_MIN)
                                        for (int d = 0; d < args[8].dim; d++){
                                            TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                            TYPE(8)* tmp2 =&arg8_l[d+thr*64];
                                            arg8h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[8].acc == OP_MAX)
                                            for (int d = 0; d < args[8].dim; d++){
                                                TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                                TYPE(8)* tmp2 =&arg8_l[d+thr*64];
                                                arg8h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                        if(args[9].argtype == OP_ARG_GBL && args[9].acc != OP_READ)
                        {
                            for (int thr = 0; thr < nthreads; thr++)
                                if(args[9].acc == OP_INC)
                                    for (int d = 0; d < args[9].dim; d++) {
                                        TYPE(9)* tmp1 =&((TYPE(9)*)arg9h)[d];
                                        TYPE(9)* tmp2 =&arg9_l[d+thr*64];
                                        *tmp1 += *tmp2;
                                    }
                                else
                                    if(args[9].acc == OP_MIN)
                                        for (int d = 0; d < args[9].dim; d++){
                                            TYPE(9)* tmp1 =&((TYPE(9)*)arg9h)[d];
                                            TYPE(9)* tmp2 =&arg9_l[d+thr*64];
                                            arg9h[d]= (char) MIN(*tmp1,*tmp2);
                                        }
                                    else
                                        if(args[9].acc == OP_MAX)
                                            for (int d = 0; d < args[9].dim; d++){
                                                TYPE(9)* tmp1 =&((TYPE(9)*)arg9h)[d];
                                                TYPE(9)* tmp2 =&arg9_l[d+thr*64];
                                                arg9h[d]= (char) MAX(*tmp1,*tmp2);
                                            }
                                        else
                                            perror("internal error: invalid reduction option");
                        }
                    }
                }//reduct
            }
            if(set_size==0 || set_size == set->core_size){
                op_mpi_wait_all(nargs, args);
            }
        } // ninds > 0
        else
        {
            // execute plan
#pragma omp parallel for
            for (int thr = 0; thr < nthreads; thr++) {
                int start = (set->size * thr) / nthreads;
                int finish = (set->size * (thr + 1)) / nthreads;
                for (int n = start; n < finish; n++) {
                    T0 *argIndexMap0;
                    if(args[0].argtype == OP_ARG_GBL){
                        if(args[0].acc != OP_READ){
                            argIndexMap0= &((T0 *)arg0_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap0= (T0 *)(args[0].data);
                        }
                    }else
                        if(args[0].map == OP_ID){
                            argIndexMap0=&((T0 *)arg0.data)[args[0].dim * n];
                        }else
                            if(args[0].map != NULL)
                            {
                                argIndexMap0=&((T0 *)arg0.data)[args[0].dim * args[0].map_data[(n * args[0].map->dim + args[0].idx)]];
                            }
                    T1 *argIndexMap1;
                    if(args[1].argtype == OP_ARG_GBL){
                        if(args[1].acc != OP_READ){
                            argIndexMap1= &((T1 *)arg1_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap1= (T1 *)(args[1].data);
                        }
                    }else
                        if(args[1].map == OP_ID){
                            argIndexMap1=&((T1 *)arg1.data)[args[1].dim * n];
                        }else
                            if(args[1].map != NULL)
                            {
                                argIndexMap1=&((T1 *)arg1.data)[args[1].dim * args[1].map_data[(n * args[1].map->dim + args[1].idx)]];
                            }
                    T2 *argIndexMap2;
                    if(args[2].argtype == OP_ARG_GBL){
                        if(args[2].acc != OP_READ){
                            argIndexMap2= &((T2 *)arg2_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap2= (T2 *)(args[2].data);
                        }
                    }else
                        if(args[2].map == OP_ID){
                            argIndexMap2=&((T2 *)arg2.data)[args[2].dim * n];
                        }else
                            if(args[2].map != NULL)
                            {
                                argIndexMap2=&((T2 *)arg2.data)[args[2].dim * args[2].map_data[(n * args[2].map->dim + args[2].idx)]];
                            }
                    T3 *argIndexMap3;
                    if(args[3].argtype == OP_ARG_GBL){
                        if(args[3].acc != OP_READ){
                            argIndexMap3= &((T3 *)arg3_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap3= (T3 *)(args[3].data);
                        }
                    }else
                        if(args[3].map == OP_ID){
                            argIndexMap3=&((T3 *)arg3.data)[args[3].dim * n];
                        }else
                            if(args[3].map != NULL)
                            {
                                argIndexMap3=&((T3 *)arg3.data)[args[3].dim * args[3].map_data[(n * args[3].map->dim + args[3].idx)]];
                            }
                    T4 *argIndexMap4;
                    if(args[4].argtype == OP_ARG_GBL){
                        if(args[4].acc != OP_READ){
                            argIndexMap4= &((T4 *)arg4_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap4= (T4 *)(args[4].data);
                        }
                    }else
                        if(args[4].map == OP_ID){
                            argIndexMap4=&((T4 *)arg4.data)[args[4].dim * n];
                        }else
                            if(args[4].map != NULL)
                            {
                                argIndexMap4=&((T4 *)arg4.data)[args[4].dim * args[4].map_data[(n * args[4].map->dim + args[4].idx)]];
                            }
                    T5 *argIndexMap5;
                    if(args[5].argtype == OP_ARG_GBL){
                        if(args[5].acc != OP_READ){
                            argIndexMap5= &((T5 *)arg5_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap5= (T5 *)(args[5].data);
                        }
                    }else
                        if(args[5].map == OP_ID){
                            argIndexMap5=&((T5 *)arg5.data)[args[5].dim * n];
                        }else
                            if(args[5].map != NULL)
                            {
                                argIndexMap5=&((T5 *)arg5.data)[args[5].dim * args[5].map_data[(n * args[5].map->dim + args[5].idx)]];
                            }
                    T6 *argIndexMap6;
                    if(args[6].argtype == OP_ARG_GBL){
                        if(args[6].acc != OP_READ){
                            argIndexMap6= &((T6 *)arg6_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap6= (T6 *)(args[6].data);
                        }
                    }else
                        if(args[6].map == OP_ID){
                            argIndexMap6=&((T6 *)arg6.data)[args[6].dim * n];
                        }else
                            if(args[6].map != NULL)
                            {
                                argIndexMap6=&((T6 *)arg6.data)[args[6].dim * args[6].map_data[(n * args[6].map->dim + args[6].idx)]];
                            }
                    T7 *argIndexMap7;
                    if(args[7].argtype == OP_ARG_GBL){
                        if(args[7].acc != OP_READ){
                            argIndexMap7= &((T7 *)arg7_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap7= (T7 *)(args[7].data);
                        }
                    }else
                        if(args[7].map == OP_ID){
                            argIndexMap7=&((T7 *)arg7.data)[args[7].dim * n];
                        }else
                            if(args[7].map != NULL)
                            {
                                argIndexMap7=&((T7 *)arg7.data)[args[7].dim * args[7].map_data[(n * args[7].map->dim + args[7].idx)]];
                            }
                    T8 *argIndexMap8;
                    if(args[8].argtype == OP_ARG_GBL){
                        if(args[8].acc != OP_READ){
                            argIndexMap8= &((T8 *)arg8_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap8= (T8 *)(args[8].data);
                        }
                    }else
                        if(args[8].map == OP_ID){
                            argIndexMap8=&((T8 *)arg8.data)[args[8].dim * n];
                        }else
                            if(args[8].map != NULL)
                            {
                                argIndexMap8=&((T8 *)arg8.data)[args[8].dim * args[8].map_data[(n * args[8].map->dim + args[8].idx)]];
                            }
                    T9 *argIndexMap9;
                    if(args[9].argtype == OP_ARG_GBL){
                        if(args[9].acc != OP_READ){
                            argIndexMap9= &((T9 *)arg9_l)[64*omp_get_thread_num()];
                        }else{
                            argIndexMap9= (T9 *)(args[9].data);
                        }
                    }else
                        if(args[9].map == OP_ID){
                            argIndexMap9=&((T9 *)arg9.data)[args[9].dim * n];
                        }else
                            if(args[9].map != NULL)
                            {
                                argIndexMap9=&((T9 *)arg9.data)[args[9].dim * args[9].map_data[(n * args[9].map->dim + args[9].idx)]];
                            }
                    kernel(
                                argIndexMap0,
                                argIndexMap1,
                                argIndexMap2,
                                argIndexMap3,
                                argIndexMap4,
                                argIndexMap5,
                                argIndexMap6,
                                argIndexMap7,
                                argIndexMap8,
                                argIndexMap9);
                }
            }
            if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[0].acc == OP_INC)
                        for (int d = 0; d < args[0].dim; d++) {
                            TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                            TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[0].acc == OP_MIN)
                            for (int d = 0; d < args[0].dim; d++){
                                TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                arg0h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[0].acc == OP_MAX)
                                for (int d = 0; d < args[0].dim; d++){
                                    TYPE(0)* tmp1 =&((TYPE(0)*)arg0h)[d];
                                    TYPE(0)* tmp2 =&((TYPE(0)*)arg0_l)[d+thr*64];
                                    arg0h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[1].acc == OP_INC)
                        for (int d = 0; d < args[1].dim; d++) {
                            TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                            TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[1].acc == OP_MIN)
                            for (int d = 0; d < args[1].dim; d++){
                                TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                arg1h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[1].acc == OP_MAX)
                                for (int d = 0; d < args[1].dim; d++){
                                    TYPE(1)* tmp1 =&((TYPE(1)*)arg1h)[d];
                                    TYPE(1)* tmp2 =&((TYPE(1)*)arg1_l)[d+thr*64];
                                    arg1h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[2].acc == OP_INC)
                        for (int d = 0; d < args[2].dim; d++) {
                            TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                            TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[2].acc == OP_MIN)
                            for (int d = 0; d < args[2].dim; d++){
                                TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                arg2h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[2].acc == OP_MAX)
                                for (int d = 0; d < args[2].dim; d++){
                                    TYPE(2)* tmp1 =&((TYPE(2)*)arg2h)[d];
                                    TYPE(2)* tmp2 =&((TYPE(2)*)arg2_l)[d+thr*64];
                                    arg2h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[3].acc == OP_INC)
                        for (int d = 0; d < args[3].dim; d++) {
                            TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                            TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[3].acc == OP_MIN)
                            for (int d = 0; d < args[3].dim; d++){
                                TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                arg3h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[3].acc == OP_MAX)
                                for (int d = 0; d < args[3].dim; d++){
                                    TYPE(3)* tmp1 =&((TYPE(3)*)arg3h)[d];
                                    TYPE(3)* tmp2 =&((TYPE(3)*)arg3_l)[d+thr*64];
                                    arg3h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[4].acc == OP_INC)
                        for (int d = 0; d < args[4].dim; d++) {
                            TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                            TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[4].acc == OP_MIN)
                            for (int d = 0; d < args[4].dim; d++){
                                TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                arg4h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[4].acc == OP_MAX)
                                for (int d = 0; d < args[4].dim; d++){
                                    TYPE(4)* tmp1 =&((TYPE(4)*)arg4h)[d];
                                    TYPE(4)* tmp2 =&((TYPE(4)*)arg4_l)[d+thr*64];
                                    arg4h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[5].acc == OP_INC)
                        for (int d = 0; d < args[5].dim; d++) {
                            TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                            TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[5].acc == OP_MIN)
                            for (int d = 0; d < args[5].dim; d++){
                                TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                arg5h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[5].acc == OP_MAX)
                                for (int d = 0; d < args[5].dim; d++){
                                    TYPE(5)* tmp1 =&((TYPE(5)*)arg5h)[d];
                                    TYPE(5)* tmp2 =&((TYPE(5)*)arg5_l)[d+thr*64];
                                    arg5h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[6].acc == OP_INC)
                        for (int d = 0; d < args[6].dim; d++) {
                            TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                            TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[6].acc == OP_MIN)
                            for (int d = 0; d < args[6].dim; d++){
                                TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                                arg6h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[6].acc == OP_MAX)
                                for (int d = 0; d < args[6].dim; d++){
                                    TYPE(6)* tmp1 =&((TYPE(6)*)arg6h)[d];
                                    TYPE(6)* tmp2 =&((TYPE(6)*)arg6_l)[d+thr*64];
                                    arg6h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[7].acc == OP_INC)
                        for (int d = 0; d < args[7].dim; d++) {
                            TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                            TYPE(7)* tmp2 =&((TYPE(7)*)arg7_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[7].acc == OP_MIN)
                            for (int d = 0; d < args[7].dim; d++){
                                TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                TYPE(7)* tmp2 =&((TYPE(7)*)arg7_l)[d+thr*64];
                                arg7h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[7].acc == OP_MAX)
                                for (int d = 0; d < args[7].dim; d++){
                                    TYPE(7)* tmp1 =&((TYPE(7)*)arg7h)[d];
                                    TYPE(7)* tmp2 =&((TYPE(7)*)arg7_l)[d+thr*64];
                                    arg7h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[8].acc == OP_INC)
                        for (int d = 0; d < args[8].dim; d++) {
                            TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                            TYPE(8)* tmp2 =&((TYPE(8)*)arg8_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[8].acc == OP_MIN)
                            for (int d = 0; d < args[8].dim; d++){
                                TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                TYPE(8)* tmp2 =&((TYPE(8)*)arg8_l)[d+thr*64];
                                arg8h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[8].acc == OP_MAX)
                                for (int d = 0; d < args[8].dim; d++){
                                    TYPE(8)* tmp1 =&((TYPE(8)*)arg8h)[d];
                                    TYPE(8)* tmp2 =&((TYPE(8)*)arg8_l)[d+thr*64];
                                    arg8h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
            if(args[9].argtype == OP_ARG_GBL && args[9].acc != OP_READ)
            {
                for (int thr = 0; thr < nthreads; thr++)
                    if(args[9].acc == OP_INC)
                        for (int d = 0; d < args[9].dim; d++) {
                            TYPE(9)* tmp1 =&((TYPE(9)*)arg9h)[d];
                            TYPE(9)* tmp2 =&((TYPE(9)*)arg9_l)[d+thr*64];
                            *tmp1 += *tmp2;
                        }
                    else
                        if(args[9].acc == OP_MIN)
                            for (int d = 0; d < args[9].dim; d++){
                                TYPE(9)* tmp1 =&((TYPE(9)*)arg9h)[d];
                                TYPE(9)* tmp2 =&((TYPE(9)*)arg9_l)[d+thr*64];
                                arg9h[d]= (char) MIN(*tmp1,*tmp2);
                            }
                        else
                            if(args[9].acc == OP_MAX)
                                for (int d = 0; d < args[9].dim; d++){
                                    TYPE(9)* tmp1 =&((TYPE(9)*)arg9h)[d];
                                    TYPE(9)* tmp2 =&((TYPE(9)*)arg9_l)[d+thr*64];
                                    arg9h[d]= (char) MAX(*tmp1,*tmp2);
                                }
                            else
                                perror("internal error: invalid reduction option");
            }
        }// else of ninds > 0
        op_mpi_set_dirtybit(nargs, args);
    }// set->size > 0
    if(args[0].argtype == OP_ARG_GBL && args[0].acc != OP_READ){
        free(arg0_l);
    }
    if(args[1].argtype == OP_ARG_GBL && args[1].acc != OP_READ){
        free(arg1_l);
    }
    if(args[2].argtype == OP_ARG_GBL && args[2].acc != OP_READ){
        free(arg2_l);
    }
    if(args[3].argtype == OP_ARG_GBL && args[3].acc != OP_READ){
        free(arg3_l);
    }
    if(args[4].argtype == OP_ARG_GBL && args[4].acc != OP_READ){
        free(arg4_l);
    }
    if(args[5].argtype == OP_ARG_GBL && args[5].acc != OP_READ){
        free(arg5_l);
    }
    if(args[6].argtype == OP_ARG_GBL && args[6].acc != OP_READ){
        free(arg6_l);
    }
    if(args[7].argtype == OP_ARG_GBL && args[7].acc != OP_READ){
        free(arg7_l);
    }
    if(args[8].argtype == OP_ARG_GBL && args[8].acc != OP_READ){
        free(arg8_l);
    }
    if(args[9].argtype == OP_ARG_GBL && args[9].acc != OP_READ){
        free(arg9_l);
    }
}
