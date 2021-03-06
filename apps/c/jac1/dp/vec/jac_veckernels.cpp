//
// auto-generated by op2.py
//

#define double_ALIGN 128
#define float_ALIGN 64
#define int_ALIGN 64
#ifdef VECTORIZE
#define SIMD_VEC 4
#define ALIGNED_double __attribute__((aligned(double_ALIGN)))
#define ALIGNED_float __attribute__((aligned(float_ALIGN)))
#define ALIGNED_int __attribute__((aligned(int_ALIGN)))
#else
#define ALIGNED_double
#define ALIGNED_float
#define ALIGNED_int
#endif

// global constants
extern double alpha;

// header
#include "../user_types.h"
#include "op_lib_cpp.h"

// user kernel files
#include "res_veckernel.cpp"
#include "update_veckernel.cpp"
