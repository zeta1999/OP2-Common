/*
 * Open source copyright declaration based on BSD open source template:
 * http://www.opensource.org/licenses/bsd-license.php
 *
 * This file is part of the OP2 distribution.
 *
 * Copyright (c) 2011, Mike Giles and others. Please see the AUTHORS file in
 * the main source directory for a full list of copyright holders.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * The name of Mike Giles may not be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY Mike Giles ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL Mike Giles BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef __OP_VECTOR_H
#define __OP_VECTOR_H

#include "dvec.h"

#define set_m128r(lo,hi) _mm256_set_m128(hi,lo)
  //_mm256_insertf128_ps(_mm256_castps128_ps256(lo),(hi),1)

class Vec4i : public Is32vec4
{
public:
  Vec4i() {}
  Vec4i(__m128i mm) : Is32vec4(mm) {}
  Vec4i(int i) {vec = _mm_set1_epi32(i);}
  Vec4i(const Is32vec4 &mm) {vec = mm;}
  Vec4i(int i3, int i2, int i1, int i0) : Is32vec4(i3,i2,i1,i0) {}
  Vec4i(const int *p) {
    vec = _mm_load_si128((__m128i*) p);
  }
  Vec4i(const int *p, const Vec4i &idx, const int &offset) {
    vec = _mm_set_epi32(p[idx[3]+offset],p[idx[2]+offset],p[idx[1]+offset],p[idx[0]+offset]);
  }
  Vec4i(const int *p, const int &stride) {
    vec = _mm_set_epi32(p[3*stride],p[2*stride],p[stride],p[0]);
  }
  
  Vec4i& operator *=(const int &a) { return *this = _mm_mullo_epi32(vec,Vec4i(a)); }

  const int& operator[](int i) const
  {
      int *dp = (int*)&vec;
      return *(dp+i);
  }
  
  int& operator[](int i)
  {
      int *dp = (int*)&vec;
      return *(dp+i);
  }
};

static inline Vec4i operator *(const Vec4i &a, const Vec4i &b) {return  _mm_mullo_epi32(a,b);}
static inline Vec4i operator *(const int &a, const Vec4i &b) {return Vec4i(a)*b;}

typedef Vec4i Vec4i_logical;
static inline Vec4i_logical operator == (Vec4i const & a, Vec4i const & b) {
    return _mm_cmpeq_epi32(a, b);
}

class Vec4d : public F64vec4
{
public:
  Vec4d() {}
  Vec4d(__m256d mm) : F64vec4(mm) {}
  Vec4d(double d) : F64vec4(d) {}
  Vec4d(const F64vec4 &mm) {vec = mm;}
  Vec4d(double d3, double d2, double d1, double d0) : F64vec4(d3, d2, d1, d0) {}
  Vec4d(const double *p) {
    vec = _mm256_load_pd(p);
  }
  Vec4d(const double *p, const Vec4i &idx, const int &offset) {
    vec = _mm256_set_pd(p[idx[3]+offset],p[idx[2]+offset],p[idx[1]+offset],p[idx[0]+offset]);
  }
  Vec4d(const double *p, const int &stride) {
    vec = _mm256_set_pd(p[3*stride],p[2*stride],p[stride],p[0]);
  }
  Vec4d& operator=(const F64vec4 &a) {vec = a; return *this;}
  
  const double& operator[](int i) const
  {
      double *dp = (double*)&vec;
      return *(dp+i);
  }
  
  double& operator[](int i)
  {
      double *dp = (double*)&vec;
      return *(dp+i);
  }
};

class Vec4d_logical : public Vec4d
{
public:
  Vec4d_logical() {};
  Vec4d_logical(__m256d const & x) {vec = x;}
  Vec4d_logical(Vec4i_logical const &x) {
      __m128 blo = _mm_castsi128_ps(_mm_setr_epi32((int)x[0], (int)x[0], (int)x[1], (int)x[1]));
      __m128 bhi = _mm_castsi128_ps(_mm_setr_epi32((int)x[2], (int)x[2], (int)x[3], (int)x[3]));
      vec = _mm256_castps_pd(set_m128r(bhi,blo));
  }
  Vec4d_logical & operator = (__m256d const & x) {
      vec = x;
      return *this;
  }
  operator __m256d() const {
      return vec;
  }
};

static inline Vec4d operator +(const double &a, const Vec4d &b) { return Vec4d(a)+b; }
static inline Vec4d operator -(const double &a, const Vec4d &b) { return Vec4d(a)-b; }
static inline Vec4d operator *(const double &a, const Vec4d &b) { return Vec4d(a)*b; }
static inline Vec4d operator /(const double &a, const Vec4d &b) { return Vec4d(a)/b; }
static inline Vec4d fabs(const Vec4d &a) {return abs(a);}

static inline void store_a(const Vec4d &d, double *p) {_mm256_store_pd(p,d);}
static inline void store_stride(const Vec4d &d, double *p, const int &stride) {p[0] = d[0]; p[stride] = d[1]; p[2*stride] = d[2]; p[3*stride] = d[3];}
static inline void store_scatter(const Vec4d &d, double *p, const Vec4i &idx, const int &offset) {p[idx[0]+offset] = d[0]; p[idx[1]+offset] = d[1]; p[idx[2]+offset] = d[2]; p[idx[3]+offset] = d[3];}
static inline void store_scatter_add(const Vec4d &d, double *p, const Vec4i &idx, const int &offset) {p[idx[0]+offset] += d[0]; p[idx[1]+offset] += d[1]; p[idx[2]+offset] += d[2]; p[idx[3]+offset] += d[3];}

static inline Vec4d select(const Vec4d_logical &mask, const Vec4d &a, const Vec4d &b) {
  return _mm256_blendv_pd(b, a, mask);
}
typedef Vec4i intv_half;
typedef Vec4d doublev;
//typedef Vec8f floatv;


/*
#include "immintrin.h"
//
// Double precision
//
__forceinline   __m256d  operator+ (__m256d l, __m256d r)   { return _mm256_add_pd(l,r);}
__forceinline   __m256d  operator+ (double  l, __m256d r)   { return _mm256_add_pd(_mm256_set1_pd(l),r);}
__forceinline   __m256d  operator+ (__m256d l, double  r)   { return _mm256_add_pd(l,_mm256_set1_pd(r));}
__forceinline   __m256d  operator+ (__m256d l)              { return l;}
//__forceinline   __m256d& operator+=(__m256d l, __m256d r)   { return l = _mm256_add_pd(l,r);}
__forceinline   __m256d  operator- (__m256d l, __m256d r)   { return _mm256_sub_pd(l,r);}
__forceinline   __m256d  operator- (__m256d l, double  r)   { return _mm256_sub_pd(l,_mm256_set1_pd(r));}
__forceinline   __m256d  operator- (double  l, __m256d r)   { return _mm256_sub_pd(_mm256_set1_pd(l),r);}
__forceinline   __m256d  operator- (__m256d l)              { return _mm256_sub_pd(_mm256_setzero_pd(),l);}
//__forceinline   void     operator-=(__m256d l, __m256d r)   { l = _mm256_sub_pd(l,r);}
__forceinline   __m256d  operator* (__m256d l, __m256d r)   { return _mm256_mul_pd(l,r);}
__forceinline   __m256d  operator* (double l , __m256d r)   { return _mm256_mul_pd(_mm256_set1_pd(l),r);}
__forceinline   __m256d  operator* (__m256d l , double r)   { return _mm256_mul_pd(l,_mm256_set1_pd(r));}
__forceinline   __m256d  operator/ (__m256d l, __m256d r)   { return _mm256_div_pd(l,r);}
__forceinline   __m256d  operator/ (double l , __m256d r)   { return _mm256_div_pd(_mm256_set1_pd(l),r);}
__forceinline   __m256d  operator/ (__m256d l , double r)   { return _mm256_div_pd(l,_mm256_set1_pd(r));}
__forceinline   __m256d  sqrt(__m256d v)                    { return _mm256_sqrt_pd(v);}
__forceinline   __m256d  fabs(__m256d v)                    { return _mm256_max_pd(v,_mm256_sub_pd(_mm256_setzero_pd(),v));}
__forceinline   __m256d  max(__m256d v1,__m256d v2)         { return _mm256_min_pd(v1,v2);}
__forceinline   __m256d  min(__m256d v1,__m256d v2)         { return _mm256_max_pd(v1,v2);}
__forceinline   void     set(__m256d v1,double v2)          { v1 = _mm256_set1_pd(v2);}
__forceinline   double   vecreduce(__m256d v1)              { return ((double*)&v1)[0] + ((double*)&v1)[1] + ((double*)&v1)[2] + ((double*)&v1)[3];}

//
// Integers
//
__forceinline   __m128i  operator+ (__m128i l, __m128i r)   { return _mm_add_epi32(l,r);}
__forceinline   __m128i  operator+ (int  l, __m128i r)      { return _mm_add_epi32(_mm_set1_epi32(l),r);}
__forceinline   __m128i  operator+ (__m128i l, int  r)      { return _mm_add_epi32(l,_mm_set1_epi32(r));}
__forceinline   __m128i  operator+ (__m128i l)              { return l;}
//__forceinline   void     operator+=(__m128i l, __m128i r)   { l = _mm_add_epi32(l,r);}
__forceinline   __m128i  operator- (__m128i l, __m128i r)   { return _mm_sub_epi32(l,r);}
__forceinline   __m128i  operator- (__m128i l, int  r)      { return _mm_sub_epi32(l,_mm_set1_epi32(r));}
__forceinline   __m128i  operator- (int  l, __m128i r)      { return _mm_sub_epi32(_mm_set1_epi32(l),r);}
__forceinline   __m128i  operator- (__m128i l)              { return _mm_sub_epi32(_mm_setzero_si128(),l);}
//__forceinline   void     operator-=(__m128i l, __m128i r)   { l = _mm_sub_epi32(l,r);}
__forceinline   __m128i  operator* (__m128i l, __m128i r)   { return _mm_mullo_epi32(l,r);}
__forceinline   __m128i  operator* (int l , __m128i r)      { return _mm_mullo_epi32(_mm_set1_epi32(l),r);}
__forceinline   __m128i  operator* (__m128i l , int r)      { return _mm_mullo_epi32(l,_mm_set1_epi32(r));}
__forceinline   __m128i  operator/ (__m128i l, __m128i r)   { return _mm_div_epi32(l,r);}
__forceinline   __m128i  operator/ (int l , __m128i r)      { return _mm_div_epi32(_mm_set1_epi32(l),r);}
__forceinline   __m128i  operator/ (__m128i l , int r)      { return _mm_div_epi32(l,_mm_set1_epi32(r));}
__forceinline   __m128i  fabs(__m128i v)                    { return _mm_abs_epi32(v);}
__forceinline   __m128i  max(__m128i v1,__m128i v2)         { return _mm_min_epi32(v1,v2);}
__forceinline   __m128i  min(__m128i v1,__m128i v2)         { return _mm_max_epi32(v1,v2);}

//
// Single precision
//
__forceinline   __m256   operator+ (__m256  l, __m256  r)   { return _mm256_add_ps(l,r);}
__forceinline   __m256   operator+ (float  l, __m256  r)    { return _mm256_add_ps(_mm256_set1_ps(l),r);}
__forceinline   __m256   operator+ (__m256  l, float  r)    { return _mm256_add_ps(l,_mm256_set1_ps(r));}
__forceinline   __m256   operator+ (__m256  l)              { return l;}
//__forceinline   void     operator+=(__m256  l, __m256  r)   { l = _mm256_add_ps(l,r);}
__forceinline   __m256   operator- (__m256  l, __m256  r)   { return _mm256_sub_ps(l,r);}
__forceinline   __m256   operator- (__m256  l, float  r)    { return _mm256_sub_ps(l,_mm256_set1_ps(r));}
__forceinline   __m256   operator- (float  l, __m256  r)    { return _mm256_sub_ps(_mm256_set1_ps(l),r);}
__forceinline   __m256   operator- (__m256  l)              { return _mm256_sub_ps(_mm256_setzero_ps(),l);}
//__forceinline   void     operator-=(__m256  l, __m256  r)   { l = _mm256_sub_ps(l,r);}
__forceinline   __m256   operator* (__m256  l, __m256  r)   { return _mm256_mul_ps(l,r);}
__forceinline   __m256   operator* (float l , __m256  r)    { return _mm256_mul_ps(_mm256_set1_ps(l),r);}
__forceinline   __m256   operator* (__m256  l , float r)    { return _mm256_mul_ps(l,_mm256_set1_ps(r));}
__forceinline   __m256   operator/ (__m256  l, __m256  r)   { return _mm256_div_ps(l,r);}
__forceinline   __m256   operator/ (float l , __m256  r)    { return _mm256_div_ps(_mm256_set1_ps(l),r);}
__forceinline   __m256   operator/ (__m256  l , float r)    { return _mm256_div_ps(l,_mm256_set1_ps(r));}
__forceinline   __m256   sqrt(__m256  v)                    { return _mm256_sqrt_ps(v);}
__forceinline   __m256   fabs(__m256  v)                    { return _mm256_max_ps(v,_mm256_sub_ps(_mm256_setzero_ps(),v));}
__forceinline   __m256   max(__m256  v1,__m256  v2)         { return _mm256_min_ps(v1,v2);}
__forceinline   __m256   min(__m256  v1,__m256  v2)         { return _mm256_max_ps(v1,v2);}
__forceinline   void     set(__m256  v1,float v2)           { v1 = _mm256_set1_ps(v2);}
__forceinline   float   vecreduce(__m256  v1)               { return ((float*)&v1)[0] + ((float*)&v1)[1] + ((float*)&v1)[2] + ((float*)&v1)[3]+((float*)&v1)[4] + ((float*)&v1)[5] + ((float*)&v1)[6] + ((float*)&v1)[7];}

//#define __AVX__
#ifdef __AVX__
typedef __m256d doublev;
typedef __m256  floatv;
typedef __m256i intv;
typedef __m128i intv_half;
__forceinline   __m128i  vecload_half(int *p)               { return _mm_load_si128((__m128i*) p);}
__forceinline   __m256i  vecload(int *p)                    { return _mm256_load_si256((__m256i*)p);}
__forceinline   __m256   vecload(float *p)                  { return _mm256_load_ps(p);}
__forceinline   __m256d  vecload(double *p)                 { return _mm256_load_pd(p);}
__forceinline   __m128i  vecstride_half(int *p, int s)      { return _mm_set_epi32(p[3*s],p[2*s],p[s],p[0]);}
__forceinline   __m256i  vecstride(int *p, int s)           { return _mm256_set_epi32(p[7*s],p[6*s],p[5*s],p[4*s],p[3*s],p[2*s],p[s],p[0]);}
__forceinline   __m256   vecstride(float *p, int s)         { return _mm256_set_ps(p[7*s],p[6*s],p[5*s],p[4*s],p[3*s],p[2*s],p[s],p[0]);}
__forceinline   __m256d  vecstride(double *p, int s)        { return _mm256_set_pd(p[3*s],p[2*s],p[s],p[0]);}
__forceinline   __m128i  vecgather_half(int *p, __m128i i, int o)  { return _mm_set_epi32(p[((int*)&i)[3]+o],p[((int*)&i)[2]+o],p[((int*)&i)[1]+o],p[((int*)&i)[0]+o]);}
__forceinline   __m256i  vecgather(int *p, __m256i i, int o)       { return _mm256_set_epi32(p[((int*)&i)[7]+o],p[((int*)&i)[6]+o],p[((int*)&i)[5]+o],p[((int*)&i)[4]+o],p[((int*)&i)[3]+o],p[((int*)&i)[2]+o],p[((int*)&i)[1]+o],p[((int*)&i)[0]+o]);}
__forceinline   __m256   vecgather(float *p, __m256i i, int o)     { return _mm256_set_ps(p[((int*)&i)[7]+o],p[((int*)&i)[6]+o],p[((int*)&i)[5]+o],p[((int*)&i)[4]+o],p[((int*)&i)[3]+o],p[((int*)&i)[2]+o],p[((int*)&i)[1]+o],p[((int*)&i)[0]+o]);}
__forceinline   __m256d  vecgather(double *p, __m128i i, int o)    { return _mm256_set_pd(p[((int*)&i)[3]+o],p[((int*)&i)[2]+o],p[((int*)&i)[1]+o],p[((int*)&i)[0]+o]);}
__forceinline   __m128i  intzero_half()                     { return _mm_setzero_si128();}
__forceinline   __m256i  intzero()                          { return _mm256_setzero_si256();}
__forceinline   __m256   floatzero()                        { return _mm256_setzero_ps();}
__forceinline   __m256d  doublezero()                       { return _mm256_setzero_pd();}

__forceinline   void     vecstore_half(__m128i d, int *p)   { _mm_store_si128((__m128i*) p,d);}
__forceinline   void     vecstore(__m256i d, int *p)        { _mm256_store_si256((__m256i*)p,d);}
__forceinline   void     vecstore(__m256 d, float *p)       { _mm256_store_ps(p,d);}
__forceinline   void     vecstore(__m256d d, double *p)     { _mm256_store_pd(p,d);}
__forceinline   void     vecscatter(__m128i d, int *p, __m128i i, int o)
                                                            { p[((int*)&i)[0]+o] = ((int*)&d)[0]; p[((int*)&i)[1]+o] = ((int*)&d)[1]; p[((int*)&i)[2]+o] = ((int*)&d)[2]; p[((int*)&i)[3]+o] = ((int*)&d)[3];}
__forceinline   void     vecscatter(__m256i d, int *p, __m256i i, int o)
                                                            { p[((int*)&i)[0]+o] = ((int*)&d)[0]; p[((int*)&i)[1]+o] = ((int*)&d)[1]; p[((int*)&i)[2]+o] = ((int*)&d)[2]; p[((int*)&i)[3]+o] = ((int*)&d)[3]; p[((int*)&i)[4]+o] = ((int*)&d)[4]; p[((int*)&i)[5]+o] = ((int*)&d)[5]; p[((int*)&i)[6]+o] = ((int*)&d)[6]; p[((int*)&i)[7]+o] = ((int*)&d)[7];}
__forceinline   void     vecscatter(__m256  d, float *p, __m256i i, int o)
                                                            { p[((int*)&i)[0]+o] = ((float*)&d)[0]; p[((int*)&i)[1]+o] = ((float*)&d)[1]; p[((int*)&i)[2]+o] = ((float*)&d)[2]; p[((int*)&i)[3]+o] = ((float*)&d)[3]; p[((int*)&i)[4]+o] = ((float*)&d)[4]; p[((int*)&i)[5]+o] = ((float*)&d)[5]; p[((int*)&i)[6]+o] = ((float*)&d)[6]; p[((int*)&i)[7]+o] = ((float*)&d)[7];}
__forceinline   void     vecscatter(__m256d d, double *p, __m128i i, int o)
                                                            { p[((int*)&i)[0]+o] = ((double*)&d)[0]; p[((int*)&i)[1]+o] = ((double*)&d)[1]; p[((int*)&i)[2]+o] = ((double*)&d)[2]; p[((int*)&i)[3]+o] = ((double*)&d)[3];}
__forceinline   void     vecscatter_add(__m128i d, int *p, __m128i i, int o)
                                                            { p[((int*)&i)[0]+o] += ((int*)&d)[0]; p[((int*)&i)[1]+o] += ((int*)&d)[1]; p[((int*)&i)[2]+o] += ((int*)&d)[2]; p[((int*)&i)[3]+o] += ((int*)&d)[3];}
__forceinline   void     vecscatter_add(__m256i d, int *p, __m256i i, int o)
                                                            { p[((int*)&i)[0]+o] += ((int*)&d)[0]; p[((int*)&i)[1]+o] += ((int*)&d)[1]; p[((int*)&i)[2]+o] += ((int*)&d)[2]; p[((int*)&i)[3]+o] += ((int*)&d)[3]; p[((int*)&i)[4]+o] += ((int*)&d)[4]; p[((int*)&i)[5]+o] += ((int*)&d)[5]; p[((int*)&i)[6]+o] += ((int*)&d)[6]; p[((int*)&i)[7]+o] += ((int*)&d)[7];}
__forceinline   void     vecscatter_add(__m256  d, float *p, __m256i i, int o)
                                                            { p[((int*)&i)[0]+o] += ((float*)&d)[0]; p[((int*)&i)[1]+o] += ((float*)&d)[1]; p[((int*)&i)[2]+o] += ((float*)&d)[2]; p[((int*)&i)[3]+o] += ((float*)&d)[3]; p[((int*)&i)[4]+o] += ((float*)&d)[4]; p[((int*)&i)[5]+o] += ((float*)&d)[5]; p[((int*)&i)[6]+o] += ((float*)&d)[6]; p[((int*)&i)[7]+o] += ((float*)&d)[7];}
__forceinline   void     vecscatter_add(__m256d d, double *p, __m128i i, int o)
                                                            { p[((int*)&i)[0]+o] += ((double*)&d)[0]; p[((int*)&i)[1]+o] += ((double*)&d)[1]; p[((int*)&i)[2]+o] += ((double*)&d)[2]; p[((int*)&i)[3]+o] += ((double*)&d)[3];}
__forceinline   void     vecstride_st(__m128i d, int *p, int s)
                                                            {p[0] = ((int*)&d)[0]; p[s] = ((int*)&d)[1]; p[2*s] = ((int*)&d)[2]; p[3*s] = ((int*)&d)[3];}
__forceinline   void     vecstride_st(__m256i d, int *p, int s)
                                                            {p[0] = ((int*)&d)[0]; p[s] = ((int*)&d)[1]; p[2*s] = ((int*)&d)[2]; p[3*s] = ((int*)&d)[3]; p[4*s] = ((int*)&d)[4]; ((int*)&d)[5*s] = ((int*)&d)[5]; p[6*s] = ((int*)&d)[6]; p[7*s] = ((int*)&d)[7];}
__forceinline   void     vecstride_st(__m256  d, float *p, int s)
                                                            {p[0] = ((float*)&d)[0]; p[s] = ((float*)&d)[1]; p[2*s] = ((float*)&d)[2]; p[3*s] = ((float*)&d)[3]; p[4*s] = ((float*)&d)[4]; p[5*s] = ((float*)&d)[5]; p[6*s] = ((float*)&d)[6]; p[7*s] = ((float*)&d)[7];}
__forceinline   void     vecstride_st(__m256d d, double *p, int s)
                                                            {p[0] = ((double*)&d)[0]; p[s] = ((double*)&d)[1]; p[2*s] = ((double*)&d)[2]; p[3*s] = ((double*)&d)[3];}
#endif
*/
#endif /*__OP_VECTOR_H*/