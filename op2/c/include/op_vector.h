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

#ifdef MIC
#include "micvec.h"
#define VECSIZE 16
#define VECSIZEH 8
#else
#define VECSIZE 8
#define VECSIZEH 4
#endif

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
  Vec4i(const int *p, const Vec4i &idx) {
    vec = _mm_set_epi32(p[idx[3]],p[idx[2]],p[idx[1]],p[idx[0]]);
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
static inline Vec4i operator +(const int &a, const Vec4i &b) {return Vec4i(a)+b;}
static inline Vec4i operator +(const Vec4i &a, const int &b) {return Vec4i(b)+a;}


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
  Vec4d(const double *p, const Vec4i &idx) {
    vec = _mm256_set_pd(p[idx[3]],p[idx[2]],p[idx[1]],p[idx[0]]);
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
      __m128 bhi = _mm_castsi128_ps(_mm_setr_epi32((int)x[0], (int)x[0], (int)x[1], (int)x[1]));
      __m128 blo = _mm_castsi128_ps(_mm_setr_epi32((int)x[2], (int)x[2], (int)x[3], (int)x[3]));
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
static inline void store_scatter(const Vec4d &d, double *p, const Vec4i &idx) {p[idx[0]] = d[0]; p[idx[1]] = d[1]; p[idx[2]] = d[2]; p[idx[3]] = d[3];}
static inline void store_scatter_add(const Vec4d &d, double *p, const Vec4i &idx) {p[idx[0]] += d[0]; p[idx[1]] += d[1]; p[idx[2]] += d[2]; p[idx[3]] += d[3];}
static inline void store_scatter_add_safe(const Vec4d &d, double *p, const Vec4i &idx) {p[idx[0]] += d[0]; p[idx[1]] += d[1]; p[idx[2]] += d[2]; p[idx[3]] += d[3];}

static inline Vec4d select(const Vec4d_logical &mask, const Vec4d &a, const Vec4d &b) {
  return _mm256_blendv_pd(b, a, mask);
}

class Vec8i
{
protected:
  Vec4i lo;
  Vec4i hi;
public:
  Vec8i() {}
  Vec8i(__m128i _lo, __m128i _hi) {lo = _lo; hi = _hi;}
  Vec8i(int i) {lo = _mm_set1_epi32(i); hi = lo;}
  Vec8i(int i7, int i6, int i5, int i4, int i3, int i2, int i1, int i0) {lo = Vec4i(i3,i2,i1,i0); hi = Vec4i(i7,i6,i5,i4);}
  Vec8i(const int *p) {
    lo = _mm_load_si128((__m128i*) p);
    hi = _mm_load_si128((__m128i*) (p+4));
  }
  Vec8i& operator *=(const int &a) { lo = a*lo; hi = a*hi; return *this; }

  const int& operator[](int i) const
  {
    if (i < 4) {
      return lo[i];
    }
    else {
      return hi[i-4];
    }
  }

  int& operator[](int i)
  {
    if (i < 4) {
      return lo[i];
    }
    else {
      return hi[i-4];
    }
  }
  const Vec4i& low() const {return lo;}
  const Vec4i& high() const {return hi;}
};

static inline Vec8i operator *(const Vec8i &a, const Vec8i &b) {return Vec8i(a.low()*b.low(),a.high()*b.high());}
static inline Vec8i operator *(const int &a, const Vec8i &b) {return Vec8i(a)*b;}

typedef Vec8i Vec8i_logical;
static inline Vec8i_logical operator == (Vec8i const & a, Vec8i const & b) {
    return Vec8i(_mm_cmpeq_epi32(a.low(), b.low()),_mm_cmpeq_epi32(a.high(), b.high()));
}


class Vec8f : public F32vec8
{
public:
  Vec8f() {}
  Vec8f(__m256 mm) : F32vec8(mm) {}
  Vec8f(float d) : F32vec8(d) {}
  Vec8f(const F32vec8 &mm) {vec = mm;}
  Vec8f(float d7, float d6, float d5, float d4, float d3, float d2, float d1, float d0) : F32vec8(d7, d6, d5, d4, d3, d2, d1, d0) {}
  Vec8f(const float *p) {
    vec = _mm256_load_ps(p);
  }
  Vec8f(const float *p, const Vec8i &idx) {
    vec = _mm256_set_ps(p[idx.high()[3]],p[idx.high()[2]],p[idx.high()[1]],p[idx.high()[0]],p[idx.low()[3]],p[idx.low()[2]],p[idx.low()[1]],p[idx.low()[0]]);
  }
  Vec8f(const float *p, const int &stride) {
    vec = _mm256_set_ps(p[7*stride],p[6*stride],p[5*stride],p[4*stride],p[3*stride],p[2*stride],p[stride],p[0]);
  }
  Vec8f& operator=(const F32vec8 &a) {vec = a; return *this;}
  
  const float& operator[](int i) const
  {
      float *dp = (float*)&vec;
      return *(dp+i);
  }

  float& operator[](int i)
  {
      float *dp = (float*)&vec;
      return *(dp+i);
  }
};

class Vec8f_logical : public Vec8f
{
public:
  Vec8f_logical() {};
  Vec8f_logical(__m256 const & x) {vec = x;}
  Vec8f_logical(Vec8i_logical const &x) {
      __m128 blo = _mm_castsi128_ps(x.low());//_mm_setr_epi32((int)x[0], (int)x[1], (int)x[2], (int)x[3]));
      __m128 bhi = _mm_castsi128_ps(x.high());//_mm_setr_epi32((int)x[4], (int)x[5], (int)x[6], (int)x[7]));
      vec = set_m128r(blo,bhi);
  }
  Vec8f_logical & operator = (__m256 const & x) {
      vec = x;
      return *this;
  }
  operator __m256() const {
      return vec;
  }
};

static inline Vec8f operator +(const float &a, const Vec8f &b) { return Vec8f(a)+b; }
static inline Vec8f operator -(const float &a, const Vec8f &b) { return Vec8f(a)-b; }
static inline Vec8f operator *(const float &a, const Vec8f &b) { return Vec8f(a)*b; }
static inline Vec8f operator /(const float &a, const Vec8f &b) { return Vec8f(a)/b; }
static inline Vec8f fabs(const Vec8f &a) {return abs(a);}
static inline Vec8f min(const Vec8f &a, const Vec8f &b) {return simd_min(a,b);}
static inline Vec8f max(const Vec8f &a, const Vec8f &b) {return simd_max(a,b);}

static inline void store_a(const Vec8f &d, float *p) {_mm256_store_ps(p,d);}
static inline void store_stride(const Vec8f &d, float *p, const int &stride) {p[0] = d[0]; p[stride] = d[1]; p[2*stride] = d[2]; p[3*stride] = d[3]; p[4*stride] = d[4]; p[5*stride] = d[5]; p[6*stride] = d[6]; p[7*stride] = d[7];}
static inline void store_scatter(const Vec8f &d, float *p, const Vec8i &idx) {p[idx.low()[0]] = d[0]; p[idx.low()[1]] = d[1]; p[idx.low()[2]] = d[2]; p[idx.low()[3]] = d[3]; p[idx.high()[0]] = d[4]; p[idx.high()[1]] = d[5]; p[idx.high()[2]] = d[6]; p[idx.high()[3]] = d[7];}
static inline void store_scatter_add(const Vec8f &d, float *p, const Vec8i &idx) {p[idx.low()[0]] += d[0]; p[idx.low()[1]] += d[1]; p[idx.low()[2]] += d[2]; p[idx.low()[3]] += d[3]; p[idx.high()[0]] += d[4]; p[idx.high()[1]] += d[5]; p[idx.high()[2]] += d[6]; p[idx.high()[3]] += d[7];}
static inline void store_scatter_add_safe(const Vec8f &d, float *p, const Vec8i &idx) {p[idx.low()[0]] += d[0]; p[idx.low()[1]] += d[1]; p[idx.low()[2]] += d[2]; p[idx.low()[3]] += d[3]; p[idx.high()[0]] += d[4]; p[idx.high()[1]] += d[5]; p[idx.high()[2]] += d[6]; p[idx.high()[3]] += d[7];}

static inline Vec8f select(const Vec8f_logical &mask, const Vec8f &a, const Vec8f &b) {
  return _mm256_blendv_ps(b, a, mask);
}
static inline Vec8f select_lt(const Vec8f &a, const Vec8f &b, const Vec8f &c, const Vec8f &d) {
 return _mm256_blendv_ps(d, c, _mm256_cmp_ps(a, b, _CMP_LT_OS));
}

static inline float min_horizontal(const Vec8f &a)
{
    F32vec8 temp = _mm256_min_ps(a, _mm256_permute_ps(a, 0xee));
    temp = _mm256_min_ps(temp, _mm256_movehdup_ps(temp));
    return _mm_cvtss_f32(_mm_min_ss(_mm256_castps256_ps128(temp), _mm256_extractf128_ps(temp,1)));
}

//
// MIC types
//
#ifdef MIC

class Vec16_logical
{
protected:
  __mmask16 vec;
public:
  Vec16_logical() {};
  Vec16_logical(__mmask16 const & x) {vec = x;}

  Vec16_logical & operator = (__mmask16 const & x) {
      vec = x;
      return *this;
  }
  operator __mmask16() const {
      return vec;
  }
};

class Vec16i : public Is32vec16
{
public:
  Vec16i() {}
  Vec16i(__m512i mm) : Is32vec16(mm) {}
  Vec16i(int i) {vec = _mm512_set1_epi32(i);}
  Vec16i(const Is32vec16 &mm) {vec = mm;}
  Vec16i(int d15, int d14, int d13, int d12, int d11, int d10, int d9, int d8,
         int d7, int d6, int d5, int d4, int d3, int d2, int d1, int d0) : Is32vec16(d15, d14, d13, d12, d11, d10, d9, d8,d7, d6, d5, d4, d3, d2, d1, d0) {}
  Vec16i(const int *p) {
    vec = _mm512_load_epi32((__m512i*) p);
  }
  Vec16i(const int *p, const Vec16i &idx) {
    vec = _mm512_i32gather_epi32(idx,p,4);
  }
  Vec16i(const int *p, const int &stride) {
    Vec16i idx = _mm512_set_epi32(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0);
    idx = _mm512_mullo_epi32(_mm512_set1_epi32(stride),idx);
    vec = _mm512_i32gather_epi32(idx,p,4);
  }
  
  Vec16i& operator *=(const int &a) { return *this = _mm512_mullo_epi32(vec,Vec16i(a)); }

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

static inline Vec16i operator *(const Vec16i &a, const Vec16i &b) {return  _mm512_mullo_epi32(a,b);}
static inline Vec16i operator *(const int &a, const Vec16i &b) {return Vec16i(a)*b;}

static inline Vec16_logical operator == (Vec16i const & a, Vec16i const & b) {
    return _mm512_cmpeq_epi32_mask(a, b);
}


class Vec16f : public F32vec16
{
public:
  Vec16f() {}
  Vec16f(__m512 mm) : F32vec16(mm) {}
  Vec16f(float d) : F32vec16(d) {}
  Vec16f(const F32vec16 &mm) {vec = mm;}
  Vec16f(float d15, float d14, float d13, float d12, float d11, float d10, float d9, float d8,
         float d7, float d6, float d5, float d4, float d3, float d2, float d1, float d0) : F32vec16(d15, d14, d13, d12, d11, d10, d9, d8,d7, d6, d5, d4, d3, d2, d1, d0) {}
  Vec16f(const float *p) {
    vec = _mm512_load_ps(p);
  }
  Vec16f(const float *p, const Vec16i &idx) {
    vec = _mm512_i32gather_ps(idx,p, 4);
//    vec = _mm512_set_ps(
//p[idx[15]],p[idx[14]],p[idx[13]],p[idx[12]],
//p[idx[11]],p[idx[10]],p[idx[9]],p[idx[8]],
//p[idx[7]],p[idx[6]],p[idx[5]],p[idx[4]],
//p[idx[3]],p[idx[2]],p[idx[1]],p[idx[0]]);
  }
  Vec16f(const float *p, const int &stride) {
    Vec16i idx = _mm512_set_epi32(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0);
    idx = idx*stride;
    vec = _mm512_i32gather_ps(idx, p, 4);
  }
  Vec16f& operator=(const F32vec16 &a) {vec = a; return *this;}
  
  const float& operator[](int i) const
  {
      float *dp = (float*)&vec;
      return *(dp+i);
  }

  float& operator[](int i)
  {
      float *dp = (float*)&vec;
      return *(dp+i);
  }
};

static inline Vec16f operator +(const float &a, const Vec16f &b) { return Vec16f(a)+b; }
static inline Vec16f operator -(const float &a, const Vec16f &b) { return Vec16f(a)-b; }
static inline Vec16f operator *(const float &a, const Vec16f &b) { return Vec16f(a)*b; }
static inline Vec16f operator /(const float &a, const Vec16f &b) { return Vec16f(a)/b; }
static inline Vec16f fabs(const Vec16f &a) {return max(a,0.0f-a);}
static inline Vec16f min(const Vec16f &a, const Vec16f &b) {return simd_min(a,b);}
static inline Vec16f max(const Vec16f &a, const Vec16f &b) {return simd_max(a,b);}
static inline Vec16_logical operator == (Vec16f const & a, Vec16f const & b) {
    return _mm512_cmpeq_ps_mask(a, b);
}

static inline void store_a(const Vec16f &d, float *p) {_mm512_store_ps(p,d);}
static inline void store_stride(const Vec16f &d, float *p, const int &stride) {Vec16i idx = _mm512_set_epi32(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0);
                                                                              idx = idx*stride;
                                                                              _mm512_i32scatter_ps(p,idx,d,4);}
static inline void store_scatter(const Vec16f &d, float *p, const Vec16i &idx) {_mm512_i32scatter_ps(p,idx,d,4);}
static inline void store_scatter_add_safe(const Vec16f &d, float *p, const Vec16i &idx) {
                                                                                Vec16f tmp(p,idx);
                                                                                _mm512_i32scatter_ps(p,idx,tmp+d,4);}
static inline void store_scatter_add(const Vec16f &d, float *p, const Vec16i &idx) {
                                                                               p[idx[0]] += d[0]; p[idx[1]] += d[1]; p[idx[2]] += d[2]; p[idx[3]] += d[3];
                                                                               p[idx[4]] += d[4]; p[idx[5]] += d[5]; p[idx[6]] += d[6]; p[idx[7]] += d[7];
                                                                               p[idx[8]] += d[8]; p[idx[9]] += d[9]; p[idx[10]] += d[10]; p[idx[11]] += d[11];
                                                                               p[idx[12]] += d[12]; p[idx[13]] += d[13]; p[idx[14]] += d[14]; p[idx[15]] += d[15];}
static inline void mask_store_scatter_add(const Vec16_logical &mask, const Vec16f &d, float *p, const Vec16i &idx) {
                                                                              Vec16f a = _mm512_mask_add_ps(d,mask,_mm512_mask_i32gather_ps(d,mask,idx,p, 4),d);
                                                                              _mm512_mask_i32scatter_ps(p,mask,idx,a,4);}
                                                                              
static inline Vec16f select(const Vec16_logical &mask, const Vec16f &a, const Vec16f &b) {
  return _mm512_mask_blend_ps(mask,b, a);
}
static inline Vec16f select_lt(const Vec16f &a, const Vec16f &b, const Vec16f &c, const Vec16f &d) {
 return _mm512_mask_blend_ps(_mm512_cmp_ps_mask(a, b, _CMP_LT_OS), d, c);
}
static inline float min_horizontal(const Vec16f &a) {return _mm512_reduce_min_ps(a);}


class Vec8d_logical
{
protected:
  __mmask8 vec;
public:
  Vec8d_logical() {};
  Vec8d_logical(__mmask8 const & x) {vec = x;}

  Vec8d_logical & operator = (__mmask8 const & x) {
      vec = x;
      return *this;
  }
  operator __mmask8() const {
      return vec;
  }
};

class Vec8im : public Vec16i
{
public:
  Vec8im() {}
  Vec8im(__m512i mm) : Vec16i(mm) {}
  Vec8im(int i) {vec = _mm512_set1_epi32(i);}
  Vec8im(const Is32vec16 &mm) {vec = mm;}
  Vec8im(int d7, int d6, int d5, int d4, int d3, int d2, int d1, int d0) : Vec16i(0,0,0,0,0,0,0,0,d7, d6, d5, d4, d3, d2, d1, d0) {}
  Vec8im(const int *p) {
    //vec = _mm512_load_epi32((void*) p);
    //vec = ((__m256i *)p)[0];
    //Vec8im vec1 = Vec8im(0);//_mm512_set1_epi32(0);
    //printf("%d %d %d %d %d %d %d %d\n",vec[8],vec[9],vec[10],vec[11],vec[12],vec[13],vec[14],vec[15]);
    vec = _mm512_setzero_epi32();
    //for (int i =8; i < 16; i++) ((int*)&vec)[i]=0;
    vec = _mm512_loadunpacklo_epi32(vec,p);
    //printf("%d %d %d %d %d %d %d %d\n",vec[8],vec[9],vec[10],vec[11],vec[12],vec[13],vec[14],vec[15]);
    //Vec8im temp;
    //for (int i =0; i < 8; i++) temp[i]=p[i];
    //for (int i =8; i < 16; i++) ((int*)&vec)[i]=0;
    //vec = temp;
  }
  Vec8im(const int *p, const Vec8im &idx) {
    vec = _mm512_i32gather_epi32(idx,p,4);
  }
  Vec8im(const int *p, const int &stride) {
    Vec8im idx = _mm512_set_epi32(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0);
    idx = _mm512_mullo_epi32(_mm512_set1_epi32(stride),idx);
    vec = _mm512_i32gather_epi32(idx,p,4);
  }
  // operator __m256i() const {
  //     return _mm512_castsi512_si256(vec);
  // }
};

static inline Vec8im operator *(const Vec8im &a, const Vec8im &b) {return  _mm512_mullo_epi32(a,b);}
static inline Vec8im operator *(const int &a, const Vec8im &b) {return Vec8im(a)*b;}
static inline Vec8im operator +(const int &a, const Vec8im &b) {return Vec8im(a)+b;}
static inline Vec8im operator +(const Vec8im &a, const int &b) {return Vec8im(b)+a;}

static inline Vec16_logical operator == (Vec8im const & a, Vec8im const & b) {
    return _mm512_cmpeq_epi32_mask(a, b);
}


class Vec8d : public F64vec8
{
public:
  Vec8d() {}
  Vec8d(__m512d mm) : F64vec8(mm) {}
  Vec8d(double d) : F64vec8(d) {}
  Vec8d(const F64vec8 &mm) {vec = mm;}
  Vec8d(double d7, double d6, double d5, double d4, double d3, double d2, double d1, double d0) : F64vec8(d7, d6, d5, d4, d3, d2, d1, d0) {}
  Vec8d(const double *p) {
    vec = _mm512_load_pd(p);
  }
  Vec8d(const double *p, const Vec8im &idx) {
    //vec = _mm512_i32gather_pd((void*)p,idx, 8);
    vec = _mm512_i32logather_pd(idx,p, 8);
  }
  Vec8d(const double *p, const int &stride) {
    Vec8im idx = _mm512_set_epi32(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0);
    idx = idx*stride;
    //vec = _mm512_i32gather_pd((void*)p,idx, 8);
    vec = _mm512_i32logather_pd(idx,p, 8);
  }
  Vec8d& operator=(const F64vec8 &a) {vec = a; return *this;}
  
  operator __m512d() const {
      return vec;
  }
  
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

static inline Vec8d operator +(const double &a, const Vec8d &b) { return Vec8d(a)+b; }
static inline Vec8d operator -(const double &a, const Vec8d &b) { return Vec8d(a)-b; }
static inline Vec8d operator *(const double &a, const Vec8d &b) { return Vec8d(a)*b; }
static inline Vec8d operator /(const double &a, const Vec8d &b) { return Vec8d(a)/b; }
static inline Vec8d fabs(const Vec8d &a) {return max(a,0.0-a);}
static inline Vec8d min(const Vec8d &a, const Vec8d &b) {return simd_min(a,b);}
static inline Vec8d max(const Vec8d &a, const Vec8d &b) {return simd_max(a,b);}
static inline Vec16_logical operator == (Vec8d const & a, Vec8d const & b) {
    return _mm512_cmpeq_pd_mask(a, b);
}

static inline void store_a(const Vec8d &d, double *p) {_mm512_store_pd(p,d);}
static inline void store_stride(const Vec8d &d, double *p, const int &stride) {Vec8im idx = _mm512_set_epi32(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0);
                                                                              idx = idx*stride;
                                                                              _mm512_i32loscatter_pd(p,idx,d,8);}
static inline void store_scatter(const Vec8d &d, double *p, const Vec8im &idx) {_mm512_i32loscatter_pd(p,idx,d,8);}
static inline void store_scatter_add_safe(const Vec8d &d, double *p, const Vec8im &idx) {
                                                                               Vec8d tmp(p,idx);
                                                                               _mm512_i32loscatter_pd(p,idx,tmp+d,8);}
static inline void store_scatter_add(const Vec8d &d, double *p, const Vec8im &idx) {
                                                                               p[idx[0]] += d[0]; p[idx[1]] += d[1]; p[idx[2]] += d[2]; p[idx[3]] += d[3];
                                                                               p[idx[4]] += d[4]; p[idx[5]] += d[5]; p[idx[6]] += d[6]; p[idx[7]] += d[7];}

static inline void mask_store_scatter_add(const Vec16_logical &mask, const Vec8d &d, double *p, const Vec8im &idx) {
                                                                              Vec8d a = _mm512_mask_add_pd(d,mask,_mm512_mask_i32logather_pd(d,mask,idx,p, 8),d);
                                                                              _mm512_mask_i32loscatter_pd(p,mask,idx,a,8);}

static inline Vec8d select(const Vec16_logical &mask, const Vec8d &a, const Vec8d &b) {
  return _mm512_mask_blend_pd(mask,b, a);
}
static inline Vec8d select_lt(const Vec8d &a, const Vec8d &b, const Vec8d &c, const Vec8d &d) {
 return _mm512_mask_blend_pd(_mm512_cmp_pd_mask(a, b, _CMP_LT_OS), d, c);
}
static inline double min_horizontal(const Vec8d &a) {return _mm512_reduce_min_pd(a);}

#endif

#ifdef MIC
typedef Vec8im intv_half;
typedef Vec8d doublev;
typedef Vec16f floatv;
typedef Vec16i intv;
#else
typedef Vec4i intv_half;
typedef Vec4d doublev;
typedef Vec8f floatv;
typedef Vec8i intv;
#endif

#endif /*__OP_VECTOR_H*/
