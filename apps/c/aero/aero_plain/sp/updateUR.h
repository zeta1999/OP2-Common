inline void updateUR(float *u, float *r, float *p, float *v, const float *alpha) {
  *u += (*alpha)*(*p);
  *r -= (*alpha)*(*v);
  *v = 0.0f;
}
