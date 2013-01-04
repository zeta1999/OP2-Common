inline void updateP(float *r, float *p, const float *beta) {
  *p = (*beta)*(*p)+(*r);
}
