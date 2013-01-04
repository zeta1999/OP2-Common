inline void update(float *phim, float *res, float *u, float *rms){
  *phim -= *u;
  *res = 0.0;
  *rms += (*u)*(*u);
}
