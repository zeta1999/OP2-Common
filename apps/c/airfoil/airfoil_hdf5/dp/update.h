inline void update(const double *qold, double *q, double *res, const double *adt, double *rms){
  double del, adti;

  adti = 1.0f/(*adt);

  for (int n=0; n<4; n++) {
    del    = adti*res[STRIDE(n,cells_stride)];
    q[STRIDE(n,cells_stride)]   = qold[STRIDE(n,cells_stride)] - del;
    res[STRIDE(n,cells_stride)] = 0.0f;
    *rms  += del*del;
  }
}

