inline void save_soln(const double *q, double *qold){
  for (int n=0; n<4; n++) qold[STRIDE(n,cells_stride)] = q[STRIDE(n,cells_stride)];
}

