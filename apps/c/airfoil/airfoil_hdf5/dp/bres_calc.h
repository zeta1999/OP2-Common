inline void bres_calc(const double *x1, const double *x2, const double *q1,
                      const double *adt1,double *res1,const int *bound) {
  double dx,dy,mu, ri, p1,vol1, p2,vol2, f;

  dx = x1[STRIDE(0,nodes_stride)] - x2[STRIDE(0,nodes_stride)];
  dy = x1[STRIDE(1,nodes_stride)] - x2[STRIDE(1,nodes_stride)];

  ri = 1.0/q1[STRIDE(0,cells_stride)];
  p1 = gm1*(q1[STRIDE(3,cells_stride)]-0.5*ri*(q1[STRIDE(1,cells_stride)]*q1[STRIDE(1,cells_stride)]+q1[STRIDE(2,cells_stride)]*q1[STRIDE(2,cells_stride)]));

  vol1 =  ri*(q1[STRIDE(1,cells_stride)]*dy - q1[STRIDE(2,cells_stride)]*dx);

  ri   = 1.0/qinf[0];
  p2   = gm1*(qinf[3]-0.5*ri*(qinf[1]*qinf[1]+qinf[2]*qinf[2]));
  vol2 =  ri*(qinf[1]*dy - qinf[2]*dx);

  mu = (*adt1)*eps;

  f = 0.5*(vol1* q1[STRIDE(0,cells_stride)]         + vol2* qinf[0]        ) + mu*(q1[STRIDE(0,cells_stride)]-qinf[0]);
  res1[0] += select(*bound==1.0,0.0,f);
  f = 0.5*(vol1* q1[STRIDE(1,cells_stride)] + p1*dy + vol2* qinf[1] + p2*dy) + mu*(q1[STRIDE(1,cells_stride)]-qinf[1]);

  res1[1] += select(*bound==1,p1*dy,f);
  f = 0.5*(vol1* q1[STRIDE(2,cells_stride)] - p1*dx + vol2* qinf[2] - p2*dx) + mu*(q1[STRIDE(2,cells_stride)]-qinf[2]);
  res1[2] += select(*bound==1,-p1*dx,f);
  f = 0.5*(vol1*(q1[STRIDE(3,cells_stride)]+p1)     + vol2*(qinf[3]+p2)    ) + mu*(q1[STRIDE(3,cells_stride)]-qinf[3]);
  res1[3] += select(*bound==1,0.0,f);
}
