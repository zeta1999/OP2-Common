inline void res_calc(const double *x1, const double *x2, const double *q1, const double *q2,
                     const double *adt1, const double *adt2, double *res1, double *res2) {
  double dx,dy,mu, ri, p1,vol1, p2,vol2, f;

  dx = x1[STRIDE(0,nodes_stride)] - x2[STRIDE(0,nodes_stride)];
  dy = x1[STRIDE(1,nodes_stride)] - x2[STRIDE(1,nodes_stride)];

  ri   = 1.0f/q1[STRIDE(0,cells_stride)];
  p1   = gm1*(q1[STRIDE(3,cells_stride)]-0.5f*ri*(q1[STRIDE(1,cells_stride)]*q1[STRIDE(1,cells_stride)]+q1[STRIDE(2,cells_stride)]*q1[STRIDE(2,cells_stride)]));
  vol1 =  ri*(q1[STRIDE(1,cells_stride)]*dy - q1[STRIDE(2,cells_stride)]*dx);

  ri   = 1.0f/q2[STRIDE(0,cells_stride)];
  p2   = gm1*(q2[STRIDE(3,cells_stride)]-0.5f*ri*(q2[STRIDE(1,cells_stride)]*q2[STRIDE(1,cells_stride)]+q2[STRIDE(2,cells_stride)]*q2[STRIDE(2,cells_stride)]));
  vol2 =  ri*(q2[STRIDE(1,cells_stride)]*dy - q2[STRIDE(2,cells_stride)]*dx);

  mu = 0.5f*((*adt1)+(*adt2))*eps;

  f = 0.5f*(vol1* q1[STRIDE(0,cells_stride)]         + vol2* q2[STRIDE(0,cells_stride)]        ) + mu*(q1[STRIDE(0,cells_stride)]-q2[STRIDE(0,cells_stride)]);
  res1[0] += f;
  res2[0] -= f;
  f = 0.5f*(vol1* q1[STRIDE(1,cells_stride)] + p1*dy + vol2* q2[STRIDE(1,cells_stride)] + p2*dy) + mu*(q1[STRIDE(1,cells_stride)]-q2[STRIDE(1,cells_stride)]);
  res1[1] += f;
  res2[1] -= f;
  f = 0.5f*(vol1* q1[STRIDE(2,cells_stride)] - p1*dx + vol2* q2[STRIDE(2,cells_stride)] - p2*dx) + mu*(q1[STRIDE(2,cells_stride)]-q2[STRIDE(2,cells_stride)]);
  res1[2] += f;
  res2[2] -= f;
  f = 0.5f*(vol1*(q1[STRIDE(3,cells_stride)]+p1)     + vol2*(q2[STRIDE(3,cells_stride)]+p2)    ) + mu*(q1[STRIDE(3,cells_stride)]-q2[STRIDE(3,cells_stride)]);
  res1[3] += f;
  res2[3] -= f;
}

