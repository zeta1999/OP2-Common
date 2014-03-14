inline void adt_calc(const double *x1, const double *x2, const double *x3, const double *x4, const double *q,double *adt){
  double dx,dy, ri,u,v,c;

  ri =  1.0f/q[STRIDE(0,cells_stride)];
  u  =   ri*q[STRIDE(1,cells_stride)];
  v  =   ri*q[STRIDE(2,cells_stride)];
  c  = sqrt(gam*gm1*(ri*q[STRIDE(3,cells_stride)]-0.5f*(u*u+v*v)));

  dx = x2[STRIDE(0,nodes_stride)] - x1[STRIDE(0,nodes_stride)];
  dy = x2[STRIDE(1,nodes_stride)] - x1[STRIDE(1,nodes_stride)];
  *adt  = fabs(u*dy-v*dx) + c*sqrt(dx*dx+dy*dy);

  dx = x3[STRIDE(0,nodes_stride)] - x2[STRIDE(0,nodes_stride)];
  dy = x3[STRIDE(1,nodes_stride)] - x2[STRIDE(1,nodes_stride)];
  *adt += fabs(u*dy-v*dx) + c*sqrt(dx*dx+dy*dy);

  dx = x4[STRIDE(0,nodes_stride)] - x3[STRIDE(0,nodes_stride)];
  dy = x4[STRIDE(1,nodes_stride)] - x3[STRIDE(1,nodes_stride)];
  *adt += fabs(u*dy-v*dx) + c*sqrt(dx*dx+dy*dy);

  dx = x1[STRIDE(0,nodes_stride)] - x4[STRIDE(0,nodes_stride)];
  dy = x1[STRIDE(1,nodes_stride)] - x4[STRIDE(1,nodes_stride)];
  *adt += fabs(u*dy-v*dx) + c*sqrt(dx*dx+dy*dy);

  *adt = (*adt) / cfl;
}

