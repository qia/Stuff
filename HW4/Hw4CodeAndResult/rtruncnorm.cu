#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <math.h>
#include <curand_kernel.h>
#include <math_constants.h>

extern "C"

__global__ void rtruncnorm_kernel(float *x, int n, 
                  float *mu, float *sigma, float *a, float *b,
                  int numbtries, int maxtries)
{
    int myblock = blockIdx.x + blockIdx.y * gridDim.x;
    int blocksize = blockDim.x * blockDim.y * blockDim.z;
    int subthread = threadIdx.z*(blockDim.x * blockDim.y) + threadIdx.y*blockDim.x + threadIdx.x;
    int idx = myblock * blocksize + subthread;
    curandState rng;
    curand_init (idx,0,0,&rng);

if ( idx < n) { 
  int accepted = 0 ; // 0 means False , 1 means True
  while ( accepted == 0 && numbtries < maxtries) { 
    numbtries = numbtries + 1;
    x[idx] = mu[idx] + sigma[idx]*curand_normal(&rng);
  if (x[idx] >=a[idx] && x[idx]<=b[idx]){
    accepted = 1;
    }
  }

  while ( accepted == 0 ) {
  float u_bar = 0.;
  float psi = 0.;
  if (isinf(b[idx]!=0)) {
  u_bar = a[idx];
  } else {
  u_bar = -b[idx];
  }
  float alpha = (u_bar + sqrt((pow(u_bar,2)+4)))/2;
  float z  = u_bar  - log (curand_uniform(&rng)/alpha);
  if (u_bar  < alpha ){
  psi  = exp (-pow(alpha -z ,2)/2);
}else{
  psi = exp(-pow(u_bar -alpha,2)/2)*exp(-pow(alpha-z,2)/2);
}

if (curand_uniform(&rng) < psi  ) { 
  if (isinf(b[idx]!=0)) {
  x[idx] = mu[idx] + sigma[idx]*z ;
  accepted = 1;
  }else {
  x[idx] = mu[idx] - sigma[idx]*z ;
  accepted = 1;
  }  
}
}  
}
} // END extern "C"
