library(RCUDA)

cat("Setting cuGetContext(TRUE)...\n")
cuGetContext(TRUE)
cat("done. Profiling CUDA code...\n")

cat("Loading module...\n")
m = loadModule("rtruncnorm.ptx")
cat("done. Extracting kernel...\n")
k = m$rtruncnorm_kernel
cat("done. Setting up miscellaneous stuff...\n")
n = 100L
x = rep (0.0,n)
mu = rep (2.0,n)
sigma = rep(1.0,n)
a = rep( 0,n)
b = rep( Inf,n)
numbtries = 0
maxtries = 2000

# if...
# N = 1,000,000
# => 1954 blocks of 512 threads will suffice
# => (62 x 32) grid, (512 x 1 x 1) blocks

# Fix block dims:
if (n <= 512L) {
  threads_per_block  = 512L
  block_dims = c(threads_per_block, 1L, 1L)
  grid_d1 = 1
  grid_d2 = 1
  grid_dims = c(grid_d1, grid_d2, 1L)
}else{
  threads_per_block = 512L
  block_dims = c(threads_per_block, 1L, 1L)
  grid_d1 = floor(sqrt(n/threads_per_block))
  grid_d2 = ceiling(n/(grid_d1*threads_per_block))
  grid_dims = c(grid_d1, grid_d2, 1L)
}


cat("Grid size:\n")
print(grid_dims)
cat("Block size:\n")
print(block_dims)

nthreads = prod(grid_dims)*prod(block_dims) 
cat("Total number of threads to launch = ",nthreads,"\n")
if (nthreads < n){
  stop("Grid is not large enough...!")
}

cat("TODO: Add cudaDeviceSynchronize() to see if initialization is affecting timing...\n")

cat("Running CUDA kernel...\n")

cu_time = system.time({
  cat("Copying random N(0,1)'s to device...\n")
  cu_copy_to_time = system.time({mem = copyToDevice(x)})
  cu_kernel_time = system.time({.cuda(k, mem, n, mu, sigma, a, b, numbtries, maxtries,
                                       inplace = TRUE, gridDim = grid_dims, blockDim = block_dims)})
  cat("Copying result back from device...\n")
  cu_copy_back_time = system.time({cu_ret = copyFromDevice(obj=mem,nels=mem@nels,type="float")})
  # Equivalently:
  #cu_ret = mem[]
})

cat("done. Finished profile run! :)\n")

# Not the best comparison but a rough real-world comparison:

cat("CUDA time:\n")
print(cu_time)
cat("Copy to device:\n")
print(cu_copy_to_time)
cat("Kernel:\n")
print(cu_kernel_time)
cat("Copy from device:\n")
print(cu_copy_back_time)

# TODO: free memory...