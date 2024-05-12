real*4 function mof(x)
implicit none
real*4 x
mof = -2. + (20 * x**2 + 1) / (1 + x**2)**2
end
