program secantLIB
implicit none
real*4 epsilon, x1, x2, mof, guess
integer*2 iflag
parameter (epsilon = 1.E-6)
external mof
print*, "Enter two starting values ..."
read*, x1, x2
print*, "the function values are: ", mof(x1),mof(x2)
guess = x1
call fzero(mof, x1, x2, guess, 0., epsilon, iflag)
print*, "iflag = ", iflag
print*, "Interval: ", x1, x2
end

