program secantMethod
implicit none
real*4 epsilon, x1, x2, answer
integer*2 convergence
parameter (epsilon = 1.E-6)
print*, "Enter two starting values ..."
read*, x1, x2
call secant(epsilon, x1, x2, answer, convergence)
if (convergence .EQ. -1) then
   print*, "Convergence not reached ... the method failed."
else
   print*, "After ", convergence, " iterations,"
   print*, "a zero was found at ", answer
end if
end

! -------------------------------------------------------------------
! Determine the zero of mof using the secant method given x1 and x2.
! The zero is computed to within the passed eps.  Upon return, count
! gives the number of iterations it took for convergence.  It is set
! to  -1 if convergence was not reached after 1000 iterations, or if
! a horizontal secant was encountered.
! -------------------------------------------------------------------
subroutine secant(eps, x1, x2, zero, count)
implicit none
real*4 x1, y1, x2, y2, x3, y3, deltaY, zero, eps, mof
integer*2 count, max /1000/
count = 0
y1 = mof(x1)
y2 = mof(x2)
do while (.true.)
   count = count + 1
   deltaY = y1 - y2
   if (abs(deltaY) .LT. eps) then
      count = - 1
      exit
   end if
   x3 = (y1 * x2 - y2 * x1) / deltaY
   y3 = mof(x3)
   if (abs(y3) .LT. eps .or. abs(x3-x2) .LT. eps) then
      zero = x3
      exit
   end if
   x1 = x2
   y1 = y2
   x2 = x3
   y2 = y3
end do
end

