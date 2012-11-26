
module OP2_Constants

  real(8) :: gam, gm1, cfl, eps, mach, alpha, air_const, qinf(4)

  contains

subroutine initialise_flow_field ( ncell, q, res )

	implicit none

	! formal parameters
	integer(4) :: ncell

	real(8), dimension(:) :: q
	real(8), dimension(:) :: res

	! local variables
	real(8) :: p, r, u, e

	integer(4) :: n, m

  print *, 'initialising constants'

	gam = 1.4
  gm1 = gam - 1.0
  cfl = 0.9
  eps = 0.05

  print *, 'initialising constants 2'

  mach  = 0.4
  alpha = 3.0 * atan(1.0) / 45.0
  p     = 1.0
  r     = 1.0
	u     = sqrt ( gam * p / r ) * mach
  e     = p / ( r * gm1 ) + 0.5 * u * u

  print *, 'initialising constants 3'

  qinf(1) = r
  qinf(2) = r * u
  qinf(3) = 0.0
  qinf(4) = r * e

  print *, 'initialising flow field, ncell = ', ncell

	! -4 in the subscript is done to adapt C++ code to fortran one
	do n = 1, ncell
		do m = 1, 4
			q( (4 * n + m) - 4) = qinf(m)
			res( (4 * n + m) - 4) = 0.0
		end do
	end do

end subroutine initialise_flow_field



end module
