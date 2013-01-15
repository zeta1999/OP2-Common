SUBROUTINE save_soln(qdim,q,qold)
  IMPLICIT NONE
  INTEGER(kind=4) :: qdim
  REAL(kind=8), DIMENSION(qdim) :: q
  REAL(kind=8), DIMENSION(*) :: qold
  INTEGER(kind=4) :: i

  DO i = 1, qdim
    qold(i) = q(i)
  END DO
END SUBROUTINE
