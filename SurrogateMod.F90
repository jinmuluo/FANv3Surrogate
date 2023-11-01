module SurrogateMod 

!-------------------------------------------------------------------------------
! DESCRIPTION:
! This module is built for improving the CLM-sp performance across the cron-belt
! regions by assimilating the mesocosm observations.
!
!                                         Jinmu Luo, Oct 31 2023, Ithaca
!-------------------------------------------------------------------------------

use shr_kind_mod                     , only : r8 => shr_kind_r8
use clm_varctl                       , only : iulog
use abortutils                       , only : endrun
use clm_time_manager                 , only : get_curr_date


implicit none
private

! for surrogate model 
public :: initial_sampling   
public :: RBF 
public :: cand

! for data module
public :: read_mesocosm
public :: read_clmoutput


subroutine initial_sampling(n2o_nit, nox_n2o_nit, nox_n2o_denit, n2_n2o_denit)
  ! random sampling
  real(r8), intent(inout) :: n2o_nit
  real(r8), intent(inout) :: nox_n2o_nit
  real(r8), intent(inout) :: nox_n2o_denit
  real(r8), intent(inout) :: n2_n2o_denit
  n2o_nit = (0.02 - 0.0006)*rand() + 0.0006 
  nox_n2o_nit = (10 - 0.1)*rand() + 0.1 
  nox_n2o_denit = (10 - 0.1)*rand() + 0.1
  n2_n2o_denit = (100 - 0.1)*rand() + 0.1

end subroutine initial_sampling


subroutine RBF(w, x, xi, y)
  real(r8), intent(in) :: w(:)          ! Weights
  real(r8), intent(in) :: xi(:, :)      ! Trainning samples
  real(r8), intent(in) :: x(:)          ! parameters (input)
  real(r8), intent(out) :: y            ! predictions (output)
  integer :: i

  y = 0
  do i = 1, size(w)
     y = y + w(i)*exp(-(theta* hypot(x, xi(:, i)))**2)
   
end subroutine RBF


subroutine cand()
end subroutine cand


subroutine read_mesocosm()
end subroutine read_mesocosm()


subroutine read_clmoutput()
end subroutine read_clmoutput



end module SurrogateMod 
