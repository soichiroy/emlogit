

!! subroutine called from R
subroutine em_mlogit_fortran(Y, X, B, tol, max_iter, ydim, xdim, bdim, ctime)

  use em
  implicit none
  integer, intent(in) :: ydim(2), xdim(2), bdim(2)
  integer, intent(in) :: max_iter
  real(8), intent(in) :: tol
  real(8), intent(in) :: Y(ydim(1), ydim(2))
  real(8), intent(in) :: X(xdim(1), xdim(2))
  real(8), intent(inout) :: B(bdim(1), bdim(2))  !! input is initialized and returns the final estimate
  real(8), intent(out)   :: ctime
  real(8) :: time_begin, time_end



  ! start keeping time ---------------------------- !
  call cpu_time(time_begin)

  ! ----------------------------------------------- !
  ! run EM algorithm and return values
  ! ----------------------------------------------- !
  call emlogit_run(Y, X, B, tol, max_iter)

  ! record the end time ----------------------------!
  call cpu_time(time_end)
  ctime = time_end - time_begin

end subroutine em_mlogit_fortran
