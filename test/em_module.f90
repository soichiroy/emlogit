
module em

  implicit none
contains

subroutine emlogit_run(Y, X, B, tol, max_iter)
  real(8), intent(in)    :: Y(:,:)
  real(8), intent(in)    :: X(:,:)
  real(8), intent(inout) :: B(:,:)
  real(8), intent(in)    :: tol
  integer, intent(in)    :: max_iter

  integer :: iter = 1
  integer :: J
  real(8) :: omega(size(Y, 1), size(B, 2)) ! N by J
  real(8) :: diff
  J = size(B, 2)

  !! loop here
  do while (iter <= max_iter)
    ! update
    call mlogit_estep(X, B, omega, J)
    call mlogit_mtep(Y, X, B, omega, J)

    ! evaluate convergence
    !  update iterater
    diff = tol
    iter = iter + 1
  end do
end subroutine emlogit_run


! --------------------------------------------- !
! E-step
! compute E[omega]: omega ~ PG(1, Xb[j])
subroutine mlogit_estep(X, B, omega, n_choice)
  real(8), intent(in)     :: X(:,:)
  real(8), intent(in)     :: B(:,:)
  integer, intent(in)     :: n_choice
  real(8), intent(inout)  :: omega(:,:)
  real(8)                 :: XB(size(X, 1), size(B, 2))
  integer                 :: j

  do j = 2, n_choice
    XB = matmul(X, B)
    

  end do




end subroutine mlogit_estep

! --------------------------------------------- !
! M-step
! update coefficients
subroutine mlogit_mtep(Y, X, B, omega, J)
  real(8), intent(in) :: Y(:,:)
  real(8), intent(in) :: X(:,:)
  real(8), intent(inout) :: B(:,:)
  real(8), intent(in) :: omega(:,:)
  integer, intent(in) :: J




end subroutine mlogit_mtep


! function update_betaj(variables)
!
! end function update_betaj
!
!
! function convergence(variables)
!
! end function convergence

end module em
