program run_store_vars
  use store_vars
  use store_vars_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  call test_do_with_recurrent_scalar

  stop
contains

  subroutine test_do_with_recurrent_scalar
    integer, parameter :: n = 3
    real :: x(n), z(n)
    real :: x_ad(n), z_ad(n)
    real :: exp_z, exp_x1, exp_x2, exp_x3

    x = (/2.0, 3.0, 4.0/)
    call do_with_recurrent_scalar(n, x, z)

    x_ad = 0.0
    z_ad = 0.0
    z_ad(n) = 1.0
    call do_with_recurrent_scalar_rev_ad(n, x, x_ad, z_ad)

    exp_z = x(1) * x(2) * x(3)
    exp_x1 = x(2) * x(3)
    exp_x2 = x(1) * x(3)
    exp_x3 = x(1) * x(2)

    if (abs(z(n) - exp_z) > tol .or. abs(x_ad(1) - exp_x1) > tol .or. &
        abs(x_ad(2) - exp_x2) > tol .or. abs(x_ad(3) - exp_x3) > tol) then
       print *, 'test_do_with_recurrent_scalar failed', z(n), x_ad
       error stop 1
    end if
    return
  end subroutine test_do_with_recurrent_scalar

end program run_store_vars
