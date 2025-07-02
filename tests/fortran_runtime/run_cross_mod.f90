program run_cross_mod
  use cross_mod_b
  use cross_mod_b_ad
  implicit none
  real, parameter :: tol = 1.0e-5

  call test_call_inc

  stop
contains

  subroutine test_call_inc
    real :: x
    real :: x_ad
    real :: exp_x, exp_x_ad

    x = 1.0
    call call_inc(x)

    x_ad = 1.0
    call call_inc_rev_ad(x, x_ad)

    exp_x = 2.0
    exp_x_ad = 1.0

    if (abs(x - exp_x) > tol .or. abs(x_ad - exp_x_ad) > tol) then
       print *, 'test_call_inc failed', x, x_ad
       error stop 1
    end if
    return
  end subroutine test_call_inc

end program run_cross_mod
