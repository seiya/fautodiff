program run_macro_sample
  use macro_sample
  use macro_sample_ad
  implicit none
  real, parameter :: tol = 1.0e-12

  integer, parameter :: I_all = 0
  integer, parameter :: I_foo = 1

  integer :: length, status
  character(:), allocatable :: arg
  integer :: i_test

  i_test = I_all
  if (command_argument_count() > 0) then
     call get_command_argument(1, length=length, status=status)
     if (status == 0) then
        allocate(character(len=length) :: arg)
        call get_command_argument(1, arg, status=status)
        if (status == 0) then
           select case(arg)
           case ("foo")
              i_test = I_foo
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_foo .or. i_test == I_all) then
     call test_foo
  end if

  stop
contains

  subroutine test_foo
    real :: out_ref
    real :: out_ad, grad

    call foo(out_ref)
    call foo_fwd_ad(out_ad, grad)
    if (abs(out_ad - out_ref) > tol) then
       print *, 'test_macro_sample_fwd value mismatch', out_ad, out_ref
       error stop 1
    end if
    if (abs(grad) > tol) then
       print *, 'test_macro_sample_fwd derivative mismatch', grad
       error stop 1
    end if

    grad = 1.0
    call foo_rev_ad(grad)
    if (abs(grad) > tol) then
       print *, 'test_macro_sample_rev derivative mismatch', grad
       error stop 1
    end if

    return
  end subroutine test_foo

end program run_macro_sample
