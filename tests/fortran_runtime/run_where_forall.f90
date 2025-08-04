program run_where_forall
  use where_forall
  use where_forall_ad
  implicit none
  real, parameter :: tol = 1.0e-4

  integer, parameter :: I_all = 0
  integer, parameter :: I_where_example = 1
  integer, parameter :: I_forall_example = 2

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
           case ("where_example")
              i_test = I_where_example
           case ("forall_example")
              i_test = I_forall_example
           case default
              print *, 'Invalid test name: ', arg
              error stop 1
           end select
        end if
        deallocate(arg)
     end if
  end if

  if (i_test == I_where_example .or. i_test == I_all) then
     call test_where_example
  end if
  if (i_test == I_forall_example .or. i_test == I_all) then
     call test_forall_example
  end if
  stop
contains

  subroutine test_where_example
    integer, parameter :: n = 3
    real :: a(n), b(n)
    real :: a0(n), b0(n)
    real :: a_ad(n), b_ad(n)
    real :: a_eps(n), b_eps(n), fd(n), eps
    real :: inner1, inner2

    eps = 1.0e-3
    a0 = (/ -1.0, 2.0, -3.0 /)
    b0 = (/ 1.0, 1.0, 1.0 /)
    a = a0
    b = b0
    call where_example(n, a, b)
    a_eps = a0 + eps
    b_eps = b0 + eps
    call where_example(n, a_eps, b_eps)
    fd(:) = (a_eps(:) - a(:)) / eps
    a = a0
    b = b0
    a_ad(:) = 1.0
    b_ad(:) = 1.0
    call where_example_fwd_ad(n, a, a_ad, b, b_ad)
    if (any(abs((a_ad - fd) / fd) > tol)) then
       print *, 'test_where_example_fwd failed'
       error stop 1
    end if

    inner1 = sum(a_ad(:)**2)
    a = a0
    b = b0
    b_ad(:) = 0.0
    call where_example_rev_ad(n, a, a_ad, b, b_ad)
    inner2 = sum(a_ad(:)) + sum(b_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_where_example_rev failed'
       error stop 1
    end if
  end subroutine test_where_example

  subroutine test_forall_example
    integer, parameter :: n = 3
    real :: a(n), b(n)
    real :: a_ad(n), b_ad(n)
    real :: b_eps(n), fd(n), eps
    real :: inner1, inner2

    eps = 1.0e-3
    a = (/1.0, 2.0, 3.0/)
    call forall_example(n, a, b)
    call forall_example(n, a + eps, b_eps)
    fd(:) = (b_eps(:) - b(:)) / eps
    a_ad(:) = 1.0
    call forall_example_fwd_ad(n, a, a_ad, b, b_ad)
    if (any(abs((b_ad - fd) / fd) > tol)) then
       print *, 'test_forall_example_fwd failed'
       error stop 1
    end if

    inner1 = sum(b_ad(:)**2)
    a_ad(:) = 0.0
    call forall_example_rev_ad(n, a, a_ad, b_ad)
    inner2 = sum(a_ad(:))
    if (abs((inner2 - inner1) / inner1) > tol) then
       print *, 'test_forall_example_rev failed'
       error stop 1
    end if
  end subroutine test_forall_example

end program run_where_forall
