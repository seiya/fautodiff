program run_data_storage
  use fautodiff_stack
  implicit none

  real :: arr(3)
  real :: out(3)
  real :: x1, x2, x3
  logical :: ok

  arr = (/10.0, 20.0, 30.0/)

  call fautodiff_stack_push_r(1.0)
  call fautodiff_stack_push_r(2.0)
  call fautodiff_stack_push_r(3.0)
  call fautodiff_stack_push_r(arr)

  call fautodiff_stack_pop_r(out)
  call fautodiff_stack_pop_r(x3)
  call fautodiff_stack_pop_r(x2)
  call fautodiff_stack_pop_r(x1)

  ok = .true.
  ok = ok .and. abs(x1 - 1.0) < 1.0e-6
  ok = ok .and. abs(x2 - 2.0) < 1.0e-6
  ok = ok .and. abs(x3 - 3.0) < 1.0e-6
  ok = ok .and. all(abs(out - arr) < 1.0e-6)

  if (ok) then
     print *, 'OK'
  else
     print *, 'FAIL'
  end if

end program run_data_storage
