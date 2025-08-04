program run_data_storage
  use fautodiff_stack
  implicit none

  integer, parameter :: n = 6
  real :: popped(2)
  real :: vals(n)
  integer :: i
  logical :: ok

  ! Force small pages so that pushes cross page boundaries
  fautodiff_stack_r4%page_size = 4
  fautodiff_stack_r4%page_num = 1
  fautodiff_stack_r4%pos = 1

  vals = [(real(i), i = 1, n)]

  ! First push crosses a page boundary
  call fautodiff_stack_push_r(vals)

  vals = [(real(n + i), i = 1, n)]

  ! Second push also crosses a page boundary
  call fautodiff_stack_push_r(vals)

  ok = .true.
  do i = 2 * n, 2, -2
     call fautodiff_stack_pop_r(popped)
     ok = ok .and. all(abs(popped - real([i - 1, i])) < 1.0e-6)
  end do

  if (ok) then
     print *, 'OK'
  else
     print *, 'FAIL'
  end if

end program run_data_storage

