program run_data_storage
  use fautodiff_data_storage
  implicit none

  real :: arr(3)
  real :: out(3)
  real :: x1, x2, x3
  logical :: ok

  arr = (/10.0, 20.0, 30.0/)

  call fautodiff_data_storage_push(1.0)
  call fautodiff_data_storage_push(2.0)
  call fautodiff_data_storage_push(3.0)
  call fautodiff_data_storage_push(arr)

  call fautodiff_data_storage_pop(out)
  call fautodiff_data_storage_pop(x3)
  call fautodiff_data_storage_pop(x2)
  call fautodiff_data_storage_pop(x1)

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
