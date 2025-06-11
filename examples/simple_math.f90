module simple_math
contains
  function add_numbers(a, b) result(c)
    real, intent(in) :: a, b
    real :: c
    c = a + b
  end function add_numbers

  subroutine multiply_numbers(a, b, result)
    real, intent(in) :: a, b
    real, intent(out) :: result
    result = a * b
  end subroutine multiply_numbers
end module simple_math
