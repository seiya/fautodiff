module where_forall
  implicit none
contains
  subroutine where_example(n, a, b)
    integer, intent(in) :: n
    real, intent(inout) :: a(n)
    real, intent(in) :: b(n)
    where (a > 0.0)
      a = a + b
    elsewhere
      a = -a
    end where
  end subroutine where_example

  subroutine forall_example(n, a, b)
    integer, intent(in) :: n
    real, intent(in) :: a(n)
    real, intent(out) :: b(n)
    integer :: i
    forall (i = 1:n)
      b(i) = 2.0 * a(i)
    end forall
  end subroutine forall_example
end module where_forall
