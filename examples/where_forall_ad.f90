module where_forall_ad
  implicit none


contains

  subroutine where_example(n, a, b)
    integer, intent(in)  :: n
    real, intent(inout) :: a(n)
    real, intent(in)  :: b(n)

    where (a > 0.0)
      a = a + b
    elsewhere
      a = - a
    end where

    return
  end subroutine where_example

  subroutine where_example_fwd_ad(n, a, a_ad, b, b_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: a(n)
    real, intent(inout) :: a_ad(n)
    real, intent(in)  :: b(n)
    real, intent(in)  :: b_ad(n)

    where (a > 0.0)
      a_ad = a_ad + b_ad ! a = a + b
      a = a + b
    elsewhere
      a_ad = - a_ad ! a = -a
      a = - a
    end where

    return
  end subroutine where_example_fwd_ad

  subroutine where_example_rev_ad(n, a, a_ad, b_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: a(n)
    real, intent(inout) :: a_ad(n)
    real, intent(inout) :: b_ad(n)

    where (a > 0.0)
      b_ad = a_ad + b_ad ! a = a + b
    elsewhere
      a_ad = - a_ad ! a = -a
    end where

    return
  end subroutine where_example_rev_ad

  subroutine forall_example(n, a, b)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(out) :: b(n)
    integer :: i

    forall (i=1:n)
      b(i) = 2.0 * a(i)
    end forall

    return
  end subroutine forall_example

  subroutine forall_example_fwd_ad(n, a, a_ad, b, b_ad)
    integer, intent(in)  :: n
    real, intent(in)  :: a(n)
    real, intent(in)  :: a_ad(n)
    real, intent(out) :: b(n)
    real, intent(out) :: b_ad(n)
    integer :: i

    forall (i=1:n)
      b_ad(i) = a_ad(i) * 2.0 ! b(i) = 2.0 * a(i)
      b(i) = 2.0 * a(i)
    end forall

    return
  end subroutine forall_example_fwd_ad

  subroutine forall_example_rev_ad(n, a_ad, b_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: a_ad(n)
    real, intent(inout) :: b_ad(n)
    integer :: i

    forall (i=1:n)
      a_ad(i) = b_ad(i) * 2.0 + a_ad(i) ! b(i) = 2.0 * a(i)
      b_ad(i) = 0.0 ! b(i) = 2.0 * a(i)
    end forall

    return
  end subroutine forall_example_rev_ad

end module where_forall_ad
