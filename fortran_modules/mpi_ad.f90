module mpi_ad
  use mpi
  use iso_c_binding
  implicit none
  private
  public :: mpi_bcast_fwd_ad
  public :: mpi_bcast_rev_ad
  public :: mpi_reduce_fwd_ad
  public :: mpi_reduce_rev_ad
  public :: mpi_allreduce_fwd_ad
  public :: mpi_allreduce_rev_ad
  public :: mpi_recv_fwd_ad
  public :: mpi_recv_rev_ad
  public :: mpi_send_fwd_ad
  public :: mpi_send_rev_ad
  public :: mpi_isend_fwd_ad
  public :: mpi_isend_fwd_rev_ad
  public :: mpi_isend_rev_ad
  public :: mpi_irecv_fwd_ad
  public :: mpi_irecv_fwd_rev_ad
  public :: mpi_irecv_rev_ad
  public :: mpi_put_fwd_ad
  public :: mpi_put_rev_ad
  public :: mpi_get_fwd_ad
  public :: mpi_get_rev_ad
  public :: mpi_accumulate_fwd_ad
  public :: mpi_accumulate_rev_ad
  public :: mpi_send_init_fwd_ad
  public :: mpi_send_init_fwd_rev_ad
  public :: mpi_send_init_rev_ad
  public :: mpi_recv_init_fwd_ad
  public :: mpi_recv_init_fwd_rev_ad
  public :: mpi_recv_init_rev_ad
  public :: mpi_start_fwd_ad
  public :: mpi_start_rev_ad
  public :: mpi_startall_fwd_ad
  public :: mpi_startall_rev_ad
  public :: mpi_wait_fwd_ad
  public :: mpi_wait_rev_ad
  public :: mpi_waitall_fwd_ad
  public :: mpi_waitall_rev_ad

  interface mpi_bcast_fwd_ad
     module procedure mpi_bcast_fwd_ad_r4
     module procedure mpi_bcast_fwd_ad_r8
     module procedure mpi_bcast_fwd_ad_scalar_r4
     module procedure mpi_bcast_fwd_ad_scalar_r8
  end interface
  interface mpi_bcast_rev_ad
     module procedure mpi_bcast_rev_ad_r4
     module procedure mpi_bcast_rev_ad_r8
     module procedure mpi_bcast_rev_ad_scalar_r4
     module procedure mpi_bcast_rev_ad_scalar_r8
  end interface
  interface mpi_reduce_fwd_ad
     module procedure mpi_reduce_fwd_ad_r4
     module procedure mpi_reduce_fwd_ad_r8
     module procedure mpi_reduce_fwd_ad_scalar_r4
     module procedure mpi_reduce_fwd_ad_scalar_r8
  end interface
  interface mpi_reduce_rev_ad
     module procedure mpi_reduce_rev_ad_r4
     module procedure mpi_reduce_rev_ad_r8
     module procedure mpi_reduce_rev_ad_scalar_r4
     module procedure mpi_reduce_rev_ad_scalar_r8
  end interface
  interface mpi_allreduce_fwd_ad
     module procedure mpi_allreduce_fwd_ad_r4
     module procedure mpi_allreduce_fwd_ad_r8
     module procedure mpi_allreduce_fwd_ad_scalar_r4
     module procedure mpi_allreduce_fwd_ad_scalar_r8
  end interface
  interface mpi_allreduce_rev_ad
     module procedure mpi_allreduce_rev_ad_r4
     module procedure mpi_allreduce_rev_ad_r8
     module procedure mpi_allreduce_rev_ad_scalar_r4
     module procedure mpi_allreduce_rev_ad_scalar_r8
  end interface
  interface mpi_recv_fwd_ad
     module procedure mpi_recv_fwd_ad_r4
     module procedure mpi_recv_fwd_ad_r8
     module procedure mpi_recv_fwd_ad_scalar_r4
     module procedure mpi_recv_fwd_ad_scalar_r8
  end interface
  interface mpi_recv_rev_ad
     module procedure mpi_recv_rev_ad_r4
     module procedure mpi_recv_rev_ad_r8
     module procedure mpi_recv_rev_ad_scalar_r4
     module procedure mpi_recv_rev_ad_scalar_r8
  end interface
  interface mpi_send_fwd_ad
     module procedure mpi_send_fwd_ad_r4
     module procedure mpi_send_fwd_ad_r8
     module procedure mpi_send_fwd_ad_scalar_r4
     module procedure mpi_send_fwd_ad_scalar_r8
  end interface
  interface mpi_send_rev_ad
     module procedure mpi_send_rev_ad_r4
     module procedure mpi_send_rev_ad_r8
     module procedure mpi_send_rev_ad_scalar_r4
     module procedure mpi_send_rev_ad_scalar_r8
  end interface
  interface mpi_isend_fwd_ad
     module procedure mpi_isend_fwd_ad_r4
     module procedure mpi_isend_fwd_ad_r8
     module procedure mpi_isend_fwd_ad_scalar_r4
     module procedure mpi_isend_fwd_ad_scalar_r8
  end interface
  interface mpi_isend_fwd_rev_ad
     module procedure mpi_isend_fwd_rev_ad_r4
     module procedure mpi_isend_fwd_rev_ad_r8
     module procedure mpi_isend_fwd_rev_ad_scalar_r4
     module procedure mpi_isend_fwd_rev_ad_scalar_r8
  end interface
  interface mpi_isend_rev_ad
     module procedure mpi_isend_rev_ad_r4
     module procedure mpi_isend_rev_ad_r8
     module procedure mpi_isend_rev_ad_scalar_r4
     module procedure mpi_isend_rev_ad_scalar_r8
  end interface
  interface mpi_irecv_fwd_ad
     module procedure mpi_irecv_fwd_ad_r4
     module procedure mpi_irecv_fwd_ad_r8
     module procedure mpi_irecv_fwd_ad_scalar_r4
     module procedure mpi_irecv_fwd_ad_scalar_r8
  end interface
  interface mpi_irecv_fwd_rev_ad
     module procedure mpi_irecv_fwd_rev_ad_r4
     module procedure mpi_irecv_fwd_rev_ad_r8
     module procedure mpi_irecv_fwd_rev_ad_scalar_r4
     module procedure mpi_irecv_fwd_rev_ad_scalar_r8
  end interface
  interface mpi_irecv_rev_ad
     module procedure mpi_irecv_rev_ad_r4
     module procedure mpi_irecv_rev_ad_r8
     module procedure mpi_irecv_rev_ad_scalar_r4
     module procedure mpi_irecv_rev_ad_scalar_r8
  end interface
  interface mpi_put_fwd_ad
     module procedure mpi_put_fwd_ad_r4
     module procedure mpi_put_fwd_ad_r8
     module procedure mpi_put_fwd_ad_scalar_r4
     module procedure mpi_put_fwd_ad_scalar_r8
  end interface
  interface mpi_put_rev_ad
     module procedure mpi_put_rev_ad_r4
     module procedure mpi_put_rev_ad_r8
     module procedure mpi_put_rev_ad_scalar_r4
     module procedure mpi_put_rev_ad_scalar_r8
  end interface
  interface mpi_get_fwd_ad
     module procedure mpi_get_fwd_ad_r4
     module procedure mpi_get_fwd_ad_r8
     module procedure mpi_get_fwd_ad_scalar_r4
     module procedure mpi_get_fwd_ad_scalar_r8
  end interface
  interface mpi_get_rev_ad
     module procedure mpi_get_rev_ad_r4
     module procedure mpi_get_rev_ad_r8
     module procedure mpi_get_rev_ad_scalar_r4
     module procedure mpi_get_rev_ad_scalar_r8
  end interface
  interface mpi_accumulate_fwd_ad
     module procedure mpi_accumulate_fwd_ad_r4
     module procedure mpi_accumulate_fwd_ad_r8
     module procedure mpi_accumulate_fwd_ad_scalar_r4
     module procedure mpi_accumulate_fwd_ad_scalar_r8
  end interface
  interface mpi_accumulate_rev_ad
     module procedure mpi_accumulate_rev_ad_r4
     module procedure mpi_accumulate_rev_ad_r8
     module procedure mpi_accumulate_rev_ad_scalar_r4
     module procedure mpi_accumulate_rev_ad_scalar_r8
  end interface
  interface mpi_send_init_fwd_ad
     module procedure mpi_send_init_fwd_ad_r4
     module procedure mpi_send_init_fwd_ad_r8
     module procedure mpi_send_init_fwd_ad_scalar_r4
     module procedure mpi_send_init_fwd_ad_scalar_r8
  end interface
  interface mpi_send_init_fwd_rev_ad
     module procedure mpi_send_init_fwd_rev_ad_r4
     module procedure mpi_send_init_fwd_rev_ad_r8
     module procedure mpi_send_init_fwd_rev_ad_scalar_r4
     module procedure mpi_send_init_fwd_rev_ad_scalar_r8
  end interface
  interface mpi_send_init_rev_ad
     module procedure mpi_send_init_rev_ad_r4
     module procedure mpi_send_init_rev_ad_r8
     module procedure mpi_send_init_rev_ad_scalar_r4
     module procedure mpi_send_init_rev_ad_scalar_r8
  end interface
  interface mpi_recv_init_fwd_ad
     module procedure mpi_recv_init_fwd_ad_r4
     module procedure mpi_recv_init_fwd_ad_r8
     module procedure mpi_recv_init_fwd_ad_scalar_r4
     module procedure mpi_recv_init_fwd_ad_scalar_r8
  end interface
  interface mpi_recv_init_fwd_rev_ad
     module procedure mpi_recv_init_fwd_rev_ad_r4
     module procedure mpi_recv_init_fwd_rev_ad_r8
     module procedure mpi_recv_init_fwd_rev_ad_scalar_r4
     module procedure mpi_recv_init_fwd_rev_ad_scalar_r8
  end interface
  interface mpi_recv_init_rev_ad
     module procedure mpi_recv_init_rev_ad_r4
     module procedure mpi_recv_init_rev_ad_r8
     module procedure mpi_recv_init_rev_ad_scalar_r4
     module procedure mpi_recv_init_rev_ad_scalar_r8
  end interface

  integer, parameter :: FAD_MPI_OP_SEND = 1
  integer, parameter :: FAD_MPI_OP_RECV = 2
  integer, parameter :: MAX_REQUESTS = 1024

  type :: req_map_t
     type(c_ptr) :: ptr_advar = c_null_ptr
     integer :: count = 0
     integer :: datatype = 0
     integer :: source = 0
     integer :: dest = 0
     integer :: tag = 0
     integer :: comm = 0
     integer :: op_type = 0
     integer :: request = MPI_REQUEST_NULL
  end type req_map_t

  type, extends(req_map_t) :: req_map_r4_t
     real, allocatable :: recvbuf(:)
  end type req_map_r4_t

  type, extends(req_map_t) :: req_map_r8_t
     real(8), allocatable :: recvbuf(:)
  end type req_map_r8_t

  type(req_map_r4_t) :: req_map_r4(MAX_REQUESTS)
  type(req_map_r8_t) :: req_map_r8(MAX_REQUESTS)

contains

  subroutine mpi_bcast_fwd_ad_r4(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout) :: buffer(*)
    real, intent(inout) :: buffer_ad(*)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Bcast(buffer, count, datatype, root, comm, ierr)
    call MPI_Bcast(buffer_ad, count, datatype, root, comm, ierr)
  end subroutine mpi_bcast_fwd_ad_r4

  subroutine mpi_bcast_rev_ad_r4(buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout) :: buffer_ad(*)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    call MPI_Reduce(buffer_ad, tmp, count, datatype, MPI_SUM, root, comm, ierr)
    if (rank == root) then
      buffer_ad(:count) = tmp(:count)
    else
      buffer_ad(:count) = 0.0
    end if
  end subroutine mpi_bcast_rev_ad_r4

  subroutine mpi_bcast_fwd_ad_r8(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real(8), intent(inout) :: buffer(*)
    real(8), intent(inout) :: buffer_ad(*)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Bcast(buffer, count, datatype, root, comm, ierr)
    call MPI_Bcast(buffer_ad, count, datatype, root, comm, ierr)
  end subroutine mpi_bcast_fwd_ad_r8

  subroutine mpi_bcast_rev_ad_r8(buffer_ad, count, datatype, root, comm, ierr)
    real(8), intent(inout) :: buffer_ad(*)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    call MPI_Reduce(buffer_ad, tmp, count, datatype, MPI_SUM, root, comm, ierr)
    if (rank == root) then
      buffer_ad(:count) = tmp(:count)
    else
      buffer_ad(:count) = 0.0_8
    end if
  end subroutine mpi_bcast_rev_ad_r8

  subroutine mpi_bcast_fwd_ad_scalar_r4(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout), target :: buffer
    real, intent(inout), target :: buffer_ad
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real, pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buffer), b, [1])
    call c_f_pointer(c_loc(buffer_ad), b_ad, [1])
    call mpi_bcast_fwd_ad_r4(b, b_ad, 1, datatype, root, comm, ierr)
  end subroutine mpi_bcast_fwd_ad_scalar_r4

  subroutine mpi_bcast_rev_ad_scalar_r4(buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout), target :: buffer_ad
    integer, intent(in), target :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buffer_ad), b_ad, [1])
    call mpi_bcast_rev_ad_r4(b_ad, 1, datatype, root, comm, ierr)
  end subroutine mpi_bcast_rev_ad_scalar_r4

  subroutine mpi_bcast_fwd_ad_scalar_r8(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real(8), intent(inout), target :: buffer
    real(8), intent(inout), target :: buffer_ad
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real(8), pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buffer), b, [1])
    call c_f_pointer(c_loc(buffer_ad), b_ad, [1])
    call mpi_bcast_fwd_ad_r8(b, b_ad, 1, datatype, root, comm, ierr)
  end subroutine mpi_bcast_fwd_ad_scalar_r8

  subroutine mpi_bcast_rev_ad_scalar_r8(buffer_ad, count, datatype, root, comm, ierr)
    real(8), intent(inout), target :: buffer_ad
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buffer_ad), b_ad, [1])
    call mpi_bcast_rev_ad_r8(b_ad, 1, datatype, root, comm, ierr)
  end subroutine mpi_bcast_rev_ad_scalar_r8

  subroutine mpi_reduce_fwd_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real, intent(in) :: sendbuf(*)
    real, intent(in) :: sendbuf_ad(*)
    real, intent(out) :: recvbuf(*)
    real, intent(out) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierr)
    call MPI_Reduce(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
  end subroutine mpi_reduce_fwd_ad_r4

  subroutine mpi_reduce_rev_ad_r4(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real, intent(inout) :: sendbuf_ad(*)
    real, intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    if (rank == root) tmp(:) = recvbuf_ad(:count)
    call MPI_Bcast(tmp, count, datatype, root, comm, ierr)
    sendbuf_ad(:count) = sendbuf_ad(:count) + tmp(:count)
    if (rank == root) recvbuf_ad(:count) = 0.0
  end subroutine mpi_reduce_rev_ad_r4

  subroutine mpi_reduce_fwd_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real(8), intent(in) :: sendbuf(*)
    real(8), intent(in) :: sendbuf_ad(*)
    real(8), intent(out) :: recvbuf(*)
    real(8), intent(out) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierr)
    call MPI_Reduce(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
  end subroutine mpi_reduce_fwd_ad_r8

  subroutine mpi_reduce_rev_ad_r8(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real(8), intent(inout) :: sendbuf_ad(*)
    real(8), intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    if (rank == root) tmp(:) = recvbuf_ad(:count)
    call MPI_Bcast(tmp, count, datatype, root, comm, ierr)
    sendbuf_ad(:count) = sendbuf_ad(:count) + tmp(:count)
    if (rank == root) recvbuf_ad(:count) = 0.0_8
  end subroutine mpi_reduce_rev_ad_r8

  subroutine mpi_reduce_fwd_ad_scalar_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real, intent(in), target :: sendbuf
    real, intent(in), target :: sendbuf_ad
    real, intent(out), target :: recvbuf
    real, intent(out), target :: recvbuf_ad
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real, pointer :: s(:), s_ad(:), r(:), r_ad(:)

    call c_f_pointer(c_loc(sendbuf), s, [1])
    call c_f_pointer(c_loc(sendbuf_ad), s_ad, [1])
    call c_f_pointer(c_loc(recvbuf), r, [1])
    call c_f_pointer(c_loc(recvbuf_ad), r_ad, [1])
    call mpi_reduce_fwd_ad_r4(s, s_ad, r, r_ad, 1, datatype, op, root, comm, ierr)
  end subroutine mpi_reduce_fwd_ad_scalar_r4

  subroutine mpi_reduce_rev_ad_scalar_r4(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real, intent(inout), target :: sendbuf_ad
    real, intent(inout), target :: recvbuf_ad
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real, pointer :: s_ad(:), r_ad(:)

    call c_f_pointer(c_loc(sendbuf_ad), s_ad, [1])
    call c_f_pointer(c_loc(recvbuf_ad), r_ad, [1])
    call mpi_reduce_rev_ad_r4(s_ad, r_ad, 1, datatype, op, root, comm, ierr)
  end subroutine mpi_reduce_rev_ad_scalar_r4

  subroutine mpi_reduce_fwd_ad_scalar_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real(8), intent(in), target :: sendbuf
    real(8), intent(in), target :: sendbuf_ad
    real(8), intent(out), target :: recvbuf
    real(8), intent(out), target :: recvbuf_ad
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real(8), pointer :: s(:), s_ad(:), r(:), r_ad(:)

    call c_f_pointer(c_loc(sendbuf), s, [1])
    call c_f_pointer(c_loc(sendbuf_ad), s_ad, [1])
    call c_f_pointer(c_loc(recvbuf), r, [1])
    call c_f_pointer(c_loc(recvbuf_ad), r_ad, [1])
    call mpi_reduce_fwd_ad_r8(s, s_ad, r, r_ad, 1, datatype, op, root, comm, ierr)
  end subroutine mpi_reduce_fwd_ad_scalar_r8

  subroutine mpi_reduce_rev_ad_scalar_r8(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real(8), intent(inout), target :: sendbuf_ad
    real(8), intent(inout), target :: recvbuf_ad
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real(8), pointer :: s_ad(:), r_ad(:)

    call c_f_pointer(c_loc(sendbuf_ad), s_ad, [1])
    call c_f_pointer(c_loc(recvbuf_ad), r_ad, [1])
    call mpi_reduce_rev_ad_r8(s_ad, r_ad, 1, datatype, op, root, comm, ierr)
  end subroutine mpi_reduce_rev_ad_scalar_r8

  subroutine mpi_allreduce_fwd_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real, intent(in) :: sendbuf(*)
    real, intent(in) :: sendbuf_ad(*)
    real, intent(out) :: recvbuf(*)
    real, intent(out) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr

    call MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr)
    call MPI_Allreduce(sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
  end subroutine mpi_allreduce_fwd_ad_r4

  subroutine mpi_allreduce_rev_ad_r4(sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
    real, intent(out) :: sendbuf_ad(*)
    real, intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr

    call MPI_Allreduce(recvbuf_ad, sendbuf_ad, count, datatype, op, comm, ierr)
    recvbuf_ad(:count) = 0.0
  end subroutine mpi_allreduce_rev_ad_r4

  subroutine mpi_allreduce_fwd_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real(8), intent(in) :: sendbuf(*)
    real(8), intent(in) :: sendbuf_ad(*)
    real(8), intent(out) :: recvbuf(*)
    real(8), intent(out) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr

    call MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr)
    call MPI_Allreduce(sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
  end subroutine mpi_allreduce_fwd_ad_r8

  subroutine mpi_allreduce_rev_ad_r8(sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
    real(8), intent(out) :: sendbuf_ad(*)
    real(8), intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr

    call MPI_Allreduce(recvbuf_ad, sendbuf_ad, count, datatype, op, comm, ierr)
    recvbuf_ad(:count) = 0.0_8
  end subroutine mpi_allreduce_rev_ad_r8

  subroutine mpi_allreduce_fwd_ad_scalar_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real, intent(in), target :: sendbuf
    real, intent(in), target :: sendbuf_ad
    real, intent(out), target :: recvbuf
    real, intent(out), target :: recvbuf_ad
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real, pointer :: s(:), s_ad(:), r(:), r_ad(:)

    call c_f_pointer(c_loc(sendbuf), s, [1])
    call c_f_pointer(c_loc(sendbuf_ad), s_ad, [1])
    call c_f_pointer(c_loc(recvbuf), r, [1])
    call c_f_pointer(c_loc(recvbuf_ad), r_ad, [1])
    call mpi_allreduce_fwd_ad_r4(s, s_ad, r, r_ad, 1, datatype, op, comm, ierr)
  end subroutine mpi_allreduce_fwd_ad_scalar_r4

  subroutine mpi_allreduce_rev_ad_scalar_r4(sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
    real, intent(out), target :: sendbuf_ad
    real, intent(inout), target :: recvbuf_ad
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real, pointer :: s_ad(:), r_ad(:)

    call c_f_pointer(c_loc(sendbuf_ad), s_ad, [1])
    call c_f_pointer(c_loc(recvbuf_ad), r_ad, [1])
    call mpi_allreduce_rev_ad_r4(s_ad, r_ad, 1, datatype, op, comm, ierr)
  end subroutine mpi_allreduce_rev_ad_scalar_r4

  subroutine mpi_allreduce_fwd_ad_scalar_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real(8), intent(in), target :: sendbuf
    real(8), intent(in), target :: sendbuf_ad
    real(8), intent(out), target :: recvbuf
    real(8), intent(out), target :: recvbuf_ad
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real(8), pointer :: s(:), s_ad(:), r(:), r_ad(:)

    call c_f_pointer(c_loc(sendbuf), s, [1])
    call c_f_pointer(c_loc(sendbuf_ad), s_ad, [1])
    call c_f_pointer(c_loc(recvbuf), r, [1])
    call c_f_pointer(c_loc(recvbuf_ad), r_ad, [1])
    call mpi_allreduce_fwd_ad_r8(s, s_ad, r, r_ad, 1, datatype, op, comm, ierr)
  end subroutine mpi_allreduce_fwd_ad_scalar_r8

  subroutine mpi_allreduce_rev_ad_scalar_r8(sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
    real(8), intent(out), target :: sendbuf_ad
    real(8), intent(inout), target :: recvbuf_ad
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real(8), pointer :: s_ad(:), r_ad(:)

    call c_f_pointer(c_loc(sendbuf_ad), s_ad, [1])
    call c_f_pointer(c_loc(recvbuf_ad), r_ad, [1])
    call mpi_allreduce_rev_ad_r8(s_ad, r_ad, 1, datatype, op, comm, ierr)
  end subroutine mpi_allreduce_rev_ad_scalar_r8

  subroutine mpi_recv_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real, intent(out) :: buf(*)
    real, intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr

    call MPI_Recv(buf, count, datatype, source, tag, comm, status, ierr)
    call MPI_Recv(buf_ad, count, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_r4

  subroutine mpi_recv_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, ierr)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out), optional :: ierr

    call MPI_Send(buf_ad, count, datatype, source, tag, comm, ierr)
    buf_ad(:count) = 0.0
  end subroutine mpi_recv_rev_ad_r4

  subroutine mpi_recv_fwd_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real(8), intent(out) :: buf(*)
    real(8), intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr

    call MPI_Recv(buf, count, datatype, source, tag, comm, status, ierr)
    call MPI_Recv(buf_ad, count, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_r8

  subroutine mpi_recv_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, ierr)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out), optional :: ierr

    call MPI_Send(buf_ad, count, datatype, source, tag, comm, ierr)
    buf_ad(:count) = 0.0_8
  end subroutine mpi_recv_rev_ad_r8

  subroutine mpi_recv_fwd_ad_scalar_r4(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real, intent(out), target :: buf
    real, intent(out), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    real, pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_fwd_ad_r4(b, b_ad, 1, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_scalar_r4

  subroutine mpi_recv_rev_ad_scalar_r4(buf_ad, count, datatype, source, tag, comm, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_rev_ad_r4(b_ad, 1, datatype, source, tag, comm, ierr)
  end subroutine mpi_recv_rev_ad_scalar_r4

  subroutine mpi_recv_fwd_ad_scalar_r8(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real(8), intent(out), target :: buf
    real(8), intent(out), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    real(8), pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_fwd_ad_r8(b, b_ad, 1, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_scalar_r8

  subroutine mpi_recv_rev_ad_scalar_r8(buf_ad, count, datatype, source, tag, comm, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_rev_ad_r8(b_ad, 1, datatype, source, tag, comm, ierr)
  end subroutine mpi_recv_rev_ad_scalar_r8

  subroutine mpi_send_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(in) :: buf(*)
    real, intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr

    call MPI_Send(buf, count, datatype, dest, tag, comm, ierr)
    call MPI_Send(buf_ad, count, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_fwd_ad_r4

  subroutine mpi_send_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)

    call MPI_Recv(tmp, count, datatype, dest, tag, comm, MPI_STATUS_IGNORE, ierr)
    buf_ad(:count) = buf_ad(:count) + tmp(:count)
  end subroutine mpi_send_rev_ad_r4

  subroutine mpi_send_fwd_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real(8), intent(in) :: buf(*)
    real(8), intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr

    call MPI_Send(buf, count, datatype, dest, tag, comm, ierr)
    call MPI_Send(buf_ad, count, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_fwd_ad_r8

  subroutine mpi_send_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, ierr)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)

    call MPI_Recv(tmp, count, datatype, dest, tag, comm, MPI_STATUS_IGNORE, ierr)
    buf_ad(:count) = buf_ad(:count) + tmp(:count)
  end subroutine mpi_send_rev_ad_r8

  subroutine mpi_send_fwd_ad_scalar_r4(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(in), target :: buf
    real, intent(in), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real, pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_fwd_ad_r4(b, b_ad, 1, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_fwd_ad_scalar_r4

  subroutine mpi_send_rev_ad_scalar_r4(buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_rev_ad_r4(b_ad, 1, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_rev_ad_scalar_r4

  subroutine mpi_send_fwd_ad_scalar_r8(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real(8), intent(in), target :: buf
    real(8), intent(in), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real(8), pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_fwd_ad_r8(b, b_ad, 1, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_fwd_ad_scalar_r8

  subroutine mpi_send_rev_ad_scalar_r8(buf_ad, count, datatype, dest, tag, comm, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_rev_ad_r8(b_ad, 1, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_rev_ad_scalar_r8

  subroutine mpi_isend_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real, intent(in) :: buf(*)
    real, intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Isend(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_isend_fwd_ad_r4

  subroutine mpi_isend_fwd_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_REQUESTS
      if (req_map_r4(i)%op_type == 0) exit
    end do
    if (i > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_isend_fwd_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r4(i)%recvbuf(count))
    req_map_r4(i)%ptr_advar = c_loc(buf_ad(1))
    req_map_r4(i)%count = count
    req_map_r4(i)%datatype = datatype
    req_map_r4(i)%source = dest
    req_map_r4(i)%tag = tag
    req_map_r4(i)%comm = comm
    req_map_r4(i)%op_type = FAD_MPI_OP_RECV
    request_ad = i
  end subroutine mpi_isend_fwd_rev_ad_r4

  subroutine mpi_isend_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    idx = request_ad
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_isend_rev_ad_r4: ", request_ad
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Wait(req_map_r4(idx)%request, MPI_STATUS_IGNORE, ierr)
    call update_advar(req_map_r4(idx))
    if (allocated(req_map_r4(idx)%recvbuf)) deallocate(req_map_r4(idx)%recvbuf)
    req_map_r4(idx)%ptr_advar = c_null_ptr
    req_map_r4(idx)%count = 0
    req_map_r4(idx)%datatype = 0
    req_map_r4(idx)%op_type = 0
    req_map_r4(idx)%request = MPI_REQUEST_NULL
  end subroutine mpi_isend_rev_ad_r4

  subroutine mpi_isend_fwd_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real(8), intent(in) :: buf(*)
    real(8), intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Isend(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_isend_fwd_ad_r8

  subroutine mpi_isend_fwd_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_REQUESTS
      if (req_map_r8(i)%op_type == 0) exit
    end do
    if (i > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_isend_fwd_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r8(i)%recvbuf(count))
    req_map_r8(i)%ptr_advar = c_loc(buf_ad(1))
    req_map_r8(i)%count = count
    req_map_r8(i)%datatype = datatype
    req_map_r8(i)%source = dest
    req_map_r8(i)%tag = tag
    req_map_r8(i)%comm = comm
    req_map_r8(i)%op_type = FAD_MPI_OP_RECV
    request_ad = i + MAX_REQUESTS  ! Offset to differentiate from r4 requests
  end subroutine mpi_isend_fwd_rev_ad_r8

  subroutine mpi_isend_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    idx = request_ad - MAX_REQUESTS  ! Adjust index for r8 requests
    call MPI_Wait(req_map_r8(idx)%request, MPI_STATUS_IGNORE, ierr)
    call update_advar(req_map_r8(idx))
    if (allocated(req_map_r8(idx)%recvbuf)) deallocate(req_map_r8(idx)%recvbuf)
    req_map_r8(idx)%ptr_advar = c_null_ptr
    req_map_r8(idx)%count = 0
    req_map_r8(idx)%datatype = 0
    req_map_r8(idx)%op_type = 0
    req_map_r8(idx)%request = MPI_REQUEST_NULL
  end subroutine mpi_isend_rev_ad_r8

  subroutine mpi_isend_fwd_ad_scalar_r4(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real, intent(in), target :: buf
    real, intent(in), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_isend_fwd_ad_r4(b, b_ad, 1, datatype, dest, tag, comm, request, request_ad, ierr)
  end subroutine mpi_isend_fwd_ad_scalar_r4

  subroutine mpi_isend_fwd_rev_ad_scalar_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_isend_fwd_rev_ad_r4(b_ad, 1, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_isend_fwd_rev_ad_scalar_r4

  subroutine mpi_isend_rev_ad_scalar_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_isend_rev_ad_r4(b_ad, 1, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_isend_rev_ad_scalar_r4

  subroutine mpi_isend_fwd_ad_scalar_r8(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real(8), intent(in), target :: buf
    real(8), intent(in), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_isend_fwd_ad_r8(b, b_ad, 1, datatype, dest, tag, comm, request, request_ad, ierr)
  end subroutine mpi_isend_fwd_ad_scalar_r8

  subroutine mpi_isend_fwd_rev_ad_scalar_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_isend_fwd_rev_ad_r8(b_ad, 1, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_isend_fwd_rev_ad_scalar_r8

  subroutine mpi_isend_rev_ad_scalar_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_isend_rev_ad_r8(b_ad, 1, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_isend_rev_ad_scalar_r8

  subroutine mpi_irecv_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real, intent(out) :: buf(*)
    real, intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Irecv(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_irecv_fwd_ad_r4

  subroutine mpi_irecv_fwd_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_REQUESTS
      if (req_map_r4(i)%op_type == 0) exit
    end do
    if (i > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_irecv_fwd_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r4(i)%recvbuf(count))
    req_map_r4(i)%recvbuf = buf_ad(:count)
    req_map_r4(i)%ptr_advar = c_loc(buf_ad(1))
    req_map_r4(i)%count = count
    req_map_r4(i)%datatype = datatype
    req_map_r4(i)%dest = source
    req_map_r4(i)%tag = tag
    req_map_r4(i)%comm = comm
    req_map_r4(i)%op_type = FAD_MPI_OP_SEND
    request_ad = i
  end subroutine mpi_irecv_fwd_rev_ad_r4

  subroutine mpi_irecv_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    idx = request_ad
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_irecv_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Wait(req_map_r4(idx)%request, MPI_STATUS_IGNORE, ierr)
    call update_advar(req_map_r4(idx))
    req_map_r4(idx)%ptr_advar = c_null_ptr
    req_map_r4(idx)%count = 0
    req_map_r4(idx)%datatype = 0
    req_map_r4(idx)%op_type = 0
    req_map_r4(idx)%request = MPI_REQUEST_NULL
  end subroutine mpi_irecv_rev_ad_r4

  subroutine mpi_irecv_fwd_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real(8), intent(out) :: buf(*)
    real(8), intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Irecv(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_irecv_fwd_ad_r8

  subroutine mpi_irecv_fwd_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_REQUESTS
      if (req_map_r8(i)%op_type == 0) exit
    end do
    if (i > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_irecv_fwd_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if

    req_map_r8(i)%ptr_advar = c_loc(buf_ad(1))
    req_map_r8(i)%count = count
    req_map_r8(i)%datatype = datatype
    req_map_r8(i)%dest = source
    req_map_r8(i)%tag = tag
    req_map_r8(i)%comm = comm
    req_map_r8(i)%op_type = FAD_MPI_OP_SEND
    request_ad = i + MAX_REQUESTS  ! Offset to differentiate from r4 requests
  end subroutine mpi_irecv_fwd_rev_ad_r8

  subroutine mpi_irecv_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    idx = request_ad - MAX_REQUESTS  ! Adjust index for r8 requests
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_irecv_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Wait(req_map_r8(idx)%request, MPI_STATUS_IGNORE, ierr)
    call update_advar(req_map_r8(idx))
    req_map_r8(idx)%ptr_advar = c_null_ptr
    req_map_r8(idx)%count = 0
    req_map_r8(idx)%datatype = 0
    req_map_r8(idx)%op_type = 0
    req_map_r8(idx)%request = MPI_REQUEST_NULL
  end subroutine mpi_irecv_rev_ad_r8

  subroutine mpi_irecv_fwd_ad_scalar_r4(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real, intent(out), target :: buf
    real, intent(out), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_irecv_fwd_ad_r4(b, b_ad, 1, datatype, source, tag, comm, request, request_ad, ierr)
  end subroutine mpi_irecv_fwd_ad_scalar_r4

  subroutine mpi_irecv_fwd_rev_ad_scalar_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_irecv_fwd_rev_ad_r4(b_ad, 1, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_irecv_fwd_rev_ad_scalar_r4

  subroutine mpi_irecv_rev_ad_scalar_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_irecv_rev_ad_r4(b_ad, 1, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_irecv_rev_ad_scalar_r4

  subroutine mpi_irecv_fwd_ad_scalar_r8(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real(8), intent(out), target :: buf
    real(8), intent(out), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_irecv_fwd_ad_r8(b, b_ad, 1, datatype, source, tag, comm, request, request_ad, ierr)
  end subroutine mpi_irecv_fwd_ad_scalar_r8

  subroutine mpi_irecv_fwd_rev_ad_scalar_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_irecv_fwd_rev_ad_r8(b_ad, 1, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_irecv_fwd_rev_ad_scalar_r8

  subroutine mpi_irecv_rev_ad_scalar_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_irecv_rev_ad_r8(b_ad, 1, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_irecv_rev_ad_scalar_r8

  subroutine mpi_put_fwd_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(in) :: origin(*)
    real, intent(in) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Put(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Put(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_fwd_ad_r4

  subroutine mpi_put_rev_ad_r4(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real :: tmp(origin_count)

    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    origin_ad(:origin_count) = origin_ad(:origin_count) + tmp(:origin_count)
    tmp = 0.0
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_rev_ad_r4

  subroutine mpi_put_fwd_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(in) :: origin(*)
    real(8), intent(in) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Put(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Put(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_fwd_ad_r8

  subroutine mpi_put_rev_ad_r8(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8) :: tmp(origin_count)

    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    origin_ad(:origin_count) = origin_ad(:origin_count) + tmp(:origin_count)
    tmp = 0.0_8
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_rev_ad_r8

  subroutine mpi_put_fwd_ad_scalar_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(in), target :: origin
    real, intent(in), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real, pointer :: o(:), o_ad(:)

    call c_f_pointer(c_loc(origin), o, [1])
    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_put_fwd_ad_r4(o, o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, win, ierr)
  end subroutine mpi_put_fwd_ad_scalar_r4

  subroutine mpi_put_rev_ad_scalar_r4(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(inout), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real, pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_put_rev_ad_r4(o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, win, ierr)
  end subroutine mpi_put_rev_ad_scalar_r4

  subroutine mpi_put_fwd_ad_scalar_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(in), target :: origin
    real(8), intent(in), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8), pointer :: o(:), o_ad(:)

    call c_f_pointer(c_loc(origin), o, [1])
    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_put_fwd_ad_r8(o, o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, win, ierr)
  end subroutine mpi_put_fwd_ad_scalar_r8

  subroutine mpi_put_rev_ad_scalar_r8(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(inout), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8), pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_put_rev_ad_r8(o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, win, ierr)
  end subroutine mpi_put_rev_ad_scalar_r8

  subroutine mpi_get_fwd_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(out) :: origin(*)
    real, intent(out) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Get(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Get(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_get_fwd_ad_r4

  subroutine mpi_get_rev_ad_r4(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, MPI_SUM, win, ierr)
    origin_ad(:origin_count) = 0.0
  end subroutine mpi_get_rev_ad_r4

  subroutine mpi_get_fwd_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(out) :: origin(*)
    real(8), intent(out) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Get(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Get(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_get_fwd_ad_r8

  subroutine mpi_get_rev_ad_r8(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, MPI_SUM, win, ierr)
    origin_ad(:origin_count) = 0.0_8
  end subroutine mpi_get_rev_ad_r8

  subroutine mpi_get_fwd_ad_scalar_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(out), target :: origin
    real, intent(out), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real, pointer :: o(:), o_ad(:)

    call c_f_pointer(c_loc(origin), o, [1])
    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_get_fwd_ad_r4(o, o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, win, ierr)
  end subroutine mpi_get_fwd_ad_scalar_r4

  subroutine mpi_get_rev_ad_scalar_r4(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(inout), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real, pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_get_rev_ad_r4(o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, win, ierr)
  end subroutine mpi_get_rev_ad_scalar_r4

  subroutine mpi_get_fwd_ad_scalar_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(out), target :: origin
    real(8), intent(out), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8), pointer :: o(:), o_ad(:)

    call c_f_pointer(c_loc(origin), o, [1])
    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_get_fwd_ad_r8(o, o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, win, ierr)
  end subroutine mpi_get_fwd_ad_scalar_r8

  subroutine mpi_get_rev_ad_scalar_r8(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(inout), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8), pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_get_rev_ad_r8(o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, win, ierr)
  end subroutine mpi_get_rev_ad_scalar_r8

  subroutine mpi_accumulate_fwd_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real, intent(in) :: origin(*)
    real, intent(in) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Accumulate(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_fwd_ad_r4

  subroutine mpi_accumulate_rev_ad_r4(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real, intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real :: tmp(origin_count)

    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    origin_ad(:origin_count) = origin_ad(:origin_count) + tmp(:origin_count)
    tmp = 0.0
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_accumulate_rev_ad_r4

  subroutine mpi_accumulate_fwd_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real(8), intent(in) :: origin(*)
    real(8), intent(in) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Accumulate(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_fwd_ad_r8

  subroutine mpi_accumulate_rev_ad_r8(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real(8), intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8) :: tmp(origin_count)

    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    origin_ad(:origin_count) = origin_ad(:origin_count) + tmp(:origin_count)
    tmp = 0.0_8
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_accumulate_rev_ad_r8

  subroutine mpi_accumulate_fwd_ad_scalar_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real, intent(in), target :: origin
    real, intent(in), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real, pointer :: o(:), o_ad(:)

    call c_f_pointer(c_loc(origin), o, [1])
    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_accumulate_fwd_ad_r4(o, o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_fwd_ad_scalar_r4

  subroutine mpi_accumulate_rev_ad_scalar_r4(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real, intent(inout), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real, pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_accumulate_rev_ad_r4(o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_rev_ad_scalar_r4

  subroutine mpi_accumulate_fwd_ad_scalar_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real(8), intent(in), target :: origin
    real(8), intent(in), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8), pointer :: o(:), o_ad(:)

    call c_f_pointer(c_loc(origin), o, [1])
    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_accumulate_fwd_ad_r8(o, o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_fwd_ad_scalar_r8

  subroutine mpi_accumulate_rev_ad_scalar_r8(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real(8), intent(inout), target :: origin_ad
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8), pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [1])
    call mpi_accumulate_rev_ad_r8(o_ad, 1, origin_datatype, target_rank, target_disp, 1, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_rev_ad_scalar_r8

  subroutine mpi_send_init_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real, intent(in) :: buf(*)
    real, intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Send_init(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Send_init(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_send_init_fwd_ad_r4

  subroutine mpi_send_init_fwd_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    do idx = 1, MAX_REQUESTS
       if (req_map_r4(idx)%request == MPI_REQUEST_NULL) exit
    end do
    if (idx > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_send_init_fwd_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r4(idx)%recvbuf(count))
    req_map_r4(idx)%ptr_advar = c_loc(buf_ad(1))
    req_map_r4(idx)%count = count
    req_map_r4(idx)%datatype = datatype
    req_map_r4(idx)%op_type = FAD_MPI_OP_RECV
    call MPI_Recv_init(req_map_r4(idx)%recvbuf, count, datatype, dest, tag, comm, req_map_r4(idx)%request, ierr)
    request_ad = idx
  end subroutine mpi_send_init_fwd_rev_ad_r4

  subroutine mpi_send_init_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    idx = request_ad
    call MPI_Request_free(req_map_r4(idx)%request, ierr)
    if (allocated(req_map_r4(idx)%recvbuf)) deallocate(req_map_r4(idx)%recvbuf)
    req_map_r4(idx)%ptr_advar = c_null_ptr
    req_map_r4(idx)%count = 0
    req_map_r4(idx)%datatype = 0
    req_map_r4(idx)%op_type = 0
    req_map_r4(idx)%request = MPI_REQUEST_NULL
  end subroutine mpi_send_init_rev_ad_r4

  subroutine mpi_send_init_fwd_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real(8), intent(in) :: buf(*)
    real(8), intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Send_init(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Send_init(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_send_init_fwd_ad_r8

  subroutine mpi_send_init_fwd_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    do idx = 1, MAX_REQUESTS
       if (req_map_r8(idx)%request == MPI_REQUEST_NULL) exit
    end do
    if (idx > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_send_init_fwd_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r8(idx)%recvbuf(count))
    req_map_r8(idx)%ptr_advar = c_loc(buf_ad(1))
    req_map_r8(idx)%count = count
    req_map_r8(idx)%datatype = datatype
    req_map_r8(idx)%op_type = FAD_MPI_OP_RECV
    call MPI_Recv_init(req_map_r8(idx)%recvbuf, count, datatype, dest, tag, comm, req_map_r8(idx)%request, ierr)
    request_ad = idx + MAX_REQUESTS ! Offset to distinguish from real(4) requests
  end subroutine mpi_send_init_fwd_rev_ad_r8

  subroutine mpi_send_init_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    idx = request_ad - MAX_REQUESTS ! Adjust index for real(8) requests
    call MPI_Request_free(req_map_r8(idx)%request, ierr)
    if (allocated(req_map_r8(idx)%recvbuf)) deallocate(req_map_r8(idx)%recvbuf)
    req_map_r8(idx)%ptr_advar = c_null_ptr
    req_map_r8(idx)%count = 0
    req_map_r8(idx)%datatype = 0
    req_map_r8(idx)%op_type = 0
    req_map_r8(idx)%request = MPI_REQUEST_NULL
  end subroutine mpi_send_init_rev_ad_r8

  subroutine mpi_send_init_fwd_ad_scalar_r4(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real, intent(in), target :: buf
    real, intent(in), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_init_fwd_ad_r4(b, b_ad, 1, datatype, dest, tag, comm, request, request_ad, ierr)
  end subroutine mpi_send_init_fwd_ad_scalar_r4

  subroutine mpi_send_init_fwd_rev_ad_scalar_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_init_fwd_rev_ad_r4(b_ad, 1, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_send_init_fwd_rev_ad_scalar_r4

  subroutine mpi_send_init_rev_ad_scalar_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_init_rev_ad_r4(b_ad, 1, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_send_init_rev_ad_scalar_r4

  subroutine mpi_send_init_fwd_ad_scalar_r8(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real(8), intent(in), target :: buf
    real(8), intent(in), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_init_fwd_ad_r8(b, b_ad, 1, datatype, dest, tag, comm, request, request_ad, ierr)
  end subroutine mpi_send_init_fwd_ad_scalar_r8

  subroutine mpi_send_init_fwd_rev_ad_scalar_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_init_fwd_rev_ad_r8(b_ad, 1, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_send_init_fwd_rev_ad_scalar_r8

  subroutine mpi_send_init_rev_ad_scalar_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_send_init_rev_ad_r8(b_ad, 1, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_send_init_rev_ad_scalar_r8

  subroutine mpi_recv_init_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real, intent(out) :: buf(*)
    real, intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Recv_init(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_recv_init_fwd_ad_r4

  subroutine mpi_recv_init_fwd_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    do idx = 1, MAX_REQUESTS
       if (req_map_r4(idx)%request == MPI_REQUEST_NULL) exit
    end do
    if (idx > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_recv_init_fwd_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if

    req_map_r4(idx)%ptr_advar = c_loc(buf_ad(1))
    req_map_r4(idx)%count = count
    req_map_r4(idx)%datatype = datatype
    req_map_r4(idx)%op_type = FAD_MPI_OP_SEND
    call MPI_Send_init(buf_ad, count, datatype, source, tag, comm, req_map_r4(idx)%request, ierr)
    request_ad = idx
  end subroutine mpi_recv_init_fwd_rev_ad_r4

  subroutine mpi_recv_init_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    idx = request_ad
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_recv_init_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Request_free(req_map_r4(idx)%request, ierr)
    req_map_r4(idx)%ptr_advar = c_null_ptr
    req_map_r4(idx)%count = 0
    req_map_r4(idx)%datatype = 0
    req_map_r4(idx)%op_type = 0
    req_map_r4(idx)%request = MPI_REQUEST_NULL
  end subroutine mpi_recv_init_rev_ad_r4

  subroutine mpi_recv_init_fwd_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real(8), intent(out) :: buf(*)
    real(8), intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Recv_init(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_recv_init_fwd_ad_r8

  subroutine mpi_recv_init_fwd_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    do idx = 1, MAX_REQUESTS
       if (req_map_r8(idx)%request == MPI_REQUEST_NULL) exit
    end do
    if (idx > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_recv_init_fwd_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if

    req_map_r8(idx)%ptr_advar = c_loc(buf_ad(1))
    req_map_r8(idx)%count = count
    req_map_r8(idx)%datatype = datatype
    req_map_r8(idx)%op_type = FAD_MPI_OP_SEND
    call MPI_Send_init(buf_ad, count, datatype, source, tag, comm, req_map_r8(idx)%request, ierr)
    request_ad = idx + MAX_REQUESTS ! Offset to distinguish from real(4) requests
  end subroutine mpi_recv_init_fwd_rev_ad_r8

  subroutine mpi_recv_init_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    idx = request_ad - MAX_REQUESTS ! Adjust index for real(8) requests
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_recv_init_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Request_free(req_map_r8(idx)%request, ierr)
    req_map_r8(idx)%ptr_advar = c_null_ptr
    req_map_r8(idx)%count = 0
    req_map_r8(idx)%datatype = 0
    req_map_r8(idx)%op_type = 0
    req_map_r8(idx)%request = MPI_REQUEST_NULL
  end subroutine mpi_recv_init_rev_ad_r8

  subroutine mpi_recv_init_fwd_ad_scalar_r4(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real, intent(out), target :: buf
    real, intent(out), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_init_fwd_ad_r4(b, b_ad, 1, datatype, source, tag, comm, request, request_ad, ierr)
  end subroutine mpi_recv_init_fwd_ad_scalar_r4

  subroutine mpi_recv_init_fwd_rev_ad_scalar_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_init_fwd_rev_ad_r4(b_ad, 1, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_recv_init_fwd_rev_ad_scalar_r4

  subroutine mpi_recv_init_rev_ad_scalar_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_init_rev_ad_r4(b_ad, 1, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_recv_init_rev_ad_scalar_r4

  subroutine mpi_recv_init_fwd_ad_scalar_r8(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real(8), intent(out), target :: buf
    real(8), intent(out), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b(:), b_ad(:)

    call c_f_pointer(c_loc(buf), b, [1])
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_init_fwd_ad_r8(b, b_ad, 1, datatype, source, tag, comm, request, request_ad, ierr)
  end subroutine mpi_recv_init_fwd_ad_scalar_r8

  subroutine mpi_recv_init_fwd_rev_ad_scalar_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_init_fwd_rev_ad_r8(b_ad, 1, datatype, source, tag, comm, request_ad, ierr)
    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
  end subroutine mpi_recv_init_fwd_rev_ad_scalar_r8

  subroutine mpi_recv_init_rev_ad_scalar_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target :: buf_ad
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [1])
    call mpi_recv_init_rev_ad_r8(b_ad, 1, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_recv_init_rev_ad_scalar_r8

  subroutine mpi_start_fwd_ad(request, request_ad, ierr)
    integer, intent(inout) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Wait(request, MPI_STATUS_IGNORE, ierr)
    call MPI_Wait(request_ad, MPI_STATUS_IGNORE, ierr)
  end subroutine mpi_start_fwd_ad

  subroutine mpi_start_rev_ad(request_ad, ierr)
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad < 0 .and. request_ad >= -MAX_REQUESTS) then
      idx = -request_ad
      call MPI_Wait(req_map_r4(idx)%request, MPI_STATUS_IGNORE, ierr)
      call update_advar(req_map_r4(idx))
      return
    else if (request_ad < - MAX_REQUESTS .and. request_ad >= - MAX_REQUESTS * 2) then
      idx = -request_ad - MAX_REQUESTS
      call MPI_Wait(req_map_r8(idx)%request, MPI_STATUS_IGNORE, ierr)
      call update_advar(req_map_r8(idx))
      return
    else
      print *, "Error: Invalid request_ad in mpi_start_rev_ad"
      call MPI_abort(MPI_COMM_WORLD, -1, ierr)
    end if
  end subroutine mpi_start_rev_ad

  subroutine mpi_startall_fwd_ad(count, array_of_requests, array_of_requests_ad, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests(count)
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(out), optional :: ierr

    call MPI_Waitall(count, array_of_requests, MPI_STATUSES_IGNORE, ierr)
    call MPI_Waitall(count, array_of_requests_ad, MPI_STATUSES_IGNORE, ierr)
  end subroutine mpi_startall_fwd_ad

  subroutine mpi_startall_rev_ad(count, array_of_requests_ad, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(out), optional :: ierr
    integer :: reqs(count)
    integer :: i, idx

    do i = 1, count
       if (array_of_requests_ad(i) < 0 .and. array_of_requests_ad(i) >= -MAX_REQUESTS) then
          idx = -array_of_requests_ad(i)
          reqs(i) = req_map_r4(idx)%request
       else if (array_of_requests_ad(i) < - MAX_REQUESTS .and. array_of_requests_ad(i) >= - MAX_REQUESTS * 2) then
          idx = -array_of_requests_ad(i) - MAX_REQUESTS
          reqs(i) = req_map_r8(idx)%request
       end if
    end do

    call MPI_Waitall(count, reqs, MPI_STATUSES_IGNORE, ierr)

    do i = 1, count
       if (array_of_requests_ad(i) < 0 .and. array_of_requests_ad(i) >= -MAX_REQUESTS) then
          idx = -array_of_requests_ad(i)
          call update_advar(req_map_r4(idx))
       else if (array_of_requests_ad(i) < - MAX_REQUESTS .and. array_of_requests_ad(i) >= - MAX_REQUESTS * 2) then
          idx = -array_of_requests_ad(i) - MAX_REQUESTS
          call update_advar(req_map_r8(idx))
       end if
    end do
  end subroutine mpi_startall_rev_ad

  subroutine mpi_wait_fwd_ad(request, request_ad, status, status_ad, ierr)
    integer, intent(inout) :: request, request_ad
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(inout) :: status_ad(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr

    call MPI_Wait(request, status, ierr)
    call MPI_Wait(request_ad, status_ad, ierr)
  end subroutine mpi_wait_fwd_ad

  subroutine mpi_wait_rev_ad(request_ad, ierr)
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: ptr_r4(:)
    real(8), pointer :: ptr_r8(:)
    integer :: idx

    if (request_ad < 0 .and. request_ad >= -MAX_REQUESTS) then
      idx = -request_ad
      call MPI_Start(req_map_r4(idx)%request, ierr)
      return
    else if (request_ad < - MAX_REQUESTS .and. request_ad >= - MAX_REQUESTS * 2) then
      idx = -request_ad - MAX_REQUESTS
      call MPI_Start(req_map_r8(idx)%request, ierr)
      return
    else if (request_ad > 0 .and. request_ad <= MAX_REQUESTS) then
      idx = request_ad
      select case (req_map_r4(idx)%op_type)
      case (FAD_MPI_OP_SEND)
        call c_f_pointer(req_map_r4(idx)%ptr_advar, ptr_r4, [req_map_r4(idx)%count])
        call MPI_ISEND(ptr_r4, req_map_r4(idx)%count, req_map_r4(idx)%datatype, req_map_r4(idx)%dest, req_map_r4(idx)%tag, req_map_r4(idx)%comm, req_map_r4(idx)%request, ierr)
      case (FAD_MPI_OP_RECV)
        call MPI_IRECV(req_map_r4(idx)%recvbuf, req_map_r4(idx)%count, req_map_r4(idx)%datatype, req_map_r4(idx)%source, req_map_r4(idx)%tag, req_map_r4(idx)%comm, req_map_r4(idx)%request, ierr)
      end select
      return
    else if (request_ad > MAX_REQUESTS .and. request_ad <= MAX_REQUESTS * 2) then
      idx = request_ad - MAX_REQUESTS
      select case (req_map_r8(idx)%op_type)
        case (FAD_MPI_OP_SEND)
          call c_f_pointer(req_map_r8(idx)%ptr_advar, ptr_r8, [req_map_r8(idx)%count])
          call MPI_ISEND(ptr_r8, req_map_r8(idx)%count, req_map_r8(idx)%datatype, req_map_r8(idx)%dest, req_map_r8(idx)%tag, req_map_r8(idx)%comm, req_map_r8(idx)%request, ierr)
        case (FAD_MPI_OP_RECV)
          call MPI_IRECV(req_map_r8(idx)%recvbuf, req_map_r8(idx)%count, req_map_r8(idx)%datatype, req_map_r8(idx)%source, req_map_r8(idx)%tag, req_map_r8(idx)%comm, req_map_r8(idx)%request, ierr)
      end select
      return
    else
      print *, "Error: Invalid request_ad in mpi_wait_rev_ad: ", request_ad
      call MPI_abort(MPI_COMM_WORLD, -1, ierr)
    end if
  end subroutine mpi_wait_rev_ad

  subroutine mpi_waitall_fwd_ad(count, array_of_requests, array_of_requests_ad, statuses, statuses_ad, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests(count)
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(inout) :: statuses(MPI_STATUS_SIZE, count)
    integer, intent(inout) :: statuses_ad(MPI_STATUS_SIZE, count)
    integer, intent(out), optional :: ierr

    call MPI_Waitall(count, array_of_requests, statuses, ierr)
    call MPI_Waitall(count, array_of_requests_ad, statuses_ad, ierr)
  end subroutine mpi_waitall_fwd_ad

  subroutine mpi_waitall_rev_ad(count, array_of_requests_ad, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(out), optional :: ierr
    integer :: req, reqs(count)
    integer :: i, idx
    logical :: flag_persistent

    flag_persistent = .false.
    do i = 1, count
      req = array_of_requests_ad(i)
      if (req < 0 .and. req >= -MAX_REQUESTS) then
        idx = -req
        reqs(i) = req_map_r4(idx)%request
        flag_persistent = .true.
      else if (req < - MAX_REQUESTS .and. req >= - MAX_REQUESTS * 2) then
        idx = -req - MAX_REQUESTS
        reqs(i) = req_map_r8(idx)%request
        flag_persistent = .true.
      else if (req > 0 .and. req <= MAX_REQUESTS) then
        idx = req
        if (flag_persistent) then
          print *, "Error: Coexistence of persistent and non-persistent requests is not supported in mpi_waitall_rev_ad"
          call MPI_abort(req_map_r4(idx)%comm, -1, ierr)
        end if
        call mpi_wait_rev_ad(req, ierr)
      else if (req <= MAX_REQUESTS * 2) then
        idx = req - MAX_REQUESTS
        if (flag_persistent) then
          print *, "Error: Coexistence of persistent and non-persistent requests is not supported in mpi_waitall_rev_ad"
          call MPI_abort(req_map_r8(idx)%comm, -1, ierr)
        end if
        call mpi_wait_rev_ad(req, ierr)
      else
        print *, "Error: Invalid request_ad in mpi_waitall_rev_ad"
      end if
    end do

    if (flag_persistent) then
      call MPI_Startall(count, reqs, ierr)
    end if
  end subroutine mpi_waitall_rev_ad

  subroutine update_advar(req_map)
    class(req_map_t), intent(inout) :: req_map
    real, pointer :: ptr_r4(:)
    real(8), pointer :: ptr_r8(:)
    integer :: i

    select type(req_map)
    type is (req_map_r4_t)
      call c_f_pointer(req_map%ptr_advar, ptr_r4, [req_map%count])
      if (req_map%op_type == FAD_MPI_OP_RECV) then
          ptr_r4(:) = ptr_r4(:) + req_map%recvbuf(:req_map%count)
          req_map%recvbuf = 0.0
      else if (req_map%op_type == FAD_MPI_OP_SEND) then
          ptr_r4(:) = 0.0
      end if
    type is (req_map_r8_t)
      call c_f_pointer(req_map%ptr_advar, ptr_r8, [req_map%count])
      if (req_map%op_type == FAD_MPI_OP_SEND) then
          ptr_r8(:) = ptr_r8(:) + req_map%recvbuf(:req_map%count)
          req_map%recvbuf = 0.0_8
      else if (req_map%op_type == FAD_MPI_OP_RECV) then
          ptr_r8(:) = 0.0_8
      end if
    end select
  end subroutine update_advar

end module mpi_ad
