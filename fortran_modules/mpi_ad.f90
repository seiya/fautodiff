module mpi_ad
  use mpi
  implicit none
  private
  public :: fautodiff_mpi_bcast_fwd_ad
  public :: fautodiff_mpi_bcast_rev_ad
  public :: fautodiff_mpi_reduce_fwd_ad
  public :: fautodiff_mpi_reduce_rev_ad
  public :: fautodiff_mpi_allreduce_fwd_ad
  public :: fautodiff_mpi_allreduce_rev_ad
  public :: fautodiff_mpi_recv_fwd_ad
  public :: fautodiff_mpi_recv_rev_ad
  public :: fautodiff_mpi_send_fwd_ad
  public :: fautodiff_mpi_send_rev_ad
  public :: fautodiff_mpi_isend_fwd_ad
  public :: fautodiff_mpi_isend_rev_ad
  public :: fautodiff_mpi_irecv_fwd_ad
  public :: fautodiff_mpi_irecv_rev_ad
  public :: fautodiff_mpi_put_fwd_ad
  public :: fautodiff_mpi_put_rev_ad
  public :: fautodiff_mpi_get_fwd_ad
  public :: fautodiff_mpi_get_rev_ad
  public :: fautodiff_mpi_accumulate_fwd_ad
  public :: fautodiff_mpi_accumulate_rev_ad
  public :: fautodiff_mpi_send_init_fwd_ad
  public :: fautodiff_mpi_send_init_rev_ad
  public :: fautodiff_mpi_recv_init_fwd_ad
  public :: fautodiff_mpi_recv_init_rev_ad
  public :: fautodiff_mpi_start_ad
  public :: fautodiff_mpi_startall_ad
  public :: fautodiff_mpi_wait_ad
  public :: fautodiff_mpi_waitall_ad

  interface fautodiff_mpi_bcast_fwd_ad
     module procedure mpi_bcast_fwd_ad_r4
     module procedure mpi_bcast_fwd_ad_r8
  end interface
  interface fautodiff_mpi_bcast_rev_ad
     module procedure mpi_bcast_rev_ad_r4
     module procedure mpi_bcast_rev_ad_r8
  end interface
  interface fautodiff_mpi_reduce_fwd_ad
     module procedure mpi_reduce_fwd_ad_r4
     module procedure mpi_reduce_fwd_ad_r8
  end interface
  interface fautodiff_mpi_reduce_rev_ad
     module procedure mpi_reduce_rev_ad_r4
     module procedure mpi_reduce_rev_ad_r8
  end interface
  interface fautodiff_mpi_allreduce_fwd_ad
     module procedure mpi_allreduce_fwd_ad_r4
     module procedure mpi_allreduce_fwd_ad_r8
  end interface
  interface fautodiff_mpi_allreduce_rev_ad
     module procedure mpi_allreduce_rev_ad_r4
     module procedure mpi_allreduce_rev_ad_r8
  end interface
  interface fautodiff_mpi_recv_fwd_ad
     module procedure mpi_recv_fwd_ad_r4
     module procedure mpi_recv_fwd_ad_r8
  end interface
  interface fautodiff_mpi_recv_rev_ad
     module procedure mpi_recv_rev_ad_r4
     module procedure mpi_recv_rev_ad_r8
  end interface
  interface fautodiff_mpi_send_fwd_ad
     module procedure mpi_send_fwd_ad_r4
     module procedure mpi_send_fwd_ad_r8
  end interface
  interface fautodiff_mpi_send_rev_ad
     module procedure mpi_send_rev_ad_r4
     module procedure mpi_send_rev_ad_r8
  end interface
  interface fautodiff_mpi_isend_fwd_ad
     module procedure mpi_isend_fwd_ad_r4
     module procedure mpi_isend_fwd_ad_r8
  end interface
  interface fautodiff_mpi_isend_rev_ad
     module procedure mpi_isend_rev_ad_r4
     module procedure mpi_isend_rev_ad_r8
  end interface
  interface fautodiff_mpi_irecv_fwd_ad
     module procedure mpi_irecv_fwd_ad_r4
     module procedure mpi_irecv_fwd_ad_r8
  end interface
  interface fautodiff_mpi_irecv_rev_ad
     module procedure mpi_irecv_rev_ad_r4
     module procedure mpi_irecv_rev_ad_r8
  end interface
  interface fautodiff_mpi_put_fwd_ad
     module procedure mpi_put_fwd_ad_r4
     module procedure mpi_put_fwd_ad_r8
  end interface
  interface fautodiff_mpi_put_rev_ad
     module procedure mpi_put_rev_ad_r4
     module procedure mpi_put_rev_ad_r8
  end interface
  interface fautodiff_mpi_get_fwd_ad
     module procedure mpi_get_fwd_ad_r4
     module procedure mpi_get_fwd_ad_r8
  end interface
  interface fautodiff_mpi_get_rev_ad
     module procedure mpi_get_rev_ad_r4
     module procedure mpi_get_rev_ad_r8
  end interface
  interface fautodiff_mpi_accumulate_fwd_ad
     module procedure mpi_accumulate_fwd_ad_r4
     module procedure mpi_accumulate_fwd_ad_r8
  end interface
  interface fautodiff_mpi_accumulate_rev_ad
     module procedure mpi_accumulate_rev_ad_r4
     module procedure mpi_accumulate_rev_ad_r8
  end interface
  interface fautodiff_mpi_send_init_fwd_ad
     module procedure mpi_send_init_fwd_ad_r4
     module procedure mpi_send_init_fwd_ad_r8
  end interface
  interface fautodiff_mpi_send_init_rev_ad
     module procedure mpi_send_init_rev_ad_r4
     module procedure mpi_send_init_rev_ad_r8
  end interface
  interface fautodiff_mpi_recv_init_fwd_ad
     module procedure mpi_recv_init_fwd_ad_r4
     module procedure mpi_recv_init_fwd_ad_r8
  end interface
  interface fautodiff_mpi_recv_init_rev_ad
     module procedure mpi_recv_init_rev_ad_r4
     module procedure mpi_recv_init_rev_ad_r8
  end interface
  interface fautodiff_mpi_start_ad
     module procedure mpi_start_ad
  end interface
  interface fautodiff_mpi_startall_ad
     module procedure mpi_startall_ad
  end interface
  interface fautodiff_mpi_wait_ad
     module procedure mpi_wait_ad
  end interface
  interface fautodiff_mpi_waitall_ad
     module procedure mpi_waitall_ad
  end interface

  integer, parameter :: FAD_MPI_OP_SEND = 1
  integer, parameter :: FAD_MPI_OP_RECV = 2
  integer, parameter :: MAX_PERSISTENT = 1024

  type :: persistent_req_r4
     real, pointer, contiguous :: buf(:) => null()
     real, allocatable :: tmp(:)
     integer :: count = 0
     integer :: datatype = 0
     integer :: op_type = 0
     integer :: request_ad = MPI_REQUEST_NULL
  end type persistent_req_r4

  type :: persistent_req_r8
     real(8), pointer, contiguous :: buf(:) => null()
     real(8), allocatable :: tmp(:)
     integer :: count = 0
     integer :: datatype = 0
     integer :: op_type = 0
     integer :: request_ad = MPI_REQUEST_NULL
  end type persistent_req_r8

  type(persistent_req_r4) :: req_map_r4(MAX_PERSISTENT)
  type(persistent_req_r8) :: req_map_r8(MAX_PERSISTENT)

contains


  subroutine mpi_bcast_fwd_ad_r4(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout) :: buffer(*)
    real, intent(inout) :: buffer_ad(*)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    call MPI_Bcast(buffer, count, datatype, root, comm, ierr)
    call MPI_Bcast(buffer_ad, count, datatype, root, comm, ierr)
  end subroutine mpi_bcast_fwd_ad_r4

  subroutine mpi_bcast_rev_ad_r4(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout) :: buffer(*)
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

  subroutine mpi_bcast_rev_ad_r8(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real(8), intent(inout) :: buffer(*)
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

  subroutine mpi_reduce_rev_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real, intent(in) :: sendbuf(*)
    real, intent(inout) :: sendbuf_ad(*)
    real, intent(in) :: recvbuf(*)
    real, intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    if (rank == root) tmp(:) = recvbuf_ad(:)
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

  subroutine mpi_reduce_rev_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real(8), intent(in) :: sendbuf(*)
    real(8), intent(inout) :: sendbuf_ad(*)
    real(8), intent(in) :: recvbuf(*)
    real(8), intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    if (rank == root) tmp(:) = recvbuf_ad(:)
    call MPI_Bcast(tmp, count, datatype, root, comm, ierr)
    sendbuf_ad(:count) = sendbuf_ad(:count) + tmp(:count)
    if (rank == root) recvbuf_ad(:count) = 0.0_8
  end subroutine mpi_reduce_rev_ad_r8

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

  subroutine mpi_allreduce_rev_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real, intent(in) :: sendbuf(*)
    real, intent(inout) :: sendbuf_ad(*)
    real, intent(in) :: recvbuf(*)
    real, intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)

    call MPI_Allreduce(recvbuf_ad, tmp, count, datatype, MPI_SUM, comm, ierr)
    sendbuf_ad(:count) = sendbuf_ad(:count) + tmp(:count)
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

  subroutine mpi_allreduce_rev_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real(8), intent(in) :: sendbuf(*)
    real(8), intent(inout) :: sendbuf_ad(*)
    real(8), intent(in) :: recvbuf(*)
    real(8), intent(inout) :: recvbuf_ad(*)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)

    call MPI_Allreduce(recvbuf_ad, tmp, count, datatype, MPI_SUM, comm, ierr)
    sendbuf_ad(:count) = sendbuf_ad(:count) + tmp(:count)
    recvbuf_ad(:count) = 0.0_8
  end subroutine mpi_allreduce_rev_ad_r8


  subroutine mpi_recv_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real, intent(out) :: buf(*)
    real, intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    call MPI_Recv(buf, count, datatype, source, tag, comm, status, ierr)
    call MPI_Recv(buf_ad, count, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_r4

  subroutine mpi_recv_rev_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real, intent(out) :: buf(*)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    call MPI_Send(buf_ad, count, datatype, source, tag, comm, ierr)
    buf_ad(:count) = 0.0
    status = 0
  end subroutine mpi_recv_rev_ad_r4

  subroutine mpi_recv_fwd_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real(8), intent(out) :: buf(*)
    real(8), intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    call MPI_Recv(buf, count, datatype, source, tag, comm, status, ierr)
    call MPI_Recv(buf_ad, count, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_r8

  subroutine mpi_recv_rev_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real(8), intent(out) :: buf(*)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr
    call MPI_Send(buf_ad, count, datatype, source, tag, comm, ierr)
    buf_ad(:count) = 0.0_8
    status = 0
  end subroutine mpi_recv_rev_ad_r8

  subroutine mpi_send_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(in) :: buf(*)
    real, intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    call MPI_Send(buf, count, datatype, dest, tag, comm, ierr)
    call MPI_Send(buf_ad, count, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_fwd_ad_r4

  subroutine mpi_send_rev_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(in) :: buf(*)
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

  subroutine mpi_send_rev_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real(8), intent(in) :: buf(*)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    call MPI_Recv(tmp, count, datatype, dest, tag, comm, MPI_STATUS_IGNORE, ierr)
    buf_ad(:count) = buf_ad(:count) + tmp(:count)
  end subroutine mpi_send_rev_ad_r8

  subroutine mpi_isend_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real, intent(in) :: buf(*)
    real, intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    call MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Isend(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_isend_fwd_ad_r4

  subroutine mpi_isend_rev_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(in) :: buf(*)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    call MPI_Irecv(tmp, count, datatype, dest, tag, comm, request_ad, ierr)
    buf_ad(:count) = buf_ad(:count) + tmp(:count)
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

  subroutine mpi_isend_rev_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(in) :: buf(*)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    call MPI_Irecv(tmp, count, datatype, dest, tag, comm, request_ad, ierr)
    buf_ad(:count) = buf_ad(:count) + tmp(:count)
  end subroutine mpi_isend_rev_ad_r8

  subroutine mpi_irecv_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real, intent(out) :: buf(*)
    real, intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    call MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Irecv(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_irecv_fwd_ad_r4

  subroutine mpi_irecv_rev_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(out) :: buf(*)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    call MPI_Isend(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    buf_ad(:count) = 0.0
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

  subroutine mpi_irecv_rev_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(out) :: buf(*)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    call MPI_Isend(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    buf_ad(:count) = 0.0_8
  end subroutine mpi_irecv_rev_ad_r8

  subroutine mpi_put_fwd_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(in) :: origin(*)
    real, intent(in) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win
    integer, intent(out), optional :: ierr
    call MPI_Put(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Put(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_fwd_ad_r4

  subroutine mpi_put_rev_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(in) :: origin(*)
    real, intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win
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
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win
    integer, intent(out), optional :: ierr
    call MPI_Put(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Put(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_fwd_ad_r8

  subroutine mpi_put_rev_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(in) :: origin(*)
    real(8), intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win
    integer, intent(out), optional :: ierr
    real(8) :: tmp(origin_count)
    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    origin_ad(:origin_count) = origin_ad(:origin_count) + tmp(:origin_count)
    tmp = 0.0_8
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_rev_ad_r8

  subroutine mpi_get_fwd_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(out) :: origin(*)
    real, intent(out) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win
    integer, intent(out), optional :: ierr
    call MPI_Get(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Get(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_get_fwd_ad_r4

  subroutine mpi_get_rev_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(out) :: origin(*)
    real, intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win
    integer, intent(out), optional :: ierr
    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, MPI_SUM, win, ierr)
    origin_ad(:origin_count) = 0.0
  end subroutine mpi_get_rev_ad_r4

  subroutine mpi_get_fwd_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(out) :: origin(*)
    real(8), intent(out) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win
    integer, intent(out), optional :: ierr
    call MPI_Get(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Get(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_get_fwd_ad_r8

  subroutine mpi_get_rev_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(out) :: origin(*)
    real(8), intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win
    integer, intent(out), optional :: ierr
    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, MPI_SUM, win, ierr)
    origin_ad(:origin_count) = 0.0_8
  end subroutine mpi_get_rev_ad_r8

  subroutine mpi_accumulate_fwd_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real, intent(in) :: origin(*)
    real, intent(in) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win
    integer, intent(out), optional :: ierr
    call MPI_Accumulate(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_fwd_ad_r4

  subroutine mpi_accumulate_rev_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real, intent(in) :: origin(*)
    real, intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win
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
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win
    integer, intent(out), optional :: ierr
    call MPI_Accumulate(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_fwd_ad_r8

  subroutine mpi_accumulate_rev_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real(8), intent(in) :: origin(*)
    real(8), intent(inout) :: origin_ad(*)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win
    integer, intent(out), optional :: ierr
    real(8) :: tmp(origin_count)
    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    origin_ad(:origin_count) = origin_ad(:origin_count) + tmp(:origin_count)
    tmp = 0.0_8
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_accumulate_rev_ad_r8

  subroutine mpi_send_init_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real, intent(in) :: buf(*)
    real, intent(in) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    call MPI_Send_init(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Send_init(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_send_init_fwd_ad_r4

  subroutine mpi_send_init_rev_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(in) :: buf(*)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_PERSISTENT
       if (req_map_r4(i)%request_ad == MPI_REQUEST_NULL) exit
    end do
    if (i > MAX_PERSISTENT) stop "too many persistent requests"

    allocate(req_map_r4(i)%tmp(count))
    req_map_r4(i)%buf => buf_ad
    req_map_r4(i)%count = count
    req_map_r4(i)%datatype = datatype
    req_map_r4(i)%op_type = FAD_MPI_OP_SEND
    call MPI_Recv_init(req_map_r4(i)%tmp, count, datatype, dest, tag, comm, request_ad, ierr)
    req_map_r4(i)%request_ad = request_ad
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

  subroutine mpi_send_init_rev_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(in) :: buf(*)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_PERSISTENT
       if (req_map_r8(i)%request_ad == MPI_REQUEST_NULL) exit
    end do
    if (i > MAX_PERSISTENT) stop "too many persistent requests"

    allocate(req_map_r8(i)%tmp(count))
    req_map_r8(i)%buf => buf_ad
    req_map_r8(i)%count = count
    req_map_r8(i)%datatype = datatype
    req_map_r8(i)%op_type = FAD_MPI_OP_SEND
    call MPI_Recv_init(req_map_r8(i)%tmp, count, datatype, dest, tag, comm, request_ad, ierr)
    req_map_r8(i)%request_ad = request_ad
  end subroutine mpi_send_init_rev_ad_r8

  subroutine mpi_recv_init_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real, intent(out) :: buf(*)
    real, intent(out) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr
    call MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Recv_init(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_recv_init_fwd_ad_r4

  subroutine mpi_recv_init_rev_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(out) :: buf(*)
    real, intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_PERSISTENT
       if (req_map_r4(i)%request_ad == MPI_REQUEST_NULL) exit
    end do
    if (i > MAX_PERSISTENT) stop "too many persistent requests"

    req_map_r4(i)%buf => buf_ad
    req_map_r4(i)%count = count
    req_map_r4(i)%datatype = datatype
    req_map_r4(i)%op_type = FAD_MPI_OP_RECV
    call MPI_Send_init(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    req_map_r4(i)%request_ad = request_ad
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

  subroutine mpi_recv_init_rev_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(out) :: buf(*)
    real(8), intent(inout) :: buf_ad(*)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_PERSISTENT
       if (req_map_r8(i)%request_ad == MPI_REQUEST_NULL) exit
    end do
    if (i > MAX_PERSISTENT) stop "too many persistent requests"

    req_map_r8(i)%buf => buf_ad
    req_map_r8(i)%count = count
    req_map_r8(i)%datatype = datatype
    req_map_r8(i)%op_type = FAD_MPI_OP_RECV
    call MPI_Send_init(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    req_map_r8(i)%request_ad = request_ad
  end subroutine mpi_recv_init_rev_ad_r8

  subroutine mpi_start_ad(request, request_ad, ierr)
    integer, intent(inout) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Wait(request, MPI_STATUS_IGNORE, ierr)
    call MPI_Wait(request_ad, MPI_STATUS_IGNORE, ierr)
    call update_persistent(request_ad)
  end subroutine mpi_start_ad

  subroutine mpi_startall_ad(count, array_of_requests, array_of_requests_ad, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests(count)
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(out), optional :: ierr
    integer :: i

    call MPI_Waitall(count, array_of_requests, MPI_STATUSES_IGNORE, ierr)
    call MPI_Waitall(count, array_of_requests_ad, MPI_STATUSES_IGNORE, ierr)
    do i = 1, count
       call update_persistent(array_of_requests_ad(i))
    end do
  end subroutine mpi_startall_ad

  subroutine update_persistent(request_ad)
    integer, intent(in) :: request_ad
    integer :: i

    do i = 1, MAX_PERSISTENT
       if (req_map_r4(i)%request_ad == request_ad) then
          if (req_map_r4(i)%op_type == FAD_MPI_OP_SEND) then
             req_map_r4(i)%buf(:req_map_r4(i)%count) = req_map_r4(i)%buf(:req_map_r4(i)%count) + &
                  req_map_r4(i)%tmp(:req_map_r4(i)%count)
             req_map_r4(i)%tmp = 0.0
          else if (req_map_r4(i)%op_type == FAD_MPI_OP_RECV) then
             req_map_r4(i)%buf(:req_map_r4(i)%count) = 0.0
          end if
          return
       end if
    end do

    do i = 1, MAX_PERSISTENT
       if (req_map_r8(i)%request_ad == request_ad) then
          if (req_map_r8(i)%op_type == FAD_MPI_OP_SEND) then
             req_map_r8(i)%buf(:req_map_r8(i)%count) = req_map_r8(i)%buf(:req_map_r8(i)%count) + &
                  req_map_r8(i)%tmp(:req_map_r8(i)%count)
             req_map_r8(i)%tmp = 0.0_8
          else if (req_map_r8(i)%op_type == FAD_MPI_OP_RECV) then
             req_map_r8(i)%buf(:req_map_r8(i)%count) = 0.0_8
          end if
          return
       end if
    end do
  end subroutine update_persistent

  subroutine mpi_wait_ad(request, request_ad, status, ierr)
    integer, intent(inout) :: request, request_ad
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr

    call MPI_Start(request, ierr)
    call MPI_Start(request_ad, ierr)
    status = 0
  end subroutine mpi_wait_ad

  subroutine mpi_waitall_ad(count, array_of_requests, array_of_requests_ad, statuses, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests(count)
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(out) :: statuses(MPI_STATUS_SIZE, count)
    integer, intent(out), optional :: ierr

    call MPI_Startall(count, array_of_requests, ierr)
    call MPI_Startall(count, array_of_requests_ad, ierr)
    statuses = 0
  end subroutine mpi_waitall_ad

end module mpi_ad
