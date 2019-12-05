Module mndr
  implicit none

  real*8        :: time, dt, lng, dx, rtime, pi
  character*100 :: ctime
  integer       :: i    , iend , t    , tend
  real*8        :: wdth , ib   , nb   , q_up, q_int
  real*8        :: h_dwn, h_int, h_lim, mnt , s    , g, k, r, tauc, ustrc, d


  ! ---- input_upstream_discharge ----
  character*100                     :: wavename
  integer                           :: n, nend
  real*8,dimension(:),  allocatable :: qq_wav, time_wav, time_in

  ! ---- input_downstream_depth ----
  integer                           :: dwn

  ! ---- input_downstream_depth ----
  real*8,dimension(:),  allocatable :: x   
  real*8,dimension(:),  allocatable :: fr1
  real*8,dimension(:,:),allocatable :: bl1 , q1  , dep1, wl1 , M1
  real*8,dimension(:,:),allocatable :: lcl1, adv1, prs1, frc1

  real*8,dimension(:),  allocatable :: fr2 , lcl_m2, adv_m2
  real*8,dimension(:,:),allocatable :: bl2 , q2    , dep2, wl2 , M2
  real*8,dimension(:,:),allocatable :: lcl2, adv2  , prs2, frc2

  character*100                     :: qbf1
  
  ! ---- drw_cntr ----
  character*100                     :: barsname, axis_b, axis_l, axis_t, axis_r
  real*8                            :: pxmin, pxmax, pymin, pymax, pdy
  real*8                            :: xmin, xmax, ymin, ymax, zmin, zmax
  integer                           :: plparseopts_rc

End Module mndr
