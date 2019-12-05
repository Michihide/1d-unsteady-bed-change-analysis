! Draw 2d contour
subroutine drw_cntr
  use mndr
  use plplot

!!$  if(t .eq. 1)then
!!$     plparseopts_rc = plparseopts(PL_PARSE_FULL)
!!$     if(plparseopts_rc .ne. 0) stop "plparseopt error"
!!$     call plscol0( 0,255,255,255)
!!$     call plscol0(15,  0,  0,  0)
!!$     call plscmap0n(0)
!!$     call plinit
!!$  endif

  call pladv(0) ; call plschr(1.5d0, 2.0d0)  

  pdx   = 0.25d0 ; pdy   = 0.28d0
  

  ! ---- devitation of bel level ----
  axis_l   = 'B.L. & W.L. [m]'
  axis_t   = ctime
  axis_b   = ''

  xmin = minval(x)  - (maxval(x)-minval(x))*0.02d0
  xmax = maxval(x)  + (maxval(x)-minval(x))*0.02d0
  ymin = minval(bl1) - (maxval(bl1)-minval(bl1))*0.001d0
  ymax = maxval(wl1) + (maxval(wl1)-minval(wl1))*0.001d0
!!$  ymin = minval(bl2) - (maxval(bl2)-minval(bl2))*0.001d0
!!$  ymax = maxval(wl2) + (maxval(wl2)-minval(wl2))*0.001d0

  
  pxmin = 0.10d0 ; pxmax = 0.750d0
!!$  pymin = 0.75d0 ; pymax = 0.95d0
  pymin = 0.55d0 ; pymax = 0.95d0  

  call set_window(xmin, xmax, ymin, ymax, trim(axis_b), trim(axis_l), trim(axis_t), pxmin, pxmax, pymin, pymax)
  call plot_line(iend , x(:), bl1(:,1) , 7) 
  call plot_line(iend , x(:), wl1(:,1) , 7)
  call plot_point(iend, x(:), bl1(:,1) , 7, 1) 
  call plot_point(iend, x(:), wl1(:,1) , 7, 1)
  
  call plot_line(iend , x(:), bl1(:,t) , 9) 
  call plot_line(iend , x(:), wl1(:,t) , 9)
  call plot_point(iend, x(:), bl1(:,t) , 9, 1) 
  call plot_point(iend, x(:), wl1(:,t) , 9, 1)

!!$  call plot_line(iend , x(:), bl2(:,t) , 1) 
!!$  call plot_line(iend , x(:), wl2(:,t) , 1)
!!$  call plot_point(iend, x(:), bl2(:,t) , 1, 1) 
!!$  call plot_point(iend, x(:), wl2(:,t) , 1, 1)

  
  ! ---- lcl & adv & prs & frc ----
  axis_l   = 'lcl & adv & prs & frc'
  axis_t   = ''
  axis_b   = ''

  xmin = minval(x)  - (maxval(x)-minval(x))*0.02d0
  xmax = maxval(x)  + (maxval(x)-minval(x))*0.02d0
  ymin = -0.003d0 ; ymax = 0.003d0
  
  pymin = pymin - pdy ; pymax = pymax - pdy
  pymin = 0.35d0 ; pymax = 0.50d0

  call set_window(xmin, xmax, ymin, ymax, trim(axis_b), trim(axis_l), trim(axis_t), pxmin, pxmax, pymin, pymax)
!!$  call plot_line(iend ,x(:), lcl2(:,t) ,7)
!!$  call plot_line(iend ,x(:), adv2(:,t) ,3)
!!$  call plot_line(iend ,x(:), prs2(:,t) ,1)
!!$  call plot_line(iend ,x(:), frc2(:,t) ,9)
  call plot_line(iend ,x(:), lcl1(:,t) ,7)
  call plot_line(iend ,x(:), adv1(:,t) ,3)
  call plot_line(iend ,x(:), prs1(:,t) ,1)
  call plot_line(iend ,x(:), frc1(:,t) ,9)


  ! ---- Fr ----
  axis_l   = 'Fr'
  axis_t   = ''
  axis_b   = ''

  xmin = minval(x)  - (maxval(x)-minval(x))*0.02d0
  xmax = maxval(x)  + (maxval(x)-minval(x))*0.02d0
  ymin = 0.00d0 ; ymax = 2.0d0
  
  pymin = pymin - pdy ; pymax = pymax - pdy
!!$  pymin = 0.35d0 ; pymax = 0.50d0
!!$  pdy   = 0.20d0
  
  call set_window(xmin, xmax, ymin, ymax, trim(axis_b), trim(axis_l), trim(axis_t), pxmin, pxmax, pymin, pymax)
  call plot_line(iend  , x(:) , fr1(:) ,9)
  call plcol0(7)
  call pljoin(minval(x), 1.0d0, maxval(x), 1.0d0)
  call pljoin(minval(x), 1.5d0, maxval(x), 1.5d0)
  call plcol0(1)
  call pljoin(minval(x), 0.738d0, maxval(x), 0.738d0)

  
!!$  ! ---- adv_m & lcl_m ----
!!$  axis_l   = 'adv_m & lcl_m [m/s2]'
!!$  axis_t   = ''
!!$  axis_b   = ''
!!$
!!$  xmin = minval(x)  - (maxval(x)-minval(x))*0.02d0
!!$  xmax = maxval(x)  + (maxval(x)-minval(x))*0.02d0
!!$  ymin = minval()  - (maxval()-minval())*0.02d0
!!$  ymax = maxval()  + (maxval()-minval())*0.02d0
!!$  ymin = -0.03d0 ; ymax = 0.03d0
!!$  write(*,*)minval(adv_m2(:)),maxval(adv_m2(:)),minval(lcl_m2(:)),maxval(lcl_m2(:))
!!$  pymin = pymin - pdy ; pymax = pymax - pdy
!!$
!!$  call set_window(xmin, xmax, ymin, ymax, trim(axis_b), trim(axis_l), trim(axis_t), pxmin, pxmax, pymin, pymax)
!!$  call plot_line(iend  , x(:) , lcl_m2(:), 7)
!!$  call plot_line(iend  , x(:) , adv_m2(:), 1)

  
!!$  ! ---- M ----
!!$  axis_l   = 'M [m/s]'
!!$  axis_t   = ''
!!$  axis_b   = ''
!!$
!!$  xmin = minval(x)  - (maxval(x)-minval(x))*0.02d0
!!$  xmax = maxval(x)  + (maxval(x)-minval(x))*0.02d0
!!$  ymin = 0.00d0 ; ymax = 0.02d0
!!$  
!!$  pymin = pymin - pdy ; pymax = pymax - pdy
!!$  pymin = 0.1d0 ; pymax = 0.30d0  
!!$
!!$  call set_window(xmin, xmax, ymin, ymax, trim(axis_b), trim(axis_l), trim(axis_t), pxmin, pxmax, pymin, pymax)
!!$  call plot_line(iend  , x(:) , M2(:,t) ,1)
!!$  call plcol0(7)
!!$  call pljoin(minval(x), 1.0d0, maxval(x), 1.0d0)
!!$  call plcol0(1)
!!$  call pljoin(minval(x), 0.738d0, maxval(x), 0.738d0)

  
!!$  ! ---- Depth ----
!!$  axis_l   = 'Depth [m]'
!!$  axis_t   = ''
!!$  axis_b   = ''
!!$
!!$  xmin = minval(x)  - (maxval(x)-minval(x))*0.02d0
!!$  xmax = maxval(x)  + (maxval(x)-minval(x))*0.02d0
!!$  ymin = 0.00d0 ; ymax = 0.02d0
!!$  
!!$  pymin = pymin - pdy ; pymax = pymax - pdy
!!$
!!$  call set_window(xmin, xmax, ymin, ymax, trim(axis_b), trim(axis_l), trim(axis_t), pxmin, pxmax, pymin, pymax)
!!$  call plot_line(iend  , x(:) , dep1(:,t) ,9)
!!$  call plcol0(1)
!!$  call pljoin(minval(x), 0.0139d0, maxval(x), 0.0139d0)
!!$
!!$  ! ---- Velocity ----
!!$  axis_l   = 'Velocity [m/s]'
!!$  axis_t   = ''
!!$  axis_b   = 'x [m]'
!!$
!!$  xmin = minval(x)  - (maxval(x)-minval(x))*0.02d0
!!$  xmax = maxval(x)  + (maxval(x)-minval(x))*0.02d0
!!$  ymin = 0.00d0 ; ymax = 0.5d0
!!$  
!!$  pymin = pymin - pdy ; pymax = pymax - pdy
!!$
!!$  call set_window(xmin, xmax, ymin, ymax, trim(axis_b), trim(axis_l), trim(axis_t), pxmin, pxmax, pymin, pymax)
!!$  call plot_line(iend  , x(:) , fr1(:)*sqrt(g*dep1(:,t)) ,9)
!!$  call plcol0(1)
!!$  call pljoin(minval(x), 0.272d0, maxval(x), 0.272d0)

  
  ! ---- devitation of bel level ----
  axis_l   = 'q(upstream)'
  axis_t   = ''
  axis_b   = 'time[s]'

  xmin = minval(time_in)  - (maxval(time_in)-minval(time_in))*0.02d0
  xmax = maxval(time_in)  + (maxval(time_in)-minval(time_in))*0.02d0
  ymin = minval(q1(1,:)) - (maxval(q1(1,:))-minval(q1(1,:)))*0.05d0
  ymax = maxval(q1(1,:)) + (maxval(q1(1,:))-minval(q1(1,:)))*0.05d0
  
  pxmin = 0.85d0 ; pxmax = 0.95d0
  pymin = 0.70d0 ; pymax = 0.90d0  

  call set_window(xmin, xmax, ymin, ymax, trim(axis_b), trim(axis_l), trim(axis_t), pxmin, pxmax, pymin, pymax)
  call plot_line(tend, time_in(:), q1(1,:) ,9)
!!$  call plot_line(tend, time_in(:), q2(1,:) ,1)
  call plcol0(7)
  call pljoin(time_in(t), -1.0d0, time_in(t), 1.0d0)

  
  if(t .eq. tend)then
!!$  if(rtime .ge. 120)then
     call plend
  endif
end subroutine drw_cntr
!===================================================================================================================================








! Plot line
!===================================================================================================================================
subroutine plot_line(iend, xp, yp, icol)
!===================================================================================================================================
  use plplot
  implicit none
  integer,intent(in) :: iend, icol
  real*8,intent(in)  :: xp(0:iend-1), yp(0:iend-1)

  call plcol0(icol)
  call plline(xp, yp)
end subroutine plot_line
!===================================================================================================================================


! Plot line reverse
!===================================================================================================================================
subroutine plot_line_rvrs(iend, xp, yp, icol)
!===================================================================================================================================
  use plplot
  implicit none
  integer            :: i
  integer,intent(in) :: iend, icol
  real*8,intent(in)  :: xp(0:iend-1), yp(0:iend-1)
  real*8             :: xpr(0:iend-1), ypr(0:iend-1)  

  do i = 0, iend-1
     ! xpr(i) = xp(iend-i-1)
     ypr(i) = yp(iend-i-1)
  enddo
  
  call plcol0(icol)
  call plline(xp, ypr)
end subroutine plot_line_rvrs
!===================================================================================================================================

! Plot point
!===================================================================================================================================
subroutine plot_point(iend, xp, yp, icol, symbl)
!===================================================================================================================================  
  use plplot
  implicit none
  integer,intent(in) :: iend, icol, symbl
  real*8,intent(in)  :: xp(0:iend-1), yp(0:iend-1)

  call plcol0(icol)
  call plpoin(xp, yp, symbl)
end subroutine plot_point
!===================================================================================================================================

! Plot point
!===================================================================================================================================
subroutine plot_point_rvrs(iend, xp, yp, icol, symbl)
!===================================================================================================================================  
  use plplot
  implicit none
  integer            :: i
  integer,intent(in) :: iend, icol, symbl
  real*8,intent(in)  :: xp(0:iend-1), yp(0:iend-1)
  real*8             :: xpr(0:iend-1), ypr(0:iend-1)  

  do i = 0, iend-1
     ! xpr(i) = xp(iend-i-1)
     ypr(i) = yp(iend-i-1)
  enddo

  call plcol0(icol)
  call plpoin(xp, ypr, symbl)
end subroutine plot_point_rvrs
!===================================================================================================================================


! Set window
!===================================================================================================================================
subroutine set_window(axmin, axmax, aymin, aymax, axis_b, axis_l, axis_t, pxmin, pxmax, pymin, pymax)
!===================================================================================================================================
  use plplot
  real*8,intent(in)       :: axmin, axmax, aymin, aymax, pxmin, pxmax, pymin, pymax
  character(*),intent(in) :: axis_b, axis_l, axis_t

  call plvpor(pxmin, pxmax, pymin, pymax)
  call plwind(axmin, axmax ,aymin, aymax)
  call plcol0(15) 
  call plbox('bcnst', 0.0d0, 5, 'bcnstv', 0.0d0, 0)
  call plmtex('b', 3.0d0, 0.5d0, 0.5d0, trim(axis_b))
  call plmtex('l', 6.5d0, 0.5d0, 0.5d0, trim(axis_l))
  call plmtex('t', 1.d00, 0.5d0, 0.5d0, trim(axis_t))
  
end subroutine set_window
!===================================================================================================================================

! Set window first_axis
!===================================================================================================================================
subroutine set_window_first_axis(axmin, axmax, aymin, aymax, axis_b, axis_l, axis_t, pxmin, pxmax, pymin, pymax)
!===================================================================================================================================
  use plplot
  real*8,intent(in)       :: axmin, axmax, aymin, aymax, pxmin, pxmax, pymin, pymax
  character(*),intent(in) :: axis_b, axis_l, axis_t

  call plvpor(pxmin, pxmax, pymin, pymax)
  call plwind(axmin, axmax ,aymin, aymax)
  call plcol0(15) 
  call plbox('bcnst', 0.0d0, 5, 'bnstv', 0.0d0, 0)
  call plmtex('b', 3.0d0, 0.5d0, 0.5d0, trim(axis_b))
  call plmtex('l', 4.5d0, 0.5d0, 0.5d0, trim(axis_l))
  call plmtex('t', 1.d0, 0.5d0, 0.5d0, trim(axis_t))
  
end subroutine set_window_first_axis
!===================================================================================================================================

! Set window second axis
!===================================================================================================================================
subroutine set_window_second_axis(axmin, axmax, aymin, aymax, axis_b, axis_r, axis_t, pxmin, pxmax, pymin, pymax)
!===================================================================================================================================
  use plplot
  real*8,intent(in)       :: axmin, axmax, aymin, aymax, pxmin, pxmax, pymin, pymax
  character(*),intent(in) :: axis_b, axis_r, axis_t

  call plvpor(pxmin, pxmax, pymin, pymax)
  call plwind(axmin, axmax ,aymin, aymax)
  call plcol0(15) 
  call plbox('bcnst', 0.0d0, 5, 'cmstv', 0.0d0, 0)
  call plmtex('b', 3.0d0, 0.5d0, 0.5d0, trim(axis_b))
  call plmtex('r', 2.0d0, 0.5d0, 0.5d0, trim(axis_r))
  call plmtex('t', 1.d0, 0.5d0, 0.5d0, trim(axis_t))
  
end subroutine set_window_second_axis
!===================================================================================================================================


! Set window title
!===================================================================================================================================
subroutine set_window_title(axmin, axmax, aymin, aymax, axis_b, axis_l, axis_t, pxmin, pxmax, pymin, pymax, title)
!===================================================================================================================================
  use plplot
  real*8,intent(in)       :: axmin, axmax, aymin, aymax, pxmin, pxmax, pymin, pymax
  character(*),intent(in) :: axis_b, axis_l, axis_t, title

  call plvpor(pxmin, pxmax, pymin, pymax)
  call plwind(axmin, axmax ,aymin, aymax)
  call plcol0(15) 
  call plbox('bcnst', 0.0d0, 5, 'bcnstv', 0.0d0, 0)
  call plmtex('b', 3.0d0, 0.5d0, 0.5d0, trim(axis_b))
  call plmtex('l', 4.5d0, 0.5d0, 0.5d0, trim(axis_l))
  call plmtex('t', 1.d0, 0.5d0, 0.5d0, trim(axis_t))
  call plmtex('t', 1.d0, 0.4d0, 0.1d0, trim(title))
end subroutine set_window_title
!===================================================================================================================================


! Set color of contour
!===================================================================================================================================
subroutine set_clr_cntr(Clr_nm, zmin, zmax)
!===================================================================================================================================  
  use Val_Cmap
  integer,intent(in) :: Clr_nm
  real*8,intent(in)  :: zmin, zmax

  iclrDst = 40
  cqmin = zmin
  cqmax = zmax
  
  if(Clr_nm.eq.3)then
     iru(1) = 0
     igu(1) = 0
     ibu(1) = 255
     iru(2) = 255
     igu(2) = 255
     ibu(2) = 255
     iru(3) = 255
     igu(3) = 0
     ibu(3) = 0
  elseif(Clr_nm.eq.5)then
     iru(1) = 0
     igu(1) = 0
     ibu(1) = 255
     iru(2) = 0
     igu(2) = 255
     ibu(2) = 255
     iru(3) = 0
     igu(3) = 255
     ibu(3) = 20
     iru(4) = 255
     igu(4) = 255
     ibu(4) = 0
     iru(5) = 255
     igu(5) = 0
     ibu(5) = 0
  endif

  if(Clr_nm .eq. 2)call remaprgb_TwoClr
  if(Clr_nm .eq. 3)call remaprgb_ThreClr
  if(Clr_nm .eq. 4)call remaprgb_FourClr
  if(Clr_nm .eq. 5)call remaprgb_FiveClr

end subroutine set_clr_cntr
!===================================================================================================================================


! Draw contour
!===================================================================================================================================
subroutine plot_cntr(imin, imax, jmin, jmax, x, y, z)
!===================================================================================================================================  
  use Val_Cmap
  integer               :: i, j
  integer,intent(in)    :: imin, imax, jmin, jmax
  real*8                :: zave
  real*8,dimension(0:4) :: xp, yp
  real*8,dimension(imin:imax,jmin:jmax),intent(in) :: x, y, z   

  do j = jmin, jmax-1
     do i = imin, imax-1
        xp(0) = x(i,j)
        yp(0) = y(i,j)
        xp(1) = x(i+1,j)
        yp(1) = y(i+1,j)
        xp(2) = x(i+1,j+1)
        yp(2) = y(i+1,j+1)
        xp(3) = x(i,j+1)
        yp(3) = y(i,j+1)
        xp(4) = x(i,j)
        yp(4) = y(i,j)
        zave = sum(z(i:i+1,j:j+1)) / 4.d0
        call clrdqnty(zave)        
        call plfill(xp, yp)
     enddo
  enddo

end subroutine plot_cntr
!===================================================================================================================================


! Draw color bars
!===================================================================================================================================
subroutine colbar(axmin, axmax, aymin, aymax, pxmin, pxmax, pymin, pymax, barname)
!===================================================================================================================================  
  use Val_Cmap
  integer                 :: i, j 
  real*8                  :: cqt, xp(0:4), yp(0:4)
  real*8,intent(in)       :: axmin, axmax, aymin, aymax, pxmin, pxmax, pymin, pymax
  character(*),intent(in) :: barname

  call plvpor(pxmin, pxmax, pymin, pymax)
  call plwind(axmin, axmax, aymin, aymax)
  call plcol0(15)
  call plbox('bc', 0.0d0, 0, 'bcmst', 0.0d0, 0)      
  call plmtex('l', 1.d0, 0.5d0, 0.5d0, barname)   

  dpRng   = aymax - aymin
  RngDlt  = dpRng / iclrDst
  
  do i = 0, iclrDst-1
     xp(0) = axmin
     yp(0) = aymin + RngDlt * (i)
     xp(1) = axmax
     yp(1) = yp(0)
     xp(2) = xp(1)
     yp(2) = aymax + RngDlt * (i)
     xp(3) = xp(0)
     yp(3) = yp(2)
     xp(4) = xp(0)
     yp(4) = yp(0)
     cqt   = rng(i)
     call clrdqnty(cqt)
     call plfill(xp, yp)
  enddo

end subroutine colbar
!===================================================================================================================================
