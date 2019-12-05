! ***** Program main *****
program main
  use mndr

  intbl    =  5
  wavename = '01'

  call set_plplot
  call input_parameter
  call cal_critical_shear_stress
  call input_array
  call set_array

  call input_upstream_discharge(q1)
  call set_initial_condition(dep1,bl1,wl1)

  call input_upstream_discharge(q2)
  call set_initial_condition(dep2,bl2,wl2)

  do t = 1, tend

     ! ---- calculate Unsteady flow analysis ----
     call set_boundary_condition(dep1,bl1,wl1,q1)
     call cal_equation_of_continuity(dep1,bl1,wl1,q1,fr1)
     call cal_equation_of_motion(dep1,bl1,wl1,q1,lcl1,adv1,prs1,frc1)

     call set_boundary_condition(dep2,bl2,wl2,q2)
     call cal_equation_of_continuity(dep2,bl2,wl2,q2,fr2)
     call cal_equation_of_motion(dep2,bl2,wl2,q2,lcl2,adv2,prs2,frc2)

     
     ! ---- calculate Bed change analysis    ----
     if(rtime.ge.300)then
        qbf1 = 'mpm' ; call cal_equation_of_bedloadflux_and_exner_unsteady(dep1,bl1,q1,qbf1)
        qbf1 = 'mpm' ; call cal_equation_of_bedloadflux_and_exner_unsteady_cip(dep1,bl1,q1,qbf1)
        call cal_equation_of_M(dep2,bl2,q2,M2,lcl_m2,adv_m2)
     endif

!!$        qbf1 = 'amf' ; call cal_equation_of_bedloadflux_and_exner_unsteady(dep2,bl2,q2,qbf1)
!!$        call cal_equation_of_M_old(dep2,bl2,q2,M2,lcl_m2,adv_m2)
!!$     endif

     if( mod(t,intbl) .eq. 0.0)then
        call drw_cntr
     endif

  enddo
  
end program main



subroutine input_parameter
  use mndr

  dt     = 0.08          ! Time spacings (s)
  dx     = 0.1           ! Grid spacings (m)
  
  lng    = 20.0          ! Length of channel (m)

  rtime  = 0.0  
  iend   = int(lng/dx)  
  
  wdth   = 0.45          ! Width of channel (m)
  ib     = 1./80.        ! Slope of channel
  nb     = 0.015         ! Manning roughness coefficient

  q_up   = qq / wdth     ! Unit flow discharge of upstream boundary condition (m3/sec)
  q_int  = 0.0           ! Initial condition (m2/sec)

  h_dwn  = 0.0           ! Flow depth of downstream boundary condition (m)
  h_int  = h_dwn         ! Flow depth of initial condition in all longitudinal section (m)
  h_lim  = 1.0E-4        ! Limitation of flow depth (m)

  mnt    = 0.007         ! Height of mount (m)
  g      = 9.81          ! Gravitational acceleration (m/s2)
  pi     = 3.14159
  s      = 1.65          ! water specific gravity of sediment
  d      = 0.00076       ! grain size (m)
  
end subroutine input_parameter



subroutine input_array
  use mndr
  open(100, file = '00_input_upstream_discharge/'//trim(wavename)//'.csv', status = 'old')
  n = 0
  do
     n = n + 1
     read(100, *, end = 998)
  enddo
998 nend = n - 1
  allocate(time_wav(nend), qq_wav(nend))
  close(100)
  
  open(10, file = '00_input_upstream_discharge/'//trim(wavename)//'.csv', status = 'old')
  do n = 1, nend
     read(10,*)time_wav(n), qq_wav(n)
  enddo
  close(10)
  
  time  = time_wav(nend)
  tend  = int(time/dt) 
  allocate(time_in(tend))
  
end subroutine input_array



subroutine cal_critical_shear_stress
  use mndr

  if(d*100 .ge. 0.303)then
     ustrc = sqrt(80.9*(d*100)) / 100
     tauc  = ustrc**2 / (s * g * d)     
  else if(d*100 .ge. 0.118 .and. d*100 .lt. 0.303)then
     ustrc = sqrt(134.6 * (d*100)**(31/32)) / 100
     tauc  = ustrc**2 / (s * g * d)     
  else if(d*100 .ge. 0.0565 .and. d*100 .lt. 0.118)then
     ustrc = sqrt(55.0 * (d*100)) / 100
     tauc  = ustrc**2 / (s * g * d)
  else if(d*100 .ge. 0.0065 .and. d*100 .lt. 0.0565)then
     ustrc = sqrt(8.41 * (d*100)**(11/32)) / 100
     tauc  = ustrc**2 / (s * g * d)
  else if(d*100 .lt. 0.0065)then
     ustrc = sqrt(226 * (d*100)) / 100
     tauc  = ustrc**2 / (s * g * d)
  endif

end subroutine cal_critical_shear_stress



subroutine set_array
  use mndr  

  allocate(x(iend))
  allocate(fr1(iend), bl1(iend,tend), dep1(iend,tend), wl1(iend,tend), q1(iend,tend), M1(iend,tend))
  allocate(lcl1(iend,tend), adv1(iend,tend), prs1(iend,tend), frc1(iend,tend))  

  allocate(fr2(iend), bl2(iend,tend), dep2(iend,tend), wl2(iend,tend), q2(iend,tend), M2(iend,tend))
  allocate(lcl2(iend,tend), adv2(iend,tend), prs2(iend,tend), frc2(iend,tend))  
  allocate(lcl_m2(iend), adv_m2(iend))
  
end subroutine set_array



subroutine input_upstream_discharge(q)
  use mndr
  real*8, intent(out) :: q(iend,tend)
  
  q(1,1)     = qq_wav(1) / wdth
  time_in(1) = time_wav(1)
  q(:,1)     = q_int
  
  ! ---- Flow discharge of upstream boundary condition (m3/sec) ----
  do t = 2, tend
     time_in(t) = time_in(t-1) + dt
     do n = 1, nend
        if(time_in(t).ge.time_wav(n) .and. time_in(t).le.time_wav(n+1))then
           q(1,t) = (qq_wav(n+1)/wdth - qq_wav(n)/wdth)/(time_wav(n+1) - time_wav(n))*(time_in(t) - time_wav(n)) + qq_wav(n)/ wdth
        endif
     enddo
  enddo
  
end subroutine input_upstream_discharge



subroutine set_initial_condition(h,bl,wl)
  use mndr
  real*8, intent(inout) :: h(iend,tend), bl(iend,tend), wl(iend,tend)
  real*8                :: bkx, bam, bmp

!!$  ! ---- Case1 ----
!!$  bkx       = 3.0       ! wave number    of bed form
!!$  bam       = 0.0005    ! wave amplitude of bed form

  ! ---- Case2 ----
  bkx       = 1.0       ! wave number    of bed form
  bam       = 0.002    ! wave amplitude of bed form

!!$  ! ---- Case3 ----
!!$  bkx       = 3.0       ! wave number    of bed form
!!$  bam       = 0.0015    ! wave amplitude of bed form
  
  h(:,1)    = h_int
  h(iend,1) = h_dwn
  
  x(1)      = 0.00
  bl(1,:)   = 20.0

  do i = 1, iend

     if(i.lt.iend)then
        x (i+1)   = x(i) + dx
        bl(i+1,:) = bl(i,:) - (dx*ib)
     endif


!!$     ! ---- Case1 & 3 ----
!!$     if(x(i).ge.4.0 .and. x(i).le.10.0)then             
!!$        bmp       = bam   * sin(2. * pi * real(i) / real(80) * bkx)
!!$        bl(i+1,:) = bl(i+1,:) - bmp
!!$     endif

     ! ---- Case2 ----
     if(x(i).ge.5.0 .and. x(i).le.7.0)then             
        bmp       = bam   * sin(2. * pi * real(i) / real(20) * bkx)
        bl(i+1,:) = bl(i+1,:) - bmp
     endif

!!$     if(x(i).ge.3.0 .and. x(i).le.5.0)then
!!$        bl(i,:) = bl(i,:) + mnt
!!$        wl(i,1) = wl(i,1) + mnt
!!$     endif
     
  enddo
  
  wl(:,1) = bl(:,1) + h(:,1)
    
end subroutine set_initial_condition



subroutine set_boundary_condition(h,bl,wl,q)
  use mndr
  real*8, intent(inout) :: h(iend,tend), bl(iend,tend), wl(iend,tend), q(iend,tend)
  
  rtime = rtime + dt
  write(ctime,'(f10.2)')rtime

  ! ---- Depth of downstream boundary (i=iend, t=1) ----
  h (iend,1)   = h_dwn
  wl(iend,1)   = h(iend,1) + bl(iend,1) 
 
end subroutine set_boundary_condition



subroutine cal_equation_of_continuity(h,bl,wl,q,fr)
  use mndr
  real*8, intent(inout) :: h(iend,tend), bl(iend,tend), wl(iend,tend), q(iend,tend), fr(iend)
  real*8                :: q_iend      , hc(iend)
  
  do i = 1, iend
     if(i .lt. iend)then
        h(i,t+1)  = h(i,t) - dt/dx * (q(i+1,t) - q(i,t))
        hc(i)     = ( ((q(i,t) + q(i+1,t))/2.)**2 / g)**(1./3.)
        
     elseif(i .eq. iend)then
        
        ! ---- Interpolate Depth of downstream boundary(i=iend, j=t) ----
        q_iend    = q(i,t) + (x(i) - x(i-1)) * (q(i,t) - q(i-1,t)) / (x(i) - x(i-1))
        h(i,t+1)  = h(i,t) - dt/dx * (q_iend - q(i,t))
        hc(i)     = ( ((q(i,t) + q_iend)/2.)**2 / g)**(1./3.)
        
     endif
     
     if(h(i,t+1) .lt. 0.0)then
        h(i,t+1) = 0.0
        hc(i)    = 0.0
     endif
     
     wl(i,t+1) = bl(i,t) + h(i,t+1)
  enddo
  
  do i = 1, iend
     fr(i) = ( hc(i) / ((h(i,t) + h(i,t+1))/2.) )**(3./2.)
     if(hc(i) .le. 0.0)then
        fr(i) = 0.0
     endif
  enddo
    
end subroutine cal_equation_of_continuity



subroutine cal_equation_of_motion(h,bl,wl,q,lcl,adv,prs,frc)
  use mndr
  real*8, intent(inout) :: h(iend,tend)  , bl(iend,tend) , wl(iend,tend) , q(iend,tend)
  real*8, intent(out)   :: lcl(iend,tend), adv(iend,tend), prs(iend,tend), frc(iend,tend)
  real*8 :: h_cal(iend,tend), bl_cal(iend,tend), wl_cal(iend,tend), h_adv(iend,tend)
  real*8 :: q_out(iend,tend), u_out(iend,tend) , h_out(iend,tend) , bl_out(iend,tend), wl_out(iend,tend)
  real*8 :: h_i1
  
  ! ---- Setting upstream boundary(i=1) ----
  h_out (1,t+1) = h (1,t+1)
  bl_out(1,t+1) = bl(1,t+1)
  wl_out(1,t+1) = wl(1,t+1)
  h_cal (1,t+1) = h (1,t+1)
  bl_cal(1,t+1) = bl(1,t+1)
  wl_cal(1,t+1) = bl_cal(1,t+1) + h_cal(1,t+1)

  do i = 2, iend
     h_adv(i,t) = 1./4. * (h(i-1,t) + h(i-1,t+1) + h(i,t) + h(i,t+1))
  enddo
  
  ! ---- Interpolate Depth of upstream boundary for calculating advection term(i=iend, j=t) ----
  h_i1          = (1./2. * (h(1,t) + h(1,t+1)))
  h_adv(1,t)    = h_i1 + (h_adv(2,t) - h_i1)

  
  do i = 2, iend

     h_cal (i,t+1) = 1./2. * (h (i-1,t+1) + h (i,t+1))
     bl_cal(i,t+1) = 1./2. * (bl(i-1,t+1) + bl(i,t+1))
     wl_cal(i,t+1) = bl_cal(i,t+1)        + h_cal(i,t+1)
     
     ! ---- Calculate Advection term ----
     adv(i,t) = 0.0
     if(q(i,t).gt.0.)then
        if(q(i+1,t).gt.0.)then
           if(h_adv(i,t).gt.h_lim .and. h_adv(i-1,t).gt.h_lim)then
              adv(i,t) = (q(i,t)**2/h_adv(i,t) - q(i-1,t)**2/h_adv(i-1,t)) / dx
           else
              adv(i,t) = 0.0
           endif
        endif
     elseif(q(i,t).lt.0.)then
        if(i.eq.iend)then
           adv(i,t) = 0.0
        else
           if(h_adv(i,t).gt.h_lim .and. h_adv(i+1,t).gt.h_lim)then
              adv(i,t) = (q(i+1,t)**2/h_adv(i+1,t) - q(i,t)**2/h_adv(i,t)) / dx      
           else
              adv(i,t) = 0.0
           endif
        endif
     endif
     
     ! ---- Calculate Pressure term ----
     if(h_cal(i,t+1).gt.h_lim)then 
        prs(i,t) = g * h_cal(i,t+1) * (wl(i,t+1) - wl(i-1,t+1)) / dx
     else
        prs(i,t) = 0.0
     endif
     
     ! ---- Calculate Friction term ----
     if(h_adv(i,t).ge.h_lim)then
        frc(i,t) = g * nb**2 * abs(q(i,t)) * q(i,t) / h_adv(i,t)**(7./3.)
     else
        frc(i,t) = 0.0
     endif
     
     ! ---- Calculate Local term ----
     q(i,t+1) = q(i,t) - dt * (adv(i,t) + prs(i,t) + frc(i,t))
     lcl(i,t) = (q(i,t+1) - q(i,t)) / dt

     ! ---- for Output ----
     q_out(i-1,t+1)  = 1./4. * (q(i-1,t) + q(i-1,t+1) + q(i,t) + q(i,t+1))

     if(i.eq.iend)then
        q_out(i,t+1) = 1./2. * (q(i,t) + q(i,t+1))
     endif
     
     h_out (i,t+1) = h (i,t+1)
     bl_out(i,t+1) = bl(i,t+1)
     wl_out(i,t+1) = wl(i,t+1)
     
  enddo 
  
end subroutine cal_equation_of_motion



subroutine cal_equation_of_bedloadflux_and_exner_unsteady(h,bl,q,qb_fml)
  use mndr
  character*100, intent(in) :: qb_fml
  real*8, intent(inout)     :: h(iend,tend), bl(iend,tend), q(iend,tend)
  real*8                    :: h_cal(iend) , h_iend       , q_cal(iend),  q_iend
  real*8                    :: ie(iend)    , tau(iend)    , tau_iend   ,  qb(iend), qb_iend
  
  
  ! ---- Calculate Depth for calculating Sheilds number and bedload discharge (i=iend, t=t+1/2) ----  
  do i = 1, iend
     if(i.eq.1)then
        ! ---- Interpolate Depth of upstream boundary       (i=1   , t=t+1/2) ----
        h_cal(i) = h(i,t) + ( ((h(i,t) + h(i+1,t) + (h(i,t+1) + h(i+1,t+1))) / 4) - h(i,t) )        
     else
        h_cal(i) = 1./4. * (h(i,t) + h(i,t+1) + h(i-1,t) + h(i-1,t+1) )
     endif
  enddo
  
  ! ---- Interpolate Depth of downstream boundary     (i=iend, t=t+1/2) ----
  h_iend   = h(iend,t) + ( h_cal(iend) - ((h(iend,t) + h(iend,t+1)) / 2))

  
  ! ---- Calculate Discharge for calculating Sheilds number and bedload discharge (i=iend, t=t+1/2) ----  
  q_cal(:) = q(:,t)
  
  ! ---- Interpolate discharge of downstream boundary (i=iend, t=t+1) ----
  q_iend  = q(iend,t)   + (x(iend) - x(iend-1)) * (q(iend,t) - q(iend-1,t))     / (x(iend) - x(iend-1))


  ! ---- Interpolate Sheilds number of downstream boundary (i=iend, t=t+1) ----
  if(h_iend.ge.h_lim)then
     r        = ( wdth * h_iend / (2*h_iend + wdth) )
     tau_iend = r / (s*d) * (nb * q_iend / h_iend**(5./3.))**(2)
  else
     tau_iend = 0.0
  endif
     
  ! ---- Calculate Sheilds number ----
  do i = 1, iend
     if(h_cal(i).ge.h_lim)then
        r        = ( wdth * h_cal(i) / (2*h_cal(i) + wdth) )
        ie(i)    = (nb * q_cal(i) / h_cal(i)**(5./3.))**(2)
        tau(i)   = r / (s*d) * ie(i)
     else
        tau(i)   = 0.0
     endif
  enddo
  
  ! ---- Interpolate Sheilds number of downstream boundary (i=iend, t=t+1) ----
  if(tau_iend.le.tauc)then
     qb_iend = 0.0
  elseif(trim(qb_fml).eq.'mpm')then
     qb_iend = 8 * (tau_iend - tauc)**(3./2.) * sqrt(s*g*d**3)     
  elseif(trim(qb_fml).eq.'amf')then
     qb(i) = 17 * tau_iend * (1 - tau_iend / tauc) * (1 - sqrt(tau_iend / tauc)) * sqrt(s*g*d**3)
  endif
  
  ! ---- Calculate Bedload discharge ----  
  do i = 1, iend
     if(tau(i).le.tauc)then
        qb(i) = 0.0
     elseif(trim(qb_fml).eq.'mpm')then
        qb(i) = 8 * (tau(i) - tauc)**(3./2.) * sqrt(s*g*d**3)
     elseif(trim(qb_fml).eq.'amf')then
        qb(i) = 17 * tau(i) * (1 - tau(i) / tauc) * (1 - sqrt(tau(i) / tauc)) * sqrt(s*g*d**3)
     endif
  enddo

  ! ---- Calculate Elevation change ----
  do i = 1, iend
     if(i .lt. iend)then
        bl(i,t+1) = bl(i,t) - dt / ((1-0.6)*dx) * (qb(i+1) - qb(i))
     else
        bl(i,t+1) = bl(i,t) - dt / ((1-0.6)*dx) * (qb_iend - qb(i))
     endif     
  enddo
!!$  write(*,*)abs(bl(iend/4,t+1) - bl(iend/4,t))/d *100
  
end subroutine cal_equation_of_bedloadflux_and_exner_unsteady



subroutine cal_equation_of_bedloadflux_and_exner_unsteady_cip(h,bl,q,qb_fml)
  use mndr
  character*100, intent(in) :: qb_fml
  real*8, intent(inout)     :: h(iend,tend), bl(iend,tend), q(iend,tend)
  real*8                    :: h_cal(iend) , h_iend       , q_cal(iend),  q_iend
  real*8                    :: ie(iend)    , tau(iend)    , tau_iend   ,  qb(iend), qb_iend
  
  
  ! ---- Calculate Depth for calculating Sheilds number and bedload discharge (i=iend, t=t+1/2) ----  
  do i = 1, iend
     if(i.eq.1)then
        ! ---- Interpolate Depth of upstream boundary       (i=1   , t=t+1/2) ----
        h_cal(i) = h(i,t) + ( ((h(i,t) + h(i+1,t) + (h(i,t+1) + h(i+1,t+1))) / 4) - h(i,t) )        
     else
        h_cal(i) = 1./4. * (h(i,t) + h(i,t+1) + h(i-1,t) + h(i-1,t+1) )
     endif
  enddo
  
  ! ---- Interpolate Depth of downstream boundary     (i=iend, t=t+1/2) ----
  h_iend   = h(iend,t) + ( h_cal(iend) - ((h(iend,t) + h(iend,t+1)) / 2))

  
  ! ---- Calculate Discharge for calculating Sheilds number and bedload discharge (i=iend, t=t+1/2) ----  
  q_cal(:) = q(:,t)
  
  ! ---- Interpolate discharge of downstream boundary (i=iend, t=t+1) ----
  q_iend  = q(iend,t)   + (x(iend) - x(iend-1)) * (q(iend,t) - q(iend-1,t))     / (x(iend) - x(iend-1))


  ! ---- Interpolate Sheilds number of downstream boundary (i=iend, t=t+1) ----
  if(h_iend.ge.h_lim)then
     r        = ( wdth * h_iend / (2*h_iend + wdth) )
     tau_iend = r / (s*d) * (nb * q_iend / h_iend**(5./3.))**(2)
  else
     tau_iend = 0.0
  endif
     
  ! ---- Calculate Sheilds number ----
  do i = 1, iend
     if(h_cal(i).ge.h_lim)then
        r        = ( wdth * h_cal(i) / (2*h_cal(i) + wdth) )
        ie(i)    = (nb * q_cal(i) / h_cal(i)**(5./3.))**(2)
        tau(i)   = r / (s*d) * ie(i)
     else
        tau(i)   = 0.0
     endif
  enddo
  
  ! ---- Interpolate Sheilds number of downstream boundary (i=iend, t=t+1) ----
  if(tau_iend.le.tauc)then
     qb_iend = 0.0
  elseif(trim(qb_fml).eq.'mpm')then
     qb_iend = 8 * (tau_iend - tauc)**(3./2.) * sqrt(s*g*d**3)     
  elseif(trim(qb_fml).eq.'amf')then
     qb(i) = 17 * tau_iend * (1 - tau_iend / tauc) * (1 - sqrt(tau_iend / tauc)) * sqrt(s*g*d**3)
  endif
  
  ! ---- Calculate Bedload discharge ----  
  do i = 1, iend
     if(tau(i).le.tauc)then
        qb(i) = 0.0
     elseif(trim(qb_fml).eq.'mpm')then
        qb(i) = 8 * (tau(i) - tauc)**(3./2.) * sqrt(s*g*d**3)
     elseif(trim(qb_fml).eq.'amf')then
        qb(i) = 17 * tau(i) * (1 - tau(i) / tauc) * (1 - sqrt(tau(i) / tauc)) * sqrt(s*g*d**3)
     endif
  enddo

  ! ---- Calculate Elevation change ----
  do i = 1, iend
     if(i .lt. iend)then
        bl(i,t+1) = bl(i,t) - dt / ((1-0.6)*dx) * (qb(i+1) - qb(i))
     else
        bl(i,t+1) = bl(i,t) - dt / ((1-0.6)*dx) * (qb_iend - qb(i))
     endif     
  enddo
!!$  write(*,*)abs(bl(iend/4,t+1) - bl(iend/4,t))/d *100
  
end subroutine cal_equation_of_bedloadflux_and_exner_unsteady_cip



subroutine cal_equation_of_M(h,bl,q,M,lcl_m,adv_m)
  use mndr
  real*8, intent(inout) :: h(iend,tend), bl(iend,tend), q(iend,tend), M(iend,tend), lcl_m(iend), adv_m(iend)
  real*8                :: h_cal(iend) , q_cal(iend)  , hc(iend)    , bl_cal(iend), bl_iend
  real*8                :: ie(iend)    , tau(iend)    , fr(iend)
  
  ! ---- Calculate Depth & Flux & Bed level for calculating M and Elevation change ----
  do i = 1, iend
     if(i.eq.1)then
        h_cal(i) = h(i,t) + (h(i+1,t) - h(i,t)) / (x(i+1) - x(i)) * (x(i+1) - x(i))/2
     else
        h_cal(i) = 1./2. * (h(i,t) + h(i-1,t))
     endif
  enddo
  
  q_cal(:)  = 1./2. * (q(:,t) + q(:,t-1))
  bl_cal(:) =  bl(:,t)
  hc(:)     = ( q_cal(:)**(2) / g)**(1./3.)


!!$  ! ---- Local term & Advection term ----
  lcl_m(1) = 0.0 ; lcl_m(iend) = 0.0
  adv_m(1) = 0.0 ; adv_m(iend) = 0.0

  do i = 2, iend-1
     lcl_m(i) = (q(i,t-1)/(1./4*( h(i-1,t) + h(i-1,t-1) + h(i,t) + h(i,t-1))) &
  - q(i,t-2)/(1./4*( h(i-1,t-1) + h(i-1,t-2) + h(i,t-1) + h(i,t-2)))) / dt
  enddo
  
  do i = 2, iend-1
     adv_m(i) = -5./3 * ( (q(i,t-1)+q(i,t-2))/2 ) / ( (h(i,t-1)+h(i-1,t-1))/2 ) * &
          ( &
          ( ( 1./4. * ( q(i+1,t-1) + q(i+1,t-2) + q(i  ,t-1) + q(i  ,t-2) ) ) / (h(i  ,t-1) ) ) - &
          ( ( 1./4. * ( q(i  ,t-1) + q(i  ,t-2) + q(i-1,t-1) + q(i-1,t-2) ) ) / (h(i-1,t-1) ) ) &
          ) / &
          ( x(i) - x(i-1) )     
  enddo
  ! -------------------------------------

  
  ! ---- Calculate M ----
  do i = 1, iend
     if(h_cal(i).ge.h_lim)then
        r      = ( wdth * h_cal(i) / (2*h_cal(i) + wdth) )
        ie(i)  = ( nb * q_cal(i) / h_cal(i)**(5./3.))**(2)
        fr(i)  = ( hc(i) / h_cal(i) )**(3./2.)
        tau(i) =  r / (s*d) * ie(i)
        M(i,t) = 28*(tau(i)-tauc)**(1./2.)*sqrt(s*g*d**(3))*ie(i) / (s*d*(1-0.6)*(1-4./9.*fr(i)**(2)))
     else
        tau(i) = 0.0
     endif
  enddo

  do i = 1, iend
     if(tau(i).le.tauc)then
        M(i,t) = 0.0
     endif
  enddo

  ! ---- Calculate Elevation change ----
  do i = 1, iend
     if(i.eq.1)then
        bl(i,t+1) = bl(i,t)
     else
        bl(i,t+1) = bl(i,t) - M(i,t)*dt * ( (bl_cal(i) - bl_cal(i-1)) / (x(i) - x(i-1)) + ie(i))
     endif
  enddo
  
end subroutine cal_equation_of_M



subroutine cal_equation_of_M_old(h,bl,q,M)
  use mndr
  real*8, intent(inout) :: h(iend,tend), bl(iend,tend), q(iend,tend), M(iend,tend)
  real*8                :: h_cal(iend) , q_cal(iend)  , hc(iend)    , bl_cal(iend), bl_iend
  real*8                :: ie(iend)    , tau(iend)    , fr(iend)
  
  ! ==== Central ====  
  ! ---- Calculate Depth for calculating Sheilds number and M and Fr (i=iend, t=t+1/2) ----  
  do i = 1, iend
     h_cal(i) = 1./2. * (h(i,t) + h(i,t+1))
  enddo


  ! ---- Calculate Discharge for calculating Sheilds number and M and Fr (i=iend, t=t+1/2) ----  
  do i = 1, iend
     if(i.lt.iend)then
        q_cal(i) = 1./2. * (q(i,t) + q(i+1,t))
     elseif(i.eq.iend)then
        q_cal(i) = ( q(iend,t) + (q(iend,t) + (q(iend,t) - q(iend-1,t)) / (x(iend) - x(iend-1)) * (x(iend) - x(iend-1))) ) / 2
     endif
  enddo

  ! ---- Calculate Elevation for calculating Elevation change (i=iend, t=t+1/2) ----  
  do i = 1, iend
     if(i.eq.1)then
        ! ---- Interpolate Elevation of upstream boundary     (i=1   , t=t+1/2) ----
        bl_cal(i) = (bl(i,t) + bl(i,t+1)) / 2
     else
        bl_cal(i) = 1./4. * (bl(i,t) + bl(i,t+1) + bl(i-1,t) + bl(i-1,t+1) )
     endif
  enddo

  ! ---- Interpolate Elevation for calculating Elevation change (i=iend, t=t+1/2) ----  
  bl_iend = bl_cal(iend) + (bl_cal(iend) - bl_cal(iend-1)) / (x(iend) - x(iend-1)) * (x(iend) - x(iend-1))
  ! =============
  

!!$  ! ==== upwind 01 ====
!!$  do i = 1, iend
!!$     if(i.eq.1)then
!!$        h_cal(i) = (1./2. * (h(i,t) + h(i,t+1))) + (1./4. * (h(i,t) + h(i,t+1) + h(i+1,t) + h(i+1,t+1))) &
!!$             - (1./2. * (h(i,t) + h(i,t+1)))
!!$     else
!!$        h_cal(i) = 1./4. * (h(i,t) + h(i,t+1) + h(i-1,t) + h(i-1,t+1))
!!$     endif
!!$  enddo
!!$
!!$  q_cal(:) = q(:,t)
!!$    
!!$  do i = 1, iend
!!$     bl_cal(i) = (bl(i,t) + bl(i,t+1)) / 2
!!$  enddo
!!$
!!$  do i = 1, iend
!!$     hc(i) = ( q_cal(i)**(2) / g)**(1./3.)
!!$  enddo
!!$  ! =============
!!$
!!$  ! ==== upwind 02 ====
!!$  do i = 1, iend
!!$     if(i.eq.1)then
!!$        h_cal(i) = h(i,t) + (h(i+1,t) - h(i,t)) / (x(i+1) - x(i)) * (x(i+1) - x(i))/2
!!$     else
!!$        h_cal(i) = 1./2. * (h(i,t) + h(i-1,t))
!!$     endif
!!$  enddo
!!$  
!!$  q_cal(:)  = 1./2. * (q(:,t) + q(:,t-1))
!!$
!!$  bl_cal(:) =  bl(:,t)
!!$
!!$  hc(:)     = ( q_cal(:)**(2) / g)**(1./3.)
!!$  ! =============


  ! ---- Calculate M ----
  do i = 1, iend
     if(h_cal(i).ge.h_lim)then
        r        = ( wdth * h_cal(i) / (2*h_cal(i) + wdth) )
        ie(i)    = ( nb * q_cal(i) / h_cal(i)**(5./3.))**(2)
        fr(i)    = ( hc(i) / h_cal(i) )**(3./2.)
        tau(i)   =  r / (s*d) * ie(i)
        M(i,t)   = 28*(tau(i)-tauc)**(1./2.)*sqrt(s*g*d**(3))*ie(i) / (s*d*(1-0.6)*(1-4./9.*fr(i)**(2)))
     else
        tau(i)   = 0.0
     endif
  enddo

  do i = 1, iend
     if(tau(i).le.tauc)then
        M(i,t)   = 0.0
     endif
  enddo
  
  ! ---- Calculate Elevation change (Central) ----
  do i = 1, iend
     if(i .lt. iend)then
        if(i.eq.1)then
           bl(i,t+1) = bl(i,t)
        else
           bl(i,t+1) = bl(i,t) - M(i,t)*dt * ( (bl_cal(i+1) - bl_cal(i)) / (x(i+1) - x(i)) + ie(i))
        endif
     else
        bl(i,t+1) = bl(i,t) - M(i,t)*dt * ( (bl_iend     - bl_cal(i)) / (x(i) - x(i-1)) + ie(i))
     endif
  enddo

!!$  ! ---- Calculate Elevation change (Upwind 01 & 02) ----
!!$  do i = 1, iend
!!$     if(i.eq.1)then
!!$        bl(i,t+1) = bl(i,t)
!!$     else
!!$        bl(i,t+1) = bl(i,t) - M(i,t)*dt * ( (bl_cal(i) - bl_cal(i-1)) / (x(i) - x(i-1)) + ie(i))
!!$     endif
!!$  enddo
  
end subroutine cal_equation_of_M_old



subroutine set_plplot
  use mndr
  use plplot
  
  plparseopts_rc = plparseopts(PL_PARSE_FULL)
  if(plparseopts_rc .ne. 0) stop "plparseopt error"
  call plscol0( 0,255,255,255)
  call plscol0(15,  0,  0,  0)
  call plscmap0n(0)
  call plinit
     
end subroutine set_plplot



!!$     call cal_Subcritical_flow(q2, bl2, h2, wl2)
!!$     call cal_equation_of_bedloadflux_and_exner_Nonuniform(h2, bl2)

!!$subroutine cal_Subcritical_flow(bl, h, wl)
!!$  use mndr
!!$  real*8, intent(in)    :: bl(iend,tend)
!!$  real*8, intent(inout) :: h(iend,tend), wl(iend,tend)
!!$  real*8                :: aa, bb, cc, fh, dfh, dh, h2, h1, hc, ho, e
!!$  
!!$  hc       = ( q**2 / (g * wdth**2) )**(1./3.)
!!$  ho       = ( q * nb / (wdth * sqrt(ib)))**(3./5.)
!!$  h2       = ho
!!$  h1       = h2
!!$  wl(1,t)  = h2
!!$  h(1,t)   = h2
!!$  e        = 0.0001
!!$  aa       = q**2 / (2 * g * wdth**2)
!!$
!!$  do i = 1, iend-1
!!$     bb        = - ( nb**2 * q**2 * dx / (2 * wdth**2) )
!!$     cc        = bl(i+1,t) - ( bl(i,t) + h2 + aa / h2**2 - bb / h2**(10./3.) )
!!$     fh        = h1 + aa/h1**2 + bb/h1**(10./3.) + cc
!!$     wl(i+1,t) = h1 + bl(i+1,t)
!!$     h(i+1,t)  = h1
!!$     
!!$     do while(abs(fh) .gt. e)
!!$        fh  = h1 + aa/h1**2 + bb/h1**(10./3.) + cc
!!$        dfh = 1 - 2 * aa/h1**3 - 10./3. * bb/h1**(13./3.)
!!$        dh  = - fh/dfh
!!$        h1  = h1 + dh
!!$        if(h1 .lt. hc)then
!!$           h1        = hc
!!$           h(i+1,t)  = hc          
!!$           wl(i+1,t) = h(i+1,t) + bl(i+1,t)
!!$           exit
!!$        else
!!$           h(i+1,t)  = h1
!!$           wl(i+1,t) = h(i+1,t) + bl(i+1,t)
!!$        endif
!!$     enddo
!!$     h2 = h1
!!$  enddo
!!$  
!!$end subroutine cal_Subcritical_flow



!!$subroutine cal_Bedload_mpm(h, nb, qb)
!!$  use mndr
!!$  real*8                :: tau_sub,ie2
!!$  real*8, intent(in)    :: h(iend,tend),nb(iend,tend)
!!$  real*8, intent(out)   :: qb(iend,tend)
!!$       
!!$  do i = 1, iend
!!$     ie2     = (nb(i,t)**2 * q**2 / (b**2 * h(i,t)**(10./3.)))
!!$     tau_sub = h(i,t) * ie2 / (s * d)
!!$     if((tau_sub-tauc).le.0.0)then
!!$        qb(i,t) = 0.0
!!$     else
!!$        qb(i,t) = 8 * (tau_sub - tauc)**(1.5) * sqrt(s*g*d**3)
!!$     endif
!!$  enddo
!!$  
!!$end subroutine cal_Bedload_mpm
!!$
!!$
!!$
!!$subroutine cal_Elevation_change(qb,dz,bl)
!!$  use mndr
!!$  real*8, intent(in)    :: qb(iend,tend)
!!$  real*8, intent(inout) :: bl(iend,tend), dz(iend,tend)
!!$
!!$  do i = 1, iend-1
!!$     dz(i,t) = - 1. / (1-0.4) * ( qb(i,t)*b - qb(i+1,t)*b ) / (dx*b) * dt
!!$     bl(i,t+1) = bl(i,t) + dz(i,t)
!!$  enddo
!!$
!!$end subroutine cal_Elevation_change
