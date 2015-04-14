!-------------------------------------------------------------------------
!
      integer m,n,ilo,ilop1,nz,mz
      integer*2 land,water,ucalc,vcalc,zcalc,copyfrom,contract
      parameter(m=197,n=234,ilo=46)
      parameter(ilop1=ilo+1)
      parameter(land=0, water=1, zcalc=2,ucalc=4,vcalc=8,copyfrom=16)
      parameter (contract=32)
      parameter (nz=n-1,mz=m-1)
      real rzet(m,n)
!
!-----------------------------------------------------------------------
!------------variables concerned with output control-----------
      integer ifile, ifileperiod, iprint,iprintperiod,savelayer
      integer nvar
      character archive*80, header*80,filenom*80
      parameter (nvar=30)
      integer icompte
      common/control/ifile, ifileperiod,iprint,iprintperiod,savelayer,icompte
      common/control3/archive,header,filenom
!
!------------variables concerned with time steps and dates--------------
      integer initial,final,kstep,time(6),tzero(6)
      real deltat,pi
      common/dates/initial,final,kstep,time,tzero,deltat,pi
!
!-------------------------------------------------------------------------------------------
!     the main solution variables
      real u(m,n,ilo),v(m,n,ilo),w(m,n,ilo),&
           u1(m,n,ilo),v1(m,n,ilo),w1(m,n,ilo),u2(m,n,ilo),v2(m,n,ilo),w2(m,n,ilo)
      real za(m,n)
!
      common /com1/u,v,w,u1,u2,v1,v2,w1,w2
      common /com2/za
      real temp(m,n,ilo),salt(m,n,ilo),temp1(m,n,ilo),salt1(m,n,ilo),temp2(m,n,ilo),salt2(m,n,ilo)
      real mid_depth(ilo)
      common /com5/temp,salt,temp1,temp2,salt1,salt2,mid_depth
!-----------------------------------------------------------------------
!-----------topography expanding/contracting variables-----------------
      integer*2 ldep(m,n), nlayer(m,n), jc(m,n)
      real layx(m,n),  layy(m,n)
      real fric(m,n)
      common /ind/ jc,ldep,nlayer
      common/ind3/fric,layx,layy
!-----------------------------------------------------------------------
      real  dt,r,g,rad,dth,gh,rdt,coru,corv
      common /gitter/ dt,r,g,rad,dth,gh,rdt,coru,corv
      real dlx(m,n),dly(m,n),rdlx(m,n),rdly(m,n)
      common /gitter2/dlx,dly,rdlx,rdly
!
!---------a few more fundamental constants------------------------------
      real dh(ilo),pd(ilo),prd(ilo),pr2d(ilo),r2d(ilo),tau(ilo),dd(ilo),rd(0:ilop1)
      common/num/dh,pd,prd,pr2d,r2d,tau,dd,rd
!
!------------further variables dealing with the solution technique------
      integer*2  izet(m,n)
      real  dz(ilo)
!     Upper layer interface depths
      real up_depth(ilo)
      integer itel
      common/extras/itel
      common/extras3/izet
      common/extras2/up_depth,dz
!
!-----------------------------------------------------------------------
!
!     Particle tracking variables
!
!     maxmum number of particles
!
      integer max_number_part
      parameter (max_number_part = 1000000)
!
!
!     particle positions
!
      real xpo(max_number_part),ypo(max_number_part)
      real zpo(max_number_part)
!
!     starting kstep
!
      integer istart_part(max_number_part)
!
!     particle start date and time
!
      integer ipart_time(max_number_part,6)
!
!     number of active particle
!
      integer npart
!
!     lenght of time the particle will move after istart_part (in second):
!
      integer ipart_time_length(max_number_part)
!
      integer ipart_step(max_number_part)
!
!     twice the time step (needed in the approximation)
!
      real dt2
!
!  Variables for the Fourth Order Runge-Kutta Method
!
      real xk1(max_number_part),xk2(max_number_part)
      real xk3(max_number_part),xk4(max_number_part)
      real yk1(max_number_part),yk2(max_number_part)
      real yk3(max_number_part),yk4(max_number_part)
      real zk1(max_number_part),zk2(max_number_part)
      real zk3(max_number_part),zk4(max_number_part)
!
!     Size of the output buffer
!
      integer nbuf
      parameter (nbuf = 100000)
!
      integer ibuf
      integer buf_time(nbuf,6)
      integer buf_number(nbuf)
!
      real buf_xpo(nbuf)
      real buf_ypo(nbuf)
      real buf_zpo(nbuf)
!
      common /traject1/ xpo,ypo,zpo,xk1,xk2,xk3,xk4,yk1,yk2,yk3,yk4,zk1,zk2,zk3,zk4,&
                       dt2,buf_xpo,buf_ypo,buf_zpo
!
      common /traject2/ istart_part,ipart_time,npart,ipart_step,&
                        buf_time,buf_number,ibuf,ipart_time_length
!-----------------------------------------------------------------------
