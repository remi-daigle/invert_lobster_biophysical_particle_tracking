      subroutine particle_input

      include 'ocean.h'
      integer ipart,i,k,j,istep
!
!-----------------------------------------------------------------------
!
! Keep input particles file
!
       !call system('cp input_particles '//archive(1:lenstr(archive))//'.input_particles')
!
!-----------------------------------------------------------------------
      dt2 = 2.*dt
!
      open (49, file='input_particles')
!      open (51,file=archive(1:lenstr(archive))//'.output_particles',form='unformatted')
      open(51,file='OUT/output_particles',form='unformatted')
!
      ipart = 1
 98   read(49,*,end=99) (ipart_time(ipart,i),i=1,6), &
                       xpo(ipart),ypo(ipart),zpo(ipart), &
                       ipart_time_length(ipart)  !In second please
      ipart = ipart + 1
      goto 98
 99   npart=ipart-1
      close(49)
!
      print*,'Number of particles (npart)= ',npart
!
!-----------------------------------------------------------------------
!     Generating particles all over the grid if npart=0
!
      if(npart.eq.0) then
!        initializing time (time would be tzero + one dt)
         call update(initial)
         print*
         print*,'npart=0, generating particle in the grid'
         print*
         ipart=0
         do i=1,m,5
         do k=1,n,5
          if(izet(i,k).gt.0) then
            ipart=ipart+1
!            ipart_time(ipart,:)=tzero(:)
            ipart_time(ipart,:)=time(:)
            xpo(ipart)=real(i)-.5
            ypo(ipart)=real(k)-.5
            zpo(ipart)=0.0   !At the surface
            ipart_time_length(ipart)=(final-initial)*dt  !The full run
          endif
         enddo
         enddo
!
         npart=ipart
!
         print*
         print*,'Generated ',npart,'particles...'
         print*
      endif
!
!-----------------------------------------------------------------------
      if (npart.gt.max_number_part) stop 'Stop, max_number_part is not big enough...'
!
      ipart_step=ipart_time_length/dt  !Array
!
      istart_part=final+1   !Array initialisation
!
!   Find particles starting kstep (istart_part) 
!
!      do istep = initial-1, final
       do istep = initial, final
        call update(istep)
        do ipart = 1, npart
          if(time(6).eq.ipart_time(ipart,6).and.  &
            time(5).eq.ipart_time(ipart,5).and.  &
            time(4).eq.ipart_time(ipart,4).and.  &
            time(3).eq.ipart_time(ipart,3).and.  &
            time(2).eq.ipart_time(ipart,2).and.  &
            !abs(time(1)-ipart_time(ipart,1)).lt.dt/2.) then 
            time(1).eq.ipart_time(ipart,1)) then 
                    istart_part(ipart) = istep
                   ! print*,'ipart,istart_part(ipart)=',ipart,istart_part(ipart)
          endif
        enddo
      enddo
!
!     To re-initialize the time
!
      call update(initial)
!
      ibuf=0 !Ouput buffer initialisation
      return
      end
!
!
      SUBROUTINE trajectory
 
      include 'ocean.h'
      integer kstep_diff,it,ipart,i
      real up,vp,wp
      real value
      real xp_temp,yp_temp,zp_temp
      real Kdiff !m2/s
      real xdiffusion, ydiffusion
 
!      Kdiff=2. !m2/s
       Kdiff=0. !m2/s  !no ramdom walk for now
!      Kdiff=5. !m2/s
 !     print*,'Diffusion for particles=',kdiff

      do ipart = 1, Npart
        i=int(xpo(ipart))+1
        k=int(ypo(ipart))+1
        kstep_diff = kstep - istart_part(ipart)
        if(kstep_diff.eq.0) then
      !     print*,'ipart,kstep_diff=',ipart,kstep_diff
!         ------------------------------------------------------------
!
!         For outputting initial particle positions
!
          ibuf = ibuf + 1
          buf_number(ibuf)= ipart
!
          do it = 1, 6
            buf_time(ibuf,it) = time(it)
          enddo
!
          buf_xpo(ibuf) = xpo(ipart)
          buf_ypo(ibuf) = ypo(ipart)
          buf_zpo(ibuf) = zpo(ipart)
!
!         output si le buffer est plein
          if(ibuf.eq.nbuf) call output_traj 
!
!         ------------------------------------------------------------
!
!        initialisation ok the k1s
!
          up =  value(v,ilo,m,n,'u',xpo(ipart),ypo(ipart),zpo(ipart))
          vp =  value(u,ilo,m,n,'v',xpo(ipart),ypo(ipart),zpo(ipart))
          wp = -value(w,ilo,m,n,'w',xpo(ipart),ypo(ipart),zpo(ipart))
! WARNING: wp=0.
           wp=0.
!
          xk1(ipart  ) = dt2*up/dly(i,k)
          yk1(ipart  ) = dt2*vp/dlx(i,k)
          zk1(ipart  ) = dt2*wp
!
        else if (kstep_diff.gt.0.and.kstep_diff.le.ipart_step(ipart)) then
!
          if(mod(kstep_diff,2).eq.0) then
!
!         Avanced time: Computation of the k4s then the k1s (then new time)
!         and the new positions
!
          up =  value(v,ilo,m,n,'u',xpo(ipart)+xk3(ipart),ypo(ipart)+yk3(ipart),zpo(ipart)+zk3(ipart))
          vp =  value(u,ilo,m,n,'v',xpo(ipart)+xk3(ipart),ypo(ipart)+yk3(ipart),zpo(ipart)+zk3(ipart))
          wp = -value(w,ilo,m,n,'w',xpo(ipart)+xk3(ipart),ypo(ipart)+yk3(ipart),zpo(ipart)+zk3(ipart))
!
! WARNING: wp=0.
           wp=0.
            xk4(ipart) = dt2*up/dly(i,k)
            yk4(ipart) = dt2*vp/dlx(i,k)
            zk4(ipart) = dt2*wp
!
            xp_temp = xpo(ipart)+( xk1(ipart)+2.*xk2(ipart)+2.*xk3(ipart)+xk4(ipart))/6.
            !Random walk
            xdiffusion=Gasdev2()*(1./dly(i,k))*(2.*Kdiff*dt2)**.5
            xp_temp=xp_temp+xdiffusion
!
            yp_temp = ypo(ipart)+( yk1(ipart)+2.*yk2(ipart)+2.*yk3(ipart)+yk4(ipart))/6.
            !Random walk
            ydiffusion=Gasdev2()*(1./dlx(i,k))*(2.*Kdiff*dt2)**.5
            yp_temp=yp_temp+ydiffusion
!
            zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.
!
!-----------------------------------------------------------------------
!            move only particles if wet
          real_depth=real(izet(int(xp_temp)+1,int(yp_temp)+1))
          if(real_depth.gt.0.0.and.zp_temp.lt.real_depth) then
               xpo(ipart) = xp_temp
               ypo(ipart) = yp_temp
               zpo(ipart) = zp_temp
!
            endif

!-----------------------------------------------------------------------
            up =  value(v,ilo,m,n,'u',xpo(ipart),ypo(ipart),zpo(ipart))
            vp =  value(u,ilo,m,n,'v',xpo(ipart),ypo(ipart),zpo(ipart))
            wp = -value(w,ilo,m,n,'w',xpo(ipart),ypo(ipart),zpo(ipart))
!
! WARNING: wp=0.
           wp=0.
!
            xk1(ipart) = dt2*up/dly(i,k)
            yk1(ipart) = dt2*vp/dlx(i,k)
            zk1(ipart) = dt2*wp
!
!           output of particle positions
!
!            IF(MOD(kstep_diff,30).EQ.0) THEN
            IF(MOD(kstep_diff,1).EQ.0) THEN
!            IF(MOD(kstep_diff,60).EQ.0) THEN
!            IF(MOD(kstep_diff,30).EQ.0) THEN
!
            ibuf = ibuf + 1
            buf_number(ibuf)= ipart
!
            do it = 1, 6
              buf_time(ibuf,it) = time(it)
            enddo
!
            buf_xpo(ibuf) = xpo(ipart)
            buf_ypo(ibuf) = ypo(ipart)
            buf_zpo(ibuf) = zpo(ipart)
!
!           output si le buffer est plein
            if(ibuf.eq.nbuf) call output_traj 
!
            ENDIF
!
          else
!
!         Half time: computation of the k2s and k3s
!
            up =  value(v,ilo,m,n,'u',xpo(ipart)+0.5*xk1(ipart),ypo(ipart)+0.5*yk1(ipart),zpo(ipart)+0.5*zk1(ipart))
            vp =  value(u,ilo,m,n,'v',xpo(ipart)+0.5*xk1(ipart),ypo(ipart)+0.5*yk1(ipart),zpo(ipart)+0.5*zk1(ipart))
            wp = -value(w,ilo,m,n,'w',xpo(ipart)+0.5*xk1(ipart),ypo(ipart)+0.5*yk1(ipart),zpo(ipart)+0.5*zk1(ipart))
!
! WARNING: wp=0.
           wp=0.
!
            xk2(ipart) = dt2*up/dly(i,k)
            yk2(ipart) = dt2*vp/dlx(i,k)
            zk2(ipart) = dt2*wp
!
            up =  value(v,ilo,m,n,'u',xpo(ipart)+0.5*xk2(ipart),ypo(ipart)+0.5*yk2(ipart),zpo(ipart)+0.5*zk2(ipart))
            vp =  value(u,ilo,m,n,'v',xpo(ipart)+0.5*xk2(ipart),ypo(ipart)+0.5*yk2(ipart),zpo(ipart)+0.5*zk2(ipart))
            wp = -value(w,ilo,m,n,'w',xpo(ipart)+0.5*xk2(ipart),ypo(ipart)+0.5*yk2(ipart),zpo(ipart)+0.5*zk2(ipart))
!
! WARNING: wp=0.
           wp=0.
!
            xk3(ipart) = dt2*up/dly(i,k)
            yk3(ipart) = dt2*vp/dlx(i,k)
            zk3(ipart) = dt2*wp
!
          endif
        endif
      enddo
      return
      end

!
!
      REAL FUNCTION value(A3d,nl,nm,nn,index,xp,yp,zp)
!
!-----------------------------------------------------------------------
!
!   This routine interpolates linearly  3D array at a given point
!   (x,y,z) where (x,y) are in grid units (i,k) and z is in meter
!   (positive downward to keep the depths positives).
!
!-----------------------------------------------------------------------
!
! index can be u,v,w or s(sigmat). One can use s for
! temperature and salinity. 
!
!-----------------------------------------------------------------------
!

      include 'ocean.h'
      real dx,dy,d_z
      character*1 index
      integer nl,nm,n3,j,j1,j2,ip,kp
      real xp,yp,zp,A3d(nl,nm,nn)
      real thick,val1,val2
      
!   find the local coordinates (dx,dy)
!
      if(index.eq.'w'.or.index.eq.'s') then
        ip = int(xp+0.5)
        kp = int(yp+0.5)
        dx= xp-(float(ip)-.5)
        dy= yp-(float(kp)-.5)
      else if (index.eq.'v') then
        ip = int(xp)
        kp = int(yp+0.5)
        dx= xp-float(ip)
        dy= yp-(float(kp)-.5)
      else if (index.eq.'u') then
        ip = int(xp+0.5)
        kp = int(yp)
        dx= xp-(float(ip)-.5)
        dy= yp-float(kp)
      else
        stop 'Index problems when calling value'
      endif

      value = 0.0
      d_z = 0.0
      j1  = 1
      j2  = 1
!
!   Find the vertical local coordinate
!
        i=int(xp)+1
        k=int(yp)+1
!
        if(zp.gt.izet(int(xp)+1,int(yp)+1)) return  !if below the bottom
                                                    !And  value = 0.
!
        if(index.eq.'w') then  !Fields at the top of each layer(ex: w)
!
          if(zp.le.0.0) then    !If position is above the surface
            d_z = 0.
            j1  = 1
            j2  = 1
          else
!
            do j = 1,nlayer(i,k)  !Find the bracketing layers
              if(zp.gt.up_depth(j).and.zp.le.dz(j)) then
                d_z = (zp-up_depth(j))/(dz(j)-up_depth(j))  !Thickness fraction
                j1 = j 
                j2 = j+1
                exit        !exit the loop to save time
              endif
            enddo
!
            if(j1.eq.nlayer(i,k)) then
               j2=j1                    !Valeur constante dans 
                                        !la derniere couche
               d_z=0.
             end if
          endif
!                      !Fields at the center of each layer (ex:u)
        else if(index.eq.'u'.or.index.eq.'v'.or.index.eq.'s') then
!
          if(zp.le.mid_depth(1)) then !Constant value if above 
                                      !middle of top layer
            d_z = 0.
            j1  = 1
            j2  = 1
          elseif (zp.gt.mid_depth(nlayer(i,k))) then !Constant value if
                                                     !if below the middle
                                                     !of bottom layer
            d_z =  0.
            j1 = nlayer(i,k)
            j2 = nlayer(i,k)
          else 
            do j = 1,nlayer(i,k)-1 !Find the braketing layers
              if(zp.gt.mid_depth(j).and.zp.le.mid_depth(j+1)) then
                thick=(dz(j)-up_depth(j))/2.+(dz(j+1)-up_depth(j+1))/2.
!                d_z = (zp-up_depth(j))/thick  !Thickness fraction
                d_z = (zp-mid_depth(j))/thick  !Thickness fraction (correct) 
                j1 = j 
                j2 = j+1
                exit        !exit the loop to save time
              endif
            enddo
          endif
        endif

!--- horizontal interpolation (bilinear)
!
!    first horizontal plan
!
      val1=    dx *    dy *A3d(j1,ip+1,kp+1)+ (1.-dx)*    dy *A3d(j1,ip,kp+1)+&
               dx *(1.-dy)*A3d(j1,ip+1,kp)+ (1.-dx)*(1.-dy)*A3d(j1,ip,kp)
!
!    second horizontal plan
!
      val2=    dx *    dy *A3d(j2,ip+1,kp+1)+ (1.-dx)*    dy *A3d(j2,ip,kp+1)+&
               dx *(1.-dy)*A3d(j2,ip+1,kp)+ (1.-dx)*(1.-dy)*A3d(j2,ip,kp)
!
!    Vertical interpolation (linear)
!     
      value= d_z*val2+(1.-d_z)*val1
!
      return
      end
!
!
      REAL FUNCTION value2D(A2d,nm,nn,index,xp,yp)
!
!-----------------------------------------------------------------------
!
!   This routine interpolates linearly  3D arraye at a given point
!   (x,y,z) where (x,y) are in grid units (i,k) and z is in meter
!   (positive downward to keep the depths positives).
!
!-----------------------------------------------------------------------
!
! index can be u,v,w or s(sigmat). One can use s for
! temperature and salinity. Use w for eta.
!
!-----------------------------------------------------------------------
!

      include 'ocean.h'
      real dx,dy,d_z
      character*1 index
      integer nm,n3,j,j1,j2,ip,kp
      real xp,yp,zp,A2d(nm,nn)
      real thick,val1,val2
      
!   find the local coordinates (dx,dy)
!
      if(index.eq.'w'.or.index.eq.'s') then
        ip = int(xp+0.5)
        kp = int(yp+0.5)
        dx= xp-(float(ip)-.5)
        dy= yp-(float(kp)-.5)
      else if (index.eq.'v') then
        ip = int(xp)
        kp = int(yp+0.5)
        dx= xp-float(ip)
        dy= yp-(float(kp)-.5)
      else if (index.eq.'u') then
        ip = int(xp+0.5)
        kp = int(yp)
        dx= xp-(float(ip)-.5)
        dy= yp-float(kp)
      endif
!
      value2D = 0.0
!
!   Find the vertical local coordinate
!

!--- horizontal interpolation (bilinear)
!
!
      val1=    dx *    dy *A2d(ip+1,kp+1)+ (1.-dx)*    dy *A2d(ip,kp+1)+&
              dx *(1.-dy)*A2d(ip+1,kp)+ (1.-dx)*(1.-dy)*A2d(ip,kp)
!
      value2d= val1

      return
      end
!
!
      subroutine output_traj 
      include 'ocean.h'
      integer i,k,j,it
!
      do it = 1, ibuf
        write(51) buf_number(it), (buf_time(it,i),i=1,6), buf_xpo(it),buf_ypo(it),buf_zpo(it)
      enddo
      ibuf = 0
      return
      end
!
!
      FUNCTION gasdev2()
!       use ifport  !for use with ifort (intel compiler)
! Returns a normally distibuted deviate with zero mean and
! unit variance using ran as a source of uniform deviates
! Joel: Modified from Gasdev. Ran has problem
      REAL gasdev2
      INTEGER iset
      REAL fac,gset,rsq,v1,v2 !use ran() intrinsic
      SAVE iset,gset
      DATA iset/0/
      if (iset.eq.0) then
1       v1=2.*rand()-1.
        v2=2.*rand()-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gset=v1*fac
        gasdev2=v2*fac
        iset=1
      else
        gasdev2=gset
        iset=0
      endif
      return
      END
!  (C) Copr. 1986-92 Numerical Recipes Software 2*$.
