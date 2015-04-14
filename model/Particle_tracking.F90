!--------------------------------------------------------
      USE netcdf
      implicit none    
      include 'ocean.h'
      integer  nDims, nVars, nGlobalAtts, unlimdimid
      integer fieldId,ncid,lonId,latId,timeId
      integer uid, vid, wid,Tid,Sid
      integer, allocatable :: dim(:), Time_stamp(:)
      integer st,i,j,k,it,it1,it2,layer,kd,kd_ini,ii
!
      real, allocatable :: lon(:,:),lat(:,:)
      real*8, allocatable :: e1v(:,:),e2u(:,:)
      real th,tt
      real, allocatable :: gdepw(:),gdept(:),e3w(:),e3t(:) 
      integer istep
      integer mpass,npass,ilopass   !Dimension for arrays
      integer nb_time !number of ouput
      character*20, allocatable :: Dim_name(:)
      character*80  chaine,command,clon
      integer ndm(12)
      data ndm/31,28,31,30,31,30,31,31,30,31,30,31/
      integer iyear1,imonth1,iday1
      integer iyear2,imonth2,iday2
      real swim,sv,Kdiff
      integer hour1,minute1,second1
      integer uzl,lzl
      character*1 modelres
      character*3 behav
      integer istart,iend,ir,layer1,layer2,layer3,ldown
      integer nloop,mloop
      real thick,pdz
      real time_between_frames
      integer lenstr
      integer time_in(6)
      logical lobster
      character*50 behaveparam

!
      CALL get_command_argument(1,behaveparam)
!-------------------------------------------------------------------------------------------
!
      open(99,file='../Run/bp/1_'//behaveparam)
!      open(99,file='../Run/behav_param')
      read(99,*)iyear1,imonth1,iday1,iyear2,imonth2,iday2,hour1,minute1,second1,swim,sv,Kdiff,behav,uzl,lzl,modelres
      print*,'iyear1,imonth1,iday1=',iyear1,imonth1,iday1,hour1,minute1,second1
      print*,'iyear2,imonth2,iday2=',iyear2,imonth2,iday2
      print*,'swim,sv,Kdiff,behav,uzl,lzl,modelres=',swim,sv,Kdiff,behav,uzl,lzl,modelres
      close(99)
!      ifile=100
!
!-----------------------------------------------------------------------
! initialisation
!
      call zeros !initialize arrays to zero
!
      if(modelres.eq."l") time_between_frames=24*3600.  !we are assuming daily output from model. Results won't be accurate otherwise
      if(modelres.eq."h") time_between_frames=3*3600.
      initial=1   !first kstep of the run
      dt=3600.
      kstep=-1
      nloop=nint(time_between_frames/dt) 
      print*,"nloop=", nloop
!
!
!      open(1,file='data/depth.dat',status='old')
!      do i=1,m
!         read(1,*) (izet(i,j),j=1,n)
!      enddo
!      close(1)

      if(modelres.eq."l") then
      	open(1,file='data/depth.dat',status='old')
      else
      	open(1,file='../HighRes/depth_for_Remi.dat',status='old')
      end if 
      do i=1,m
         read(1,*) (rzet(i,j),j=1,n)
         do j=1,n
              izet(i,j)=nint(rzet(i,j))
         enddo
      enddo
      close(1)
!
!      To fix the problem with the date in opa
       if(modelres.eq."l") then
       	print*,"Warning: cday2 is called with a reference day of April 30, 2005"
        call cday2(30,4,2005,kd_ini)
       end if
       if(modelres.eq."h".AND.iyear1.eq.2008) then
       	print*,"Warning: cday2 is called with a reference day of June 30, 2008"
        call cday2(30,6,2008,kd_ini)
       end if
       if(modelres.eq."h".AND.iyear1.eq.2009) then
       	print*,"Warning: cday2 is called with a reference day of June 30, 2009"
        call cday2(30,6,2009,kd_ini)
       end if
       if(modelres.eq."h".AND.iyear1.eq.2010) then
       	print*,"Warning: cday2 is called with a reference day of June 30, 2010"
        call cday2(30,6,2010,kd_ini)
       end if
       if(modelres.eq."h".AND.iyear1.eq.2011) then
       	print*,"Warning: cday2 is called with a reference day of June 30, 2011"
        call cday2(30,6,2011,kd_ini)
       end if
       print*,"kd_ini=",kd_ini
!
!get   the first and last record to average
       call cday2(iday1,imonth1,iyear1,istart)
       istart=istart-kd_ini       
!
       call cday2(iday2,imonth2,iyear2,iend)
       iend=iend-kd_ini
!
       
!
       final= (iend-istart)*time_between_frames/dt
       print*,"istart=",istart
       print*,"iend=",iend
       print*,'final=',final
!-------------------------------------------------------------------------------------------
!   Open NETCDF file
        if(modelres.eq."l") st=nf90_open('/globalscratch/rdaigle/L_2012_M2_ave_TSUV.nc',nf90_share,ncid)
        if(modelres.eq."h".AND.iyear1.eq.2008) st=nf90_open('../HighRes/R2008s_ave_TSUV.nc',nf90_share,ncid)
        if(modelres.eq."h".AND.iyear1.eq.2009) st=nf90_open('../HighRes/R2009s_ave_TSUV.nc',nf90_share,ncid)
        if(modelres.eq."h".AND.iyear1.eq.2010) st=nf90_open('../HighRes/R2010s_ave_TSUV.nc',nf90_share,ncid)
        if(modelres.eq."h".AND.iyear1.eq.2011) st=nf90_open('../HighRes/R2010s_ave_TSUV.nc',nf90_share,ncid)
!-----------------------------------------------------------------------
      st = nf90_inquire(ncid, nDims, nVars, nGlobalAtts, unlimdimid)
      if(st.ne.0) stop 'Problems opening netcdf file'
      print*,"st=",st
      print*,"nDims, nVars, nGlobalAtts, unlimdimid=",&
      nDims, nVars, nGlobalAtts, unlimdimid
      allocate (dim(nDims), Dim_name(nDims))
!-----------------------------------------------------------------------
! Getting array dimensions and compare with ddeclaration in ocean.h to see if they match
!
      mpass=0;npass=0;ilopass=0 !Initialisation of dimensions for 3D field
      nb_time=0
      do i=1,nDims
          st=nf90_inquire_dimension(ncid,i,Dim_name(i),dim(i))
           print*,'i,Dim_name(i),dim(i)=',i,Dim_name(i),dim(i)
          if(Dim_name(i).eq.'x') mpass=dim(i)  !dimensions are backward in netcdf
          if(Dim_name(i).eq.'y') npass=dim(i) 
          if(Dim_name(i).eq.'depthu') ilopass=dim(i) 
          if(Dim_name(i).eq.'time_counter') nb_time=dim(i) 
      enddo
      print*,'mpass,npass,ilopass=',mpass,npass,ilopass
      if (m.eq.0.or.n.eq.0.or.ilo.eq.0) stop 'Problem with m,n,ilo'
      if (mpass.ne.m.or.npass.ne.n.or.ilopass.ne.ilo) stop 'Problem with m,n,ilo(2)'
      if (nb_time.eq.0) stop 'Problem with time_counter'
      print*,'Number of outputs in file: (nb_time)=',nb_time
!
!-----------------------------------------------------------------------
!
!  get the longitudes and latitudes
      st=nf90_inq_varid(ncid,"nav_lon",lonId)  
      st=nf90_inq_varid(ncid,"nav_lat",latId)
      write(*,*)"nav_lon nav_lat ids",lonId,latId
!
      allocate(lon(m,n),lat(m,n))
      allocate(e1v(m,n),e2u(m,n))
      st=nf90_get_var(ncid,lonId,lon)
      st=nf90_get_var(ncid,latId,lat)
!
!-------------------------------------------------------------------------------------------
!  Get cell dx and dy
      call Get_dx_dy(e1v,e2u,m,n)

      dlx=e1v !Array used in trajectory
      dly=e2u !Array used in trajectory
!
!-------------------------------------------------------------------------------------------
! get thickness of layers
       allocate (gdepw(ilo),gdept(ilo),e3w(ilo),e3t(ilo))
       call gdep (ilo,gdepw,gdept,e3w,e3t)
       !pd=e3t  !calculated below
       dz(1:ilo-1)=gdepw(2:ilo)
       dz(ilo)=gdepw(ilo)+e3w(ilo)
!
!     vertikale gittergroessen bestimmen
!
      dd(1) = dz(1)
      pd(1) = dd(1)
      up_depth(1) = 0.
      mid_depth(1)=dz(1)/2. !Joel
        do j = 2,ilo
         dd(j) = dz(j)-dz(j-1)
         pd(j) = dd(j)
         up_depth(j) = dz(j-1)          !Upper interface depth (positive)
         mid_depth(j)=(dz(j)+dz(j-1))/2.   !Joel
        enddo
!
!
      print*,'up_depth=',up_depth
      print*,'mid_depth=',mid_depth
!
!-------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------
! Read in nlayer
      if(modelres.eq."l") then
      	open(11,file='../Run/data/bathy_level2.dat', status='old')
      else
      	open(11,file='../HighRes/bathy_level_for_Remi.dat', status='old')
      end if
      do i=1,m
          read(11,*)( nlayer(i,j),j=1,n)
      enddo
      close(11)
!-----------------------------------------------------------------------
!
!  get the times
      st=nf90_inq_varid(ncid,"ndastp",TimeId)  
      write(*,*)"Time ids",TimeId
      allocate(Time_stamp(nb_time))
      st=nf90_get_var(ncid,TimeId,Time_stamp)
      !print*,'Time_stamp=', Time_stamp
!-----------------------------------------------------------------------
!Get the id for u
      st=nf90_inq_varid(ncid,"vozocrtx",uId)
      write(*,*)"3d field Id for u",uId
     
!Get the id for v
      st=nf90_inq_varid(ncid,"vomecrty",vId)
      write(*,*)"3d field Id for v",vId
     
!Get the id for w
      st=nf90_inq_varid(ncid,"vovecrtz",wId)
      write(*,*)"3d field Id for w",wId
     
!Get the id for temp
      st=nf90_inq_varid(ncid,"votemper",TId)
      write(*,*)"3d field Id for temp",TId
     
!Get the id for salt
      st=nf90_inq_varid(ncid,"vosaline",SId)
      write(*,*)"3d field Id for salt",SId

      

      if(modelres.eq."h") then
         mloop=8
      else
         mloop=1
      endif

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! MAIN LOOP
!
      DO it=istart,iend-1    !Main loop in time


!
!          get right time
!           if(modelres.eq."h") then
!            kd=int(kd_ini+floor(it/8.))
!            print*,'kd',kd
!           else
            kd=kd_ini+it ! Assuming daily output
!           end if
           call dmy2(time_in(4),time_in(5),time_in(6),kd)
           time_in(3)=hour1
           time_in(2)=minute1
           time_in(1)=second1
           write(*,'(a,6(1x,i4))')'time_in: year, month, day, hour, min, sec = ',(time_in(ii),ii=6,1,-1)
!intermediate time loop
      DO it1=1,mloop         
          if(it.eq.istart.and.it1.eq.1) then
               tzero=time_in !array
               time=time_in !array
               call particle_input
               lobster=.FALSE.
               if(behav.eq.'l0a'.or.behav.eq.'l1a'.or.behav.eq.'l2a'.or.behav.eq.'l3a'.or.behav.eq.'l4a'.or.&
               behav.eq.'l0b'.or.behav.eq.'l1b'.or.behav.eq.'l2b'.or.behav.eq.'l3b'.or.behav.eq.'l4b') lobster=.TRUE. 
          endif  
!     Output file 
!
           st=nf90_get_var(ncid,uId,u1,start=(/1,1,1,(((it-1)*mloop)+it1)/)) 
           st=nf90_get_var(ncid,vId,v1,start=(/1,1,1,(((it-1)*mloop)+it1)/)) 
           st=nf90_get_var(ncid,wId,w1,start=(/1,1,1,(((it-1)*mloop)+it1)/)) 
           st=nf90_get_var(ncid,TId,temp1,start=(/1,1,1,(((it-1)*mloop)+it1)/)) 
           st=nf90_get_var(ncid,SId,salt1,start=(/1,1,1,(((it-1)*mloop)+it1)/)) 
           st=nf90_get_var(ncid,uId,u2,start=(/1,1,1,(((it-1)*mloop)+it1)+1/)) 
           st=nf90_get_var(ncid,vId,v2,start=(/1,1,1,(((it-1)*mloop)+it1)+1/)) 
           st=nf90_get_var(ncid,wId,w2,start=(/1,1,1,(((it-1)*mloop)+it1)+1/)) 
           st=nf90_get_var(ncid,TId,temp2,start=(/1,1,1,(((it-1)*mloop)+it1)+1/)) 
           st=nf90_get_var(ncid,SId,salt2,start=(/1,1,1,(((it-1)*mloop)+it1)+1/))
           !print*,'test mloop',(((it-1)*mloop)+it1)

!-------------------------------------------------------------------------------------------
!small loop
!-------------------------------------------------------------------------------------------
       do it2=0,nloop-1
          print*,'it2=',it2
!
           kstep=kstep+1
           call update(kstep)  !updating time array
           write(*,'(a,6(1x,i4))')'updated time: year, month, day, hour, min, sec = ',(time(ii),ii=6,1,-1)
!
           u=u1+it2*(u2-u1)/nloop              !Arrays manipulation, linear interpolation
           v=v1+it2*(v2-v1)/nloop              !Arrays manipulation, linear interpolation
           w=w1+it2*(w2-w1)/nloop              !Arrays manipulation, linear interpolation
           temp=temp1+it2*(temp2-temp1)/nloop  !Arrays manipulation, linear interpolation
           salt=salt1+it2*(salt2-salt1)/nloop  !Arrays manipulation, linear interpolation
!
                 i=106;j=160;k=1
                 !print*,'u,v,w,t,s at j,i,k=',j,i,k
                 !write(*,'(2(1x,f10.6),1x,f12.10,2(1x,f10.6))')u(i,j,k),v(i,j,k),w(i,j,k),temp(i,j,k),salt(i,j,k)
!-------------------------------------------------------------------------------------------
       call trajectory
!
       enddo   !Do loop on it2

       enddo !end of intermediate loop it1
!-----------------------------------------------------------------------
      endDO   !Of MAIN LOOP
      call output_traj   !Empty particle position buffer
!-----------------------------------------------------------------------

      close(22)
      end
  
!--------------------------------------------------------
      subroutine Get_dx_dy(e1v,e2u,mpass,npass)
      USE netcdf
      implicit none    
      integer  nDims, nVars, nGlobalAtts, unlimdimid
      integer fieldId,ncid,ncid2,e1vId,e2uId,timeId
      integer, allocatable :: dim(:), Time_stamp(:),bathy_level2(:,:)
      integer st,st2,i,j,k,it,layer
      integer nfstat,var1,var2,var3,var4,var5
!
      real, allocatable :: field_3D(:,:,:)
      integer m,n,ilo   !Dimension for arrays
      integer mpass,npass
      real*8 e1v(mpass,npass),e2u(mpass,npass)
      integer nb_time !number of ouput
!
      character*20, allocatable :: Dim_name(:)
      character*20 field_name  !Variable to plot from netcdf file.
      character*4  clon !String for plot file output
      real rmin,rmax,delta
      character *80 command
      integer iyear1,imonth1,iday1
      integer iyear2,imonth2,iday2
      real swim,sv,Kdiff
      integer hour1,minute1,second1
      integer uzl,lzl
      character*1 modelres,behav
      character*50 behaveparam

!
      CALL get_command_argument(1,behaveparam)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Open NETCDF file
      open(99,file='../Run/bp/1_'//behaveparam)
      read(99,*)iyear1,imonth1,iday1,iyear2,imonth2,iday2,hour1,minute1,second1,swim,sv,Kdiff,behav,uzl,lzl,modelres
      close(99)
      if(modelres.eq."l") then
      	st=nf90_open('../Run/data/coordinates.nc',nf90_share,ncid)
      else
      	st=nf90_open('../HighRes/coordinates.nc',nf90_share,ncid)
      end if
!-----------------------------------------------------------------------
      st = nf90_inquire(ncid, nDims, nVars, nGlobalAtts, unlimdimid)
      if(st.ne.0) stop 'Problems opening netcdf file (coordinates)'
      print*,"st=",st
      print*,"nDims, nVars, nGlobalAtts, unlimdimid=",&
      nDims, nVars, nGlobalAtts, unlimdimid
      allocate (dim(nDims), Dim_name(nDims))
!-----------------------------------------------------------------------
      m=0;n=0;ilo=0 !Initialisation of dimensions for 3D field
      nb_time=0
      do i=1,nDims
          st=nf90_inquire_dimension(ncid,i,Dim_name(i),dim(i))
           print*,'i,Dim_name(i),dim(i)=',i,Dim_name(i),dim(i)
          if(Dim_name(i).eq.'x') m=dim(i)  !dimensions are backward in netcdf
          if(Dim_name(i).eq.'y') n=dim(i) 
          if(Dim_name(i).eq.'z') ilo=dim(i) 
          if(Dim_name(i).eq.'time') nb_time=dim(i) 
      enddo
      print*,'m,n,ilo=',m,n,ilo
      if (m.eq.0.or.n.eq.0.or.ilo.eq.0) stop 'Problem with m,n,ilo'
      if (nb_time.eq.0) stop 'Problem with time_counter'
!      print*,'Number of outputs in file: (nb_time)=',nb_time
!
      if(m.ne.mpass.or.n.ne.npass) stop 'Dimensions dont macth'
!
!-----------------------------------------------------------------------
!
!  get the  dx dy
      st=nf90_inq_varid(ncid,"e1v",e1vId)  
      st=nf90_inq_varid(ncid,"e2u",e2uId)
!      write(*,*)"e1v e2u ids",e1vId,e2uId
!
      st=nf90_get_var(ncid,e1vId,e1v)
      st=nf90_get_var(ncid,e2uId,e2u)
!      write(*,*) 'e1v(1,1),e2u(1,1)=',e1v(1,1),e2u(1,1)
!      write(*,*) 'e1v(20,20),e2u(20,20)=',e1v(20,20),e2u(20,20)
!      write(*,*) 'e1v(40,40),e2u(40,40)=',e1v(40,40),e2u(40,40)
!      write(*,*) 'e1v(80,80),e2u(80,80)=',e1v(80,80),e2u(80,80)
!      write(*,*) 'e1v(160,160),e2u(160,160)=',e1v(160,160),e2u(160,160)
!
      nfstat = nf90_close(ncid)
!-----------------------------------------------------------------------
      return
      end
  
       subroutine gdep (jpk,gdepw,gdept,e3w,e3t)
       integer jpk
!       parameter (jpk=46)
       real :: gdepw(jpk),gdept(jpk),e3w(jpk),e3t(jpk) 
!
!-------------------------------------------------------------------------------------------
! get the OPA parameter
!
       ppkth=23.56;
       ppacr=9.000
       ppdzmin=6.0   !Sopa definition
       pphmax=5750
       zkth = ppkth   
       zacr = ppacr
       zdzmin = ppdzmin
       zhmax = pphmax
!
        za1 = ( ppdzmin - pphmax / (jpk-1) ) / ( tanh((1-ppkth)/ppacr) - ppacr/(jpk-1)*  (  log( cosh( (jpk - ppkth) / ppacr) )- &
		log( cosh( ( 1  - ppkth) / ppacr) )  )  );

         za0  = ppdzmin - za1 * tanh( (1-ppkth) / ppacr );

         zsur = - za0 - za1 * ppacr * log( cosh( (1-ppkth) / ppacr )  );

         
         
         
        !  print*,'jk,gdept(jk),gdepw(jk),e3t(jk),e3w(jk))'
        do jk = 1,jpk
            zw =  jk ;
            zt =  jk  + 0.5;
            gdepw(jk) = ( zsur + za0 * zw + za1 * zacr *log( cosh( (zw-zkth)/zacr ) )  );
            if(jk.eq.1) gdepw(jk) = 0.e0
            gdept(jk) = ( zsur + za0 * zt + za1 * zacr *log( cosh( (zt-zkth)/zacr ) )  );
            e3w  (jk) =          za0      + za1        *tanh(      (zw-zkth)/zacr   );
            e3t  (jk) =          za0      + za1        *tanh(      (zt-zkth)/zacr   );
        !    print*,jk,gdept(jk),gdepw(jk),e3t(jk),e3w(jk)
        enddo
!-------------------------------------------------------------------------------------------
        return
        end
