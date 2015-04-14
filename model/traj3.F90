      subroutine particle_input

      include 'ocean.h'
      integer ipart,i,k,j,istep
      real stage, buf_stage
      dimension stage(300),buf_stage(300)
      common /stagedata/ stage, buf_stage
      character*50 behaveparam
      character*180 filenom2

!
      CALL get_command_argument(1,behaveparam)
!          open(66,file='../Run/behav_param3')
!          read(66,*)behaveparam
!          close(66)

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
      filenom2="OUT/"//behaveparam
      open(51,file=filenom2,form='unformatted')
!
      ipart = 1
 98   read(49,*,end=99) (ipart_time(ipart,i),i=1,6), &
                       xpo(ipart),ypo(ipart),zpo(ipart),stage(ipart), &
                       ipart_time_length(ipart)  !In second please
      ipart = ipart + 1
      goto 98
 99   npart=ipart-1
      close(49)
!
      print*,'Number of particles (npart)= ',npart
      !print*,'test stage',stage
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
         do i=5,m-3,5
         do k=5,n-3,5
          if(izet(i,k).gt.0) then
            ipart=ipart+1
!            ipart_time(ipart,:)=tzero(:)
            ipart_time(ipart,:)=time(:)
            xpo(ipart)=real(i)-.5
            ypo(ipart)=real(k)-.5
            zpo(ipart)=2.0   !At one meter
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
      !real Kdiff !m2/s
      real xdiffusion, ydiffusion
      integer iyear1,imonth1,iday1
      integer iyear2,imonth2,iday2
      real swim,sv,Kdiff,tempp,swim_lob
      integer hour1,minute1,second1,rdm
      integer uzl,lzl,lob_uzl,lob_lzl
      character*1 modelres
      character*3 behav
      integer b_year,b_month,b_d,b_h,b_m,b_s,daynight,row,col,rownum
      integer iadcp,adcp_z,astep,kt
      real ADCP_x(5),ADCP_y(5)
      real adcp_u,adcp_v,adcp_w
      real stage, buf_stage
      dimension stage(300),buf_stage(300), stage_old(300)
      common /stagedata/ stage, buf_stage, stage_old
      logical lobster
      character*50 behaveparam
!
      CALL get_command_argument(1,behaveparam)
!-------------------------------------------------------------------------------------------
          open(66,file='../Run/bp/1_'//behaveparam)
          read(66,*)iyear1,imonth1,iday1,iyear2,imonth2,iday2,hour1,minute1,second1,swim,sv,Kdiff,behav,uzl,lzl,modelres
          close(66)
!extract "ADCP" comparable data use "2009	7	10	2009	8	23	0	0	0	0	0	0	a	0	100	h" in behave_table and 43 days in make_input.f
      lobster=.FALSE.
      if(behav.eq.'l0a'.or.behav.eq.'l1a'.or.behav.eq.'l2a'.or.behav.eq.'l3a'.or.behav.eq.'l4a'.or.&
      behav.eq.'l0b'.or.behav.eq.'l1b'.or.behav.eq.'l2b'.or.behav.eq.'l3b'.or.behav.eq.'l4b'.or.&
      behav.eq.'l0c'.or.behav.eq.'l1c'.or.behav.eq.'l2c'.or.behav.eq.'l3c'.or.behav.eq.'l4c'.or.&
      behav.eq.'l0d'.or.behav.eq.'l1d'.or.behav.eq.'l2d'.or.behav.eq.'l3d'.or.behav.eq.'l4d') lobster=.TRUE.
      
	  if(behav.eq.'l5a'.or.behav.eq.'l6a'.or.behav.eq.'l7a'.or.behav.eq.'l8a'.or.&
      behav.eq.'l9a') lobster=.TRUE.

      if(behav.eq.'a')then
          if(modelres.eq."l") then
        	  open(81,file='../ADCP/ADCP_low')
          else
        	  open(81,file='../ADCP/ADCP_high')
          end if
          read(81,*)ADCP_y,ADCP_x
          close(81)
        open(82,file='../ADCP/ADCPdata',POSITION="APPEND")
        do iadcp=1,5
          do adcp_z=1,25
            astep=kstep
            adcp_u = value(u,m,n,ilo,'u',ADCP_x(iadcp),ADCP_y(iadcp),adcp_z)
            adcp_v =  value(v,m,n,ilo,'v',ADCP_x(iadcp),ADCP_y(iadcp),adcp_z)
            adcp_w = -value(w,m,n,ilo,'w',ADCP_x(iadcp),ADCP_y(iadcp),adcp_z)
            print*,'test adcp data',iadcp,adcp_z,kstep,adcp_u,adcp_v,adcp_w
            write(82,*)iadcp,adcp_z,kstep,adcp_u,adcp_v,adcp_w
          enddo
        enddo
        close(82)
      end if      
!------------------------------------------------------------------------------------------- 
!      Kdiff=2. !m2/s
!      Kdiff=0. !m2/s  !no ramdom walk for now
!      Kdiff=5. !m2/s
 !     print*,'Diffusion for particles=',kdiff
!         ------------------------------------------------------------
!
!Remi's behaviour sub-model (swimming is in m/h)

 	  b_year=time(6)		    				!Remi
          b_month=time(5)	    					!Remi
          b_d=time(4)-1		    					!Remi
          b_h=time(3)+1
          b_m=time(2)		    					!Remi
          b_s=time(1)		    					!Remi

!          print*,'swim,sv,Kdiff,behav,lzl,uzl=',swim,sv,Kdiff,behav,lzl,uzl
          daynight=1
          ebbflood=1

!
! daynight

          IF (behav=='d'.AND.b_h>(6-3).AND.b_h<(17-3)) THEN             !local time minus 3 (GMT vs ADT)
          	daynight=1.						!daynight=1 if day
          ELSEIF (behav=='d') THEN
        	daynight=-1.						!daynight=-1 if night
	  END IF 			    				!Remi

!
! ebbflood
          IF (behav=='t'.AND.b_month==7) THEN
                 b_dd=b_d-1
          ELSEIF (behav=='t'.AND.b_month==8) THEN
                 b_dd=b_d+31-1
          ELSEIF (behav=='t'.AND.b_month==9) THEN
                 b_dd=b_d+62-1
          ELSEIF (behav=='t'.AND.b_month==10) THEN
	         b_dd=b_d+92-1
	  ELSEIF (behav=='t') THEN
	         write(*,*)'error: month not compatible with behaviour submodel. month=',b_month
	  END IF
!
!
	  IF (behav=='t'.AND.b_year==2010) THEN
	         b_dh=b_dd*24+7.648+b_h+(b_m/60)+(b_s/3600)+3
	  ELSEIF (behav=='t'.AND.b_year==2009) THEN
	         b_dh=b_dd*24+4.915+b_h+(b_m/60)+(b_s/3600)+3
	  ELSEIF (behav=='t'.AND.b_year==2008) THEN
	         b_dh=b_dd*24+8.065+b_h+(b_m/60)+(b_s/3600)+3
	  ELSEIF (behav=='t') THEN
	         write(*,*)'error: year not compatible with behaviour submodel. year=',b_year
	  END IF
!
          cc=2*3.141592653589793238462643383279502884197169399375/12.42
!          IF (cos(cc*b_dh)>0) THEN
!	  	ebbflood=1.						!ebbflood=1 if flooding
!	  ELSE
!		ebbflood=-1.						!ebbflood=-1 if ebbing
!	  END IF
          ebbflood=cos(cc*b_dh)
!
!


         if(lobster.eqv..TRUE.) then          
		 if(modelres.eq."h") then
		    rownum=final*8-1
		  else
		    rownum=final-1
		  end if
                  if(kstep.gt.9.and.mod(kstep,2).lt.0.5) then
		    open(79,file='../Run/stage/stages'//behaveparam,POSITION="APPEND")
		    write(79,*)stage
		    close(79)
                    print*,'testing append ','true'
                  else
                    print*,'testing append ','false'
                  end if
		  print*,'testing stage',stage(1:5)
         end if
         !print*,'test local time & tidal',b_d,b_h+3,ebbflood
!
!         ------------------------------------------------------------
!
      do ipart = 1, Npart !start individual loop
!
!         ------------------------------------------------------------
!
!! lobster temperature dependant submodel 'l0a'
         if(behav.eq.'l0a'.and.kstep.gt.9) then
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l0b'
         if(behav.eq.'l0b'.and.kstep.gt.9) then
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l1a'
         if(behav.eq.'l1a'.and.kstep.gt.9) then
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l1b'
         if(behav.eq.'l1b'.and.kstep.gt.9) then
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l2a'
         if(behav.eq.'l2a'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l2b'
         if(behav.eq.'l2b'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l3a'
         if(behav.eq.'l3a'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=6
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=9
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l3b'
         if(behav.eq.'l3b'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=6
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=9
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l4a'
         if(behav.eq.'l4a'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l4b'
         if(behav.eq.'l4b'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l0c'
         if(behav.eq.'l0c'.and.kstep.gt.9) then
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l0d'
         if(behav.eq.'l0d'.and.kstep.gt.9) then
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l1c'
         if(behav.eq.'l1c'.and.kstep.gt.9) then
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l1d'
         if(behav.eq.'l1d'.and.kstep.gt.9) then
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l2c'
         if(behav.eq.'l2c'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l2d'
         if(behav.eq.'l2d'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l3c'
         if(behav.eq.'l3c'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=6
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=9
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l3d'
         if(behav.eq.'l3d'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=6
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=9
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l4c'
         if(behav.eq.'l4c'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(0.4*24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

!! lobster temperature dependant submodel 'l4d'
         if(behav.eq.'l4d'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  17
		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=15
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=7
                 lob_lzl=15
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if

		  
		  !! lobster temperature dependant submodel 'l5a'
         if(behav.eq.'l5a'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 		  if(ipart.lt.3) print*,'test ttemp',ipart,tempp

		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=0
                 lob_lzl=1
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if
		  
		  
		  		  !! lobster temperature dependant submodel 'l6a'
         if(behav.eq.'l6a'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 		  if(ipart.lt.3) print*,'test ttemp',ipart,tempp

		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=5
                 lob_lzl=6
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=5
                 lob_lzl=6
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=5
                 lob_lzl=6
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=5
                 lob_lzl=6
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if
		  
		  		  !! lobster temperature dependant submodel 'l7a'
         if(behav.eq.'l7a'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 		  if(ipart.lt.3) print*,'test ttemp',ipart,tempp

		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=10
                 lob_lzl=11
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=10
                 lob_lzl=11
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=10
                 lob_lzl=11
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=10
                 lob_lzl=11
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if
		  
		  		  !! lobster temperature dependant submodel 'l8a'
         if(behav.eq.'l8a'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 		  if(ipart.lt.3) print*,'test ttemp',ipart,tempp

		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=15
                 lob_lzl=16
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=15
                 lob_lzl=16
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=15
                 lob_lzl=16
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=15
                 lob_lzl=16
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if
		  
		  		  !! lobster temperature dependant submodel 'l9a'
         if(behav.eq.'l9a'.and.kstep.gt.9) then
                 IF (b_h>(6-3).AND.b_h<(18-3)) THEN             !local time minus 3 (GMT vs ADT)
          	       daynight=1.						!daynight=1 if day
                 ELSEIF (behav=='d') THEN
        	       daynight=-1.						!daynight=-1 if night
	         END IF 
                 rdm=1
                 if(gasdev2().lt.0) rdm=-1 
		 tempp =  value(temp,m,n,ilo,'s',xpo(ipart),ypo(ipart),zpo(ipart))
		 		  if(ipart.lt.3) print*,'test ttemp',ipart,tempp

		 if(stage(ipart).ge.1.and.stage(ipart).lt.2) then
		 stage(ipart)=stage(ipart)+1/(24*(851*(tempp-0.84)**(-1.91)))
                 lob_uzl=20
                 lob_lzl=21
		 swim_lob=0.1844828107*10*rdm
                 sv=0.0336154396*10
		 elseif(stage(ipart).ge.2.and.stage(ipart).lt.3) then
		 stage(ipart)=stage(ipart)+1/(24*(200*(tempp-4.88)**(-1.47)))
                 lob_uzl=20
                 lob_lzl=21
		 swim_lob=0.088009247*10*rdm
                 sv=0.0351443448*10
		 elseif(stage(ipart).ge.3.and.stage(ipart).lt.4) then
		 stage(ipart)=stage(ipart)+1/(24*(252*(tempp-5.30)**(-1.45)))
                 lob_uzl=20
                 lob_lzl=21
		 swim_lob=0.0751676396*10*rdm
                 sv=0.0289636519*10
		 elseif(stage(ipart).ge.4.and.stage(ipart).lt.5) then
		 stage(ipart)=stage(ipart)+1/(24*(703.5*(tempp)**(-1.26)))
                 lob_uzl=20
                 lob_lzl=21
		 swim_lob=0.3408846614*10*rdm
                 sv=0.1368756067*10
		 elseif(stage(ipart).ge.5) then
		 stage(ipart)=5  !! stage 5 means it's settled!
		 else
		 print*,'if lobster stage loop error',stage(ipart);stage(ipart)=stage_old(ipart)
		 end if
          end if
		  
!  store old stage
		  
		  stage_old=stage
!! enforce lobster depth limits
          if(zpo(ipart).lt.lob_uzl) swim_lob=abs(swim_lob)
          if(zpo(ipart).gt.lob_lzl) swim_lob=-abs(swim_lob)

! set swimming velocity
        rdm=1
        if(behav.eq.'r'.and.gasdev2().lt.0) rdm=-1
        if(lobster.eqv..FALSE.) swimming=(swim*daynight*ebbflood*rdm+sv*gasdev2())*dt/1000.
        if(lobster.eqv..TRUE.) swimming=(swim_lob+sv*gasdev2())*dt/1000.
        if(ipart.eq.1) print*,'test swimming=',swimming,'swim=',swim_lob,'zpo(ipart)=',zpo(ipart)!,tempp,floor(stage(ipart))
!        if(ipart.eq.1) print*,'test swimming=',swimming,swim,daynight,ebbflood,rdm,sv,dt,gasdev2()
        if(behav.eq.'n') swimming=0
        i=int(xpo(ipart))+1
        k=int(ypo(ipart))+1
        kstep_diff = kstep - istart_part(ipart)
        if(kstep_diff.eq.0) then

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
          up =  value(u,m,n,ilo,'u',xpo(ipart),ypo(ipart),zpo(ipart))
          vp =  value(v,m,n,ilo,'v',xpo(ipart),ypo(ipart),zpo(ipart))
          wp = -value(w,m,n,ilo,'w',xpo(ipart),ypo(ipart),zpo(ipart))
! WARNING: wp=0.
           wp=0.
!

          xk1(ipart  ) = dt2*up/dlx(i,k)
          yk1(ipart  ) = dt2*vp/dly(i,k)
          zk1(ipart  ) = dt2*wp
!
        else if (kstep_diff.gt.0.and.kstep_diff.le.ipart_step(ipart)) then
!
          if(mod(kstep_diff,2).eq.0) then
!
!         Avanced time: Computation of the k4s then the k1s (then new time)
!         and the new positions
!
          up =  value(u,m,n,ilo,'u',xpo(ipart)+xk3(ipart),ypo(ipart)+yk3(ipart),zpo(ipart)+zk3(ipart))
          vp =  value(v,m,n,ilo,'v',xpo(ipart)+xk3(ipart),ypo(ipart)+yk3(ipart),zpo(ipart)+zk3(ipart))
          wp = -value(w,m,n,ilo,'w',xpo(ipart)+xk3(ipart),ypo(ipart)+yk3(ipart),zpo(ipart)+zk3(ipart))
!
! WARNING: wp=0.
           wp=0.
            xk4(ipart) = dt2*up/dlx(i,k)
            yk4(ipart) = dt2*vp/dly(i,k)
            zk4(ipart) = dt2*wp
!
            xp_temp = xpo(ipart)+( xk1(ipart)+2.*xk2(ipart)+2.*xk3(ipart)+xk4(ipart))/6.
            !Random walk
            xdiffusion=Gasdev2()*(1./dlx(i,k))*(2.*Kdiff*dt2)**.5
            xp_temp=xp_temp+xdiffusion
!
            yp_temp = ypo(ipart)+( yk1(ipart)+2.*yk2(ipart)+2.*yk3(ipart)+yk4(ipart))/6.
            !Random walk
            ydiffusion=Gasdev2()*(1./dly(i,k))*(2.*Kdiff*dt2)**.5
            yp_temp=yp_temp+ydiffusion
!
!            zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.+2.*swimming
!-----------------------------------------------------------------------
!            enforce depth limits uzl and lzl

          if(zpo(ipart).gt.(lzl-5)) then
             if(zpo(ipart).gt.lzl)then
                zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.-2.*abs(swimming)
             !else if(swimming.gt.(0.)) then
                !zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.+(zptemp-lzl)/5*2.*swimming
                !zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.+2.*swimming
             else
                zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.+2.*swimming
             end if
          else if(zpo(ipart).lt.(uzl+5)) then
             if(zpo(ipart).lt.uzl)then
                zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.+2.*abs(swimming)
             !else if(swimming.lt.(0.)) then
                !zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.+(zptemp-uzl)/5*2.*swimming
                !zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.+2.*swimming
             else
                zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.+2.*swimming
             end if
          else
             zp_temp = zpo(ipart)+( zk1(ipart)+2.*zk2(ipart)+2.*zk3(ipart)+zk4(ipart))/6.+2.*swimming
          end if
          !if(ipart.eq.1) print*,'test uzl and lzl',uzl,lzl,zp_temp,zpo(ipart),swimming,zp_temp-zpo(ipart)

!-----------------------------------------------------------------------		  
		  !! enforce lobster depth limits
          if(zp_temp.lt.lob_uzl) zp_temp=lob_uzl
          if(zp_temp.gt.lob_lzl) zp_temp=lob_lzl
          if(ipart.eq.1) print*,'test lob_uzl and lzl',lob_uzl,lob_lzl,zp_temp,zpo(ipart)

!-----------------------------------------------------------------------
!            move only particles if underwater
          if(zp_temp.lt.0) then
               zp_temp = 0.
          endif
!-----------------------------------------------------------------------
!            bottom is hard
          real_depth=real(izet(int(xp_temp)+1,int(yp_temp)+1))
          if(zp_temp.gt.real_depth) then
               zp_temp = real_depth-0.0001
          endif
!-----------------------------------------------------------------------
!            move only particles if wet
          if(real_depth.gt.0.0.and.zp_temp.lt.real_depth) then
               xpo(ipart) = xp_temp
               ypo(ipart) = yp_temp
               zpo(ipart) = zp_temp
          else
               if(ipart.eq.1) print*,'particle not wet'
          endif


!-----------------------------------------------------------------------
            up =  value(u,m,n,ilo,'u',xpo(ipart),ypo(ipart),zpo(ipart))
            vp =  value(v,m,n,ilo,'v',xpo(ipart),ypo(ipart),zpo(ipart))
            wp = -value(w,m,n,ilo,'w',xpo(ipart),ypo(ipart),zpo(ipart))
!
! WARNING: wp=0.
           wp=0.
!
            xk1(ipart) = dt2*up/dlx(i,k)
            yk1(ipart) = dt2*vp/dly(i,k)
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
            up =  value(u,m,n,ilo,'u',xpo(ipart)+0.5*xk1(ipart),ypo(ipart)+0.5*yk1(ipart),zpo(ipart)+0.5*zk1(ipart))
            vp =  value(v,m,n,ilo,'v',xpo(ipart)+0.5*xk1(ipart),ypo(ipart)+0.5*yk1(ipart),zpo(ipart)+0.5*zk1(ipart))
            wp = -value(w,m,n,ilo,'w',xpo(ipart)+0.5*xk1(ipart),ypo(ipart)+0.5*yk1(ipart),zpo(ipart)+0.5*zk1(ipart))
!
! WARNING: wp=0.
           wp=0.
!
            xk2(ipart) = dt2*up/dlx(i,k)
            yk2(ipart) = dt2*vp/dly(i,k)
            zk2(ipart) = dt2*wp
!
            up =  value(u,m,n,ilo,'u',xpo(ipart)+0.5*xk2(ipart),ypo(ipart)+0.5*yk2(ipart),zpo(ipart)+0.5*zk2(ipart))
            vp =  value(v,m,n,ilo,'v',xpo(ipart)+0.5*xk2(ipart),ypo(ipart)+0.5*yk2(ipart),zpo(ipart)+0.5*zk2(ipart))
            wp = -value(w,m,n,ilo,'w',xpo(ipart)+0.5*xk2(ipart),ypo(ipart)+0.5*yk2(ipart),zpo(ipart)+0.5*zk2(ipart))
!
! WARNING: wp=0.
           wp=0.
!
            xk3(ipart) = dt2*up/dlx(i,k)
            yk3(ipart) = dt2*vp/dly(i,k)
            zk3(ipart) = dt2*wp
!
          endif
        endif
      enddo
          if(lobster.eqv..TRUE.) then
	     ! open(79,file='../Run/stages_temp')
	     ! write(79,*)stage
	     ! close(79)
          endif
      return
      end

!
!
      REAL FUNCTION value(A3d,nm,nn,nl,index,xp,yp,zp)
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
      real xp,yp,zp,A3d(nm,nn,nl)
      real thick,val1,val2
      
!   find the local coordinates (dx,dy)
!
      if(index.eq.'w'.or.index.eq.'s') then
        ip = int(xp+0.5)
        kp = int(yp+0.5)
        dx= xp-(float(ip)-.5)
        dy= yp-(float(kp)-.5)
      !else if (index.eq.'v') then
      else if (index.eq.'u') then  !To be verified  -----> Verified by Joel!! May 13, 2011
        ip = int(xp)
        kp = int(yp+0.5)
        dx= xp-float(ip)
        dy= yp-(float(kp)-.5)
      !else if (index.eq.'u') then
      else if (index.eq.'v') then  !To be verified  -----> Verified by Joel!!, May 13, 2011
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
      val1=    dx *    dy *A3d(ip+1,kp+1,j1)+ (1.-dx)*    dy *A3d(ip,kp+1,j1)+&
               dx *(1.-dy)*A3d(ip+1,kp,j1)+ (1.-dx)*(1.-dy)*A3d(ip,kp,j1)
!
!    second horizontal plan
!
      val2=    dx *    dy *A3d(ip+1,kp+1,j2)+ (1.-dx)*    dy *A3d(ip,kp+1,j2)+&
               dx *(1.-dy)*A3d(ip+1,kp,j2)+ (1.-dx)*(1.-dy)*A3d(ip,kp,j2)
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
      !else if (index.eq.'v') then
      else if (index.eq.'u') then
        ip = int(xp)
        kp = int(yp+0.5)
        dx= xp-float(ip)
        dy= yp-(float(kp)-.5)
      !else if (index.eq.'u') then
      else if (index.eq.'v') then
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
