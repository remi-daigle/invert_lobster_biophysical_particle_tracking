      integer s,m,h,d,mth,yr,dt
      integer iyear2,imonth2,iday2
      real swim,sv,Kdiff,stage
      integer diel,uzl,lzl
      character*1 modelres,behav
      character*50 behaveparam
!
      CALL get_command_argument(1,behaveparam)
   
!      open(4,file='../Run/bp/behav_param')
      open(4,file='../Run/bp/1_'//behaveparam)
      read(4,*)yr,mth,d,iyear2,imonth2,iday2,h,m,s,swim,sv,Kdiff,behav,uzl,lzl,modelres
!      s = 0
!      m = 0
!      h = 12
!      d = 30
!      mth = 7
!      yr = 2010
!      z = 0 !use this if all at one depth
      dt = 60*60*24*70
      stage=1.
c
c-----------------------------------------------------------------------
      open(1,file='/home/rdaigle/track/part_loc.xy')
      open(2,file='input_particles')
      open(3,file='/home/rdaigle/track/z')
c   

      DO
        read(1,*,end=999)xp,yp,rlon,rlat
      	read(3,*,end=999)z
      	write(2,*)s,m,h+10,d,mth,yr,xp,yp,z,stage,dt
      endDO
999   close(1)
      close(2)
      close(3)
      close(4)
      end
