       implicit none       
       real lats,longs,cornerlat,cornerlong,offsetlat,offsetlong
       character*50 arg,arg2
       integer i
!----------------------------------------------------------------
!  
       call getarg(1,arg)
       read(arg,*) cornerlong
       call getarg(2,arg2)
       read(arg2,*) cornerlat
       open(1,file='/home/rdaigle/track/part_loc.ll')
       open(2,file='/home/rdaigle/track/part_loc_forgrid.ll') 

!----------------------------------------------------------------
!
!       print *, cornerlat,cornerlong

!       random.choice(open('/home/rdaigle/track/part_loc_forgrid.ll').readlines( ))

       DO i=1,150000
        read(2,*)longs,lats
     	write(1,*)cornerlong+longs,cornerlat+lats
       end DO
      close(1)
      close(2)
      end
