c
c-----------------------------------------------------------------------
      open(1,file='part_loc.ll',status='old')
      open(2,file='part_loc.xy')
c   
      DO
        read(1,*,end=999)rlon,rlat
        call ll_to_xy_NEMO(rlon,rlat,xp,yp)
        write(2,'(2(1x,f9.4),2(1x,f9.4))')xp,yp,rlon,rlat
      endDO
999   close(1)
      close(2)
      end
