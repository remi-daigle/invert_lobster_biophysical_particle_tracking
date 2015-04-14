      include 'parameter.h'
      character*80 titre,colormap,anumb
      character*25 file,nom
      PARAMETER(MPART=150000)
      PARAMETER(MSTEP=500)
      INTEGER IPOINT(MPART)
      REAL XP(MPART,MSTEP)
      REAL YP(MPART,MSTEP),ZP(MPART,MSTEP)
      REAL xmean,ymean,zmean,x,y,z
      INTEGER icount,i,id
      real depth(m,n)
      character*180 filenom,clon,archive,filepath
      real cornerlat,cornerlong
      character*50 arg,arg2,underscore
      open(98,file='../Run/behav_param')
      read(98,*)filepath
      print*,filepath
c
c---------------------------------------------------------------------
c 
      call getarg(1,arg)
      read(arg,*) cornerlong
      call getarg(2,arg2)
      read(arg2,*) cornerlat
      underscore='_'
      print*,arg,arg2,TRIM('tmp/'//filepath)
      filepath=TRIM('tmp/'//filepath//'_'//arg//'_'//arg2)
c-----------------------------------------------------------------------
      !filenom=archive(1:lenstr(archive))//'.output_particles'
      filenom='../Run/OUT/output_particles'
      OPEN(10,FILE=filenom,form='unformatted',status='old')
      
      do ii = 1, mpart
        ipoint(ii) = 0
      enddo

      imax=0
      Do 
          read(10,end=20) i,ibid1,ibid2,ibid3,ibid4,ibid5,ibid6,x,y,z
c          print*,i,ipoint(i)
c          print*, i,ibid1,ibid2,ibid3,ibid4,ibid5,ibid6,x,y,z
          if(i.eq.105) print*,ibid1,ibid2,ibid3,ibid4,ibid5,ibid6
          if(i.gt.imax)imax=i
          ipoint(i) = ipoint(i) + 1
          if(ipoint(i).gt.mstep) stop 'Probleme: mstep'
          xp(i,ipoint(i)) = x
          yp(i,ipoint(i)) = y
          zp(i,ipoint(i)) = z
      endDO
c-----------------------------------------------------------------------
20    continue
      print*,'imax=',imax
      print*,'ipoint(1)=',ipoint(1)
c
      Do i=1,imax,1
        call nb_char(i,anumb)
        open(11,file=TRIM(filepath)//'/track'//anumb(1:lenstr(anumb)))
        do iout=1,ipoint(i),1
            call xy_to_ll_NEMO(xp(i,iout),yp(i,iout),rlon,rlat)
            write(11,*)rlon,rlat,zp(i,iout)
       enddo
       close(11)
      endDO
      close(98)
      close(10)        
542   continue
*
      end
c
c
