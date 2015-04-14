      include 'parameter.h'
      character*80 titre,colormap,anumb
      character*25 file,nom
      PARAMETER(MPART=150000)
      PARAMETER(MSTEP=1000)
      INTEGER IPOINT(MPART)
      REAL XP(MPART,MSTEP)
      REAL YP(MPART,MSTEP),ZP(MPART,MSTEP)
      REAL xmean,ymean,zmean,x,y,z,stage
      INTEGER icount,i,id
      real depth(m,n)
      character*180 filenom,clon,archive,filepath
      character*180 bp

      CALL get_command_argument(1,bp)
      open(98,file='../Run/bp/2_'//bp)
      read(98,*)filepath
      filepath=TRIM('tmp/'//filepath)
c
c---------------------------------------------------------------------
c 
      call getarg(1,archive)
      print*,'archive=',archive
c-----------------------------------------------------------------------
      !filenom=archive(1:lenstr(archive))//'.output_particles'
      filenom='../Run/OUT/'//bp
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
