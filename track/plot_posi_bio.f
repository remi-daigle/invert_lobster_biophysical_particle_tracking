c
      include 'parameter.h'
      character*80 titre2,colormap,anumb
      character*25 file,nom
      PARAMETER(MPART=10000000)
      PARAMETER(MSTEP=4000)
      INTEGER IPOINT(MPART)
      INTEGER IPOINT_start(MPART)
      REAL XP(MPART)
      REAL YP(MPART),ZP(MPART)
      integer istage(mpart)
      real *8 stage
      integer time(6)
      REAL xmean,ymean,zmean,x,y,z
      INTEGER icount,i,id
      character*2 atime(6),aa
      character*4 a_annee
c
c-----------------------------------------------------------------------
c
      character*80 filenom,clon
c
c-----------------------------------------------------------------------
c
      ipoint_start=0
c 
      OPEN(10,FILE=
c     &'/home4/tmp/Lobster_IML_2006.bio_output_particles',
c     &'/home4/tmp/Lobster_IML2_1996.bio_output_particles',
c     &'/home4/tmp/Lobster_IML_2004_swim18.bio_output_particles',
c    &'/home4/tmp/Lobster_IML_2006_swim18.bio_output_particles',
     &'../Run/OUT/Test_bio.bio_output_particles',
     &        form='unformatted',status='old')
      
c-----------------------------------------------------------------------
c      write(551)(time(i),i=1,6),BIO_iout
c      do it = 1, BIO_npart
c       if(BIO_alive(it).eq.1) then
c         write(551)BIO_part_id(it),
c     >             BIO_xpo(it),BIO_ypo(it),BIO_zpo(it),
c     >             BIO_stage(it),BIO_N(it)              !ATTENTION real*8
c     >             ,BIO_temp(it)
c     >             ,BIO_N_sum_rem(it)
c       endif

c
       ir=0
       iout=0
       Do  
c
          read(10,end=20) (time(i),i=1,6),imax
          print*,(time(i),i=1,6),imax
          if (imax.gt.mpart) stop 'mpart not big enough'
          do  i=1,imax
              read(10)iiiii,x,y,z,stage
              xp(i) = x
              yp(i) = y
              zp(i) = z
              istage(i)=int(stage)
          enddo
          ir=ir+1
c
c
c        if(mod(ir,2).eq.0) then
        if(mod(ir,1).eq.0) then
c
        call system('rm posi1.dat posi2.dat posi3.dat posi4.dat')
        call system('rm time.dat')
c
        open(11,file='posi1.dat')  !Stage 1 larvae
        open(21,file='posi2.dat')  !Stage 2 larvae
        open(31,file='posi3.dat')  !Stage 3 larvae
        open(41,file='posi4.dat')  !Stage 4 larvae
        open(12,file='time.dat')
       print*,'time=',time
c
c  pour ecrire les titres
c
      write(a_annee,'(i4.4)')time(6)
      do i=1,5
         write(atime(i),'(i2.2)')time(i)
      enddo
c
      titre2=a_annee//'-'//atime(5)//'-'//atime(4)
     > //'    '//atime(3)//':'//atime(2)//':'//atime(1)//'  (GMT)'
      titre2=titre2(1:lenstr(titre2))
c      print*,'titre2='
c        write(12,'(a)') titre2
      titre2=trim(titre2)
      print*,'titre2=',titre2    
      write(12,'(a)') trim(titre2)
        Do i=1,imax,1
            call xy_to_ll_NEMO(xp(i),yp(i),rlon,rlat)
            if(istage(i).eq.1) write(11,*)rlon,rlat
            if(istage(i).eq.2) write(21,*)rlon,rlat
            if(istage(i).eq.3) write(31,*)rlon,rlat
            if(istage(i).eq.4) write(41,*)rlon,rlat
       enddo
       close(11)
       close(21)
       close(31)
       close(41)
       close(12)
c       ----------------------------------------------------------------
c
c GMT postscript
c
            iout=iout+1
            call nb_char(iout,clon)
            write(anumb,'(i4.4)')iout
             call system('posi_bio.gmt')
             call system('convert -density 100 -trim position_bio.ps '//
     >                 'p'//anumb(1:4)//'.gif') 
c       ----------------------------------------------------------------
c
      endif
      endDO
20    continue
c
      end
