      include 'parameter.h'
      character*80 titre,colormap,anumb
      character*25 file,nom
      PARAMETER(MPART=1000000)
      REAL XP(MPART)
      REAL YP(MPART),ZP(MPART)
      REAL xmean,ymean,zmean,x,y,z
      INTEGER icount,i,id,ii,ifirst,ilast
      real depth(m,n)
      character*80 filenom,clon
      integer time(mpart,6)

c 
c---------------------------------------------------------------------
c 
      OPEN(10,FILE= '../Run/OUT/output_particles',form='unformatted',
     >status='old')
      
 
      read(10,end=20) 
     &         i,ibid1,ibid2,ibid3,ibid4,ibid5,ibid6,x,y,z
      ifirst=i

      do ii=1,12000

         ilast=0
         icount=0
 
         Do while(i.gt.ilast)

            icount=icount+1 
            if(icount.gt.Mpart) stop 'mpart not big enough'
            ilast = i
            xp(icount) = x
            yp(icount) = y
            zp(icount) = z
            time(icount,1)=ibid1
            time(icount,2)=ibid2
            time(icount,3)=ibid3
            time(icount,4)=ibid4
            time(icount,5)=ibid5
            time(icount,6)=ibid6

         read(10,end=20) i,ibid1,ibid2,ibid3,ibid4,ibid5,ibid6,x,y,z
c        write(*,*)i,ibid1,ibid2,ibid3,ibid4,ibid5,ibid6,x,y,z      
c         if(mod(i,1000).eq.0) print*,'PARTI=',i,x,y,z      

         endDo
         print*,'icount=',icount
c         if (mod(ii,8) .eq. 0) then
c         if (mod(ii,2) .eq. 0) then   !for Green Grab
         if (mod(ii,1) .eq. 0) then
c         if (mod(ii,10) .eq. 0) then
             write(*,*) 'timestep,ifirst,ilast,time: ',
     &	        ii,ifirst,ilast,time(icount,:)        
            print*,'passe 1'
            call gmt_stuff(time(icount,:),xp,yp,icount)	    
         end if  
   
         ifirst=i

      endDo 
    

c-----------------------------------------------------------------------
20    continue

 
      end
c
c
      subroutine gmt_stuff(time1,xp,yp,icount)

      PARAMETER(MPART=1000000)
      REAL XP(MPART)
      REAL YP(MPART)
      real rlat,rlon 
      character*80 titre2
      
      integer time1(6),icount,ii,igraph
      character*2 atime(6),aa
      character*4 a_annee
      character*4 a     
      
      data igraph/0/ 
      save igraph

      igraph=igraph+1
      write(a,'(i4.4)')igraph
      
      print*,'GMT_stuff called:',igraph

      open(11,file='posi.dat')
      open(12,file='time.dat')      


      write(a_annee,'(i4.4)') time1(6)
      do i=1,5
            write(atime(i),'(i2.2)')time1(i)
            print*,'i,atime(i)=',i,atime(i)
      enddo
c
       titre2=a_annee//'-'//atime(5)//'-'//atime(4)
     > //'    '//atime(3)//':'//atime(2)//':'//atime(1)//'  (GMT)'
c
c       titre2=titre2(1:lenstr(titre2))
       titre2=trim(titre2)
       print*,'titre2=',titre2    
       write(12,'(a)') trim(titre2)



       do ii=1,icount
          call xy_to_ll_NEMO(xp(ii),yp(ii),rlon,rlat)
          write(11,*) rlon,rlat                  
       enddo

      close(11)
      close(12)


      call system('posi.gmt')
c      print*,'trans_to_jpg position.ps '//'p'//a(1:4)//'.jpg'
c      call system('trans_to_jpg position.ps '//'p'//a(1:4)//'.jpg') 

c      call system('convert -density 100 -crop 0x0
      call system('convert -density 100 -trim
     & position.ps '//'p'//a(1:4)//'.gif')
c     & position.ps '//'p'//a(1:4)//'.jpg')

c
c Note use " to make movie
c whirlgif -global -time 20 -o movie.gif p*.gif      
c 

      return
      end
      
