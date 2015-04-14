c
c
      integer function lenstr( s )
      integer i,total
      character s*(*)
        total = len( s )
        do 100 i = len( s ),1,-1
        if ( s(i:i).ne.' ' ) then
           lenstr = i
           return
        endif
100     continue
        lenstr = 0
      end
c
c
c
      subroutine nb_char(nb,a)
      character*(*) a
      integer n(200)
c
      nb_back=nb
      nb=abs(nb)
c
      do 1 i=1,len(a)
      a(i:i) = ' '
      n(i) = 0
    1 continue
c
c check length of number
c
      if(nb.lt.1000000000) l=9
      if(nb.lt.100000000) l=8
      if(nb.lt.10000000) l=7
      if(nb.lt.1000000) l=6
      if(nb.lt.100000) l=5
      if(nb.lt.10000) l=4
      if(nb.lt.1000) l=3
      if(nb.lt.100) l=2
      if(nb.lt.10) l=1
c
      do 5,i=1,l
         n(i) =mod(nb/10**(i-1),10)
5     continue
c
      do 10 i=l,1,-1
         a(i:i) = char(n(l-i+1)+48)
   10 continue
c
c
      if (nb_back.lt.0.0) then
          l=l+1
          do 51,i=l,2,-1
             a(i:i)=a(i-1:i-1)
51        continue
          a(1:1)='-'
      end if
c
      nb=nb_back
c
      return
      end
c
c
      subroutine rb_char(rb,a,nd)
c pour la longeur des nombres reels
c
c     nd=nombre de decimale voulues
c
      character*(*) a
      character *80 b
      integer n(200)
c
c      print*,'lon de a=', len(a)
      rb_back=rb
      rb=abs(rb)
      r_entier=aint(rb)
      r_deci=rb-r_entier
      nb_deci=nint(r_deci*10**nd)
      nb=int(rb)
      if(nb_deci.eq.10.and.nd.eq.1) nb=nb+1
      if(nd.eq.0) nb=nint(rb)
c
      do 1 i=1,len(a)
      a(i:i) = ' '
      n(i) = 0
    1 continue
c
      do 2,i=1,80
      b(i:i)=' '
2     continue
c
c check length of number
c
      if(nb.lt.1000000000) l=9
      if(nb.lt.100000000) l=8
      if(nb.lt.10000000) l=7
      if(nb.lt.1000000) l=6
      if(nb.lt.100000) l=5
      if(nb.lt.10000) l=4
      if(nb.lt.1000) l=3
      if(nb.lt.100) l=2
      if(nb.lt.10) l=1
c
      do 5,i=1,l
         n(i) =mod(nb/10**(i-1),10)
5     continue
c
      do 10 i=l,1,-1
         a(i:i) = char(n(l-i+1)+48)
   10 continue
c
      if (rb_back.lt.0.0) then
          l=l+1
          do 51,i=l,2,-1
             a(i:i)=a(i-1:i-1)
51        continue
          a(1:1)='-'
      end if
c
      do 6,i=1,nd
         n(i) =mod(nb_deci/10**(i-1),10)
6     continue
c
      do 11 i=nd,1,-1
         b(i:i) = char(n(nd-i+1)+48)
   11 continue
c
c      print*,'l,nd=',l,nd
      if(nd.eq.0) then
         a=a(1:l)
      else
         a=a(1:l)//'.'//b(1:nd)
      end if
c
      rb=rb_back
c
      return
      end
c
