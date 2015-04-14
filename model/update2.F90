      subroutine update(jpass)
!======================================================================
!    subroutine to update time array
!======================================================================
      include "ocean.h"
      integer isec,imin,ihour,iday,imon, iyear,j
      integer isecnew, iminnew, ihournew, kd,ishift,jpass


        j = jpass

        iday = tzero(4)
        imon = tzero(5)
        iyear = tzero(6)
        call  cday2(iday, imon, iyear,kd)
        isecnew = tzero(1) + dt*j
        isec = mod(isecnew,60)
        iminnew = tzero(2) + isecnew/60
        imin = mod(iminnew,60)
        ihournew =  tzero(3) + iminnew/60
        ihour  = mod(ihournew,24)
        kd = kd + ihournew/24
        call  dmy2(iday,imon,iyear,kd)
        time(1) = isec
        time(2) = imin
        time(3) = ihour
        time(4) = iday
        time(5) = imon
        time(6) = iyear
      return
      end


      subroutine cday2(idd,imm,iyy_pass,kd)
!!
!!  given day,month (each 2 digits) and year (4 digits), cday returns
!!  the day#, kd based on the gregorian calendar.
!!  the gregorian calendar, currently 'universally' in use was
!!  initiated in europe in the sixteenth century. note that cday
!!  is valid only for gregorian calendar dates.
!
!   kd=1 corresponds to january 1, 0000
!       
!       note that the gregorian reform of the julian calendar 
!       omitted 10 days in 1582 in order to restore the date
!       of the vernal equinox to march 21 (the day after
!       oct 4, 1582 became oct 15, 1582), and revised the leap 
!       year rule so that centurial years not divisible by 400
!       were not leap years.
!
!   this routine was written by eugene neufeld, at ios, in june 1990.
!    modified by Joel Chasse (January 20th, 1995)
!
      integer jfy,kk,jyyy,l,i,jyy,jfh,jcc,lp,kd,kkd
      integer idd,iyy,iyy_pass,icc,imm
      integer ndp(13)
      integer ndm(12)
      data ndp/0,31,59,90,120,151,181,212,243,273,304,334,365/
      data ndm/31,28,31,30,31,30,31,31,30,31,30,31/
!!
   
       icc=int(iyy_pass/100.)
       iyy=iyy_pass-icc*100
      lp = 6
!!  test for invalid input:
      if(icc.lt.0)then
         write(lp,5000)icc
         stop
      endif
      if(iyy.lt.0.or.iyy.gt.99)then
         write(lp,5010)iyy
         stop
      endif
      if(imm.le.0.or.imm.gt.12)then
         write(lp,5020)imm
         stop
      endif
      if(idd.le.0)then
         write(lp,5030)idd
         stop
      endif
      if(imm.ne.2.and.idd.gt.ndm(imm))then
         write(lp,5030)idd
         stop
      endif
      if(imm.eq.2.and.idd.gt.29)then
         write(lp,5030)idd
         stop
      endif
      if(imm.eq.2.and.idd.gt.28.and.((iyy/4)*4-iyy.ne.0.or.(iyy.eq.0.and.(icc/4)*4-icc.ne.0)))then
         write(lp,5030)idd
         stop
      endif
5000  format(' input error. icc = ',i7)
5010  format(' input error. iyy = ',i7)
5020  format(' input error. imm = ',i7)
5030  format(' input error. idd = ',i7)
!!
!!  calculate day# of last day of last century:
      kd = icc*36524 + (icc+3)/4
!!
!!  calculate day# of last day of last year:
      kd = kd + iyy*365 + (iyy+3)/4
!!
!!  adjust for century rule:
!!  (viz. no leap-years on centurys except when the 2-digit
!!  century is divisible by 4.)
      if(iyy.gt.0.and.(icc-(icc/4)*4).ne.0) kd=kd-1
!!  kd now truly represents the day# of the last day of last year.
!!
!!  calculate day# of last day of last month:
      kd = kd + ndp(imm)
!!
!!  adjust for leap years:
      if(imm.gt.2.and.((iyy/4)*4-iyy).eq.0.and.((iyy.ne.0).or.(((icc/4)*4-icc).eq.0))) kd=kd+1
!!  kd now truly represents the day# of the last day of the last
!!  month.
!!
!!  calculate the current day#:
      kd = kd + idd
      return
!!
!!
      entry dmy2(idd,imm,iyy_pass,kd)
!!
!!  given the (gregorian) day#, kd, as calculated above in this routine,
!!  entry dmy2 returns the (gregorian) day, month, year and century.
!!
!!  test for valid input:
      if(kd.le.0) write(lp,5040)kd
5040  format(' kd = ',i7,'  invalid input. dmy2 stop.')
!!
!!  save kd
      kkd=kd
!!  calculate icc and subtract the number of days represented by icc
!!  from kkd
!!  jfh is the number of 400 year intervals up to kkd
!!  jcc is the number of additional centuries up to kkd
      jfh = kkd/146097
      kkd = kkd - jfh*146097
      if(kkd.lt.36525)then
         jcc = 0
      else
         kkd = kkd - 36525
         jcc = 1 + kkd/36524
         kkd = kkd - (jcc-1)*36524
      end if
      icc = 4*jfh + jcc
      if(kkd.eq.0)then
         icc = icc-1
         iyy = 99
         imm = 12
         idd = 31
         iyy_pass=icc*100+iyy
         return
      endif
!!
!!  calculate iyy. jfy is the number of four year intervals in the
!!  current century. the first four year interval is short (1460 days
!!  rather than 1461)if the current century is not divisible by 4, and
!!  in this case jcc.ne.0 as calculated above.
!!
!!  calculate jfy:
      jfy = 0
      if(jcc.eq.0)goto 10
      if(kkd.lt.1460)goto 10
      jfy = 1
      kkd = kkd - 1460
10    kk = kkd/1461
      jfy = jfy + kk
      kkd = kkd - kk*1461
!!
!!  calculate jyy, the remaining years of the current century up to the
!!  current day:
      jyy = 0
!!  the next year is not a leap year if jfy=0 and jcc.ne.0.
      if(jfy.eq.0.and.jcc.ne.0)goto 20
      if(kkd.lt.366)goto 30
      jyy = 1
      kkd = kkd - 366
20    jyyy = kkd/365
      jyy = jyy + jyyy
      kkd = kkd - jyyy*365
30    iyy = 4*jfy + jyy
      if(kkd.eq.0) then
         iyy=iyy-1
         imm=12
         idd=31
         iyy_pass=icc*100+iyy
         return
      end if
!!
!!  set l=1 if we have a leap year.
      l=0
      if(iyy-(iyy/4)*4.ne.0)goto 40
      if(iyy.eq.0.and.(icc-(icc/4)*4).ne.0)goto 40
      l=1
!!
!!  calculate imm and idd
40    if(kkd.gt.31) goto 50
      imm=1
      idd=kkd
      iyy_pass=icc*100+iyy
      return
!!
50    if(kkd.gt.59)goto 60
      imm = 2
      idd = kkd-31
      iyy_pass=icc*100+iyy
      return
!!
60    if(kkd.gt.60)goto 70
      if(l.eq.0)goto 70
      imm = 2
      idd = 29
      iyy_pass=icc*100+iyy
      return
!!
70    if(l.eq.1) kkd=kkd-1
      do 80 i=4,13
         if(kkd.gt.ndp(i))goto 80
         imm = i-1
         idd = kkd - ndp(i-1)
         iyy_pass=icc*100+iyy
         return
!!
80    continue
90    write(lp,5050)
5050  format(' error in dmy2')
      stop
      end SUBROUTINE 
