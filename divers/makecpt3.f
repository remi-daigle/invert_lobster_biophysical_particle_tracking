      character *80 cptFile
      character *80 arg(9)
      character *200 line
      character *80 colormap
      real RGBV(3,1:1024) !1024 couleur maximum
      real plot_min,plot_max
      integer GFlag,IFlag,MFlag,CFlag,LFlag,UFlag
c
c-----------------------------------------------------------------------
c initialisation
c
      arg='     '
      GFlag=0
      IFlag=0
      MFlag=0
      CFlag=0
      LFlag=0
      UFlag=0
c
c-----------------------------------------------------------------------
c
      call getarg(1,arg(1))
      call getarg(2,arg(2))
      call getarg(3,arg(3))
      call getarg(4,arg(4))
      call getarg(5,arg(5))
      call getarg(6,arg(6))
      call getarg(7,arg(7))
      call getarg(8,arg(8))
      call getarg(9,arg(9))
c
c-----------------------------------------------------------------------
c
c  Validation
c
      if(arg(1)(1:2).eq.'  '.and.
     >   arg(2)(1:2).eq.'  '.and.
     >   arg(3)(1:2).eq.'  '.and.
     >   arg(4)(1:2).eq.'  '.and.
     >   arg(5)(1:2).eq.'  '.and.
     >   arg(6)(1:2).eq.'  '.and.
     >   arg(7)(1:2).eq.'  '.and.
     >   arg(8)(1:2).eq.'  '.and.
     >   arg(9)(1:2).eq.'  ') then
         print*,'usage:'
         print*,
c
     >   'makecpt3 -M<colorMap> -C<cptFile> -L<
     >lower_limit> -U<higher_limit>'
         stop
       endif
c
c   checking argument
c
      Do i=1,9
         if(arg(i)(1:2).eq.'-M') then
            colormap=arg(i)(3:lenstr(arg(i)))
            MFlag=1
         endif
         if(arg(i)(1:2).eq.'-C') then
            cptFile=arg(i)(3:lenstr(arg(i)))
            CFlag=1
         endif
         if(arg(i)(1:2).eq.'-L') then
            line=arg(i)(3:lenstr(arg(i)))
            read(line,*) plot_min  !valeur minimale a dessiner
            LFlag=1
         endif
         if(arg(i)(1:2).eq.'-U') then
            line=arg(i)(3:lenstr(arg(i)))
            read(line,*) plot_max  !valeur maximale a dessiner
            UFlag=1
         endif
      endDO
c
      if(Mflag.eq.0) then
         print*,'-M: Must specify colormap File (Input)'
         stop
      endif
c
      if(Cflag.eq.0) then
         print*,'-C: Must specify cpt File (Output)'
         stop
      endif
c
      if(Lflag.eq.0) then
         print*,'-L: Must specify lower limit to plot (Input)'
         stop
      endif
c
      if(Uflag.eq.0) then
         print*,'-U: Must specify Upper limit to plot (Input)'
         stop
      endif
c
      if(plot_min.gt.plot_max) then
         print*,'Upper limit (-U) must be higher than lower limit (-L)'
         stop
       endif
c-----------------------------------------------------------------------
       vmin=plot_min
       vmax=plot_max
c
c Processing
c
      if(plot_min.gt.vmax) then
         print*,'plot_min=',plot_min
         print*,'vmax=',vmax
         print*,'Lower limit (-L) must be Lower than the maximun value'
         print*,'in the grd file (see info file)'
         stop
      endif
c
      if(plot_max.lt.vmin) then
         print*,'plot_max=',plot_max
         print*,'vmin=',vmin
         print*,'Upper limit (-U) must be higher than the minimum value'
         print*,'in the grd file (see info file)'
         stop
      endif
c
c-----------------------------------------------------------------------
c
c Read colormap
C
      open(1,file='/home/chassej/ps/'//trim(colormap),status='old')
      nb_color=0
      i=1
      DO 
         read(1,*,end=121) rgbv(1,i),rgbv(2,i),rgbv(3,i)
         nb_color=nb_color+1
         i=i+1
      endDo
121   print*,'nb_color=',nb_color
      close(1)
c
c     transformation en "255"
      rgbv=rgbv*255
c
c-----------------------------------------------------------------------
c Fixing the colors
c
      open(2,file=trim(cptFile))
c 
c Core colors
c
      delta=(plot_max-plot_min)/(nb_color) 
      plot_last=plot_max
      Do val=plot_min,plot_last-delta,delta
          val2=val+delta
          x=((val-plot_min)*(nb_color-2))/(plot_max-plot_min)
          ic=nint(x)+2
      write(2,10)val,nint(rgbv(1,ic)),nint(rgbv(2,ic)),nint(rgbv(3,ic)),
     >val2,nint(rgbv(1,ic)),nint(rgbv(2,ic)),nint(rgbv(3,ic))
      enddo
c
10    format(f25.8,3(1x,i3),1x,f25.8,3(1x,i3))
      end
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
