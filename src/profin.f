

c               EROS Data Center, Shuguang Liu, 10/99
c                       All Rights Reserved

C...profin.F
!...modified by Gail smith, 2010
      subroutine profin()

c...Read in the profile variable type in the prof.100 file
c...created by GEMS for deep soil profile

!#include "const.inc"
      include 'const.inc'
      include 'prof.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'param.inc'

c...Local variables
c ..... layer1, layer1C and layer1D are the cumulative thickness,
c       SOC, and bulk density of the top layer before separating
c   	this layer into 20 cm and the rest
c 	bulkden(i) and SOC(i) are temporary arrays for holding the 
c	profile information as input by the user
c	the global variables adep(i), lyrden(i) and lyrsoc(i) are
c	for adjusted depth, bulk density, and SOC respectively.
  
      integer   i, j, k
      real      layer1, layer1C, layer1D, bulkden(MAXLYR)
      real      SOC(MAXLYR), a, c, layer1rl
      real      layer1sd, layer1cl,sandf(MAXLYR), clayf(MAXLYR)
      real      depth, rootfr(MAXLYR), sensi, temp
      character(100) errmsg

      open(unit=12, file='prof.100', status='OLD')
      rewind(12)
c20    continue
      
c....read edratio
	read(12, *)
	read(12,*) edratio
       
c....read thickness of each layer (cm)
	read(12, *)
	
        depth = 0.0
        do 25 i = 1, MAXLYR 
          read(12, *) lyrthk(i)
          adep(i) = lyrthk(i)
c	         print *, 'adep', adep(i), i          
25      continue

c....read bulk density of each layer (g/cm3)
	read(12, *)
        do 35 i = 1, MAXLYR 
          read(12, *) lyrden(i)
c	 lyrden(i) = 1.0*lyrden(i)    ! adds no value??
          bulkden(i) = lyrden(i)
35      continue

c....read total SOC in each layer (g C/m2) 
c.... SOC in the top layer is specified in the site file, only a place holder here
	read(12, *)
	sensi = 1.0
	temp =-0.0
        do 45 i = 1, MAXLYR 
          read(12, *) lyrsocp(i,1),lyrsocp(i,2),lyrsocp(i,3)
	lyrsocp(i,1) = lyrsocp(i,1)*sensi
	lyrsocp(i,2) = lyrsocp(i,2)*sensi+temp*lyrsocp(i,2)
	lyrsocp(i,3) = lyrsocp(i,3)*sensi-temp*lyrsocp(i,2)
          lyrsoc(i) = lyrsocp(i,1)+lyrsocp(i,2)+lyrsocp(i,3)
c          print *, "lyrsoc", i, lyrsocp(i,1), lyrsocp(i,2),lyrsocp(i,3)
          SOC(i) = lyrsoc(i)
45      continue
 
c....read sand fraction of each layer (g/cm3)
	read(12, *) 
        do 55 i = 1, MAXLYR 
          read(12, *) lyrsand(i)
c.........Valid values should be between 0 and 1, otherwise exit
          if (lyrsand(i) < 0.0 .or. lyrsand(i) > 1.0) then
            call message('   ***Error in the lyrsand value.  It '//
     +          'should be between 0.0 and 1.0.')
            write (errmsg, *) 'lyrsand(', i, '): ', lyrsand(i)
            call message(errmsg)
            STOP 'Input data error for lyrsand in prof.100'
          else
             sandf(i) = lyrsand(i)
          endif
55      continue

c....read clay fraction of each layer (g/cm3)
	read(12, *)
        do 65 i = 1, MAXLYR 
          read(12, *) lyrclay(i)
c	  lyrclay(i) = 1.0 * lyrclay(i)   ! adds no value??
c.........Valid values should be between 0 and 1, otherwise exit
          if (lyrclay(i) < 0.0 .or. lyrclay(i) > 1.0) then
            call message('   ***Error in the lyrclay value.  It '//
     +          'should be between 0.0 and 1.0.')
            write (errmsg, *) 'lyrclay(', i, '): ', lyrclay(i)
            call message(errmsg)
            STOP 'Input data error for lyrclay in prof.100'
          else
            clayf(i) = lyrclay(i)
          endif
65      continue

c...Add by zp
c...Read in the drainage coefficient from input file
	read(12,*)
c...drainage change coefficient or drainage curve flatness transitioning from weel-drained to poorly-drained
c...tile_depth is the burial depth of tile drainage system
        read(12,*) drain_coefficient
        read(12,*) tile_depth
c.......Valid values should be between 0 and 1, otherwise exit
        if (drain_coefficient < 0.0 .or. drain_coefficient > 1.0) then
            call message('   ***Error in the drain_coefficient '//
     +          'value.  It should be between 0.0 and 1.0.')
            write (errmsg, *) 'drain_coefficient: ', drain_coefficient
            call message(errmsg)
            STOP 'Input data error for drain_coefficient in prof.100'
        endif
        
c...root allocations by layer (fractions)
        read(12,*)
        read(12,*) root_flag
!        print *, 'root flag', root_flag
        if ( root_flag.eq. 1) then
        do 70 i = 1, MAXLYR
	    read(12,*) lyrrtfr(i) 
c            print *, lyrrtfr(i)
70      continue
        else
c....root fraction is calculated according to Jackson et al., (1996) Oecologia for
          call root_fr()
        endif

c...Store the root fraction
      do 300 i = 1, MAXLYR
          rootfr(i) = lyrrtfr(i)
300   continue

c.....adjusting the profile properties to form a top layer of 20 cm in thickness

      if (lyrthk(1) .ne. TOPLYR) then
      
	layer1 = 0.
	layer1D = 0.
	layer1C = 0.
	layer1sd = 0.
	layer1cl = 0.
	layer1rl = 0.
	
	j=0
	do 105 i=1, MAXLYR  
75	     j = j + 1     
	     layer1 = layer1 + lyrthk(j)
	     
             if (layer1 .gt. TOPLYR) then
                adep(1) = TOPLYR
                adep(2) = layer1 - TOPLYR
                lyrden(1) = layer1D + bulkden(j)*(lyrthk(j) - 
     $                layer1 + TOPLYR)/TOPLYR
                lyrsand(1) = layer1sd + sandf(j)*(lyrthk(j) - 
     $                layer1 + TOPLYR)/TOPLYR
                lyrclay(1) = layer1cl + clayf(j)*(lyrthk(j) - 
     $                layer1 + TOPLYR)/TOPLYR
                lyrrtfr(1) = layer1rl + rootfr(j)*(lyrthk(j) - 
     $                layer1 + TOPLYR)/TOPLYR
                lyrsoc(1) = layer1C + SOC(j)*(lyrthk(j) - 
     $                layer1 + TOPLYR)/lyrthk(j)                 
                lyrden(2) = bulkden(j)
                lyrsoc(2) = SOC(j)*(layer1 - TOPLYR)/lyrthk(j)    
85                do 95 k = 3, MAXLYR
		   if (j .gt. MAXLYR-1) then
		      adep(k) = 0.
		      lyrden(k) = 0.
		      lyrsoc(k) = 0.
		      lyrsand(k) = 0.
		      lyrclay(k) = 0.
		      lyrrtfr(k) = 0.
		   else
                      adep(k) = lyrthk(j+1)
                      lyrden(k) = bulkden(j+1)
                      lyrsand(k) = sandf(j+1)
                      lyrclay(k) = clayf(j+1)
                      lyrrtfr(k) = rootfr(j+1)
                      lyrsoc(k) = SOC(j+1)
                   endif
                   j = j + 1
95              continue
		goto 160
             elseif (layer1 .lt. TOPLYR) then 
                 layer1D = layer1D + lyrden(j)*lyrthk(j)/TOPLYR
                 layer1sd = layer1sd + lyrsand(j)*lyrthk(j)/TOPLYR
                 layer1cl = layer1cl + lyrclay(j)*lyrthk(j)/TOPLYR
                 layer1rl = layer1rl + lyrrtfr(j)*lyrthk(j)/TOPLYR
                 layer1C = layer1C + lyrsoc(j)*lyrthk(j)/TOPLYR   
c                 print *, j, lyrden(j), lyrsoc(j), layer1D, layer1C              
                 goto 75
             elseif (layer1 .eq. TOPLYR) then
                 goto 85
             endif
105 	continue 
      endif	  
      
c      lyrsoc(1) = 0.0
      
c      do 110 i=1,2
c      lyrsoc(1) = lyrsoc(1) + som1ci(i,i) + som2ci(i) + som3ci(i)
c110	continue          

      
160	print *
c	print *, '=======Initial Profile Info============='
c	print *
c	print *, ' layer thickness(cm)   BulkD     SOC      Sand
c     $      Clay    RootFr'
c	print * 
	a = 0.
	c = 0.
	do 170 i=1, MAXLYR
	  if (adep(i) .gt. 0) then
c	   print 200,i,adep(i),lyrden(i),lyrsoc(i),lyrsand(i),lyrclay(i),
c     $        lyrrtfr(i)	 
	      a = a + adep(i)
	      c = c + lyrsoc(i)
	  endif
170	continue
c	print *
c	print 210, 'Total', a, c
c	print *
c	print *, '   layer    fast     slow    passive'
c	do 180 i=1, MAXLYR
c          print 200,i, lyrsocp(i,1),lyrsocp(i,2),lyrsocp(i,3)
c180	continue

c200	format(i8, 6f10.2)
c210	format(a8, f10.2, f20.2)

      return

      end
