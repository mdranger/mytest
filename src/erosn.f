
c               EROS Data Center
c		Shuguang Liu


	subroutine erosn()

c...Soil is removed from the system.

c...psloss is the rate of soil loss (kg/m**2/m)
c...bulkd is the bulk density of the soil (kg/liter)
c...edepth is the depth of the soil (m)
c...enrich is the enrichment factor for SOM losses
c...scloss is the total carbon loss from soil organic
c     matter and below ground litter for this month
c...sclosa is the accumulator for scloss

      include 'chrvar.inc'
      include 'comput.inc'
      include 'const.inc'
      include 'param.inc'
      include 'parfx.inc'
      include 'plot1.inc'
	include 'plot2.inc'
        include 'zztim.inc'
        include 'prof.inc'
        include 'timvar.inc'
        include 'wth.inc'

 
c...Local variables
        integer   i, j, filled
        real      flost, depth
        
c...Compute fraction of soil which is lost.

	flost = psloss/(10.*lyrden(1)*adep(1)) 
	
c...calculating the soil depth being removed from the top layer
c	according to the amount of soil moved.  
c	The top layer is subsequently being filled by moving an equivalent 
c	thickness of soil from the second layer. 
c	If the second layer is thinner than the soil depth being moved,
c	the third layer is used to fill the rest

	depth = flost * adep(1)
	
c......carbon is moved out from the top layer by pools
	call soilerod()
	
c....fill up the top layer to 20 cm (TOPLYR) and adjust C
	i = 2
	filled = 0
	
5	continue
	if (i .le. MAXLYR .and. filled .ne. 1) then
	  if (adep(i) .ge. depth) then
	    call soilup(i, depth, depth)
	    adep(i) = adep(i) - depth
	    filled = 1
	  else
	    call soilup(i, adep(i), depth)
	    depth = depth - adep(i)
	    adep(i) = 0.
	  endif
	  i = i + 1
	goto 5
	endif	  

	 
c	write (*, 6), adep(1), adep(2), adep(3)
c	format (3f10.4)	
		
	if (i .eq. MAXLYR+1 .and. filled .eq. 0) then
	   print *, 'No sub-soil can be used to maintain the 
     $  20 cm top layer!'
	   print *, 'Erosion rates may be too high!'
	   stop
	endif


c..... update soil layers, deleting the layers with 0 depth	
	do 20 i = 2, MAXLYR
	  if (adep(i) .lt. 1.e-7 .and. i .lt. MAXLYR) then
	     do 10 j = i, MAXLYR-1
	 	adep(j) = adep(j+1)   
	 	lyrden(j) = lyrden(j+1)
	 	lyrsand(j) = lyrsand(j+1) 
	 	lyrclay(j) = lyrclay(j+1) 
	 	lyrsocp(j,1) = lyrsocp(j+1,1)
	 	lyrsocp(j,2) = lyrsocp(j+1,2)
	 	lyrsocp(j,3) = lyrsocp(j+1,3)
	 	deadrt(j) = deadrt(j+1)
	 	livroot(j) = livroot(j+1)
10	     continue
	     adep(MAXLYR) = 0.
	     lyrden(MAXLYR) = 0.
	     lyrsand(MAXLYR) = 0.
	     lyrclay(MAXLYR) = 0.
	 	lyrsocp(MAXLYR,1) = 0.
	 	lyrsocp(MAXLYR,2) = 0.
	 	lyrsocp(MAXLYR,3) = 0.
	 	deadrt(MAXLYR) = 0.
	 	livroot(MAXLYR) = 0.
	  endif
20 	continue


	return
	end
