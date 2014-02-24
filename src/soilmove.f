
c               Copyright EROS Data Center
c                       All Rights Reserved


	subroutine soilmove()


c...Soil is removed from the system.

c...psloss is the rate of soil loss (eorsion: positive) and 
c	soil gain (deposition: negative) (kg/m**2/m)

	include 'param.inc'
	include 'parfx.inc'
	include 'prof.inc'
	
	integer   j, layer, rtlayers
	real      rtdepth
	
c ..... according to CENTURY's tradition, positive values represent
c	soil erosion. Therefore, in the modified version, deposition 
c	is represented by negative values.  Liu, 10/99

	if (psloss .gt. 0) then
	    call erosn()
	elseif (psloss .lt. 0) then
	    call depos()
	endif
	
	
c.....update soil root fraction following depth change
c	call rtdpth()
c	call root_fr()
	
c....calculate the layers in the rooting depth (rootdep).  Liu 1/00
c	 rtdepth = 0.0

c	 rtlayers = 0
c         do 25 j = 1, MAXLYR
c           rtdepth = rtdepth + adep(j)
c              rtlayers = rtlayers + 1
c           if (rtdepth .ge. rootdep) then
c              goto 26
c           endif
c25       continue

c...Calculate available water holding capacity 11/91 lh
26        awhc = 0.0
c       do 210 layer = 1, rtlayers
c           if (layer .lt. rtlayers) then
c              awhc = awhc + (afiel(layer) - awilt(layer)) * adep(layer)
c           else
c              awhc = awhc + (afiel(layer) - awilt(layer)) * adep(layer)        
c     $		*(1.0-(sdepth-survdep)/adep(slayers))              
c           endif
c210     continue

	return
	end
