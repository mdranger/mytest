
c               EROS Data Center, Shuguang Liu, 10/99
c                       All Rights Reserved


C...rootfr.F
!...Calculate the root fraction according to the input parameter
      subroutine root_fr()

c...calculate root fraction

      include 'prof.inc'
      include 'const.inc'
      include 'parfx.inc'
	include 'param.inc'

  
      integer   i
      real      depth, rootfr(MAXLYR),rtdens

	   depth = 0.0

	   do 10 i = 1, MAXLYR
	      depth = adep(i) + depth
  	      rootfr(i) = 1.0 - rtprof**depth
c        print *, i, depth, rtprof, rootfr(i)
c  temperate grassland
c           rootfr(i) = 1.0 - 0.943**depth
c...wheat and other crops (from ...)
c  	      rootfr(i) = 1.0 - 0.90**depth
c temperate forests
c  	      rootfr(i) = 1.0 - 0.976**depth
c...more shallow roots temperate forest
c  	      rootfr(i) = 1.0 - 0.966**depth
   	      lyrrtfr(i) = rootfr(i)

   	      if (i .gt. 1) then
   	        lyrrtfr(i) = rootfr(i) - rootfr(i-1)
   	      endif
c           print *, i, adep(i), lyrrtfr(i)

10	    continue	  

      return

      end
