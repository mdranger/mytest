
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

C...GROCHK.F

      integer function grochk(tave)

c...This function determines whether or not there is
c   potential growth and if it is the first month of
c   the growing season.
c
c   grochk = 1:  Month of greenup in deciduous forests
c   grochk = 0:  No greenup
c

      include 'pheno.inc'
      include 'timvar.inc'
      include 'zztim.inc'

      real	tave

      logical	startd
      save	startd

c...If this is the first year for this cell, reset STARTD.
      if ((time - strtyr) .le. .00001) startd = .FALSE.

c...If it is spring and the temperature is high enough and
c     you haven't already reapportioned carbon to leaves...
      if ((hrsinc) .and. (.not. startd) .and.
     $		(tave .gt. 10.)) then
	   grochk = 1
	   startd = .TRUE.

      else
	 grochk = 0
         if (.not. hrsinc) then
	   startd = .FALSE.
         endif
      endif

      return
      end
	 
