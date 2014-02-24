
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

C...NUTRLM.F
c
c...Nutrient limitation for plants is based on demand
c...nelem: number of nutrient
c...nparts: number of crop parts
c...cprodl: potential production input
c...eprodl: production output with nutrient limitation
c...maxeci, mineci: max E/C ratio, reverse of input cerfor value
c...maxec: E/C requrired by the plant growth, it calculated by
c...       summing up all the plant parts growth with E/C ratio
c...cfrac: fraction of production for each plant part
c...eavail: available nutrient

      subroutine nutrlm(nelem, nparts, cprodl, eprodl, maxec,
     $                  maxeci, mineci, cfrac, eavail, nfix,
     $                  snfxmx, snfxac, elimit, eup)

      include 'const.inc'
      include 'zztim.inc'

      integer nelem, elimit, nparts
      real cprodl, eprodl(MAXIEL), maxec(MAXIEL), 
     $     eavail(MAXIEL), nfix, snfxmx, snfxac, 
     $     eup(nparts,MAXIEL), mineci(nparts,MAXIEL),
     $     maxeci(nparts,MAXIEL), cfrac(nparts)

c...Local Variables
c   NOTE:  Local variables cannot have adjustable array size.  ECFOR
c          is set to the largest array size which may occur.
      integer 	iel, ipart
      real 	mxnfix, cpbe(MAXIEL), demand, ecfor(5,MAXIEL)

!...Definitions of Local variables
!	mxnfix		- Maximum N fixation
!	cpbe		- Carbon production limited by element
!       ecfor		- Actual E/C ratio by part



c...Compute production limitation for all the nutrition      
      do 10 iel = 1, nelem

c...Initialize fixation to 0
	mxnfix = 0.0
c...DEMAND based on the maximum E/C ratio.
        demand = cprodl * maxec(iel)

c...N FIXATION
c...snfxmx eq 0 most of the time unless for
c... soybean and N-fix alapha
	if (iel .eq. N) then
	  mxnfix = snfxmx * cprodl
        endif

c...New C/E ratios by part based on E available.
	if ((eavail(iel) + mxnfix) .gt. demand) then
	  do 20 ipart = 1, nparts
	    ecfor(ipart,iel) = maxeci(ipart,iel)
 20       continue

	else
	  do 30 ipart = 1, nparts
	    ecfor(ipart,iel) = mineci(ipart,iel) +
     $                    (maxeci(ipart,iel) - mineci(ipart,iel))
     $			* (eavail(iel) + mxnfix) / (0.0001+demand)

 30       continue
	endif

c...Initialize local variables to zero
	cpbe(iel) = 0.0

c...Total potential production with nutrient limitation
	do 40 ipart = 1, nparts
	  cpbe(iel) = cpbe(iel) + cfrac(ipart) * ecfor(ipart,iel)
 40     continue
      
        cpbe(iel) = (eavail(iel) + mxnfix) / cpbe(iel)

c...Put automatic fertilization here when necessary
c	if (aufert .gt. 0)

 10   continue

c...Compute the limiting element

c...Choose the most limit result
      do 50 iel = 1, nelem
        if (cprodl .gt. cpbe(iel)) then 
	  cprodl = cpbe(iel)
	  elimit = REAL(iel)
        endif
 50   continue

c...Recompute EPRODL
      do 70 iel = 1, nelem
        eprodl(iel) = 0.0

c...Total potential production with nutrient limitation
	do 60 ipart = 1, nparts
	  eup(ipart,iel) = cprodl * cfrac(ipart) *
     $                       ecfor(ipart,iel)
	  eprodl(iel) = eprodl(iel) + eup(ipart,iel)
 60     continue

c...Check to make sure the total flow won't exceed what's available.
c...DEBUG
c	  if (eprodl(N) - (eavail(N) + mxnfix) .gt. .000001) then
c	     call message('In NUTRLM: EPRODL > FIX + AVAIL')
c	     write(*,*) 'NPARTS: ', nparts, ' EPRODL: ', eprodl(iel)
c	     write(*,*) 'EAVAIL: ', eavail(iel) 
c	     write(*,*) 'MXNFIX: ', mxnfix
c	     stop
c         endif

 70   continue

c...Compute N fixation which actually occurs and add to the
c   N fixation accumulator.
      nfix = max(eprodl(N) - eavail(N), 0.0)
      snfxac = snfxac + nfix

      return
      end

