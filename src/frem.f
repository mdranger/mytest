
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

c...FREM
      subroutine frem()

c...Forest removal - fire or cutting (cutting includes storms)
c   Includes litter burning in forest systems.

c...Called from:	simsom

      include 'const.inc'
      include 'forrem.inc'

c...Local variables
      real	accum(ISOS)

      accum(LABELD) = 0.0
      accum(UNLABL) = 0.0

c...Removal for both CUT and FIRE events
      if ((evntyp .eq. 0) .or. (evntyp .eq. 1)) then

c...Live Removal
	 call livrem(accum)

c...Dead Removal
	 call dedrem(accum)

c...Death of Roots
	 call killrt(accum)

      endif

c...Returns from cutting event
      if (evntyp .eq. 0) then
	 call cutrtn(accum)

c...Returns from fire event
      else if (evntyp .eq. 1) then
	 call firrtn()

      endif

      return
      end

