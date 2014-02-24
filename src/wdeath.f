
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

c...WDEATH

      subroutine wdeath (tave, wfunc)

      real tave, wfunc

c...Death of leaves, fine branches, large wood, fine roots, and coarse roots.

      include 'const.inc'
      include 'dovars.inc'
      include 'isovar.inc'
      include 'param.inc'
      include 'parfs.inc'
      include 'pheno.inc'
      include 'plot3.inc'
      include 'timvar.inc'
      include 'zztim.inc'

c...Local variables
      integer iel
      logical drpdlv
      real accum(ISOS), ctodie, etodie, fr14, 
     $      recres(MAXIEL), tostore,agefact, agefact1

c...Save variables
      save drpdlv

      accum(LABELD) = 0.0
      accum(UNLABL) = 0.0
  
c....wood death rate increase with age before 30 years.  Liu Sept 2001
       if (forage < 20) then
         agefact = 0.0
       else if (steady < 1) then
         agefact = float(forage-20)/100.0
       else
        agefact = 1
       endif
       if (agefact > 1.0) then 
          agefact = 1.0
       endif
          agefact = 1.0

        agefact1 = 1.0
      if ((time - strtyr) .le. 0.00001) drpdlv = .FALSE.

c...Death of leaves
c   NOTE:  WOODDR(1) is the death rate in fall for deciduous forests
c          LEAFDR(MTH) is the monthly death rate for leaves in every
c                      case except for fall in deciduous forests.
      if (rleavc .gt. 0.0001) then
	if (decid .ge. 1) then

c...Deciduous forest
c...If the daylight hours are increasing - it must be spring
	  if ((hrsinc) .and. (tave .gt. 7.0)) drpdlv = .FALSE.

c...If daylight hours are decreasing and the temperature is low
c   enough drop leaves for fall season.
          if (decid .eq. 1) then
	    if ((tave .lt. 7.0) .and. (.not. drpdlv)) then
	      ctodie = rleavc * wooddr(LEAF)
	      drpdlv = .TRUE.
            else
	      ctodie = rleavc * leafdr(month)
            endif
          elseif (decid .eq. 2) then
c...Compute death for drought deciduous forests
	    ctodie = rleavc * (1. - wfunc) * wooddr(LEAF)
          endif
        else

c...Continuous forest
c   Use leaf death rate multiplier from EACHYR
          ctodie = rleavc * leafdr(month) * ldrmlt
        endif
        
cc.......add the following to prevent rleavc from becoming zero (address error), April 17, 2007
        if (rleavc > 1 .and. rleavc - ctodie < 0.5) then
            ctodie = ctodie - 0.5
        endif 

c...Compute E/C ratios
        do 10 iel = 1, nelem
	  recres(iel) = rleave(iel) / (0.0001+rleavc)

c...Compute flow to retranslocation storage
	  tostore = recres(iel) * ctodie * forrtf(iel)
	  call flow(rleave(iel), forstg(iel), time, tostore)

c...Decrease E/C by the amount that is retranslocated
	  recres(iel) = recres(iel) * (1 - forrtf(iel))
 10     continue

	fr14 = rlvcis(LABELD) /(0.0001+rleavc)
	call partit(ctodie, recres, 1, rlvcis, rleave, wdlig(LEAF), fr14)
      endif

c...Death of fine roots
      if (frootc .gt. 0.0) then
	ctodie = frootc * wooddr(FROOT)
        if (ctodie .lt. 0.0) then
           ctodie = 0.0
        endif
	do 20 iel = 1, nelem
	  recres(iel) = froote(iel) / (0.0001+ frootc)
 20     continue
	fr14 = frtcis(LABELD) / (0.0001+ frootc)
	call partit(ctodie, recres, 2, frtcis, froote, wdlig(FROOT), fr14)
      endif

c...Fine Branches, Large Wood, and Coarse Roots go to the dead wood
c   compartments: WOOD1, WOOD2, WOOD3

c...Death of fine branches
      if (fbrchc .gt. 0.0) then
	ctodie = agefact1 * fbrchc * wooddr(FBRCH)
        if (ctodie .lt. 0.0) then
           ctodie = 0.0
        endif
	call csched(ctodie, cisotf, 1.0,
     $              fbrcis(UNLABL), wd1cis(UNLABL),
     $              fbrcis(LABELD), wd1cis(LABELD),
     $              1.0, accum)

	do 30 iel = 1, nelem
	  etodie = ctodie * (fbrche(iel) / (0.0001+ fbrchc))
	  call flow(fbrche(iel), wood1e(iel), time, etodie)
 30     continue
      endif
c      deadagwd = deadagwd + ctodie

c...Death of large wood
      if (rlwodc .gt. 0.0) then
	ctodie = rlwodc * agefact*wooddr(LWOOD)
        if (ctodie .lt. 0.0) then
           ctodie = 0.0
        endif
	call csched(ctodie, cisotf, 1.0,
     $              rlwcis(UNLABL), wd2cis(UNLABL),
     $              rlwcis(LABELD), wd2cis(LABELD),
     $              1.0, accum)

	do 40 iel = 1, nelem
	  etodie = ctodie * (rlwode(iel) / (0.0001+rlwodc))
	  call flow(rlwode(iel), wood2e(iel), time, etodie)
 40     continue
      endif
c      deadagwd = deadagwd + ctodie
 

c...Death of coarse roots
      if (crootc .gt. 0.0) then
	ctodie = crootc * agefact*wooddr(CROOT)
        if (ctodie .lt. 0.0) then
           ctodie = 0.0
        endif
	call csched(ctodie, cisotf, 1.0,
     $              crtcis(UNLABL), wd3cis(UNLABL),
     $              crtcis(LABELD), wd3cis(LABELD),
     $              1.0, accum)

	do 50 iel = 1, nelem
	  etodie = ctodie * (croote(iel) / (0.0001+crootc))
	  call flow(croote(iel), wood3e(iel), time, etodie)
 50     continue
      endif

      return
      end
