
!              Copyright 1993 Colorado State University
!                       All Rights Reserved


      subroutine potcrp (month,cancvr)
      integer month
      real cancvr

!...Compute monthly production potential based upon montly precip
!     and restrict potential production based upon method specified
!     by grzeff.

      include 'comput.inc'
      include 'const.inc'
      include 'dovars.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot3.inc'
      include 'potent.inc'
      include 'seq.inc'
      include 'wth.inc'
      include 'zztim.inc'
      include 'parfs.inc'
!...Function declarations
      real     gpdf, pevap, pprdwc
      external gpdf, pevap, pprdwc

!...Local variables
      integer  iel

      real     agprod, aisc, bgp, bgprod, bio, bioc, biof,scale1,
     $         bioprd, bop, ctemp, fracrc, potprd, pptprd, ratlc,
     $         sdlng, shdmod, subcan, temp1, temp2, temp3, tmns, tmxs,
     $		hi_low, pfac
	

!.........file for monthly output
       if (debug_flag > 0)then
          open(unit=93, file="monthly.txt", status="unknown")
       endif
       
!...Compute shading modifier for savanna
      if (cursys .eq. SAVSYS) then
        if (cancvr .eq. 0) then
          aisc = 0.
        else
          aisc = 5 * exp(-.0035 * (rleavc*2.5)/cancvr)
        endif
        subcan = aisc/(aisc + 1.)
        shdmod = (1.0-cancvr) + (cancvr*subcan)
      else
        shdmod = 1.0
      endif

!...Estimate the root/shoot ratio as a function of annual precip
      if (frtc(1) .eq. 0.0) then
        rtsh = (bgppa + grwprc*bgppb)/(agppa + grwprc*agppb)

      else
        if (msplt .gt. frtc(3)) then
          fracrc = frtc(2)
        else
          fracrc = frtc(1) - msplt*(frtc(1)-frtc(2))/frtc(3)
        endif
	rtsh = fracrc/(1 - fracrc)
      endif
!...may have negative value at the 1st month      
      if (rtsh .le. 0 ) then
          rtsh = 0.1
      endif
!       fracrc = max(frtc(2), frtc(1) - msplt*(frtc(1)-frtc(2))/frtc(3))
!       rtsh = fracrc/(1 - fracrc)


!...Estimate plant production:
!...  reset the potprd
!................................................................
      potprd = 0

      if (stemp .gt. 0.0 .and. crpgrw .ne. 0) then
         growthlen = growthlen + 1

!      if (cum_temp(month) .gt. 100.0) then
!......gradual impact of cumulative temperature
         cum_tmp_imp = (cum_temp(month)-100.)/300.0
         if (cum_tmp_imp .gt. 1.0) then
              cum_tmp_imp = 1.0
          endif
         cum_tmp_imp = 1.0
                                                                                         
!...Change root/shoot ratio if burning occurs
	if (dofire(CRPSYS).or.dofire(SAVSYS)) then
          rtsh = rtsh + frtsh
        endif

!...Change root/shoot ratio by effect of CO2
	rtsh = rtsh * co2crs(CRPSYS)


!...Calculate temperature effect on growth
!...potprd saves the temperature effect, zp
!...Removal of litter effects on soil temperature as it 
!     drives plant productin (keith paustian)
        bio = aglivc * 2.5

        bio = min(bio,pmxbio)

!...Maximum temperature
        tmxs=maxtmp(month)+(25.4/(1.+18.*exp(-.20*maxtmp(month))))
     $             *(exp(pmxtmp*bio)-.13)
    
!...Minimum temperature
        tmns=mintmp(month)+pmntmp*bio-1.78
        
!...Average surface temperature

        ctemp=(tmxs+tmns)/2.

        potprd = gpdf(ctemp, ppdf(1,1), ppdf(2,1), ppdf(3,1), 
     $                ppdf(4,1))

!...Calculate temperature effect on growth.
!...        potprd = gpdf(stemp, ppdf(1,2), ppdf(2,2), ppdf(3,2),
!..     $                ppdf(4,2))


!...Value for potential plant production is now calculated from the
!     equation of a line whose intercept changes depending on water
!     content based on soil type.  The function PPRDWC contains the
!     equation for the line.

	
        pet = pevap(month)
        if (pet .ge. .01) then
          pptprd = (avh2o(1) + prcurr(month) 
     $       + irract)/pet
        else
!...changed the default value to 1.0 since the first sim year
!...will have very low value        
          pptprd = 0.01
!           pptprd = 1.0
        endif

        pptprd = pprdwc(wc,pptprd,pprpts)
        
!...Calculate biof, 
!...= 1 production should be reduced by physical obstruction
	if (bioflg .eq. 1) then
!...Calculate maximum potential effect of standing dead on plant growth
!     (the effect of physical obstruction of litter and standing dead)
	  bioc = stdedc + .1*strucc(SRFC)
	  if (bioc .le. 0.) then
            bioc = .01
          endif

          if (bioc .gt. pmxbio) then
            bioc = pmxbio
          endif
	  bioprd = 1. - (bioc/(biok5+bioc))

!...Calculate the effect of the ratio of live biomass to dead biomass
!     on the reduction of potential growth rate.  the intercept of this 
!     equation ( highest negative effect of dead plant biomass ) is equal
!     to bioprd when the ratio is zero.
	  temp1 = (1. - bioprd)
	  temp2 = temp1*0.75
	  temp3 = temp1*0.25
	  ratlc = aglivc/bioc 
	  if (ratlc .le. 1.0) then
            biof = bioprd+(temp2*ratlc)       
	  else if (ratlc .gt. 1.0 .and. ratlc .le. 2.0) then
       	    biof = (bioprd+temp2)+temp3*(ratlc-1.)
	  else 
            biof = 1.0
          endif
	else
	  biof = 1.0
	endif

!...Restriction on seedling growth
!     sdlng is the fraction that prdx is reduced
        if (aglivc .gt. fulcan) then
          seedl = 0
        endif

        if (seedl .eq. 1) then
 	  sdlng = min(1.0, pltmrf + aglivc*(1-pltmrf) /fulcan)
        else
 	  sdlng = 1.0
        endif

!...Calculate potential production (biomass)
!....the historical change of prdx(1) is consider by using a scaling factor 
!... based on historical wheat grain production
!... in the following, 4270 is maximum attainable prdx(1) and 0.70 indicates in 1990
!.... the prdx(1) has reached about 70% of its maximum (Liu)
!....a similar treatment can be done to the wilting point calculation
!	if (grcurv .eq. 0) then
!	   scale1 = scale
!	else if (grcurv .eq. 1) then
!            scale1 = (500.0 + 1875.0/(1.+1.4*exp(-0.03*
!!...	   scale1 = (500.0 + 1875.0/(1.+6.4*exp(-0.0565*
!     +	      (time-1930.))))/2375.0 * scale
!	else if (grcurv .eq. 2) then
!	   scale1 = (940.0 + 2529.0/(1.+194.0*exp(-0.0756*
!     +	      (time-1900.))))/3469.0 * scale
!	else if (grcurv .eq. 3) then
!	   scale1 = (692.0 + 2734.0/(1.+124.0*exp(-0.1208*
!     +	      (time-1921.))))/3426.0 * scale
!	else if (grcurv .eq. 4) then
!	   scale1 = (1353.0 + 5699.0/(1.+464.0*exp(-0.0736*
!     +	      (time-1902.))))/7052.0 * scale
!	else if (grcurv .eq. 5) then
!	   scale1 = (915.0 + 3354.0/(1.+109.0*exp(-0.0556*
!     +	      (time-1897.))))/4269.0 *scale
!	else if (grcurv .eq. 6) then
!	   scale1 = (1347.0 + 5822.0/(1.+43.0*exp(-0.0885*
!     +	      (time-1931.))))/7169.0 * scale
!	else
!	   scale1 = (869.0 + 2876.0/(1.+50.0*exp(-0.1402*
!     +      (time-1932.))))/3745.0 * scale
!	endif


!        replaced with new crop growth curve provided by Jeremy
!zp, 08/15/2011
!	CV_BRLYY1 CV_CORNGY1 CV_HAYALY1	CV_RICEY1 CV_SYBNY1 CV_WHTWY1 CV_SUGCY1
!c	0.3853	0.235	0.413	0.0852	0.0846	0.2832	0.4044
!k	1.9614	1.2095	0.6681	1.6525	2.1279	1.939	0.6252
!a	8.6459	4.2095	1.5526	2.3209	3.4339	6.6342	0.0575
!b	-0.038	-0.0602	-0.06	-0.0291	-0.0256	-0.0365	-0.09
!d	1960	1960	1960	1955	1955	1955	1960
!Formula is: c + k/(1.+a *exp( b * (time - d)))) * scale

        if (grcurv .eq. 0) then
	   scale1 = scale
	else if (grcurv .eq. 1) then
            scale1 = (0.3853 + 1.9614/(1.+ 8.6459*exp(-0.038*
     +	      (time-1960.))))* scale
	else if (grcurv .eq. 2) then
	   scale1 = (0.235 + 1.2095/(1.+ 4.2095*exp(-0.0602*
     +	      (time-1960.))))* scale
	else if (grcurv .eq. 3) then
	   scale1 = (0.413 + 0.6681/(1.+ 1.5526*exp(-0.06*
     +	      (time-1960.))))* scale
	else if (grcurv .eq. 4) then
	   scale1 = (0.0852 + 1.6525/(1.+ 2.3209*exp(-0.0291*
     +	      (time-1955.))))* scale
	else if (grcurv .eq. 5) then
	   scale1 = (0.0846 + 2.1279/(1.+ 3.4339*exp(-0.0256*
     +	      (time-1955.))))* scale
	else if (grcurv .eq. 6) then
	   scale1 = (0.2832 + 1.939/(1.+ 6.6342*exp(-0.0365*
     +	      (time-1955.))))* scale
	else
           scale1 = (0.4044 + 0.6252/(1.+ 0.0575*exp(-0.09*
     +	      (time-1960.))))* scale
	endif

!        finish growth curve
	   hi_low = himax - 0.0016 * (1995. - 1920.)
	   if (hi_low .lt. 0.10) then
	      hi_low = 0.10
	   endif
	   
	  if (time .lt. 1920.) then
	     hi = hi_low
	  else
	     hi = hi_low + 0.0016 * (time - 1920.)  
	  endif
	
	  if (hi .gt. himax) then
	     hi = himax
	  endif
	  
	if (himax .gt. 0.0) then
	  scale1 = scale1 * hi / himax
	else
	  scale1 = scale1
	endif
	
	pfac = potprd * pptprd * biof * shdmod *
     +           sdlng * co2cpr(CRPSYS) * scale1

!.... Stop the crop inversion using input NPP due to difficulty in inversion     
!....add input to estimate the NPP by just doing one single factor
       if ((npp_input .eq. 1).or.(npp_input .eq. 3)) then
           !print *, 'using input NPP', gpp_eco, prdxcrop
           agprod =  gpp_eco * pfac
!           agprod =  prdxcrop * pfac
       else
	     agprod = prdx(1) * pfac
       endif
       
!       if (prdx(1) < 5 ) then
!           flaginv = 0
!           agprod = prdx(1) * pfac
!       else
!          flaginv = 1 + flaginv
!       endif
!................................................................
       
      pcropcfac = pcropcfac + pfac
        if (debug_flag .eq. 1 ) then
          if (title_flag .ne. 1) then
           write(93, *) 'time agprod potprd ctemp potprd scale1 shdmod
     $   biof sdlng co2cpr cercrp(ab,min) cercrp(ab,max) cercrp cercrp' 
           title_flag = 1
	  endif
        endif
!...Calculate the above ground production according to the prdx value and 
!...Environmental constrains
!...Make sure the NPP is bigger than 0
!           if (cgpp .gt. cresp) then
!             pp = cgpp - cresp
!           else
!             pp = 0.0
!           endif
!           pcropc = pp
       if (npp_input.eq.5) then
!...when using ECLUE as calculation of GPP, 
           if ( prdx(1) .gt. 1000) then
!...C4 crop
             cgpp = ecluegpp(int(time), month) * 1.333
!             print *, "C4 ", cgpp, time
             else
!...C3 crop            
            cgpp = ecluegpp(int(time), month)
           endif
           cresp = cropResp(cgpp)
           tgprod = cgpp - cresp
           agprod = tgprod / ( 1 + rtsh)
           bgprod = agprod * rtsh
	agp = agprod
	bgp = bgprod
       else
!..Not using GPP
!        agprod = prdx(1) * pfac   agprod should be calculated before
	bgprod = agprod * rtsh
	agp = agprod
	bgp = bgprod
	tgprod = agp + bgp
        endif

      else
!(stemp .gt. 0.0 .and. crpgrw .ne. 0)
        tgprod = 0.0
        agp = 0.0
        bgp = 0.0
        pcropc = 0.0
        goto 10
      endif
!...end using ECLUE as input
      
!...Determine if grazing occurs
      if (dograz) then

!...Restrict production due to grazing:
c	grzeff = 0	grazing has no direct effect on production
c	grzeff = 1	linear impact on agp
c	grzeff = 2	quadratic impact on agp and root/shoot ratio
c	grzeff = 3	quadratic impact on root/shoot ratio
c	grzeff = 4	linear impact on root/shoot ratio
c	grzeff = 5	quadratic impact on agp and 
c                       linear impact on root/shoot ratio
c	grzeff = 6	linear impact on agb and root/shoot ratio


        if (grzeff .ne. 0) then
	  if (agp .le. 0.02) then
            agp = 0.02
          endif
	  if (grzeff .eq. 1) then
            agp = (1 - (2.21*flgrem)) * agp
            if (agp .lt. 0.02) then
              agp = 0.02
            endif
            bgp = rtsh * agp
          elseif (grzeff .eq. 2) then
            agp = (1 + 2.6*flgrem - (5.83*(flgrem**2)))*agp
            if (agp .lt. 0.02) then
              agp = 0.02
            endif
            bop = rtsh + 3.05*flgrem  - 11.78*(flgrem**2)
            if (bop .le. 0.01) then
              bop = 0.01
            endif
            bgp = agp * bop
          else if (grzeff .eq. 3) then
            bop = rtsh + 3.05*flgrem  - 11.78*(flgrem**2)
            if (bop .le. 0.01) then
              bop = 0.01
            endif
            bgp = agp * bop
          else if (grzeff .eq. 4) then
            bop = 1 - (flgrem * gremb)
            bgp = agp * bop
          else if (grzeff .eq. 5) then
            agp = (1 + 2.6*flgrem - (5.83*(flgrem**2)))*agp
            if (agp .lt. 0.02) then
              agp = 0.02
            endif
            bop = 1 - (flgrem * gremb)
            bgp = agp * bop
          else if (grzeff .eq. 6) then
            agp = (1 - (2.21*flgrem)) * agp
            if (agp .lt. 0.02) then
              agp = 0.02
            endif
            bop = 1 - (flgrem * gremb)
            bgp = agp * bop
          endif
	  tgprod = agp + bgp
          rtsh = bgp/agp
	endif
      endif


!...Compute the minimum and maximum C/N, C/P, and C/S ratios allowed
!     in plants.
!     Changed grwprc to 2.5*aglivc in calculation of cercrp

10    do 20 iel=1,nelem
        cercrp(IMIN,ABOVE,iel) =
     +          min(pramn(iel,1)+(pramn(iel,2)-pramn(iel,1)) *
     +          2.5 * aglivc / biomax,pramn(iel,2))
        cercrp(IMAX,ABOVE,iel) = 
     +          min(pramx(iel,1)+(pramx(iel,2)-pramx(iel,1)) *
     +          2.5 * aglivc / biomax,pramx(iel,2))

	cercrp(IMIN,BELOW,iel) = prbmn(iel,1)+prbmn(iel,2)*grwprc
        cercrp(IMAX,BELOW,iel) = prbmx(iel,1)+prbmx(iel,2)*grwprc
20    continue

!...If burning occurs, modify C/N ratio of shoots & roots.
      if (dofire(CRPSYS) .or. dofire(SAVSYS)) then
        cercrp(IMIN,ABOVE,N) = cercrp(IMIN,ABOVE,N) + 0.
        cercrp(IMAX,ABOVE,N) = cercrp(IMAX,ABOVE,N) + fnue(1)
        cercrp(IMIN,BELOW,N) = cercrp(IMIN,BELOW,N) + 0.
        cercrp(IMAX,BELOW,N) = cercrp(IMAX,BELOW,N) + fnue(2)
      endif

!...Added effect of co2
      do 30 iel = 1, nelem
  	cercrp(IMIN,ABOVE,iel) = cercrp(IMIN,ABOVE,iel) * 
     +                           co2cce(CRPSYS,IMIN,iel)
	cercrp(IMAX,ABOVE,iel) = cercrp(IMAX,ABOVE,iel) *
     +                           co2cce(CRPSYS,IMAX,iel)
30    continue

!...Update accumulators & compute potential C production
      ptagc = ptagc + agp/2.5
      ptbgc = ptbgc + bgp/2.5
      pcropc = tgprod / 2.5


!...write the monthly to monthly.txt for debugging only
       if (debug_flag  > 0) then
        write (93, 45) time,agprod, pptprd, ctemp, potprd,scale1,shdmod,
     $     biof, sdlng, co2cpr(CRPSYS), cercrp(IMIN,ABOVE,N), 
     $    cercrp(IMAX,ABOVE,N),cercrp(IMIN,BELOW,N),cercrp(IMAX,BELOW,N)
!     $     biof, sdlng, co2cpr(CRPSYS), potprd*pptprd, ctemp, 
!     $     prcurr(month),avh2o(1) 
        endif
45     format (f8.2, 15f8.2)

      return
      end
