
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine simsom()

c...Simulate flow of carbon, nitrogen, phosphorous, and sulfur.
c     This routine is executed each time step.  It calls the decomposition
c     submodel and a producer submodel.  It also includes a bunch of
c     N fixation stuff that needs to be rewritten and put in its own routine.
c
c     Added new local variable FSOL.  Added calls to new function FSFUNC
c     to calculate the amount of mineral P that is in solution.  Added
c     call to new subroutine PSCHEM, which calculates and schedules the
c     Phosophorus and Sulfur flows during decomposition.  Previously
c     this was calculated in the DECOMP routine.  -rm 6/91
c	
!01/23/2009  Add extra variables to save the monthly NEE related variables
!
      include 'comput.inc'
      include 'const.inc'
      include 'dovars.inc'
      include 'fertil.inc'
      include 'isovar.inc'
      include 'ligvar.inc'
      include 'param.inc'
      include 'parfs.inc'
      include 'parfx.inc'
      include 'pheno.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'potent.inc'
      include 'seq.inc'
      include 'site.inc'
      include 't0par.inc'
      include 'timvar.inc'
      include 'wth.inc'
      include 'zztim.inc'
      include 'prof.inc'
      include 'schvar.inc'
      include 'wetland.inc'
!...Function declarations
      real      fsfunc, rtimp, daylen
      external  fsfunc, rtimp, daylen


c...Local variables
      integer   iel, kts, lyr, iso
      real      biof, cancvr, cmn, 
     $          frlech(MAXIEL), fsol, fwdfx, fxbiom,
     $          satm, sirr, stot, tbiom, texeff, tmplen,
     $          wdbmas, wdfnp, wdfxm, wfunc
      real      pre_fsysc, pre_somtc, pre_co2, pre_profco2
! variable for heterotrophic respiration and autotrophic resp
!
! in plot1.inc file
! 01/20/2009


!...Added for flux comparation with NEE and soil respiration 
!...the difference of fsysc compared with NEE
!...the difference of somtc compared with soil respiration
      if ( debug_flag .eq. 2 ) then
          open(unit=96, file="fluxmonthly.txt", status="unknown")
      endif
!      open(unit=95, file="nutrient_mon.txt", status="unknown")      
!...save the current value, will compare after the calculation          
       pre_fsysc = fsysc
       pre_somtc = somtc
       pre_co2 = totco2
c...co2ac is the deep soil co2 release
       pre_profco2 = co2ac
!...Clear the monthly variables for output
!...They include gpp and resp for crop and forest
!...monthly NEE
       fgpp = 0.0
       cgpp = 0.0
       fresp = 0.0
       cresp = 0.0
       sresp = 0.0
       sdresp = 0.0
       nee = 0.0

c...Added below for savanna model (rm)
      if (cursys .eq. SAVSYS ) then
        wdbmas = (fbrchc + rlwodc) * 2.0
        trbasl = wdbmas / (0.001+basfct)
        cancvr = 1 - exp(-0.064 * trbasl)
        if (trbasl .eq. 0) then
          trbasl = 0.1
        endif
      endif

c...Set aminrl for use in routines called from decomp
      do 10 iel = 1, nelem
        if (iel .eq. P) then
          fsol = fsfunc(minerl(1,P), pslsrb, sorpmx)
        else
          fsol = 1.0
        endif
        aminrl(iel) = minerl(1,iel) * fsol
10    continue

!...Determine decomposition factor and initialize accumulators
!     Added CANCVR to argument list.  -rm 6/91
!      print *, "cycle ", month
      call cycle(month, cancvr, wfunc)
      
!...N Fixation
!     Does not take into account the effect of irrigation 
      if (nsnfix .eq. 1 .and. nelem .ge. P) then

!...Compute mineral N:P ratio for N-Fixation (use suface layer only)
!       rnpml1 is used to control soil N-fixation using a regression
!       equation based on Kansas data. This ratio is used only if nelem = 2.
!       rnpml1 is flagged if either minerl(1,1) or minerl(1,2) is zero.

        rnpml1 = minerl(1,N)/minerl(1,P)
     $           *fsfunc(minerl(1,P),pslsrb,sorpmx)

!...Wet-dry fixation of nitrogen -- monthly flow
!...Atmospheric fixation is split between monthly dry fall and
!     wdfnp is the N:P ratio control function for non-symbiotic
!     soil fixation.
!...Both wdfnp and fxbiom are relative functions
!     which vary from 0 to 1.
!...wdfnp computed as a negative natural log of rnpml1
!...symnfx is the symbiotic N fixation by legumes derived from Cole and
!     Heil (1981) using data from Walker et al. 1959.
	if (rnpml1 .eq. 0) then
	  wdfnp = 1.
	else
          wdfnp = min(1., ((-alog(rnpml1))/fxnpb)-.2)
	endif


!...The definitions of fxmca and fxmcb originally refered to water,
!     not biomass. (Alister 11/91)
	tbiom = aglivc+stdedc+strucc(SRFC)
	biof  = fxmca + tbiom * fxmcb
	fxbiom = 1 - biof
        fxbiom = min(1.,fxbiom)
        if (wdfnp .lt. 0 .or. fxbiom .lt. 0 .or. stemp .lt. 7.5) then
          fwdfx = 0.0
        else
          fwdfx = wdfnp * fxbiom
        endif

!...Compute N-fixation for the month


!...Wet fall depending on the monthly precipitation (wdfxma)
        wdfxma = wdfxa *  prcurr(month)/(0.001+prcann)
        wdfxms = fxmxs * fwdfx
        wdfxm  = wdfxma + wdfxms

!...Compute annual N-fixation accumulators for atmosphere and soils
        wdfxas = wdfxas + wdfxms
        wdfxaa = wdfxaa + wdfxma
        call flow(esrsnk(N),minerl(1,N),time,wdfxm)
        nfixac = nfixac+wdfxm

!...Monthly N-fixation based on annual parameters

      else
!...USE PRCURR/PRCANN INSTEAD OF DT
        wdfxms = wdfxs*   prcurr(month)/(0.001+prcann)
        wdfxma = wdfxa*   prcurr(month)/(0.001+prcann)
        wdfxas = wdfxas + wdfxms
        wdfxaa = wdfxaa + wdfxma
        wdfxm = wdfxma + wdfxms
        call flow(esrsnk(N),minerl(1,N),time,wdfxm)
        nfixac = nfixac + wdfxm
      endif

!...Monthly atmospheric S deposition
       if (nelem .eq. S) then
         satm = satmt * prcurr(month) / (0.001+prcann)
         satmac = satmac + satm
         if (doirri) then
           sirr = sirri * irract * 0.01
         else
           sirr = 0
         endif
         sirrac = sirrac + sirr
         stot = satm + sirr
         call flow(esrsnk(S),minerl(SRFC,S),time,stot)
       endif
         
!-----------------------------------------------------------
!        print *, "decomp ", month
!...Decomposition Submodel

!...Determine  whether cultivation occurs this month.
!...Removed this call from DECOMP and put it here -rm 2/91
!...Removed it from here and put it in CYCLE.  -vk 3/91

!...Initialize stream variables for organic leaching (they are
!     monthly output).  -rm 3/92
      do 20 kts = 5, 8
        stream(kts) = 0.0
20    continue
      strm5l = 0.0
      strm5u = 0.0

c...Initialize monthly co2 accumlators (10/92)
      do 25 iso = 1, 2
        st1c2(iso) = 0.0
        st2c2(iso) = 0.0
        mt1c2(iso) = 0.0
        mt2c2(iso) = 0.0
        s11c2(iso) = 0.0
        s21c2(iso) = 0.0
        s2c2(iso)  = 0.0
        s3c2(iso)  = 0.0
25    continue

c...Call decomp routines ntspm times per month.
c...Removed the P and S chemistry from decomp and created the
c     subroutine pschem.  -rm  6/91
      do 40 kts = 1, ntspm
        call decomp(decodt,decsys)
	if (nelem .ge. P) then
          call pschem(decodt)
        endif

c...Update decomposition and nitrogen fixation flows.
!        print *, time, "simsom01: structural: ", strcis(2,1)
        call flowup(time)
!        print *, time, "simsom02: structural: ", strcis(2,1)
        call sumcar
!        print *, time, "simsom03: structural: ", strcis(2,1)

c...aminrl contains the average amount of N, P, and S 
c     available in the top layer for the time period covered by 
c     dt/ntspm.  minerl contains the current value of mineral N,
c     P, and S by layer.
        do 30 iel = 1, nelem
	  if (iel .eq. P) then
            fsol = fsfunc(minerl(SRFC,P), pslsrb, sorpmx)
          else
            fsol = 1.0
          endif
	  aminrl(iel) = (aminrl(iel) + minerl(SRFC,iel)*fsol)/2.0
30      continue
40    continue

      if (debug_flag .eq. 1) then
      print *, "In SIMSOM: after decomp", month, wfunc
      endif

c...Annual co2 accumulators (10/92)
      amt1c2 = amt1c2 + mt1c2(UNLABL) + mt1c2(LABELD)
      amt2c2 = amt2c2 + mt2c2(UNLABL) + mt2c2(LABELD)
      as11c2 = as11c2 + s11c2(UNLABL) + s11c2(LABELD)
      as2c2 = as2c2 + s2c2(UNLABL) + s2c2(LABELD)
      as3c2 = as3c2 + s3c2(UNLABL) + s3c2(LABELD)
      ast1c2 = ast1c2 + st1c2(UNLABL) + st1c2(LABELD)
      as21c2 = as21c2 + s21c2(UNLABL) + s21c2(LABELD)
      ast2c2 = ast2c2 + st2c2(UNLABL) + st2c2(LABELD)
!...Save the surface soil respiration for the cycle (month)
      sresp = mt1c2(UNLABL) + mt1c2(LABELD)
     $        + mt2c2(UNLABL) + mt2c2(LABELD)
     $        + s11c2(UNLABL) + s11c2(LABELD)
     $        + s2c2(UNLABL) + s2c2(LABELD)
     $        + s3c2(UNLABL) + s3c2(LABELD)
     $      + st1c2(UNLABL) + st1c2(LABELD)
     $      + s21c2(UNLABL) + s21c2(LABELD)
     $    + st2c2(UNLABL) + st2c2(LABELD)
!...Volatilization loss of nitrogen as a function of
!     gross mineralization
      volgm = vlossg*gromin(1)
      minerl(SRFC,N) = minerl(SRFC,N) - volgm
      esrsnk(N) = esrsnk(N) + volgm

c...Set frlech to leaching fraction vek june90
c     Compute normal value for frlech.  Recompute in flood routine
c     if flooding occurs.
      texeff = fleach(1) + fleach(2) * sand
      do 50 iel = 1, nelem
        if (iel .eq. P) then
          fsol = fsfunc(minerl(SRFC,P), pslsrb, sorpmx)
        else
          fsol = 1.0
        endif
        frlech(iel) = texeff * fleach(iel+2) * fsol
50    continue
      if (debug_flag .eq. 1) then
      print *, "In SIMSOM: before erod", month, wfunc
      endif
!...Soil erosion
      if (doerod) then
c..        call erosn(psloss,bulkd,edepth,enrich,lhzci,lhze,nelem)
           call soilmove()
      else
        scloss = 0.0
      endif
        if (debug_flag .eq. 1) then
              print *, "In SIMSOM: before fert", month, wfunc
        endif
c...Fertilization option
      if (dofert) then
        do 60 iel = 1, nelem
          esrsnk(iel) = esrsnk(iel)-feramt(iel)
          minerl(SRFC,iel) = minerl(SRFC,iel)+feramt(iel)
          fertot(iel) = fertot(iel) + feramt(iel)
60      continue
      endif

c...Available nutrients
c     tminrl is the total amount of each element available in
c     mineral form.
      do 80 iel = 1, nelem
        tminrl(iel) = 0.
        if (iel .eq. P) then
          fsol = fsfunc(minerl(SRFC,P), pslsrb, sorpmx)
        else 
          fsol = 1.0
        endif
	       
c        do 70 lyr = 1, nlayer
        do 70 lyr = 1, slayers

c...Plants can only uptake from a layer with a positive
c     value, so only the positive layers are summed here.
          if (minerl(lyr,iel) .gt. 0.)  then
     		tminrl(iel) = tminrl(iel) + minerl(lyr,iel) * fsol
          endif
70        continue
80      continue
c...write out tminrl to see how it works
c        write(95, 75) time, tminrl(1), minerl(1,1)
c75      format(f8.2, 3f8.2)


c...Compute the fraction of labile (non-sorbed) P in the surface
c     layer available to plants
      favail(2) = max(favail(4),
     $                min(favail(4) + minerl(SRFC,N)*
     $                   (favail(5) - favail(4)) / (0.0001+favail(6)),
     $                    favail(5)))


c...Add to fallow rain
      if (falprc .eq. 1 .and. .not. dohrvt) then
        prcfal = prcfal + prcurr(month)
      endif


c...Call the producer submodel
      
c...Compute daylength for use in phenology of trees
      tmplen = daylen(month, sitlat)

c...Determine if daylength is increasing or decreasing
      if (tmplen .lt. dayhrs) then
	 hrsinc = .FALSE.
      else
	 hrsinc = .TRUE.
      endif

      dayhrs = tmplen


c...Crop and Forest removal options - moved here from CROP
c     and TREES so the mineral pools are not radically changed
c     between the growth routines. - rm 7/94
      if (cursys .eq. CRPSYS) then
        call crop(time, wfunc)
c...Fire and grazing
        if (dofire(CRPSYS) .or. dograz) then
          call grem()

        endif
      else if (cursys .eq. FORSYS) then
        call trees(cisotf, wfunc)
c...Fire or cutting events
        if (dofire(FORSYS) .or. dotrem) then
          call frem()

        endif

      else if (cursys .eq. SAVSYS) then
        call crop(time, wfunc)
        call trees(cisotf, wfunc)

c...Fire and grazing
        if (dofire(SAVSYS) .or. dograz) then
          call grem()
!          print *, "done with grazing"
        endif
c...Fire or cutting events
        if (dotrem) then
          call frem()
        endif
      endif

      if (debug_flag .eq. 1) then
              print *, "In SIMSOM: before root_fr", month, wfunc
        endif
!....Added by Liu, 10/99
!....update root distribution
       if ( root_flag .ne. 1) then
        call root_fr()
       endif

       if (debug_flag .eq. 1) then
              print *, "In SIMSOM: before soiltemp", month, wfunc
        endif
!.....calculate soil temperature profile
        call soiltemp()
  
!...        print *, "lyrthk(i), lyrrtfr(i), st(i), defacl(i),asmos(i)"
!...        do 888 i=1,10
!...          print *, lyrthk(i), lyrrtfr(i), st(i), defacl(i),asmos(i)
!...888        continue
!         stop                                                                                         
                                                                                                 
!.....
!....calculate 1) abiotic decomsition factor as a function
c       of soil temperature and moisture
c       and 2) anaerobic impact on decomposition
                                                                                                 
        call defact()
                                                                                                 
!       calculate decomposition of SOC in profile
                                                                                                 
      if (debug_flag .eq. 1) then
      print *, "In SIMSOM: before decom_p", month, wfunc
      endif

!...Calculate the deep soil decompostion in all the layers
        do 87 i=1,ntspm
        call decom_p(decodt)
87     continue
!  Add the deep soil resp in to the total decomposition 
        sdresp = co2ac - pre_profco2
        
      if (debug_flag .eq. 1) then
      print *, "In SIMSOM: after decom_p", month, wfunc
      endif

c...Update state variables and accumulators and sum carbon isotopes.
      call flowup(time)
        do 89 i=1, 5
        if (minerl(i,N) .lt. 0.0) minerl(i,N) = 3
89      continue

      
      call sumcar
!... Calculate NEE before harvesting
      if (debug_flag .eq. 1) then
      print *, "In SIMSOM: after sumcar", month, wfunc
      endif
c...Harvest may be performed after updating flows.  Put here for
c     consistency with the Savanna model - moved calls to flowup, 
c     sumcar and harvst from CROP routine to here. -rm 7/94
      if (dohrvt) then
        call harvst(month,pltlig)
      endif

      if (debug_flag .eq. 1) then
      print *, "In SIMSOM: before leach", month, wfunc
      endif
c...Leaching
      call leach(amov, nelem, nlayer, minerl, minlch, frlech, stream,
     $           basef, stormf)

      if (debug_flag .eq. 1) then
      print *, "In SIMSOM: after leach", month, wfunc
      endif
c...Update state variables and accumulators and sum carbon isotopes.
      call flowup(time)
      call sumcar

c...Accumulate leached C,N,P,S
      csrsnk(UNLABL) = csrsnk(UNLABL) + strm5u
      csrsnk(LABELD) = csrsnk(LABELD) + strm5l
      stream(5) = strm5u + strm5l
      do 90 iel = 1, nelem
        esrsnk(iel) = esrsnk(iel) + stream(iel+1) + stream(iel+5)
90    continue

c...Volatilization loss as a function of the mineral n which
c     remains after uptake by plants
       volex = 0.0
      if (minerl(SRFC,N) .gt. 0.0) then
        volex = vlosse*minerl(SRFC,N)*dt
        minerl(SRFC,N) = minerl(SRFC,N) - volex
        esrsnk(N) = esrsnk(N) + volex
      endif
!...Volatilization, 
!      vlogma is the accumulator of volgm nitrification

      volgma = volgma+volgm
      volexa = volexa+volex
!...Add by zp, n2oa is the total of all 3 N gasous emission
!...it is also an accumulator, but volgma is the accumlator through
!...the simulation time
!...A fraction should be used to get the N2O since it's only part of total
!...N loss

      n2oa = n2oa + volgm + volex + volpl
!...Add a factor to increase the CH4 production through
!...plant CH4 production
      if (doflood) then
          if ( cprodc > 0) then
!...herb or rice wetlands
              ch4vege = 0.008 * cprodc * exp(stemp*0.01)*surfwater
          else if (cprodf > 0) then
              ch4vege = 0.01 * cprodf * exp(stemp*0.01)*surfwater
          endif
            ch4soil = ch4prod(tave, som1c(2))
      else
          ch4soil = 0
          ch4vege = 0
      endif

      ch4a = ch4a + ch4soil + ch4vege
!      print *, month, "CH4", ch4soil, ch4vege, ch4a
!      print *, month, "NPP", cprodc, cprodf, cproda
!...Production
      cproda = cproda + cprodc + cprodf
      
c...Net Mineralization
      do 100 iel = 1, nelem

c...Net mineralization for the mineralizing compartments
c...The structural component of litter and the wood compartments
c     are not mineralizers.  They should not be added into cmn or
c     sumnrs.
        cmn = metmnr(SRFC,iel) + metmnr(SOIL,iel) + 
     $        s1mnr(SRFC,iel) + s1mnr(SOIL,iel) +
     $        s2mnr(iel) + s3mnr(iel)
	sumnrs(iel) = sumnrs(iel) + cmn

c...soilnm is net mineralization in the soil.
        soilnm(iel) = soilnm(iel) + s1mnr(SOIL,iel) +
     $                s2mnr(iel) + s3mnr(iel) +
     $                metmnr(SOIL,iel) + strmnr(SOIL,iel) + w3mnr(iel)

c...Total net mineralization
        tnetmn(iel) = tnetmn(iel) + cmn + 
     $                strmnr(SRFC,iel) + strmnr(SOIL,iel) +
     $                w1mnr(iel) + w2mnr(iel) + w3mnr(iel)
100   continue

      if (debug_flag .eq. 1) then
      print *, "In SIMSOM: end", month, wfunc
      endif

c...Compute output variables for printing or plotting.
      call savarp
      if ( debug_flag .eq. 2 ) then
          write(96, 200) aint(time), month, fsysc - pre_fsysc, 
     $             somtc - pre_somtc, totco2 - pre_co2,
     $             co2ac - pre_profco2
      pre_fsysc = fsysc
      pre_somtc = somtc
      pre_co2 = totco2
      pre_profco2 = co2ac
      
      endif
200   format(f8.1, i4, 4f8.2)      
      return
      end
