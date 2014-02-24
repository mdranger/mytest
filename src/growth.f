
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine growth(cisofr)

      real      cisofr

c...Simulate production for the month.

      include 'comput.inc'
      include 'const.inc'
      include 'dovars.inc'
      include 'fertil.inc'
      include 'monprd.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'potent.inc'
      include 'seq.inc'
      include 'site.inc'
      include 'zztim.inc'
      include 'prof.inc'

c...Function declarations
      real      fsfunc, rtimp
      external  fsfunc, rtimp

c...Local variables
      integer   iel, lyr
      real      agfrac, amt, avefrc(MAXIEL), bgfrac,
     $          calcup, cfrac(CPARTS),
     $          euf(CPARTS), fsol,
     $          gnfrac, rimpct, tm, uptake(4,MAXIEL)

c...Initialize monthly production variables
      mcprd(ABOVE) = 0.0
      mcprd(BELOW) = 0.0
      mcnupt(ABOVE) = 0.0
      mcnupt(BELOW) = 0.0
c      open(unit=95, file="nutrient_mon.txt", status="unknown")
c...Determine actual production values, restricting the C/E ratios
      if (crpgrw .eq. 1 .and. pcropc .gt. 0.0 .and. .not. dosene) 
     $    then

c...Calculate impact of root biomass on available nutrients
        rimpct = rtimp(riint, rictrl, bglivc)

c...Calculate carbon fraction in each part
        cfrac(ABOVE) = agp / (0.0001+tgprod)
        cfrac(BELOW) = 1 - cfrac(ABOVE)

c...Calulate savanna available fractions
        if (cursys .eq. SAVSYS) then
          if (tminrl(N) .gt. 1.5) then
            tm = 1.5

          else
            tm = tminrl(N)

          endif

          gnfrac = exp(-1.664*exp(-.00102*tm*sitpot)*basfc2*trbasl)

c...Bound GNFRAC between 0 and 1
          if (gnfrac .gt. 1.0) then
            gnfrac = 1.0
          elseif (gnfrac .lt. 0.0) then
            gnfrac = 0.0
          endif

	  avefrc(N) = tminrl(N) * gnfrac
        else
  	  avefrc(N) = tminrl(N)
        endif

        avefrc(P) = tminrl(P)
        avefrc(S) = tminrl(S)


	call restrp(nelem,CPARTS,avefrc,cercrp,cfrac,pcropc,rimpct,
     $              crpstg,snfxmx(CRPSYS),cprodc,eprodc,eup,
     $              uptake,elimit,nfix,snfxac(CRPSYS),ptagc)

c...If growth occurs...
        if (cprodc .gt. 0.) then

c...Update accumulators for N, P, and S uptake
          do 20 iel = 1, nelem
            eupacc(iel) = eupacc(iel) + eprodc(iel)
c           eup(ABOVE) is the fraction of element allocated to aboveground
c           eup(BELOW) is the fraction of element allocated to belowground
            eupaga(iel) = eupaga(iel) + eup(ABOVE,iel)
            eupbga(iel) = eupbga(iel) + eup(BELOW,iel)
20        continue

c...C/N ratio for production
          tcnpro = cprodc/(0.00001+eprodc(N))

c...Growth of shoots
          agfrac = agp/tgprod
          mcprd(ABOVE) = cprodc * agfrac
          call csched(mcprd(ABOVE),cisofr,1.0,
     $                csrsnk(UNLABL),aglcis(UNLABL),
     $                csrsnk(LABELD),aglcis(LABELD),
     $                1.0,agcisa)

c...Growth of roots
          bgfrac = 1. - agfrac
          mcprd(BELOW) = cprodc * bgfrac
          call csched(mcprd(BELOW),cisofr,1.0,
     $                csrsnk(UNLABL),bglcis(UNLABL),
     $                csrsnk(LABELD),bglcis(LABELD),
     $                1.0,bgcisa)

c...Actual uptake
          do 40 iel = 1, nelem
	    euf(ABOVE) = eup(ABOVE,iel) /(0.0001+eprodc(iel))
	    euf(BELOW) = eup(BELOW,iel) /(0.0001+eprodc(iel))

c...Take up nutrients from internal storage pool
	    amt = uptake(ESTOR,iel) * euf(ABOVE)
	    call flow(crpstg(iel),aglive(iel),time,amt)
	    amt = uptake(ESTOR,iel) * euf(BELOW)
	    call flow(crpstg(iel),bglive(iel),time,amt)

c...Take up nutrients from soil
            do 30 lyr = 1, nlayer
              if (minerl(lyr,iel) .gt. 0.) then
                fsol = 1.0
                if (iel .eq. P) then
                  fsol = fsfunc(minerl(SRFC,P), pslsrb, sorpmx)
                endif
                calcup = uptake(ESOIL,iel) *
     $             minerl(lyr,iel) * fsol / (0.0001+tminrl(iel))
	        amt = calcup * euf(ABOVE)
                mcnupt(ABOVE) = mcnupt(ABOVE) + amt
	        call flow(minerl(lyr,iel),aglive(iel),time,amt)
	        amt = calcup * euf(BELOW)
                mcnupt(BELOW) = mcnupt(BELOW) +amt
	        call flow(minerl(lyr,iel),bglive(iel),time,amt)
	      endif
30	    continue

c...Take up nutrients from nitrogen fixation
	    if (iel .eq. N .and. nfix .gt. 0) then
	      amt = uptake(ENFIX,iel) * euf(ABOVE)
              call flow(esrsnk(iel),aglive(iel),time,amt)
 	      amt = uptake(ENFIX,iel) * euf(BELOW)
              call flow(esrsnk(iel),bglive(iel),time,amt)
	    endif

c...Take up nutrients from fertilizer
	    if (aufert .ne. 0 .and. uptake(EFERT,iel) .gt. 0.0) then

c  Liu, 16Mar05,  test half amount of fertilizer, will remove 0.5 later
c...Automatic fertilizer added to plant pools
              amt = uptake(EFERT,iel) * euf(ABOVE )
c              amt = uptake(EFERT,iel) * euf(ABOVE ) * 0.5
              call flow(esrsnk(iel),aglive(iel),time,amt)
              amt = uptake(EFERT,iel) * euf(BELOW)
c              amt = uptake(EFERT,iel) * euf(BELOW) * 0.5
              call flow(esrsnk(iel),bglive(iel),time,amt)

c...Automatic fertilizer added to mineral pool
      amt = uptake(EFERT,iel) * (1.0/(0.0001+favail(iel)) - 1.0)  
c      amt = uptake(EFERT,iel) * (1.0/(0.0001+favail(iel)) - 1.0)*0.5  
              fertot(iel) = fertot(iel) + uptake(EFERT,iel) + amt
              call flow(esrsnk(iel),minerl(SRFC,iel),time,amt)
            endif
40        continue
	endif

c...Else no production this month
      else
	cprodc = 0.0
        do 50 iel = 1, nelem
          eprodc(iel) = 0.0
50      continue
        nfix = 0
      endif

      return
      end
