
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine prelim

c...Initialize variables and parameters
 
      include 'comput.inc'
      include 'const.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfs.inc'
      include 'parfx.inc'
      include 'pheno.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'seq.inc'
      include 'site.inc' 
      include 't0par.inc'
      include 'timvar.inc'
      include 'wth.inc'
      include 'zztim.inc'
      include 'prof.inc'
      include 'monprd.inc'
      include 'accu.inc'
      include 'wetland.inc'

c...Function declarations
      real      atanf, tcalc
      external  atanf, tcalc

c...Local variables
      integer   i, iel, iso, lyr, m
      real      dely, delx, fcbd(6), fccl(6), fcin(6), fcom(6), 
     $          fcsa(6), fcsi(6), fcwp(6), ompc, xslope,
     $          textur, tfunc, wfunc, wpbd(6), wpcl(6), wpin(6),
     $          wpom(6), wpsa(6), wpsi(6), wpwp(6), yint
     
c...new variables to calcualte the drainage for each layer
      real lyr_drain(MAXLYR), tempfc

c...swflag lets the model user choose between using actual data 
c     for awilt and afiel or equations from Gupta and Larson (1979) 
c     or Rawls et al (1982).

c     swflag=0 Use actual data
c     swflag=1 Use G&L for both awilt (-15 bar) and afiel (-0.33 bar)
c     swflag=2 Use G&L for both awilt (-15 bar) and afiel (-0.10 bar)
c     swflag=3 Use Rawls for both awilt (-15 bar) and afiel (-0.33 bar)
c     swflag=4 Use Rawls for both awilt (-15 bar) and afiel (-0.10 bar)
c     swflag=5 Use Rawls for afiel (-0.33 bar) and actual data for awilt
c     swflag=6 Use Rawls for afiel (-0.10 bar) and actual data for awilt

c...       swflag   1          2          3        4       5       6
      data fcsa / 0.3075,    0.5018,   -0.20,   -0.30,  -0.19,   0.31/
      data fcsi / 0.5886,    0.8548,    0.0,     0.0,    0.0,    0.0/
      data fccl / 0.8039,    0.8833,    0.36,    0.23,   0.0,    0.0/
      data fcom / 2.208E-03, 4.966E-03, 0.0299,  0.0317, 0.0210, 0.0260/
      data fcbd /-0.1434,   -0.2423,    0.0,     0.0,    0.0,    0.0/
      data fcwp / 0.0,       0.0,       0.0,     0.0,    0.72,   0.41/
      data fcin / 0.0,       0.0,       0.2576,  0.4118, 0.2391, 0.4103/
      data wpsa /-0.0059,   -0.0059,    0.0,     0.0,    0.0,    0.0/
      data wpsi / 0.1142,    0.1142,    0.0,     0.0,    0.0,    0.0/
      data wpcl / 0.5766,    0.5766,    0.50,    0.50,   0.0,    0.0/
      data wpom / 2.228E-03, 2.228E-03, 0.0158,  0.0158, 0.0,    0.0/
      data wpbd / 0.02671,   0.02671,   0.0,     0.0,    0.0,    0.0/
      data wpwp / 0.0,       0.0,       0.0,     0.0,    1.0,    1.0/    
      data wpin / 0.0,       0.0,       0.0260,  0.0260, 0.0,    0.0/
      
      
      
c...Time initializations -  time step is one month
      dt = 1.0/12.0
      time = strtyr
      title_flag = 0
      ftitle_flag = 0
      month = 0
      dayhrs = 0.0
	actlayers = MAXLYR
      prdx_scalar = 1.0
      
c...Allow for time step < 1 month for running decomp
c     ntspm is the number of time steps per month for decomp
c     (read from the fix.100 file)
c     decodt is the time step used in subroutine decomp
      decodt = dt/real(ntspm)
 
c...Initializations
      crpgrw = 0
      seedl = 0
      forgrw = 0
      falprc = 0

c...Initialize volitalization accumulators
      volgma = 0.0
      volexa = 0.0
      volpla = 0.0
!...add by zp, for n2o emission total
      n2oa = 0.0
      
c...Initialize erosion variables
      scloss = 0.0
      sclosa = 0.0
	pslossa = 0.0
	temp1a = 0.0
	temp2a = 0.0
	temp3a = 0.0
	temp4a = 0.0
	temp5a = 0.0
      deadrt(1) = 0.0
c...the following variables have not been initialized and caused
c    a lot of problems.  Liu Feb 2001
	esrsnk(N)=0.0
	esrsnk(P)=0.0
	esrsnk(S)=0.0
      
c.....calculate root distribution and rooting depth
       if (root_flag .ne.1) then
	call root_fr()
       endif

c.....calculate rooting and survival depth
	call rtdpth()

c....initialize C in live roots     
c.... according to Jobbagy and Jackson, 1998?

	do 1 i = 1, MAXLYR
c	    if (i .eq. 1) then
c	      depth = 0.0
c	    else
c	      depth = adep(i) + depth
c	    endif
c   	    rtfr(i) = 0.74*exp(-0.0386*(depth+0.5*adep(i)))*adep(i)/20.0
c.......mcprd(BELOW): monthly C below-ground (top layer) production (GROWTH.F)
c	    if (i .gt. 1) then
 	       livroot(i) = 0.0
 	       deadrt(i) = 0.0
 	       deadcrt(i) = 0.0
		livcroot(i) = 0.0
		livfroot(i) = 0.0
c 	    endif
c   	    root = root + rtfr(i)
c   	    print *, i, rtfr(i), adep(i), livroot(i)	   
1	continue
      
c...initialize harvest accumulators
	strawa = 0.0
	graina = 0.0      
 
c....dead root C accumulator
	deadroot = 0.0
	
c...initialize total NPP
	npp = 0.0
	 
c...Initialize total co2 loss
	ast1 = 0.0
	ast2 = 0.0
	amt1 = 0.0
	amt2 = 0.0
	as11 = 0.0
	as21 = 0.0
	as2 = 0.0
	as3 = 0.0
       totco2 = 0.0
c...Initialize the production factor       
           pforcfac = 1.0
           pcropcfac = 1.0   	
c...Initialize accumulators
      call annacc
      call inprac

c...Open the c14 data file 
      if (labtyp .eq. 1) then
        open(unit=10,file='c14data',status='OLD')
      endif

c...Calculate C,N,P,S in lower horizon soil pools for use as soil
c     replacement with erosion events
      do 3 iso = 1, 2
        lhzci(1,iso) = som1ci(SOIL,iso)*lhzf(1)
        lhzci(2,iso) = som2ci(iso)*lhzf(2)
        lhzci(3,iso) = som3ci(iso)*lhzf(3)
3     continue
      do 5 iel = 1, nelem
        lhze(1,iel) = som1e(SOIL,iel)*lhzf(1)
        lhze(2,iel) = som2e(iel)*lhzf(2)
        lhze(3,iel) = som3e(iel)*lhzf(3)
5     continue

c...Field capacity and wilting point.  Computations based on
c     Gupta and Larson 1979, 'Estimating soil and water retention
c     characteristics from particle size distribution, organic 
c     matter percent and bulk density'. Water Resources Research 15:1633
c     or Rawls et al (1982) 'Estimation of soi water properties'
c     Trans. ASAE ???:1316
c...Field capacity options of -0.1 or -0.33 bar.
c...Wilting point assumed to be water content at -15 bars.
c...Calculate organic matter from initial conditions, ivauto or 
c     or value at the beginning of an extend
c...Note that Gupta and Larson and Rawls use % for texture
c     but values here are fractions.

c	print *, nlayer

      if (swflag .ne. 0) then
        ompc = somsc*1.724/(10000.*bulkd*edepth)
        do 10 lyr = 1, actlayers
          afiel(lyr) =   fcsa(swflag)*sand  + fcsi(swflag)*silt
     $                 + fccl(swflag)*clay  + fcom(swflag)*ompc
     $                 + fcbd(swflag)*bulkd + fcwp(swflag)*awilt(lyr)
     $                 + fcin(swflag)
          awilt(lyr) =   wpsa(swflag)*sand  + wpsi(swflag)*silt
     $                 + wpcl(swflag)*clay  + wpom(swflag)*ompc
     $                 + wpbd(swflag)*bulkd + wpwp(swflag)*awilt(lyr)
     $                 + wpin(swflag)
          ompc = ompc * 0.85
          if (lyr .eq. 1) then
	      depth = 10.0
	    else
	      depth = adep(lyr) + depth
         endif
c...10/24/2008, add new algorithm according to Leo's formula
c...drainage=drain0*(1-1/(1+EXP(-d0*(depth - tile_depth + 40))))/(1-1/(1+EXP(d0*(tile_depth-40))))
c...
c...         print *, 'drain0', drain0, 'coe', drain_coefficient, tile_depth
         lyr_drain(lyr) = drain0 * (1-1/(1+exp(-1*drain_coefficient*
     $   (depth - tile_depth + 40))))
     $   /(1-1/(1+exp(drain_coefficient*(tile_depth-40))))
      

c...Calculate the field capacity
c...Make sure the consider of layer drain can increase
c...the original field capacity 10/24/08
c...modified by Leo's algorithm
c...New field capacity = field capacity + (porosity – field capacity) * (1-drainage)
c...Field capacity should be lower than porosity and higher than wilting point
         if (afiel(lyr) .le. (1.0-lyrden(lyr)/2.65)) then
         tempfc = afiel(lyr) + (1.0-lyrden(lyr)/2.65 - afiel(lyr)) * 
     $   (1 - lyr_drain(lyr)) 
         if (tempfc .gt. afiel(lyr) ) then
           afiel(lyr) = tempfc
         endif
         endif
c        print *,'depth', depth, lyrden(lyr), afiel(lyr), lyr_drain(lyr) 
     
   10   continue
      endif
        
c...Added calculation for water content which will be used to
c     determine plant production in POTGRS. 10-90 -rm
      wc = afiel(1)-awilt(1)

c...Add the initialization of avh2o(1)
c...05/20/2008 zli
      avh2o(1) = 1.0
c...Re-calculate awhc for the first crop (also done in cropin when crops
c     are changed)
      awhc  = 0.0
      do 12 lyr = 1, nlaypg
        awhc = awhc + (afiel(lyr) - awilt(lyr)) * adep(lyr)
12    continue

c...Calculate total water holding capacity 11/91 lh
      twhc = 0.0
      do 15 lyr = 1, actlayers
        twhc = twhc + (afiel(lyr) * adep(lyr))
15    continue

c...Calculate initial asmos based on relative water content (rwcf)
      do 18 lyr = 1, actlayers
        asmos(lyr) = (awilt(lyr) + rwcf(lyr) * 
     +                 (afiel(lyr) - awilt(lyr))) * adep(lyr)
!      print *, "asmos",lyr, asmos(lyr), awilt(lyr),rwcf(lyr), afiel(lyr)
18    continue

c...Computations related to decomposition of soil organic matter
c          Added 08/91   vek
      call predec(sand)

c...Initialize the layer beyond the last one the used for safety
c...commented by Liu Feb 2001
c      do 19 iel = 1, 3
c        minerl(nlayer + 1, iel) = 0.0
c19    continue
      watertank = 0.0
c      asmos(nlayer + 1) = 0.0
c      rwcf(nlayer + 1) = 0.0

c...Intercept for the texture equation of secondary P depends upon
c     pH input.  Note that this code changes the value of a
c     'fixed' parameter (texesp(2))
      if (ph .le. phesp(1)) then
	texesp(2) = phesp(2)
      else if (ph .ge. phesp(3)) then
        texesp(2) = phesp(4)
      else
 	dely = phesp(4) - phesp(2)
	delx = phesp(3) - phesp(1)
	xslope = dely / delx
	yint = phesp(2) - (xslope*phesp(1))
	texesp(2) = (xslope*ph) + yint
      endif

      if (micosm .eq. 0) then
c...Preset array which will contain monthly values of defac
        do 20 i = 1, MONTHS
          defacm(i) = -1.
   20   continue
        adefac = 0.
        defac = 0.
      else
	if (cursys .eq. FORSYS .or. cursys .eq. SAVSYS) then
          call message(' ')
          call message('   Microcosm works for cropping system only.')
c...There is currently no check that only Fallow is chosen
          STOP
        endif
        stemp = mctemp
        tfunc = tcalc(stemp, teff)
        wfunc = 1./(1.+4.*exp(-6.*rwcf(1)))
        defac = tfunc*wfunc

c...Bound defac to >= 0.0 12/21/92
         if (defac .lt. 0.0) defac = 0.0
        do 30 m = 1, MONTHS
          prcurr(m) = mctemp
          prcnxt(m) = mctemp
          defacm(m) = defac
   30   continue
        do 40 lyr = 1,actlayers
          amov(lyr) = 0.0
   40   continue
      endif

c...Effect of soil texture on the microbe decomposition rate
c      eftext = peftxa+peftxb*sand
      do 45 lyr=1, MAXLYR
          eftxtl(lyr) = peftxa+peftxb*lyrsand(lyr)
45	continue
	eftext = eftxtl(1)

c...Compute parameters which control decomposition of som1
c     p1co2 must be computed for surface and soil.   vek  08/91
c...Note that p1co2b(1) must equal 0 because there is no
c     soil texture effect on the surface.
      p1co2(SRFC) = p1co2a(SRFC)
      p1co2(SOIL) = p1co2a(SOIL)+p1co2b(SOIL)*sand
      
c...Decomposition of som1 to som3 is a function of clay content
c             vek june90
      fps1s3 = ps1s3(1) + ps1s3(2) * clay
      fps2s3 = ps2s3(1) + ps2s3(2) * clay
      
      
c************************************************      
c....added by Liu for deep soil layers, 10/99
c.... som1 to som2 is calculated as the difference between 
c	in somdec.f and decom_p.f
      do 50 i = 2, MAXLYR
c....calculate the flow fractions      
        s1co2(i) = p1co2a(SOIL)+p1co2b(SOIL)*lyrsand(i)
        s1s3(i) = ps1s3(1) + ps1s3(2) * lyrclay(i)
        s2s3(i) = ps2s3(1) + ps2s3(2) * lyrclay(i)
50    continue

c.....initialize the accumulators for CO2 emissions and C leaching out
c	of the bottom of the soil profile 
	co2ac = 0.
	lchout = 0.
c************************************************


      if (texepp(1) .eq. 1.0) then

c...Calculate pparmn(2)
c     Include effect of texture; weathering factor should be per year
        textur = clay + silt
        pparmn(2) = 12.0 * atanf(textur, texepp(2), texepp(3),
     +                           texepp(4), texepp(5))
      endif

      if (texesp(1) .eq. 1.0) then

c...Calculate psecmn(2)
c     Include effect of texture
        psecmn(2) = 12.0 * (texesp(2) + texesp(3) * sand)
      endif

c...Save initial values for printing or plotting
      call savarp

c...Clear the flow stack.
      call floclr

      return
      end
