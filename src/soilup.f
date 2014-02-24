
c               EROS Data Center
c		Shuguang Liu, 11/99

	subroutine soilup(layer, depth, dth)
	
	integer 	layer
	real 		depth, dth
        
c...Soil is being moved up to the top layer (20 cm).

c....layer: the layer from which soil is moved up to the top soil
c....depth:is the thickness to be moved from the layer
c....dth is the total thickness of soil needed to fill up the TOPLYR

c...psloss is the rate of soil loss (kg/m**2/m)
c...bulkd is the bulk density of the soil (kg/liter)
c...edepth is the depth of the soil (m)
c...enrich is the enrichment factor for SOM losses
c...scloss is the total carbon loss from soil organic
c     matter and below ground litter for this month
c...sclosa is the accumulator for scloss

        include 'chrvar.inc'
        include 'comput.inc'
        include 'const.inc'
        include 'param.inc'
        include 'parfx.inc'
        include 'plot1.inc'
	include 'plot2.inc'
        include 'zztim.inc'
        include 'prof.inc'
        include 'timvar.inc'
        include 'wth.inc'
	include 'ligvar.inc'
	external flow
c...Local variables

        integer   iel, iso, temp
        real      flost, input

c...Compute fraction of soil which is being moved up (loss to the top layer).

	flost = depth / adep(layer) 
	
c	if (time .gt. 1880.) stop
	
c...calculating the soil depth being removed from the top layer
c	according to the amount of soil moved.  
c	The top layer is subsequently being filled by moving an equivalent 
c	thickness of soil from the second layer. 
c	If the second layer is thinner than the soil depth being moved,
c	the third layer is used to fill the rest



c.....adjusting bulk density and texture

                lyrden(1) = (lyrden(1)*(TOPLYR-dth) + 
     $            lyrden(layer)*depth)/TOPLYR
                lyrsand(1) = (lyrsand(1)*(TOPLYR-dth) + 
     $            lyrsand(layer)*depth)/TOPLYR
                lyrclay(1) =  (lyrclay(1)*(TOPLYR-dth) + 
     $            lyrclay(layer)*depth)/TOPLYR          
                    

	
c...Calculate input of organic matter from next soil horizon
c     using an equivalent depth of soil to that eroded
	
        do 30 iso = UNLABL, LABELD 
        
c	to calculate the SOC pools change according to the pool 
c	composition of the top layer
       
          lhzci(1,iso) = som1ci(SOIL,iso)/(som1ci(SOIL,UNLABL) + 
     $		som1ci(SOIL,LABELD))*lyrsocp(layer,1)
          lhzci(2,iso) = som2ci(iso)/(som2ci(UNLABL) + 
     $		som2ci(LABELD))*lyrsocp(layer,2)
	  lhzci(3,iso) = som3ci(iso)/(som3ci(UNLABL) + 
     $		som3ci(LABELD))*lyrsocp(layer,3)
     
          input = flost*lhzci(1,iso)
          lhzcac = lhzcac + input
          call flow(csrsnk(iso),som1ci(SOIL,iso),time,input)
          
          input = flost*lhzci(2,iso)
          lhzcac = lhzcac + input
          call flow(csrsnk(iso),som2ci(iso),time,input)
                 
          input = flost*lhzci(3,iso)
          lhzcac = lhzcac + input
          call flow(csrsnk(iso),som3ci(iso),time,input)
          
          input = flost*deadrt(layer)*metcis(SOIL,iso)/
     $		(metabc(2)+strucc(2))
          lhzcac = lhzcac + input
          call flow(csrsnk(iso),metcis(SOIL,iso),time,input)
          
          input = flost*deadrt(layer)*strcis(SOIL,iso)/
     $   	(metabc(2)+strucc(2))
          lhzcac = lhzcac + input
          call flow(csrsnk(iso),strcis(SOIL,iso),time,input)
                           
30      continue


        do 40 iel = 1, nelem
       
          lhze(1,iel) = som1e(SOIL,iel)/(som1ci(SOIL,UNLABL) + 
     $		som1ci(SOIL,LABELD))*lyrsocp(layer,1)
          lhze(2,iel) = som2e(iel)/(som2ci(UNLABL) + 
     $		som2ci(LABELD))*lyrsocp(layer,2)
	  lhze(3,iel) = som3e(iel)/(som3ci(UNLABL) + 
     $		som3ci(LABELD))*lyrsocp(layer,3)
	  
          input = flost*lhze(1,iel)
          lhzeac(iel) = lhzeac(iel) + input
          call flow(esrsnk(iel),som1e(SOIL,iel),time,input)
          
          input = flost*lhze(2,iel)
          lhzeac(iel) = lhzeac(iel) + input
          call flow(esrsnk(iel),som2e(iel),time,input)
          
          input = flost*lhze(3,iel)
          lhzeac(iel) = lhzeac(iel) + input
          call flow(esrsnk(iel),som3e(iel),time,input)
          
          temp = struce(2,iel)/(metabe(2,iel)+struce(2,iel))        
          input = flost*deadrt(layer)*temp
          lhzeac(iel) = lhzeac(iel) + input
          call flow(esrsnk(iel),struce(SOIL,iel),time,input)
          
          temp = metabe(2,iel)/(metabe(2,iel)+struce(2,iel))
          input = flost*deadrt(layer)*temp
          lhzeac(iel) = lhzeac(iel) + input
          call flow(esrsnk(iel),metabe(SOIL,iel),time,input)
          
40      continue

	lyrsocp(layer,1) = lyrsocp(layer,1) - 
     $		flost*(lhzci(1,UNLABL) + lhzci(1,LABELD))
	lyrsocp(layer,2) = lyrsocp(layer,2) - 
     $		flost*(lhzci(2,UNLABL) + lhzci(2,LABELD))
	lyrsocp(layer,3) = lyrsocp(layer,3) - 
     $		flost*(lhzci(3,UNLABL) + lhzci(3,LABELD))
	
	deadrt(layer) = deadrt(layer) * (1.0 - flost)
	
	if (bglivc .gt. 0.) then
	  bglcis(1) = bglcis(1) + bglcis(1)/(bglcis(1)+bglcis(2)) 
     $        * livroot(layer) * flost
	  bglcis(2) = bglcis(2) + bglcis(2)/(bglcis(1)+bglcis(2)) 
     $        * livroot(layer) * flost
        else
          bglcis(1) = livroot(layer) * flost
        endif
        
	livroot(layer) = livroot(layer) * (1.0 - flost)
	
	lyrsoc(layer)=lyrsocp(layer,1)+lyrsocp(layer,2)
     $	           +lyrsocp(layer,3) + deadrt(layer)
	
c	print *

         if (lyrsoc(layer) .lt. 0.0001) then
           lyrsoc(layer) = 0.0001
         endif 
         
      

	return
	end
