
c               EROS Data Center
c		Shuguang Liu, 11/99


	subroutine soilerod()


c...Soil is removed from the system.

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
        include 't0par.inc'


c...Local variables
        real      flost, somc, temp
	real temp1, temp2, temp3,temp4,temp5
        
	open(unit=25, file='soilerod.out',status='UNKNOWN')
	open(unit=26, file='soilerod_cum.out',status='UNKNOWN')

c...Compute fraction of soil carbon which is lost considering the enrichment factor
	
	temp = 1.0
	flost = temp*psloss/(10.*lyrden(1)*adep(1)) * enrich

c...Total carbon loss 
	temp1 = som1c(SOIL)*flost
	temp2 = som2c*flost
	temp3 = som3c*flost
	temp4 = metabc(SOIL)*flost
	temp5 = strucc(SOIL)*flost
	scloss = temp1+temp2+temp3+temp4+temp5 
	temp1a = temp1a +temp1
	temp2a = temp2a +temp2
	temp3a = temp3a +temp3
	temp4a = temp4a +temp4
	temp5a = temp5a +temp5
	sclosa = sclosa + scloss
	pslossa = pslossa + psloss

	
	write (25, 10) time, month, psloss*1000.0, scloss/(psloss*10.),scloss,
     $        temp1,temp2,temp3,temp4,temp5
	write (26, 10) time, month, pslossa, scloss/(psloss*10.),sclosa,
     $        temp1a,temp2a,temp3a,temp4a,temp5a
10	format (f10.3, i5, 9f10.3)

	
c...Soil losses from below ground som1, som2, som3, below ground
c     metabolic and structural pools
c...Argument after nelem tells how many layers of som1, som2, or
c     som3 are being modeled.  Only the soil layer is used in soilos.
c                                                       vek 08/91
	
        call soilos(time,nelem,2,flost,som1c(SOIL),som1ci,csrsnk,som1e,
     $              esrsnk)
        call soilos(time,nelem,1,flost,som2c,som2ci,csrsnk,som2e,
     $              esrsnk)
        call soilos(time,nelem,1,flost,som3c,som3ci,csrsnk,som3e,
     $              esrsnk)
        call soilos(time,nelem,2,flost,metabc(SOIL),metcis,csrsnk,
     $              metabe,esrsnk)
        call soilos(time,nelem,2,flost,strucc(SOIL),strcis,csrsnk,
     $              struce,esrsnk)


	return
	end
