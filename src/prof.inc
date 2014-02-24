
c               Shuguang Liu
c                       All Rights Reserved


c**** Soil profile information for carbon accounting, considering the
c     impact of soil erosion and deposition
c-----the number of layers is fixed at 10
c-----lyrthk(layer): soil thickness of each layer in cm
c-----lyrden(layer): soil bulk density of each layer (g/cm3)
c-----lyrsoc(layer): SOC amount in each layer (g/m2)
c-----lyrsand(layer): sand fraction of each layer 
c-----lyrclay(layer): clay fraction in each layer 
c-----lyrrtfr(layer): root fraction in each layer 
c-----eftxtl(layer): soil texture impact on decomposition
c-----surft(12): monthly soil surface temperature predicted by
c		the original CENTURY
c-----st00(12):monthly soil surface temperature predicted by
c		the new method without adjustification
c-----lyrsocp(10, 3): SOC amount in each layer by pools (g/m2)
c-----co2ac:  accumulator of CO2 released from the whole profile
c	except from the top layer (which is accounted for by the
c	original CENTURY
c-----orglchl(10): leaching of the organic compounds (in DECOM_P)
c-----lchout:  total C leached out of the bottom layer (to streams?)
c-----st(layer): soil temperature of each layer (C)
c-----defacl(layer): decomposition factor for each layer based on
c	soil temperature and moisture 
c-----anerobl(layer): impact of anaerobic condition on decomposition
c....  used to save the soil moisture content at the end of each month for next month
c....  total variables numbers are 

 	common /prof/ lyrthk(10), lyrden(10), lyrsoc(10),
     -         surft(12), st00(12), lyrsand(10), lyrclay(10),
     -         eftxtl(10),lyrsocp(11,3),co2ac, orglchl(10),
     -         lchout, st(10),defacl(10), anerobl(10),deadroot,
     -		lyrrtfr(10), rootdep, survdep, slayers,
     -		rlayers, aroot, sroot, mois(31,10),edratio,
     -         temp1a,temp2a,temp3a,temp4a,temp5a,pslossa

	
	real lyrthk,lyrden,lyrsoc,surft,st00,lyrsand,lyrclay,
     -         eftxtl,lyrsocp,co2ac,orglchl,lchout,st,defacl,
     -	       anerobl,deadroot,lyrrtfr,rootdep, survdep,
     -		aroot, sroot, mois,edratio,
     -         temp1a,temp2a,temp3a,temp4a,temp5a,pslossa
     
        integer   slayers, rlayers

	save /prof/
