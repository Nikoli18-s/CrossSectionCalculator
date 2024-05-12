      PROGRAM CONVN
      
      IMPLICIT NONE
      
      character*14 infile,output
      infile = 'Kr83.cni'
      output = 'Kr83.cno'
      
      CALL ICCCON(infile, output)
      
      END
      
      SUBROUTINE DRINF(E,U,NT,NS,NSB,HB,L,AJ,G,F)
      IMPLICIT REAL*8(A-H,O-Z)
C     pPOgPAMMA iHTEgPiPOBAHiq YP. diPAKA
C     PAdiAlxHOgO dlq E.LT.MC**2
C     E(pOlHAq)=MC**2-E
C     iHTEgPiPOBAHiE BEdETCq OT X(NT,NS)
C     dO X(1,NSB), NSB.LE.NS
C     G-bOlx{Aq,F(NT,NS)-MAlAq KOMpOHEHTy
C     HOPMiPOBKA: G(1,NSB)=1.
C     U(NT,NS) TAblicA -V(X)*X,
C**** U(1,1) COOTBETCTBYET X=0
C     G(1,1) COOTBETCTBYET 0=X
      DIMENSION U(NT),G(NT),F(NT),Q1(4),Q2(4)
      DATA CS/137.0388D0/,CS2/274.0776D0/
      DM=1.D-30
      XN=2.D0**(NS-1)
      HN=XN*HB
      XN=HB*(NT*(XN-1.D0)+(NT-1)*XN)
      X=XN
      DC=1.D0/CS
      EDC=E*DC
      H2=-HN*0.5D0
      H=-HN
      IU=NT*NS
      G(IU)=1.D-40
      AK=(AJ+0.5D0)*(L-AJ)*2.D0
      EDC2=CS2-EDC
      ALF=DSQRT(EDC*EDC2)
      DX=1.D0/X
      V1=U(IU)*DX*DC
      A=EDC2+V1
      B=EDC-V1
      F(IU)=G(IU)*(AK*DX-ALF)/A
      DO 1 I=1,3
      X1=X+H2
      DX1=1.D0/X1
      V12=0.125D0*(3.D0*U(IU)+6.D0*U(IU-1)-U(IU-2))
     **DX1*DC
      AKD=AK*DX
      AK1=(A*F(IU)-AKD*G(IU))*H
      AM1=(B*G(IU)+AKD*F(IU))*H
      A=EDC2+V12
      B=EDC-V12
      AKD=AK*DX1
      Y=G(IU)+AK1*0.5D0
      Z=F(IU)+AM1*0.5D0
      AK2=(A*Z-AKD*Y)*H
      AM2=(B*Y+AKD*Z)*H
      Y=G(IU)+AK2*0.5D0
      Z=F(IU)+AM2*0.5D0
      AK3=(A*Z-AKD*Y)*H
      AM3=(B*Y+AKD*Z)*H
      Y=G(IU)+AK3
      Z=F(IU)+AM3
      X=X+H
      DX=1.D0/X
      AKD=AK*DX
      V1=U(IU-1)*DX*DC
      A=EDC2+V1
      B=EDC-V1
      AK4=(A*Z-AKD*Y)*H
      AM4=(B*Y+AKD*Z)*H
      IU=IU-1
      G(IU)=G(IU+1)+(AK1+AK2+AK2+AK3+AK3+AK4)/6.D0
      F(IU)=F(IU+1)+(AM1+AM2+AM2+AM3+AM3+AM4)/6.D0
C**** pPigOTOBili HA~AlxHyE zHA~EHiq dlq
C**** ~ETyPEXTO~E~HOj CXEMy
      Q1(I)=AK1/H
      Q2(I)=AM1/H
    1 CONTINUE
      P21=0.D0
      P11=0.D0
      C21=0.D0
      C11=0.D0
      JMAX=NT-4
      A2=Q1(1)
      A3=Q1(2)
      A4=Q1(3)
      A5=A*F(IU)-AKD*G(IU)
      B2=Q2(1)
      B3=Q2(2)
      B4=Q2(3)
      B5=B*G(IU)+AKD*F(IU)
      Y2=G(IU+1)
      Y3=G(IU)
      Z2=F(IU+1)
      Z3=F(IU)
      H3=H/3.D0
      H9=H/96.D0
      C1=928.D0/1037.D0
      C2=109.D0/1037.D0
      W1=1.D0/864.D0
      W2=325.D0*W1
      W1=W1*539.D0
      W3=1.D0/46080.D0
      W4=W2/2880.D0
      T1=83.D0/64.D0
      T2=1.D0/69120.D0
      T3=T1/1080.D0
      IUMX=NT*NS
      DO 2 I=1,JMAX
      X=X+H
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      Y1=Y2
      Y2=Y3
      Z1=Z2
      Z2=Z3
      P12=Y1+H3*(8.D0*A4-5.D0*A3+4.D0*A2-A1)
      P22=Z1+H3*(8.D0*B4-5.D0*B3+4.D0*B2-B1)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      DX=1.D0/X
      IU=IU-1
      V=U(IU)*DX*DC
      AKD=AK*DX
      A=EDC2+V
      B=EDC-V
      FZ1=A*AM2-AKD*AM1
      FZ2=B*AM1+AKD*AM2
      C12=1.75D0*Y2-0.75D0*Y1+H9*(39.D0*FZ1+37.D0*A4-59.D0*A3+7.D0*A2)
      C22=1.75D0*Z2-0.75D0*Z1+H9*(39.D0*FZ2+37.D0*B4-59.D0*B3+7.D0*B2)
      C11=C12
      P11=P12
      C21=C22
      P21=P22
      Y3=C11+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      A5=A*Z3-AKD*Y3
      B5=B*Y3+AKD*Z3
      G(IU)=Y3
      F(IU)=Z3
    2 CONTINUE
      IF(DABS(Y3).LT.1.D+30) GO TO 102
      A2=A2*DM
      A1=A1*DM
      A3=A3*DM
      A4=A4*DM
      A5=A5*DM
      B1=B1*DM
      B2=B2*DM
      B3=B3*DM
      B4=B4*DM
      B5=B5*DM
      Z1=Z1*DM
      Z2=Z2*DM
      Z3=Z3*DM
      Y1=Y1*DM
      Y2=Y2*DM
      Y3=Y3*DM
      C11=C11*DM
      C21=C21*DM
      P11=P11*DM
      P21=P21*DM
      DO 3 J1=IU,IUMX
      G(J1)=G(J1)*DM
    3 F(J1)=F(J1)*DM
  102 CONTINUE
      JNS=NS-NSB
      IF(JNS) 4,5,6
C   4 PRINT 7,NS,NSB
    4 WRITE(2, 7) NS,NSB
    7 FORMAT(1X,119(1H-)/10X,
     *'O{ibKA pAPAMETPOB: NS=',I2,'NSB=',I2/
     *1X,119(1H-))
      STOP
    6 CONTINUE
      DO 8 J=1,JNS
      DO 43 L2=1,2
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      Y1=Y2
      Y2=Y3
      Z1=Z2
      Z2=Z3
      P12=Y1+H3*(8.D0*A4-5.D0*A3+4.D0*A2-A1)
      P22=Z1+H3*(8.D0*B4-5.D0*B3+4.D0*B2-B1)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      X=X+H
      DX=1.D0/X
      IU=IU-2
      V=U(IU)*DX*DC
      AKD=AK*DX
      A=EDC2+V
      B=EDC-V
      FZ1=A*AM2-AKD*AM1
      FZ2=B*AM1+AKD*AM2
      C12=1.75D0*Y2-0.75D0*Y1+H9*(39.D0*FZ1+37.D0*A4-59.D0*A3
     *+7.D0*A2)
      C22=1.75D0*Z2-0.75D0*Z1+H9*(39.D0*FZ2+37.D0*B4-59.D0*B3
     *+7.D0*B2)
      C11=C12
      P11=P12
      C21=C22
      P21=P22
      Y3=C11+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      G(IU)=Y3
      F(IU)=Z3
      A5=A*Z3-AKD*Y3
      B5=B*Y3+AKD*Z3
      YZ=W3*(4137.D0*A5+22342.D0*A4-4118.D0*A3+1167.D0*A2-163.D0*A1)
      ZZ=W3*(4137.D0*B5+22342.D0*B4-4118.D0*B3+1167.D0*B2-163.D0*B1)
C***
      YZ=YZ-W4*(1274.D0*A5+2044.D0*A4-516.D0*A3+154.D0*A2-22.D0*A1)
      ZZ=ZZ-W4*(1274.D0*B5+2044.D0*B4-516.D0*B3+154.D0*B2-22.D0*B1)
C***
      YZ=W1*Y2+W2*Y3+H*YZ
      ZZ=W1*Z2+W2*Z3+H*ZZ
      X1=X-H2
      DX=1.D0/X1
      V=U(IU+1)*DX*DC
      AKD=AK*DX
      A=EDC2+V
      B=EDC-V
      FZ1=A*ZZ-AKD*YZ
      FZ2=B*YZ+AKD*ZZ
      YZ=T2*(39.D0*A2-472.D0*A3+16398.D0*A4+20864.D0*FZ1-2352.D0*A5)
      ZZ=T2*(39.D0*B2-472.D0*B3+16398.D0*B4+20864.D0*FZ2-2352.D0*B5)
C***
      YZ=YZ+T3*(-A3+189.D0*A4+704.D0*FZ1+189.D0*A5)
      ZZ=ZZ+T3*(-B3+189.D0*B4+704.D0*FZ2+189.D0*B5)
C***
      YZ=Y2+T1*(Y2-Y3)+H*YZ
      ZZ=Z2+T1*(Z2-Z3)+H*ZZ
C***
      AZ=A*ZZ-AKD*YZ
      BZ=B*YZ+AKD*ZZ
      G(IU+1)=YZ
      F(IU+1)=ZZ
      Q1(L2)=AZ
      Q2(L2)=BZ
   43 CONTINUE
      A1=A3
      B1=B3
      A2=Q1(1)
      B2=Q2(1)
      A3=A4
      B3=B4
      A4=AZ
      B4=BZ
      Y2=YZ
      Z2=ZZ
      H=H*0.5D0
      H2=H2*0.5D0
      H9=H9*0.5D0
      H3=H3*0.5D0
      C11=0.D0
      P21=0.D0
      P11=0.D0
      C21=0.D0
      DO 10 I=5,NT
      X=X+H
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      Y1=Y2
      Y2=Y3
      Z1=Z2
      Z2=Z3
      P12=Y1+H3*(8.D0*A4-5.D0*A3+4.D0*A2-A1)
      P22=Z1+H3*(8.D0*B4-5.D0*B3+4.D0*B2-B1)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      DX=1.D0/X
      IU=IU-1
      V=U(IU)*DX*DC
      AKD=AK*DX
      A=EDC2+V
      B=EDC-V
      FZ1=A*AM2-AKD*AM1
      FZ2=B*AM1+AKD*AM2
      C12=1.75D0*Y2-0.75D0*Y1+H9*(39.D0*FZ1+37.D0*A4-59.D0*A3+7.D0*A2)
      C22=1.75D0*Z2-0.75D0*Z1+H9*(39.D0*FZ2+37.D0*B4-59.D0*B3+7.D0*B2)
      C11=C12
      P11=P12
      C21=C22
      P21=P22
      Y3=C11+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      A5=A*Z3-AKD*Y3
      B5=B*Y3+AKD*Z3
      G(IU)=Y3
      F(IU)=Z3
   10 CONTINUE
      IF(DABS(Y3).LT.1.D+30) GO TO 8
      A1=A1*DM
      A2=A2*DM
      A3=A3*DM
      A4=A4*DM
      A5=A5*DM
      B1=B1*DM
      B2=B2*DM
      B4=B4*DM
      B3=B3*DM
      B5=B5*DM
      Z1=Z1*DM
      Z2=Z2*DM
      Z3=Z3*DM
      Y1=Y1*DM
      Y2=Y2*DM
      Y3=Y3*DM
      C11=C11*DM
      C21=C21*DM
      P11=P11*DM
      P21=P21*DM
      DO 11 J1=IU,IUMX
      G(J1)=G(J1)*DM
   11 F(J1)=F(J1)*DM
    8 CONTINUE
    5 CONTINUE
      RETURN
      END
      SUBROUTINE DRDIS(EN,N,L,AJ,HN,HS,HB,
     *NT,NS,G,F,U,E,EPS)
       IMPLICIT REAL*8(A-H,O-Z)
C***  CM.ERDREM (pPEpPiHT)
      COMMON /BLDRDS/NQG
      DIMENSION G(NT,NS),F(NT,NS),U(NT,NS)
      EPSBF=1.D-4
      JMETK=1
  250 CONTINUE
C***  pPEdpOlAgAETCq ~TO U(I,J)=V(X(I,J))
C***  U(1,1)-X=0
    1 CONTINUE
      ELN=1.D0
      I=N-L+1
      J=I/2
      I=I-J-J
      IF(I)801,802,801
  801 ELN=-1.D0
  802 CONTINUE
C***  OpPEdElEHiE NSB (PAdiYC C{iBKi)
      DO 555 J=1,NS
      LQ=NS+1-J
      AW=2.D0**(LQ-1)
      DO 556 I=1,NT
      K=NT+1-I
      X=HB*((AW-1.D0)*NT+(K-1)*AW)
      RN=EN-U(K,LQ)/X
      IF(RN)557,557,556
  556 CONTINUE
  555 CONTINUE
    6 CONTINUE
      NSB=NS
      GO TO  5
C 370 PRINT 371,N,L,AJ
  370 WRITE(2,371) N,L,AJ
  371 FORMAT(10X,'DIRDIS: TAKOgO YPOBHq HET'
     *,': NLJ=',2I5,F5.1)
      RETURN
  557 CONTINUE
      NSB=LQ+1
      IF(NSB.GT.NS) GO TO 6
    5 CONTINUE
      NSB1=NSB-1
      GO TO (200,251),JMETK
  200 E=EN
      H=HN
      AW=1.D0
      NTQ=NT-1
      NQ=1
      RN1=DSQRT(E+E)
      RN1=12.D0/RN1/HB
  506 RN=NT*(AW-1.D0)+NTQ*AW
      IF(RN.GE.RN1) GO TO 505
      NQ=NQ+1
      AW=AW+AW
      GO TO 506
  505 IF(NQ.GE.NS) NQ=NS
      CALL DRZER(E,U,NT,NQ,HB,L,AJ,G,F)
      FE=G(NT,NQ)*ELN
      IF(FE)803,601,805
  803 EA=E
      FA=FE
      NA=NQG
      EB=E+H
  809 CALL DRZER(EB,U,NT,NQ,HB,L,AJ,G,F)
      FB=G(NT,NQ)*ELN
      NB=NQG
      IF(FB)806,807,510
  807 E=EB
      GO TO 601
  806 EA=EB
      FA=FB
      NA=NB
      EB=EA+H
      GO TO 809
  805 EB=E
      FB=FE
      NB=NQG
      EA=EB-H
  607 IF(EA)605,605,606
  605 H=H*0.5D0
      IF(H)701,370,701
  701 EA=EA+H
      GO TO 607
  606 CONTINUE
      CALL DRZER(EA,U,NT,NQ,HB,L,AJ,G,F)
      FA=G(NT,NQ)*ELN
      NA=NQG
      IF(FA)510,811,812
  811 E=EA
      GO TO 601
  812 E=EA
      FE=FA
      NQG=NA
      GO TO 805
C***  HA{li pPEdB. E.
  510 CONTINUE
  514 S=10.D0**(NB-NA)
      E=EA-FA*(EA-EB)/(FA-FB*S)
      CALL DRZER(E,U,NT,NQ,HB,L,AJ,G,F)
      FR=G(NT,NQ)
      FR=FR*ELN
      IF(FR/FA)525,601,521
  525 IF((EB-E)/E.LT.EPSBF) GO TO 526
      EB=E
      FB=FR
      NB=NQG
      GO TO 514
  521 IF((E-EA)/E.LT.EPSBF) GO TO 526
      EA=E
      FA=FR
      NA=NQG
      GO TO 514
  601 CONTINUE
  526 CONTINUE
      H=E*EPSBF
C****pPEdBAPiTElxHO HA{li E.
      CALL DRZRE(E,U,NT,NSB1,HB,L,AJ,G,F)
      CALL DRINF(E,U,NT,NS,NSB,HB,L,AJ,G,F)
      FQ=DSQRT(G(1,1)**2+F(1,1)**2)
      FQ=FQ*DSQRT(G(1,NSB)**2+F(1,NSB)**2)
      FA=FQ
      FA=(G(1,1)*F(1,NSB)-F(1,1)*G(1,NSB))/FA
  400 CONTINUE
      EA=E
      EB=E+H
   13 CALL DRZRE(EB,U,NT,NSB1,HB,L,AJ,G,F)
      CALL DRINF(EB,U,NT,NS,NSB,HB,L,AJ,G,F)
      FQ=DSQRT(G(1,1)**2+F(1,1)**2)
      FQ=FQ*DSQRT(G(1,NSB)**2+F(1,NSB)**2)
      FB=FQ
      FB=(G(1,1)*F(1,NSB)-F(1,1)*G(1,NSB))/FB
      IF(FB) 822,825,822
  825 E=EB
      GO TO 401
  824 EA=EB
      EB=EB+H
      FA=FB
      GO TO 13
  823 CONTINUE
  822 IF(FA/FB) 10,401,840
  840 IF(FA/FB.GT.1.D0) GO TO 824
      EB=EA
      FB=FA
      EA=EA-H
      CALL DRZRE(EA,U,NT,NSB1,HB,L,AJ,G,F)
      CALL DRINF(EA,U,NT,NS,NSB,HB,L,AJ,G,F)
      FQ=DSQRT(G(1,1)**2+F(1,1)**2)
      FQ=FQ*DSQRT(G(1,NSB)**2+F(1,NSB)**2)
      FA=FQ
      FA=(G(1,1)*F(1,NSB)-F(1,1)*G(1,NSB))/FA
      GO TO 822
   10 CONTINUE
      INDE=1
  550 EP=E
      E=(EA+EB)*0.5D0
      CALL DRZRE(E,U,NT,NSB1,HB,L,AJ,G,F)
      CALL DRINF(E,U,NT,NS,NSB,HB,L,AJ,G,F)
      FQ=DSQRT(G(1,1)**2+F(1,1)**2)
      FQ=FQ*DSQRT(G(1,NSB)**2+F(1,NSB)**2)
      FR=FQ
      FR=(G(1,1)*F(1,NSB)-G(1,NSB)*F(1,1))/FR
      IF(FR/FA)530,100,531
  530 EB=E
      FB=FR
      GO TO 14
  531 EA=E
      FA=FR
   14 E=EA-FA*(EA-EB)/(FA-FB)
      CALL DRZRE(E,U,NT,NSB1,HB,L,AJ,G,F)
      CALL DRINF(E,U,NT,NS,NSB,HB,L,AJ,G,F)
      FQ=DSQRT(G(1,1)**2+F(1,1)**2)
      FQ=FQ*DSQRT(G(1,NSB)**2+F(1,NSB)**2)
      FR=FQ
      FR=(G(1,1)*F(1,NSB)-G(1,NSB)*F(1,1))/FR
      GO TO (540,541),INDE
  540 INDE=2
      GO TO 550
  541 IF(DABS(EP-E)/E.LT.EPS) GO TO 100
      IF(FR/FA.GT.0.D0) GO TO 21
      IF(FR.EQ.0.D0) GO TO 100
      EB=E
      FB=FR
      GO TO 550
   21 CONTINUE
      EA=E
      FA=FR
      GO TO 550
  100 CONTINUE
  401 CONTINUE
      EI=G(1,1)/G(1,NSB)
      G(1,1)=0.D0
      F(1,1)=0.D0
      DO 22 J=NSB,NS
      DO 22 I=1,NT
      G(I,J)=EI*G(I,J)
   22 F(I,J)=EI*F(I,J)
      CALL ZRDIRD(G,NT,NS,MG,IG,JG)
      MG=MG-N+L
      IF(MG)31,32,33
   31 EN=EN-HS
      GO TO 200
   33 EN=EN+HS
      GO TO 200
   32 CONTINUE
      S=0.D0
      SI=0.D0
      H=HB
      DO 40 J=1,NS
      DO 41 I=2,NT,2
   41 SI=SI+4.D0*(G(I,J)**2+F(I,J)**2)
      DO 42 I=3,NT,2
   42 SI=SI+2.D0*(G(I,J)**2+F(I,J)**2)
      YW=0.D0
      IF(J.NE.NS) YW=G(1,J+1)**2+F(1,J+1)**2
      S=S+H/3.D0*(SI+G(1,J)**2+F(1,J)**2+YW)
      SI=0.D0
   40 H=H+H
      S=1.D0/DSQRT(S)
      DO 44 J=1,NS
      DO 44 I=1,NT
      G(I,J)=G(I,J)*S
   44 F(I,J)=F(I,J)*S
      RETURN
      ENTRY DRDSI(EN,N,L,AJ,HN,HS,HB,NT,NS,G,F,U,E,EPS)
      JMETK=2
      GO TO 250
  251 E=EN
      CALL DRZRE(E,U,NT,NSB1,HB,L,AJ,G,F)
      CALL DRINF(E,U,NT,NS,NSB,HB,L,AJ,G,F)
      GO TO 100
      END
      SUBROUTINE WRGD(G,NT,NSM,GA,NAR,NS,HB)
C
C     TRANSFORMATION OF DISCRETE TABLES
C     TO CONTINUOUS TSBLES
C
C     G(NT,NST)- Table for discrete function
C     GA(NA)- Transformed function
C     NAR - The number of equal steps
C     NS - The number of transition to equal steps
C     HB - Initial step
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION G(NT,NSM),GA(NAR)
C
      HM=HB*2.D0**(NS-1)
      RM=HB*NT*(HM/HB-1.D0)+(NT-1.D0)*HM
C
      IF(NAR-1) 6,6,5
    5 NAQ=NAR
        GA(2)=RTAB(G,NT,NSM,NS,HM,RM,1)
      DO 4 I=3,NAQ
    4   GA(I)=RTAB(G,NT,NSM,NS,HM,RM,2)
    6  RETURN
      END
      
      SUBROUTINE ICCCON(infile, output)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C     Program for start conversion probability/per electron
C
C       This program can be run after RHFS calculations
C
C     Entry:
C
C       Format directed input consits of
C
C       1. Char*14 File name for selfconcistent field.
C       2. Char*2  Symbol name of element Calculated
C       3. EPS     The accuracy
C       4. LTRW(2) The calculated multipole of transition
C       5. NOBW    The number of shells for conversion
C       6. NW,ISW  The main quantum and orbital numbers (NOBW)
C       7. NTEW    The number of energy transitions
C       8. EMINW,EMAXW Minimun and maximum energy of transition
C
C     All the results are in atomic units
C
C
C     By~iClEHiE |lEKTPOHHyX fAKTOPOB
C     BEPOqTHOCTi KOHBEPCii (HA OdiH |lEKTPOH)
C
C     MOdElx:
C   A)bEz pPOHiKHOBEHiq
C   b)BCE |lEKTPOHy, BKl`~Aq ulETA`}ij,
C     B OdHOM pOlE (HFSL)
C
C
C
      DIMENSION EOB(100),QOB(100),
     *N(40),E(100),Q(100),
     *ET(201),WT(201),EINW(10)
C
        PARAMETER (MASDIM=4020)
      COMMON /ICCA1/ VFBG(MASDIM),VFBF(MASDIM)
      COMMON /ICCA2/ VFCG(MASDIM),VFCF(MASDIM)
      COMMON /ICCA3/ VJL(MASDIM),VNL(MASDIM)
      COMMON /ICCA4/ VJL1(MASDIM),VNL1(MASDIM)
      COMMON  /BLVTAB/U(1020),AV,NT,NS,HB
      COMMON /NUMBF/ NUMBF
      DIMENSION LTRW(2),NW(30),WWE(30,201),EBPR(201),QCONV(30)
        REAL EMINW,EMAXW
        character*3 month(12)
C
C
      COMMON /BLNSI/NSI
        COMMON/ERNREM/ P,SM,HM,RM,RP,HINF
c       3.03.92         Tkalya said to modify
c       Rref    - Radius of sphere
c       Bref    - sqrt from coefficient of Reflection
      common/refl/ rref, bref
        real*4 ft(2)
        character*14 infile,oufile,output
        character*24 fldate
        character*2 trtype,MEM,MEE,IST(9),IS(40),ISW(30),IQ,NMEL
C
      DATA MEM/'M '/,MEE/'E '/
      DATA IST/'S+','P-','P+','D-','D+','F-','F+','G-','G+'/

        DATA month /'Jan','Feb','Mar','Apr','May','Jun',
     *            'Jul','Aug','Sep','Oct','Nov','Dec'/

c       This for C host======
c       call dtime(ft)
c       call fdate(fldate)
c       ==============
c       CALL GETTIM (ihr0,imin0,isec0,ith0)
C       CALL GETDAT (iyear,imon,iday)
        WRITE (fldate,1090) ihr0,imin0,isec0
 1090   format(I2,1X,A3,1X,I4,5X,I2,':',I2,':',I2)
C
      NL=1
      EPS=1.D-5
        PI=3.14159
C
        OPEN(1, file=infile)
        
        read(1,1005) oufile
        read(1,1007) NMEL
        READ(1,*) EPS
        READ (1,*) LTRW
        READ (1,*) NOBW
        DO 10 I=1,NOBW
  10    READ (1,1030) NW(I),ISW(I)
        READ (1,*) NTEW
        READ (1,*) (EINW(I),I=1,NTEW)
C   E.Tkalya:   rref - reflection radius,
C               bref - reflection coefficient
C       read (5,*) rref, bref
        rref=1.0
        bref=0.0
        bref=sqrt(bref)
C     E.V.Tkalia,   NUMBF - number of wave function values to print
        READ (1,*) NUMBF
        
        CLOSE(1)
        
        OPEN(UNIT=NL,FILE=OUFILE,FORM='UNFORMATTED')
        READ (NL) Z,AN,ZZ,NT,NS,HB,E,Q,U
        CLOSE(NL)
        
        CONTINUE
C
C
C      J=1

      OPEN(2, FILE=output)
      
      NTQ=NT*NS
        NA=MASDIM-1020                  !Max number
        NAR=NA          !Real number
C
  103 CONTINUE
      DO 104 I=1,30
      IS(I)=ISW(I)
  104 N(I)=NW(I)
      EMIN=EMINW/27.2107D-3
      EMAX=EMAXW/27.2107D-3
      NTE=NTEW
C       IF (NTE.NE.1) THEN
C       HE=(EMAX-EMIN)/(NTE-1)
C       ELSE
C       HE=0.D0
C       ENDIF
      NOB=NOBW
C
       LTR=LTRW(1)
C      IF(LTR.EQ.0) GO TO 111
       LT=IABS(LTR)
       CALL TBEGSJ(30.D0,LT)
C
C
        DO 431 IOB=1,NOB
C                               Shell loop
      DO 21 I=1,9
      IF(IS(IOB).EQ.IST(I)) GO TO 22
   21 CONTINUE
C     PRINT 23,IS(IOB)
      WRITE(2, 23) IS(IOB)
   23 FORMAT(2X,'ICW: OTCuTCTBuET:',A2)
      STOP
   22 CONTINUE
      L=I/2
      AJ=L-0.5D0
      J=I-L-L
      IF(J.NE.0) AJ=AJ+1.D0
      I=0
      DO 24 NI=1,10
      DO 25 IL=1,NI
      LI=IL-1
      DO 26 IJ=1,2
      AJI=LI-1.5D0+IJ
      IF(AJI)26,26,27
   27 I=I+1
      IF(NI.NE.N(IOB)) GO TO 26
      IF(LI.NE.L) GO TO 26
      IF(DABS(AJI-AJ).LE.1.D-10) GO TO 28
   26 CONTINUE
   25 CONTINUE
   24 CONTINUE
   28 CONTINUE
      IND=1
      IF(Q(I).NE.0.D0) IND=2
      GO TO (29,30),IND
   30 EB=E(I)
        QCONV(IOB)=Q(I)
      GO TO 31
   29 CALL ENERBG(N(IOB),L,EB,U,NT,NS,HB,0.05D0)
        QCONV(IOB)=1.
   31 AEXP=25.D0
      P=DSQRT(EB*2.D0)
      NSB=1
      AW=1.D0
      AQ=AEXP/P/HB
   34 AQ1=AQ-(AW-1.D0)*NT-(NT-1)*AW
      IF(AQ1)32,32,33
   33 AW=AW+AW
      NSB=NSB+1
      GO TO 34
   32 NSB=MIN0(NS,NSB)
      GO TO (35,36),IND
   35 CONTINUE
      HN=EB*(1.D0-N(IOB)**2/(N(IOB)+1.D0)**2)*0.1D0
      HS=HN*4.D0
      EPSD=1.D-8
      CALL DRDIS(EB,N(IOB),L,AJ,HN,HS,HB,NT,NSB,VFBG,
     *           VFBF,U,EB1,EPSD)
      EB=EB1
      GO TO 37
   36 CALL DRDSI(EB,N(IOB),L,AJ,HN,HS,HB,NT,NSB,VFBG,
     *           VFBF,U,EB1,EPS)
   37 CONTINUE
C
  107 CONTINUE
C
        DO 432 IX=1,NTE
              W=0.
        EX=EINW(IX)/27.2107D-3
        EC=EX-EB
        IF(EC) 41,41,42
   42   CONTINUE
C
      CALL PARICC(EC,U,NT,NSB,NS,NA,HB,EPS,
     *            NSBR,NSI,NAR)
C
                NTQ1=NTQ+1
                CALL WRGD(VFBG,NT,NSB,VFBG(NTQ1),NAR,NSI,HB)
                CALL WRGD(VFBF,NT,NSB,VFBF(NTQ1),NAR,NSI,HB)
C
      CALL ICCATM(EX,LTR,L,AJ,EB,NT,NS,NSBR,NSI,NA,NAR,HB,W,EPS)
C
  41    ET(IX)=EX*27.2107D-3
                EBPR(IOB)=EB*27.2107D-3
        WWE(IOB,IX)=W
                WRITE (2,*) 'Energy loop =',IX
 432  continue
C       End of energy loop
C
                WRITE (2,*) 'Shell loop =', IOB
 431    continue
C                       The end of shell loop
C
C======= The print block=======
C
C
C       Calculation of Nucl.Matr.Elem.
C
        E2M=5.66D+18
        RATM=1.2*AN**(1./3.)
        ABOHR=5.29D+4
        WCNCON=E2M/ABOHR**(2*LT)
        ESQT=1./137.
        BWL=0.25*ESQT/PI*(3./(3.+LT))**2
C
      IF(LTR.LT.0) THEN
                IQ=MEM
        LQ=-LTR
                WCNCON=WCNCON*FLOAT(LT+1)/FLOAT(LT)
                BWL=BWL*RATM**(2*(LQ-1))*0.441
        ELSE
        IQ=MEE
        LQ=LTR
                BWL=BWL*RATM**(2*LQ)
        END IF
        CBEM=WCNCON*BWL
C
        MNSTR=10
        MREPT=NOB/MNSTR
        IF(MOD(NOB,MNSTR).NE.0) MREPT=MREPT+1
C       PRINT 1040,nmel,fldate
        WRITE(2, 1040) nmel,fldate
C       PRINT 1010,OUFILE
        WRITE(2, 1010) OUFILE
C       PRINT 1020,Z,AN,ZZ,EPS
        WRITE(2, 1020) Z,AN,ZZ,EPS
C       PRINT 2001,NT,NS,HB
        WRITE(2, 2001) NT,NS,HB
 2001   FORMAT(1X,'NT=',I4,' NS=',I10,' HB=',E10.3)
C       print 1021, rref,bref**2
C
C       Printout the electron factors
C
        DO 433 MRPT=1,MREPT
                NOBB=(MREPT-1)*MNSTR
                IF (MRPT.EQ.MREPT) THEN
                        NOBK=NOBB+MOD(NOB,MNSTR)
                ELSE
                        NOBK=NOBB+MNSTR
                ENDIF
                NOBB=NOBB+1
C
C       PRINT 1050,IQ,LQ,NMEL
        WRITE(2, 1050) IQ,LQ,NMEL
C       PRINT 1060,(N(IOB),IS(IOB),IOB=NOBB,NOBK)
        WRITE(2,1060) (N(IOB),IS(IOB),IOB=NOBB,NOBK)
C       PRINT 1070,(EBPR(IOB),IOB=NOBB,NOBK)
        WRITE(2,1070) (EBPR(IOB),IOB=NOBB,NOBK)
C
                DO 434 IX=1,NTE
C               PRINT 1080,ET(IX),(WWE(IOB,IX),IOB=NOBB,NOBK)
                WRITE(2,1080) ET(IX),(WWE(IOB,IX),IOB=NOBB,NOBK)
 434            continue
 433    continue
        CCZ=1.
        DO 435 IX=1,LT
                CCZ=0.25*CCZ/(FLOAT(2*IX+1))**2
 435    continue
        CCZ=CCZ*12.*PI*FLOAT(LT+1)/FLOAT(LT)*BWL*10.**(18-10*LT)
C       PRINT 1210
        WRITE(2,1210)
        DO 436 IX=1,NTE
                WCONT=0.
C
                DO 437 IOB=1,NOB
                        WCONT=WCONT+QCONV(IOB)*WWE(IOB,IX)
 437            continue
        WCONT=CBEM*WCONT
        WGAM=CCZ*ET(IX)**(2*LT+1)
        ALCON=WCONT/WGAM
        T12CNV=0.
        IF (WCONT.GT.1D-15) T12CNV=DLOG(2.D+0)/WCONT
C       PRINT 1200,ET(IX),BWL,WCONT,WGAM,ALCON,T12CNV
        WRITE(2,1200) ET(IX),BWL,WCONT,WGAM,ALCON,T12CNV
 436    continue
C
c       ==========
c       call etime(ft)
c       ==========

c       CALL GETTIM (ihr1,imin1,isec1,ith1)
        ft(1)=(ihr1-ihr0)*3600.+(imin1-imin0)*60+(isec1-isec0)

C       print 1100,ft(1)
        WRITE(2,1100) ft(1)
C       PRINT 1110
        WRITE(2,1110)
C
C========The format block)=======
C
 1020 FORMAT(10X,'Nucleus charge',46(1H.),F5.1/
     *10X,'Nuclons number (RN=1.2A**1/3fM)',28(1H.),
     *F6.2/10X,'Number of electrons',41(1H.),F5.1,/
     *10X,'Accuracy',42(1H.),D15.8//)
 1021 format(10x,'Reflection radius in A.Bohr:',F6.2,/
     *       10x,'Reflection Coefficient:',F7.3)
 1005   FORMAT(A14)
 1007   FORMAT(A2)
 1010   FORMAT(10X,'Data from file:',A14,/)
 1030   FORMAT(I1,A2)
 1040   FORMAT(1H1/10X,'Program  ICCC for ',a2,2x,'at ',a24)
 1050   FORMAT(20X,A1,I1,2X,'Transition for ',A2,1X,'nucleus')
 1060   FORMAT(9X,10(5X,I1,A2,4X))
 1070   FORMAT(9X,10(2X,F8.4,2X))
 1080   FORMAT(1X,F8.5,10D12.4)
 1100   format(10x,'Ellapsed time:',f7.2,' s')
 1110   FORMAT(10X,'=========End of ICCC========')
 1210   FORMAT(20X,'Total Summary for Conversion',/,2X,
     *  '   Energy   ',' Nucl.BWL   ','Wconversion ',
     *  '   Wgamma   ',' Alpha conv.','  Tconv 1/2 ')
 1200   FORMAT(2X,F12.6,5D12.4)
        RETURN
      END
      SUBROUTINE ENERBG(N,L,E,U,NT,NS,HB,EPS)
        IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U(NT,NS)
      HE=0.1D0
      S=3.141592654D0*(N-DSQRT(L*(L+1.D0)))
      R=1.5D-4
      I=R/HB+1
      I=MIN0(I,NT)
      Z=U(I,1)
      Z=DMAX1(Z,1.D0)
      EMAX=Z*Z/1.5D0/N/N
C     pPEdpOlOvEHiE
    3 CALL CVZCL1(HE,U,NT,NS,HB,L,A)
      A=A-S
      IF(A)1,1,2
    1 HE=HE*0.5D0
      IF(HE.GT.1.D-10) GO TO 3
C     PRINT 30
      WRITE(2,30)
   30 FORMAT(10X,'ENERB:HET YPOBHq')
      STOP
    2 FA=A
      A=HE
    5 CONTINUE
      B=EMAX
      CALL CVZCL1(B,U,NT,NS,HB,L,FB)
      FB=FB-S
      IF(FB)4,32,33
   32 ER=B
      GO TO 20
   33 FA=FB
      A=B
      EMAX=EMAX*2.D0
      GO TO 5
    4 CONTINUE
      IN=1
      ER1=0.D0
   48 CONTINUE
      ER=A-FA*(A-B)/(FA-FB)
      GO TO (43,44),IN
   43 IN=2
      GO TO 45
   44 IF(DABS(ER-ER1)/ER.LE.EPS) GO TO 20
   45 ER1=ER
      CALL CVZCL1(ER,U,NT,NS,HB,L,FR)
      FR=FR-S
      IF(FR*FA.GT.0.D0) GO TO 10
      B=ER
      FB=FR
      GO TO 41
   10 CONTINUE
      A=ER
      FA=FR
   41 CONTINUE
      EX=(A+B)*0.5D0
      CALL CVZCL1(EX,U,NT,NS,HB,L,FX)
      FX=FX-S
      IF(FX*FA.GT.0.D0) GO TO 42
      B=EX
      FB=FX
      GO TO 48
   42 A=EX
      FA=FX
      GO TO 48
   20 E=ER
C***  pPOgPAMMA pOiCKA |HEPgii pO
C***  KBAziKlACCiKE (CM.viPHOB)
      RETURN
      END
      SUBROUTINE CVZCL1(E,U,NT,NS,HB,L,S)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION U(NT)
C
C     BCpOMOgATElxHAq dlq ENERBG
C**** U(1,1)=V(0.)
      MX=NT*NS
      M=1
      S=0.D0
      H=HB
      H3=H/3.D0
      AL=L*(L+1.D0)
      X=0.D0
      Y=0.D0
      IK=NT-2
      DX=1.D0
      DO 1 J=1,NS
      SI=Y
      DO 2 I=2,IK,2
      M=M+1
      X=X+H
      DX=1.D0/X
      F=2.D0*(U(M)*DX-E)-AL*DX*DX
      F=DMAX1(F,0.D0)
      F=DSQRT(F)
      SI=SI+4.D0*F
      X=X+H
      DX=1.D0/X
      M=M+1
      F=2.D0*(U(M)*DX-E)-AL*DX*DX
      F=DMAX1(F,0.D0)
      F=DSQRT(F)
      SI=SI+2.D0*F
    2 CONTINUE
      M=M+1
      X=X+H
      DX=1.D0/X
      F=2.D0*(U(M)*DX-E)-AL*DX*DX
      F=DMAX1(0.D0,F)
      SI=SI+4.D0*DSQRT(F)
      X=X+H
      DX=1.D0/X
      M=M+1
      Y=0.D0
      IF(M.GT.MX) GO TO 50
      F=2.D0*(U(M)*DX-E)-AL*DX*DX
      F=DMAX1(0.D0,F)
      Y=DSQRT(F)
   50 S=S+(SI+Y)*H3
      H3=H3+H3
    1 H=H+H
      RETURN
  500 S=S+SI*H3
      RETURN
      END
        SUBROUTINE TBEGSJ(XMAX,LMAX)
C
        IMPLICIT REAL*8 (A-H,O-Z)
        COMMON/BLTBSJ/AJ1(502),AJ2(502),DJ1(502),DJ2(502),
     *  XMAX1,H,DH,QL,INDJ,LMAX1,NZ,NZ1,NZ2,L1,LM1
C
        DIMENSION A(21)
C
C
C     LMAX=20
C
      DATA N/500/,N1/501/,N2/502/,LMMAX/20/
C
      IF(LMAX.LE.LMMAX) GO TO 1
C     PRINT 2,LMAX
      WRITE(2, 2) LMAX
    2 FORMAT(2X,'TBEGSJ: HAdO PAC{iPiTx A',
     *1X,'i B :LMAX=',I6)
      STOP
    1 CONTINUE
      XMAX1=XMAX
      LMAX1=LMAX
      NZ=N
      NZ1=N1
      NZ2=N2
      H=XMAX/N
      DH=1.D0/H
      LM1=LMAX-1
      QL=LM1+LMAX
      INDJ=LMAX*DH
      INDJ=MAX0(2,INDJ)
      IF(INDJ.GT.N) INDJ=N1
C
C
      X=0.D0
      AJ1(1)=1.D0
      AJ2(1)=1.D0
C
C***  AJ1-LMAX,AJ2-LMAX-1
C
      L1=LMAX+1
      DO 3 I=1,LMAX
    3 AJ1(1)=AJ1(1)/(I+I+1.D0)
      AJ2(1)=AJ1(1)*(LMAX+LMAX+1.D0)
C
C
      X=H
      DO 4 IX=2,N1
      CALL TBSPJ(X,LMAX,A)
C
      IF(IX-INDJ)5,6,7
    5 X1=X**(LMAX-1)
      AJ2(IX)=A(LMAX)/X1
      AJ1(IX)=A(L1)/X1/X
      GO TO 8
    6 X1=X**(LMAX-1)
      AJ2(IX)=A(LMAX)/X1
      AJ1(IX)=A(L1)/X1/X
    7 K=IX+1
      AJ2(K)=A(LMAX)
      AJ1(K)=A(L1)
    8 CONTINUE
    4 X=X+H
C
C**** By~iClili J i N
C
C***  TEpEPx pPOizBOdHyE
C
C     DJ(L)=L/X*J(L)-J(L+1)=-(L+1)/X*J(L)+J(L-1)
C
C     pPEdpOlAgAEM ~TO X HE Cli{KOM MAl
C     T.E. My pOlu~AEM 1/X*(1-1)=X
C     1=1+AX**2
C     C XOPO{Ej TO~HOCTx`
      AQ=LMAX*2+1.D0
      X=0.D0
      DJ1(1)=0.D0
      DJ2(1)=0.D0
      X=H
      DO 100 IX=2,N1
      IF(IX-INDJ)101,102,103
  101 DJ1(IX)=(AJ2(IX)-AQ*AJ1(IX))/X
      DJ2(IX)=-X*AJ1(IX)
      GO TO 110
  102 DJ1(IX)=(AJ2(IX)-AQ*AJ1(IX))/X
      DJ2(IX)=-X*AJ1(IX)
  103 K=IX+1
      DJ1(K)=AJ2(K)-L1/X*AJ1(K)
      DJ2(K)=AJ2(K)/X*(LMAX-1.D0)-AJ1(K)
  110   CONTINUE
  120   CONTINUE
C
C
        X=X+H
  100   CONTINUE
        RETURN
        END
      SUBROUTINE PARICC(E,G,NT,NSD,NSM,NA,HB,EPS,
     *NSDR,NS,NAR)
      IMPLICIT REAL*8(A-H,O-Z)
C**** OpPEdElEHiE pAPAMETPOB dlq ERNWR
C     E-KiHETi~ECKAq |HEPgiq
C     G(NT,NSD)-MACCiB (fYHKciq diCKPETHAq)
C     pO zApOlHEHi` |TOgO MACCiBA OpPEdElqETCq
C     pPEdElxHyj PAdiYC iHTEgPiPOBAHiq YP-Hij
C     i NAR-~iClO TO~EK PABHOMEPHOgO Y~ACTKA
C     NSM-MAX. BOzMOvHOE NS
C     NA-dliHA EdiHicy zApiCi PABH.Y~ACTKA
C     HB-HA~AlxHyj {Ag
C     EPS-vElAEMAq OTHOCiTElxHAq TO~HOCTx
C    *NSDR-PEAlxHAq gPAHicA MACCiBA G(NT,NSD)
C    *NS-TO~KA pEPEXOdA K PABH.CETKE
C    *NAR-~iClO TO~EK PABH.CETKi
C     INM-PAzMEPHOCTx MACCiBA iMEH
C     NA=2+N*6
C     NAR=K*NA+2.+N*6
C
        COMMON/ERNREM/ P,SM,HM,RM,RP,HINF
C
      DIMENSION G(NT,NSD),Y(4),Z(4),Y1(5),Z1(5)
      DATA Y/0.1D0,0.2D0,0.3D0,0.4D0/,Z/0.D0,2.22D0,3.47D0,4.06D0/
      P=DSQRT(E+E+(E/137.0388D0)**2)
      EPS1=DLOG10(EPS)
      EPS1=7.D0+EPS1
      CALL POLINT(Y,Z,4,EPS1,HV)
      HV=HV/P/HB
      AW=1.D0
      NS=1.D0
    4 IF(AW-HV)1,2,3
    1 AW=AW+AW
      NS=NS+1
      GO TO 4
    3 AW=AW*0.5D0
      NS=NS-1
      IF(NS)5,5,2
    5 NS=1
      AW=1.D0
    2 CONTINUE
      HM=HB*AW
      RM=HB*(NT*(AW-1.D0)+(NT-1.D0)*AW)
      IF(NS.LE.NSM) GO TO 6
      NS=NSM
      AW=2**(NS-1)
      HM=HB*AW
      RM=HB*(NT*(AW-1.D0)+(NT-1.D0)*AW)
      HV=HV*HB
C     PRINT 66,NSM,HM,HV
      WRITE(2, 66) NSM,HM,HV
   66 FORMAT(10X,'PARERW NSMAX=',I4,
     *'HMREAL=',E12.5,'HVNEED=',E12.5)
    6 CONTINUE
      NT1=NT+1
      NS1=NSD+1
      DO 20 J=1,NSD
      DO 20 I=1,NT
      K=NT1-I
      L=NS1-J
      Q=G(K,L)
      IF(DABS(Q).GT.EPS) GO TO 21
   20 CONTINUE
   21 I=NT-I
      J=NSD-J
      RI=2.D0**J
      RI=HB*(NT*(RI-1.D0)+I*RI)
      NSDR=J+1
      NAR=(RI-RM)/HM+1
        IF (NAR.GT.NA) NAR=NA
C      J=NAR/NA
C      K=NAR-J*NA
C      M=MOD(K-2,6)
C      K=K-M
C      IF(K.LE.2) K=0
C      NAR=K+J*NA
C      IF(NAR.LE.(INM-1)*NA) GO TO 16
C      NAR=NA*(INM-1)
      RMS=(NAR-1)*HM+RM
c      PRINT 77,RMS,RI
c   77 FORMAT(10X,'RREAL=',E12.5,'RNEED=',E12.5)
   16 NAR=MAX0(1,NAR)
      RETURN
      END
      SUBROUTINE DRZER(E,U,NT,NS,HB,L,AJ,R1,R2)
       IMPLICIT REAL*8(A-H,O-Z)
      COMMON /BLDRDS/NQ
      DIMENSION R1(NT,NS),R2(NT,NS),U(NT),F1(4),
     *F2(4),UR(5)
      REAL *8 K
      DATA  CS/137.0388D0/, CS2/274.0776D0/, PI/3.1415926535897932D0/
C
C
      JQON=1
   30 CONTINUE
      UR(1)=U(1)
      DO 287 J=1,4
  287 UR(J+1)=0.125D0*(U(J)*3.D0+6.D0*U(J+1)-U(J+2))
 1    R1(1,1)=0.D0
      R2(1,1)=0.D0
      DW=1.D-30
      H=HB
      SK=AJ+.5D0
      K=SK*(L-AJ)*2.D0
      V1=U(1)
      ST=DSQRT(K*K-(V1/CS)**2)
      EC=E/CS
      H2=H*.5D0
      A2=UR(1)
      A3=U(2)
      A4=UR(2)
      UC=A2/CS
      EH=EC*H2
      CH=CS*H
      A=-K-ST
      B=K-ST
      D=EH-UC
      C=CH-D
      A1=1.D0
      B1=1.D0
      IF(V1.EQ.0.D0) GO TO 2
      AK1=H
      AM1=-CH*A/V1
      A=1.D0+A
      B=1.D0+B
      GO TO 4
    2 AK2=(6.D0*A2-3.D0*A3+2.D0/3.D0*A4)/CS/H
      IF(K.GE.0.D0) GO TO 3
      AK1=1.D0
      AM1=(AK2-EC)/(B-1.D0)
      A1=H
      B1=1.D0/H
      C=C*H2
      D=D/H2
      A=1.D0+A
      GO TO 4
    3 AM1=H
      AK1=(AK2-EC+CS2)/(1.D0-A)*H
      A1=1.D0/H
      B1=H
      C=C/H2
      D=D*H2
      B=1.D0+B
    4 AK2=A*AK1+C*AM1
      AM2=B*AM1+D*AK1
      AK3=A*AK2+C*AM2
      AM3=B*AM2+D*AK2
      UC=A3/CS
      D=EH+EH-UC
      C=(CH+CH-D)*A1
      D=D*B1
      AK4=A*AK3+C*AM3
      AM4=B*AM3+D*AK3
      AK1=AK1+AK2+AK2+AK3+AK3+AK4
      AM1=(AM1+AM2+AM2+AM3+AM3+AM4)*A1
      R1(2,1)=AK1
      R2(2,1)=AM1
      X=H
      H6=H/6.D0
      DX=1.D0/X
      B=UC*DX
      D=EC-B
      C=CS2-D
      DS=1.D0/CS
      AQ=1.D0-K-ST
      BQ=1.D0+K-ST
      AW=AQ*DX
      BW=BQ*DX
      F1(1)=C*R2(2,1)+AW*R1(2,1)
      F2(1)=D*R1(2,1)+BW*R2(2,1)
      DO 7 I=3,5
      X=X+H
      X1=X-H2
      I1=I-1
      I2=I-2
      DX=1.D0/X1
      B=UR(I)*DX*DS
      D=EC-B
      C=CS2-D
      AW=AQ*DX
      BW=BQ*DX
      A1=R1(I1,1)+H2*F1(I2)
      B1=R2(I1,1)+H2*F2(I2)
      AK2=C*B1+AW*A1
      AM2=D*A1+BW*B1
      A1=R1(I1,1)+H2*AK2
      B1=R2(I1,1)+H2*AM2
      AK3=C*B1+AW*A1
      AM3=D*A1+BW*B1
      DX=1.D0/X
      B=U(I)*DX*DS
      D=EC-B
      C=CS2-D
      AW=AQ*DX
      BW=BQ*DX
      A1=R1(I1,1)+AK3*H
      B1=R2(I1,1)+AM3*H
      AK4=C*B1+AW*A1
      AM4=D*A1+BW*B1
      R1(I,1)=R1(I1,1)+H6*(F1(I2)+AK2+AK2+AK3+AK3+AK4)
      R2(I,1)=R2(I1,1)+H6*(F2(I2)+AM2+AM2+AM3+AM3+AM4)
      F1(I1)=C*R2(I,1)+AW*R1(I,1)
 7    F2(I1)=D*R1(I,1)+BW*R2(I,1)
      C11=0.D0
      P11=0.D0
      C21=0.D0
      P21=0.D0
      A2=F1(1)
      A3=F1(2)
      A4=F1(3)
      A5=F1(4)
      B2=F2(1)
      B3=F2(2)
      B4=F2(3)
      B5=F2(4)
      Y2=R1(4,1)
      Y3=R1(5,1)
      Z2=R2(4,1)
      Z3=R2(5,1)
      H3=H6+H6
      H9=H/96.D0
      C1=928.D0/1037.D0
      C2=109.D0/1037.D0
      X=X+H
      P12=Y2+H3*(8.D0*A5-5.D0*A4+4.D0*A3-A2)
      P22=Z2+H3*(8.D0*B5-5.D0*B4+4.D0*B3-B2)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      DX=1.D0/X
      B=U(6)*DX*DS
      D=EC-B
      C=CS2-D
      AW=AQ*DX
      BW=BQ*DX
      FZ1=C*AM2+AW*AM1
      FZ2=D*AM1+BW*AM2
      C12=1.75D0*Y3-0.75D0*Y2+H9*(39.D0*FZ1+37.D0*A5-
     *59.D0*A4+7.D0*A3)
      C22=1.75D0*Z3-0.75D0*Z2+H9*(39.D0*FZ2+37.D0*B5-
     *59.D0*B4+7.D0*B3)
      Y1=Y2
      Y2=Y3
      Z1=Z2
      Z2=Z3
      C11=C12
      P11=P12
      C21=C22
      P21=P22
      Y3=C11+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      A6=C*Z3+AW*Y3
      B6=D*Y3+BW*Z3
      R1(6,1)=Y3
      R2(6,1)=Z3
      P12=Y2+H3*(8.D0*A6-5.D0*A5+4.D0*A4-A3)
      P22=Z2+H3*(8.D0*B6-5.D0*B5+4.D0*B4-B3)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      X=X+H
      DX=1.D0/X
      AW=AQ*DX
      BW=BQ*DX
      B=U(7)*DX*DS
      C=CS2+B-EC
      D=EC-B
      FZ1=C*AM2+AW*AM1
      FZ2=D*AM1+BW*AM2
      C12=1.75D0*Y3-0.75D0*Y2+H9*(39.D0*FZ1+37.D0*A6-
     *59.D0*A5+7.D0*A4)
      C22=1.75D0*Z3-0.75D0*Z2+H9*(39.D0*FZ2+37.D0*B6-
     *59.D0*B5+7.D0*B4)
      C11=C12
      P11=P12
      C21=C22
      P21=P22
      Y1=Y2
      Y2=Y3
      Z1=Z2
      Z2=Z3
      Y3=C11+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      A7=C*Z3+AW*Y3
      B7=D*Y3+BW*Z3
      R1(7,1)=Y3
      R2(7,1)=Z3
      P12=Y2+H3*(8.D0*A7-5.D0*A6+4.D0*A5-A4)
      P22=Z2+H3*(8.D0*B7-5.D0*B6+4.D0*B5-B4)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      X=X+H
      DX=1.D0/X
      AW=AQ*DX
      BW=BQ*DX
      B=U(8)*DX*DS
      C=CS2+B-EC
      D=EC-B
      FZ1=C*AM2+AW*AM1
      FZ2=D*AM1+BW*AM2
      C12=1.75D0*Y3-0.75D0*Y2+H9*(39.D0*FZ1+37.D0*A7-
     *59.D0*A6+7.D0*A5)
      C22=1.75D0*Z3-0.75D0*Z2+H9*(39.D0*FZ2+37.D0*B7-
     *59.D0*B6+7.D0*B5)
      Y3=C12+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      A8=C*Z3+AW*Y3
      B8=D*Y3+BW*Z3
      C=1.D0/60480.D0
      C7=198721.D0*C
      C6=-447288.D0*C
      C5=705549.D0*C
      C4=-688256.D0*C
      C3=407139.D0*C
      C2=-134472.D0*C
      C1=19087.D0*C
      D7=19087.D0*C
      D6=65112.D0*C
      D5=-46461.D0*C
      D4=37504.D0*C
      D3=-20211.D0*C
      D2=6312.D0*C
      D1=-863.D0*C
      Z2=Z3
      Y2=Y3
      NTJ=NT
      IF(NS.EQ.1) NTJ=NT-1
      DO 9 I=8,NTJ
      X=X+H
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      A5=A6
      A6=A7
      A7=A8
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      B5=B6
      B6=B7
      B7=B8
      Y1=Y2
      Z1=Z2
      R1(I,1)=Y1
      R2(I,1)=Z1
      AM1=Y1+H*(C1*A1+C2*A2+C3*A3+C4*A4+
     *C5*A5+C6*A6+C7*A7)
      DX=1.D0/X
      AW=AQ*DX
      BW=BQ*DX
      B=U(I+1)*DX*DS
      C=CS2+B-EC
      D=EC-B
      AM2=Z1+H*(C1*B1+C2*B2+C3*B3+
     *C4*B4+C5*B5+C6*B6+C7*B7)
      FZ1=C*AM2+AW*AM1
      FZ2=D*AM1+BW*AM2
      Y2=Y1+H*(D1*A2+D2*A3+D3*A4+D4*A5+
     *D5*A6+D6*A7+D7*FZ1)
      Z2=Z1+H*(D1*B2+D2*B3+D3*B4+D4*B5+
     *D5*B6+D6*B7+D7*FZ2)
      A8=C*Z2+AW*Y2
      B8=D*Y2+BW*Z2
    9 CONTINUE
      AQ=ST-1.D0
      FZ1=H**AQ
      IF(FZ1.LT.1.D-30) FZ1=1.D-30
      DO 289 I=2,NTJ
      FZ2=FZ1*(I-1.D0)**AQ
      R1(I,1)=R1(I,1)*FZ2
  289 R2(I,1)=R2(I,1)*FZ2
      X1=H*(NTJ-6)
      FZ2=FZ1*(NTJ-6.D0)**AQ
      A2=A2*FZ2+AQ/X1*R1(NTJ-5,1)
      B2=B2*FZ2+AQ/X1*R2(NTJ-5,1)
      X1=X1+H
      FZ2=FZ1*(NTJ-5.D0)**AQ
      A3=A3*FZ2+AQ/X1*R1(NTJ-4,1)
      B3=B3*FZ2+AQ/X1*R2(NTJ-4,1)
      X1=X1+H
      FZ2=FZ1*(NTJ-4.D0)**AQ
      A4=A4*FZ2+AQ/X1*R1(NTJ-3,1)
      B4=B4*FZ2+AQ/X1*R2(NTJ-3,1)
      X1=X1+H
      FZ2=FZ1*(NTJ-3.D0)**AQ
      A5=A5*FZ2+AQ/X1*R1(NTJ-2,1)
      B5=B5*FZ2+AQ/X1*R2(NTJ-2,1)
      X1=X1+H
      FZ2=FZ1*(NTJ-2.D0)**AQ
      A6=A6*FZ2+AQ/X1*R1(NTJ-1,1)
      B6=B6*FZ2+AQ/X1*R2(NTJ-1,1)
      X1=X1+H
      FZ2=FZ1*(NTJ-1.D0)**AQ
      A7=A7*FZ2+AQ/X1*R1(NTJ,1)
      B7=B7*FZ2+AQ/X1*R2(NTJ,1)
      X1=X1+H
      FZ2=FZ1*(NTJ+0.D0)**AQ
      A8=(A8+AQ/X1*Y2)*FZ2
      B8=(B8+AQ/X1*Z2)*FZ2
      Y2=Y2*FZ2
      Z2=Z2*FZ2
      JL=NTJ+1
      IF(DABS(Y2).LT.1.D+30) GO TO 109
      NQ=NQ+30
      DO 8 M=1,NTJ
      R1(M,1)=R1(M,1)*DW
    8 R2(M,1)=R2(M,1)*DW
      A1=A1*DW
      A2=A2*DW
      A3=A3*DW
      A4=A4*DW
      A5=A5*DW
      A6=A6*DW
      A7=A7*DW
      A8=A8*DW
      B1=B1*DW
      B2=B2*DW
      B3=B3*DW
      B4=B4*DW
      B5=B5*DW
      B6=B6*DW
      B7=B7*DW
      B8=B8*DW
      Z1=Z1*DW
      Z2=Z2*DW
      Y1=Y1*DW
      Y2=Y2*DW
  109 CONTINUE
      IF (NS.EQ.1) GO TO 13
      N2=NT-11
      N3=NT-9
      N4=NT-7
      DO 12 J=2,NS
      J1=J-1
      XY=X-12.D0*H
      DX=1.D0/XY
      A=K*DX
      B=U(JL-12)*DX*DS
      A5=A2
      B5=B2
      A7=A6
      B7=B6
      A6=A4
      B6=B4
      C=CS2+B-EC
      D=EC-B
      A2=C*R2(N2,J1)-A*R1(N2,J1)
      B2=D*R1(N2,J1)+A*R2(N2,J1)
      XY=X-10.D0*H
      DX=1.D0/XY
      A=K*DX
      B=U(JL-10)*DX*DS
      C=CS2+B-EC
      D=EC-B
      A3=C*R2(N3,J1)-A*R1(N3,J1)
      B3=D*R1(N3,J1)+A*R2(N3,J1)
      XY=X-8.D0*H
      DX=1.D0/XY
      A=K*DX
      B=U(JL-8)*DX*DS
      D=EC-B
      C=CS2-D
      A1=R1(N4,J1)
      B=R2(N4,J1)
      A4=C*B-A*A1
      B4=D*A1+A*B
      H=H+H
      JK=NT
      IF (J.GE.NS) JK=NT-1
      DO 11 I=1,JK
      X=X+H
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      Y1=Y2
      R1(I,J)=Y1
      A5=A6
      A6=A7
      A7=A8
      B5=B6
      B6=B7
      B7=B8
      Z1=Z2
      R2(I,J)=Z1
      AM1=Y1+H*(C1*A1+C2*A2+C3*A3+C4*A4+
     *C5*A5+C6*A6+C7*A7)
      AM2=Z1+H*(C1*B1+C2*B2+C3*B3+C4*B4+
     *C5*B5+C6*B6+C7*B7)
      DX=1.D0/X
      A=K*DX
      JL=JL+1
      B=U(JL)*DX*DS
      D=EC-B
      C=CS2-D
      FZ1=C*AM2-A*AM1
      FZ2=D*AM1+A*AM2
      Y2=Y1+H*(D1*A2+D2*A3+D3*A4+D4*A5+
     *D5*A6+D6*A7+D7*FZ1)
      Z2=Z1+H*(D1*B2+D2*B3+D3*B4+D4*B5+
     *D5*B6+D6*B7+D7*FZ2)
      A8=C*Z2-A*Y2
      B8=D*Y2+A*Z2
   11 CONTINUE
      IF(DABS(Y2).LT.1.D+30) GO TO 111
      NQ=NQ+30
      DO 15 M2=1,J
      DO 15 M1=1,NT
      R1(M1,M2)=R1(M1,M2)*DW
   15 R2(M1,M2)=R2(M1,M2)*DW
      A1=A1*DW
      A2=A2*DW
      A3=A3*DW
      A4=A4*DW
      A5=A5*DW
      A6=A6*DW
      A7=A7*DW
      A8=A8*DW
      B1=B1*DW
      B2=B2*DW
      B3=B3*DW
      B4=B4*DW
      B5=B5*DW
      B6=B6*DW
      B7=B7*DW
      B8=B8*DW
      Z1=Z1*DW
      Z2=Z2*DW
      Y1=Y1*DW
      Y2=Y2*DW
C***
  111 CONTINUE
 12   CONTINUE
   13 R1(NT,NS)=Y2
      R2(NT,NS)=Z2
      GO TO (31,32),JQON
   31 CONTINUE
      RETURN
C     By~iClEHiE G,F(1,NS+1)
   32 X=X+H
      AM1=Y2+H*(C1*A2+C2*A3+C3*A4+C4*A5+C5*A6+C6*A7
     *+C7*A8)
      AM2=Z2+H*(C1*B2+C2*B3+C3*B4+C4*B5+C5*B6+C6*B7
     *+C7*B8)
      DX=1.D0/X
      A=K*DX
      B=U(JL+1)*DX*DS
      D=EC-B
      C=CS2-D
      FZ1=C*AM2-A*AM1
      FZ2=D*AM1+A*AM2
      R1(1,1)=Y2+H*(D1*A3+D2*A4+D3*A5+D4*A6+D5*A7
     *+D6*A8+D7*FZ1)
      R2(1,1)=Z2+H*(D1*B3+D2*B4+D3*B5+D4*B6+D5*B7
     *+D6*B8+D7*FZ2)
      RETURN
      ENTRY DRZRE(E,U,NT,NS,HB,L,AJ,R1,R2)
      JQON=2
      GO TO 30
      END
      SUBROUTINE ERNREL(V,E,L,AJ,R1,R2,NT,NS,AF,FF,NA,EPS,HB)
       IMPLICIT REAL*8(A-H,O-Z)
C     PE{EHiE YP. diPAKA dlq HEpPEPyBHOgO
C     CpEKTPA
C     V-fYHKciq pOTEHciAl (V(X)=Z(X))
C     E-|HEPgiq
C     L-OPbiTAlxHyj MOMEHT
C     AJ-pOlHyj MOMEHT
C     R1-TAblicA bOlx{Oj KOMpOHEHTy
C     BOlHOBOj fYHKcii
C     R2-MAlAq KOMpOHEHTA
C     NT,NS-pAPAMETPy TAblic R1 R2
C     (CM. ERDREM)
C     AF-TAblicA AMpliTYdy
C     FF-TAblicA fAzy
C     G(X)=A(X)*SIN(P*X+F(X))
C     F(X)=A(X)*COS(P*X+F(X))*MHOviTElx
C     NA-PAzMEPHOCTx TAblic AF i FF
C     RP-pPEdElxHyj PAdiYC HOPMiPOBKi.
C     HB-HA~AlxHyj {Ag.
C     HOPMiPOBKA: G(R)=SIN(P*R+F)
C     pPi R CTPEMq}EMCq K bECKOHE~HOCTi.
C     B blOKE /ERNREM/:
C     P-iMpYlxC
C     SM-MHOviTElx**(-1)
C     HM-{Ag AF i FF
C     RM=X(NT,NS)
C     HOPMiPOBKA: HA bECKOHE~HOCTi
C     RP-pPEdElxHyj PAdiuC HOPMiPOBKi
C     G=SIN(P*X+FAZA)
      COMMON /ERNREM/P,SM,HM,RM,RP,HINF
      DIMENSION R1(NT,NS), R2(NT,NS), AF(NA),FF(NA)
      DIMENSION F1(4), F2(4),CW(6)
      REAL*8    K
      DATA  CS/137.0388D0/, CS2/274.0776D0/, PI/3.1415926535897932D0/
      DATA  CW/5.D0,1.D0,6.D0,1.D0,5.D0,2.D0/
 1    R1(1,1)=0.D0
      R2(1,1)=0.D0
      DW=1.D-30
      H=HB
      SK=AJ+.5D0
      K=SK*(L-AJ)*2.D0
      V1=V(0.D0)
      ST=DSQRT(K*K-(V1/CS)**2)
      EC=-E/CS
      H2=H*.5D0
      A2=V(H2)
      A3=V(H)
      A4=V(H+H2)
      UC=A2/CS
      EH=EC*H2
      CH=CS*H
      A=-K-ST
      B=K-ST
      D=EH-UC
      C=CH-D
      A1=1.D0
      B1=1.D0
      IF(V1.EQ.0.D0) GO TO 2
      AK1=H
      AM1=-CH*A/V1
      A=1.D0+A
      B=1.D0+B
      GO TO 4
    2 AK2=(6.D0*A2-3.D0*A3+2.D0/3.D0*A4)/CS/H
      IF(K.GE.0.D0) GO TO 3
      AK1=1.D0
      AM1=(AK2-EC)/(B-1.D0)
      A1=H
      B1=1.D0/H
      C=C*H2
      D=D/H2
      A=1.D0+A
      GO TO 4
    3 AM1=H
      AK1=(AK2-EC+CS2)/(1.D0-A)*H
      A1=1.D0/H
      B1=H
      C=C/H2
      D=D*H2
      B=1.D0+B
    4 AK2=A*AK1+C*AM1
      AM2=B*AM1+D*AK1
      AK3=A*AK2+C*AM2
      AM3=B*AM2+D*AK2
      UC=A3/CS
      D=EH+EH-UC
      C=(CH+CH-D)*A1
      D=D*B1
      AK4=A*AK3+C*AM3
      AM4=B*AM3+D*AK3
      AK1=AK1+AK2+AK2+AK3+AK3+AK4
      AM1=(AM1+AM2+AM2+AM3+AM3+AM4)*A1
      R1(2,1)=AK1
      R2(2,1)=AM1
      X=H
      H6=H/6.D0
      DX=1.D0/X
      B=UC*DX
      D=EC-B
      C=CS2-D
      DS=1.D0/CS
      AQ=1.D0-K-ST
      BQ=1.D0+K-ST
      AW=AQ*DX
      BW=BQ*DX
      F1(1)=C*R2(2,1)+AW*R1(2,1)
      F2(1)=D*R1(2,1)+BW*R2(2,1)
      DO 7 I=3,5
      X=X+H
      X1=X-H2
      I1=I-1
      I2=I-2
      DX=1.D0/X1
      B=V(X1)*DX*DS
      D=EC-B
      C=CS2-D
      AW=AQ*DX
      BW=BQ*DX
      A1=R1(I1,1)+H2*F1(I2)
      B1=R2(I1,1)+H2*F2(I2)
      AK2=C*B1+AW*A1
      AM2=D*A1+BW*B1
      A1=R1(I1,1)+H2*AK2
      B1=R2(I1,1)+H2*AM2
      AK3=C*B1+AW*A1
      AM3=D*A1+BW*B1
      DX=1.D0/X
      B=V(X)*DX*DS
      D=EC-B
      C=CS2-D
      AW=AQ*DX
      BW=BQ*DX
      A1=R1(I1,1)+AK3*H
      B1=R2(I1,1)+AM3*H
      AK4=C*B1+AW*A1
      AM4=D*A1+BW*B1
      R1(I,1)=R1(I1,1)+H6*(F1(I2)+AK2+AK2+AK3+AK3+AK4)
      R2(I,1)=R2(I1,1)+H6*(F2(I2)+AM2+AM2+AM3+AM3+AM4)
      F1(I1)=C*R2(I,1)+AW*R1(I,1)
 7    F2(I1)=D*R1(I,1)+BW*R2(I,1)
      C11=0.D0
      P11=0.D0
      C21=0.D0
      P21=0.D0
      A2=F1(1)
      A3=F1(2)
      A4=F1(3)
      A5=F1(4)
      B2=F2(1)
      B3=F2(2)
      B4=F2(3)
      B5=F2(4)
      Y2=R1(4,1)
      Y3=R1(5,1)
      Z2=R2(4,1)
      Z3=R2(5,1)
      H3=H6+H6
      H9=H/96.D0
      C1=928.D0/1037.D0
      C2=109.D0/1037.D0
      X=X+H
      P12=Y2+H3*(8.D0*A5-5.D0*A4+4.D0*A3-A2)
      P22=Z2+H3*(8.D0*B5-5.D0*B4+4.D0*B3-B2)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      DX=1.D0/X
      B=V(X)*DX*DS
      D=EC-B
      C=CS2-D
      AW=AQ*DX
      BW=BQ*DX
      FZ1=C*AM2+AW*AM1
      FZ2=D*AM1+BW*AM2
      C12=1.75D0*Y3-0.75D0*Y2+H9*(39.D0*FZ1+37.D0*A5-
     *59.D0*A4+7.D0*A3)
      C22=1.75D0*Z3-0.75D0*Z2+H9*(39.D0*FZ2+37.D0*B5-
     *59.D0*B4+7.D0*B3)
      Y1=Y2
      Y2=Y3
      Z1=Z2
      Z2=Z3
      C11=C12
      P11=P12
      C21=C22
      P21=P22
      Y3=C11+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      A6=C*Z3+AW*Y3
      B6=D*Y3+BW*Z3
      R1(6,1)=Y3
      R2(6,1)=Z3
      P12=Y2+H3*(8.D0*A6-5.D0*A5+4.D0*A4-A3)
      P22=Z2+H3*(8.D0*B6-5.D0*B5+4.D0*B4-B3)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      X=X+H
      DX=1.D0/X
      AW=AQ*DX
      BW=BQ*DX
      B=V(X)*DX*DS
      C=CS2+B-EC
      D=EC-B
      FZ1=C*AM2+AW*AM1
      FZ2=D*AM1+BW*AM2
      C12=1.75D0*Y3-0.75D0*Y2+H9*(39.D0*FZ1+37.D0*A6-
     *59.D0*A5+7.D0*A4)
      C22=1.75D0*Z3-0.75D0*Z2+H9*(39.D0*FZ2+37.D0*B6-
     *59.D0*B5+7.D0*B4)
      C11=C12
      P11=P12
      C21=C22
      P21=P22
      Y1=Y2
      Y2=Y3
      Z1=Z2
      Z2=Z3
      Y3=C11+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      A7=C*Z3+AW*Y3
      B7=D*Y3+BW*Z3
      R1(7,1)=Y3
      R2(7,1)=Z3
      P12=Y2+H3*(8.D0*A7-5.D0*A6+4.D0*A5-A4)
      P22=Z2+H3*(8.D0*B7-5.D0*B6+4.D0*B5-B4)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      X=X+H
      DX=1.D0/X
      AW=AQ*DX
      BW=BQ*DX
      B=V(X)*DX*DS
      C=CS2+B-EC
      D=EC-B
      FZ1=C*AM2+AW*AM1
      FZ2=D*AM1+BW*AM2
      C12=1.75D0*Y3-0.75D0*Y2+H9*(39.D0*FZ1+37.D0*A7-
     *59.D0*A6+7.D0*A5)
      C22=1.75D0*Z3-0.75D0*Z2+H9*(39.D0*FZ2+37.D0*B7-
     *59.D0*B6+7.D0*B5)
      Y3=C12+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      A8=C*Z3+AW*Y3
      B8=D*Y3+BW*Z3
      AQ=ST-1.D0
      X1=H
      A2=A2+AQ/X1*R1(2,1)
      B2=B2+AQ/X1*R2(2,1)
      FZ1=H**AQ
      IF(FZ1.LT.1.D-15) FZ1=1.D-15
      A2=A2*FZ1
      B2=B2*FZ1
      R1(2,1)=R1(2,1)*FZ1
      R2(2,1)=R2(2,1)*FZ1
      FZ2=FZ1*2.D0**AQ
      X1=X1+H
      A3=(A3+AQ/X1*R1(3,1))*FZ2
      B3=(B3+AQ/X1*R2(3,1))*FZ2
      R1(3,1)=R1(3,1)*FZ2
      R2(3,1)=R2(3,1)*FZ2
      FZ2=FZ1*3.D0**AQ
      X1=X1+H
      A4=(A4+AQ/X1*R1(4,1))*FZ2
      B4=(B4+AQ/X1*R2(4,1))*FZ2
      R1(4,1)=R1(4,1)*FZ2
      R2(4,1)=R2(4,1)*FZ2
      FZ2=FZ1*4.D0**AQ
      X1=X1+H
      A5=(A5+AQ/X1*R1(5,1))*FZ2
      B5=(B5+AQ/X1*R2(5,1))*FZ2
      R1(5,1)=FZ2*R1(5,1)
      R2(5,1)=FZ2*R2(5,1)
      FZ2=FZ1*5.D0**AQ
      X1=X1+H
      A6=(A6+AQ/X1*R1(6,1))*FZ2
      B6=(B6+AQ/X1*R2(6,1))*FZ2
      R1(6,1)=R1(6,1)*FZ2
      R2(6,1)=R2(6,1)*FZ2
      FZ2=FZ1*6.D0**AQ
      X1=X1+H
      A7=(A7+AQ/X1*R1(7,1))*FZ2
      B7=(B7+AQ/X1*R2(7,1))*FZ2
      R1(7,1)=R1(7,1)*FZ2
      R2(7,1)=R2(7,1)*FZ2
      FZ2=FZ1*7.D0**AQ
      X1=X1+H
      A8=(A8+AQ/X1*Y3)*FZ2
      B8=(B8+AQ/X1*Z3)*FZ2
      Z3=Z3*FZ2
      Y3=Y3*FZ2
      C=1.D0/60480.D0
      C7=198721.D0*C
      C6=-447288.D0*C
      C5=705549.D0*C
      C4=-688256.D0*C
      C3=407139.D0*C
      C2=-134472.D0*C
      C1=19087.D0*C
      D7=19087.D0*C
      D6=65112.D0*C
      D5=-46461.D0*C
      D4=37504.D0*C
      D3=-20211.D0*C
      D2=6312.D0*C
      D1=-863.D0*C
      Z2=Z3
      Y2=Y3
      NTJ=NT
      IF(NS.EQ.1) NTJ=NT-1
      DO 9 I=8,NTJ
      X=X+H
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      A5=A6
      A6=A7
      A7=A8
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      B5=B6
      B6=B7
      B7=B8
      Y1=Y2
      Z1=Z2
      R1(I,1)=Y1
      R2(I,1)=Z1
      AM1=Y1+H*(C1*A1+C2*A2+C3*A3+C4*A4+
     *C5*A5+C6*A6+C7*A7)
      DX=1.D0/X
      A=K*DX
      B=V(X)*DX*DS
      C=CS2+B-EC
      D=EC-B
      AM2=Z1+H*(C1*B1+C2*B2+C3*B3+
     *C4*B4+C5*B5+C6*B6+C7*B7)
      FZ1=C*AM2-A*AM1
      FZ2=D*AM1+A*AM2
      Y2=Y1+H*(D1*A2+D2*A3+D3*A4+D4*A5+
     *D5*A6+D6*A7+D7*FZ1)
      Z2=Z1+H*(D1*B2+D2*B3+D3*B4+D4*B5+
     *D5*B6+D6*B7+D7*FZ2)
      A8=C*Z2-A*Y2
      B8=D*Y2+A*Z2
  9   CONTINUE
      IF(DABS(Y2).LT.1.D+30) GO TO 109
      DO 8 M=1,NTJ
      R1(M,1)=R1(M,1)*DW
    8 R2(M,1)=R2(M,1)*DW
      A1=A1*DW
      A2=A2*DW
      A3=A3*DW
      A4=A4*DW
      A5=A5*DW
      A6=A6*DW
      A7=A7*DW
      A8=A8*DW
      B1=B1*DW
      B2=B2*DW
      B3=B3*DW
      B4=B4*DW
      B5=B5*DW
      B6=B6*DW
      B7=B7*DW
      B8=B8*DW
      Z1=Z1*DW
      Z2=Z2*DW
      Y1=Y1*DW
      Y2=Y2*DW
  109 CONTINUE
      IF (NS.EQ.1) GO TO 13
      N2=NT-11
      N3=NT-9
      N4=NT-7
      DO 12 J=2,NS
      J1=J-1
      XY=X-12.D0*H
      DX=1.D0/XY
      A=K*DX
      B=V(XY)*DX*DS
      A5=A2
      B5=B2
      A7=A6
      B7=B6
      A6=A4
      B6=B4
      C=CS2+B-EC
      D=EC-B
      A2=C*R2(N2,J1)-A*R1(N2,J1)
      B2=D*R1(N2,J1)+A*R2(N2,J1)
      XY=X-10.D0*H
      DX=1.D0/XY
      A=K*DX
      B=V(XY)*DX*DS
      C=CS2+B-EC
      D=EC-B
      A3=C*R2(N3,J1)-A*R1(N3,J1)
      B3=D*R1(N3,J1)+A*R2(N3,J1)
      XY=X-8.D0*H
      DX=1.D0/XY
      A=K*DX
      B=V(XY)*DX*DS
      D=EC-B
      C=CS2-D
      A1=R1(N4,J1)
      B=R2(N4,J1)
      A4=C*B-A*A1
      B4=D*A1+A*B
      H=H+H
      JK=NT
      IF (J.GE.NS) JK=NT-1
      DO 11 I=1,JK
      X=X+H
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      Y1=Y2
      R1(I,J)=Y1
      A5=A6
      A6=A7
      A7=A8
      B5=B6
      B6=B7
      B7=B8
      Z1=Z2
      R2(I,J)=Z1
      AM1=Y1+H*(C1*A1+C2*A2+C3*A3+C4*A4+
     *C5*A5+C6*A6+C7*A7)
      AM2=Z1+H*(C1*B1+C2*B2+C3*B3+C4*B4+
     *C5*B5+C6*B6+C7*B7)
      DX=1.D0/X
      A=K*DX
      B=V(X)*DX*DS
      D=EC-B
      C=CS2-D
      FZ1=C*AM2-A*AM1
      FZ2=D*AM1+A*AM2
      Y2=Y1+H*(D1*A2+D2*A3+D3*A4+D4*A5+
     *D5*A6+D6*A7+D7*FZ1)
      Z2=Z1+H*(D1*B2+D2*B3+D3*B4+D4*B5+
     *D5*B6+D6*B7+D7*FZ2)
      A8=C*Z2-A*Y2
      B8=D*Y2+A*Z2
  11  CONTINUE
      IF(DABS(Y2).LT.1.D+30) GO TO 111
      DO 15 M2=1,J
      DO 15 M1=1,NT
      R1(M1,M2)=R1(M1,M2)*DW
   15 R2(M1,M2)=R2(M1,M2)*DW
      A1=A1*DW
      A2=A2*DW
      A3=A3*DW
      A4=A4*DW
      A5=A5*DW
      A6=A6*DW
      A7=A7*DW
      A8=A8*DW
      B1=B1*DW
      B2=B2*DW
      B3=B3*DW
      B4=B4*DW
      B5=B5*DW
      B6=B6*DW
      B7=B7*DW
      B8=B8*DW
      Z1=Z1*DW
      Z2=Z2*DW
      Y1=Y1*DW
      Y2=Y2*DW
C***
  111 CONTINUE
 12   CONTINUE
   13 R1(NT,NS)=Y2
      R2(NT,NS)=Z2
      RM=X
      EC=-EC/CS
      P=DSQRT(E+E+E*EC)
      EC1=EC+1.D0
      P2=P+P
      DP=1.D0/P
      SM=DSQRT(1.D0+2.D0/EC)
      I=NT-6
      AM1=R1(I,NS)
      AM2=R2(I,NS)*SM
      B2=B2*SM
      AK1=AM1*AM1+AM2*AM2
      AK2=DSQRT(AK1)
      A=(A2*AM1+B2*AM2)/AK2
      B2=(A2*AM2-B2*AM1)/AK1-P
      A2=A
      I=I+1
      AM1=R1(I,NS)
      AM2=R2(I,NS)*SM
      B3=B3*SM
      AK1=AM1*AM1+AM2*AM2
      AK2=DSQRT(AK1)
      A=(A3*AM1+B3*AM2)/AK2
      B3=(A3*AM2-B3*AM1)/AK1-P
      A3=A
      I=I+1
      AM1=R1(I,NS)
      AM2=R2(I,NS)*SM
      B4=B4*SM
      AK1=AM1*AM1+AM2*AM2
      AK2=DSQRT(AK1)
      A=(A4*AM1+B4*AM2)/AK2
      B4=(A4*AM2-B4*AM1)/AK1-P
      A4=A
      I=I+1
      AM1=R1(I,NS)
      AM2=R2(I,NS)*SM
      B5=B5*SM
      AK1=AM1*AM1+AM2*AM2
      AK2=DSQRT(AK1)
      A=(A5*AM1+B5*AM2)/AK2
      B5=(A5*AM2-B5*AM1)/AK1-P
      A5=A
      I=I+1
      AM1=R1(I,NS)
      AM2=R2(I,NS)*SM
      B6=B6*SM
      AK1=AM1*AM1+AM2*AM2
      AK2=DSQRT(AK1)
      A=(A6*AM1+B6*AM2)/AK2
      B6=(A6*AM2-B6*AM1)/AK1-P
      A6=A
      I=I+1
      AM1=R1(I,NS)
      AM2=R2(I,NS)*SM
      B7=B7*SM
      AK1=AM1*AM1+AM2*AM2
      AK2=DSQRT(AK1)
      A=(A7*AM1+B7*AM2)/AK2
      B7=(A7*AM2-B7*AM1)/AK1-P
      A7=A
      Z1=Z2*SM
      B8=B8*SM
      AK1=Y2*Y2+Z1*Z1
      AK2=DSQRT(AK1)
      A=(A8*Y2+B8*Z1)/AK2
      B8=(A8*Z1-B8*Y2)/AK1-P
      A8=A
      IF(Z1) 70,71,70
   71 IF(Y2) 72,73,73
   72 Z2=-PI*.5D0
      GO TO 74
   73 Z2=PI*.5D0
      GO TO 74
   70 Z2=DATAN2(Y2,Z1)
   74 Z2=Z2-P*X
      ILM=Z2*.5D0/PI
      Z2=Z2-ILM*2*PI
      Y2=AK2
      NQ=1
      HM=H
      AF(1)=Y2
      FF(1)=Z2
      SUM1=0.D0
      SUM2=0.D0
      SUM3=0.D0
      SUM4=0.D0
      NV=PI*DP/HM
      M=MOD(NV,6)
      IF(M) 120,120,121
  120 M=6
  121 NV=NV+6-M
      IF(NA.EQ.1) NQ=2
      I=1
      RP=SK*DP*.5D0/DSQRT(EPS)
      NINF=(RP-RM)/HM
      NINF=MAX0(NA,NINF)
      DV2=0.D0
   25 I=I+1
      DV1=DV2
      X=X+H
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      Y1=Y2
      Z1=Z2
      A5=A6
      A6=A7
      A7=A8
      B5=B6
      B6=B7
      B7=B8
      AM1=Y1+H*(C1*A1+C2*A2+C3*A3+C4*A4+
     *C5*A5+C6*A6+C7*A7)
      AM2=Z1+H*(C1*B1+C2*B2+C3*B3+C4*B4+
     *C5*B5+C6*B6+C7*B7)
      DX=1.D0/X
      PR=P2*X
      DV2=V(X)
      VDP=DV2*DP
      ARG=PR+2.D0*AM2
      C=DCOS(ARG)
      S=DSIN(ARG)
      FZ1=AM1*(K*C-VDP*S)*DX
      FZ2=(VDP*(EC1-C)-K*S)*DX
      Y2=Y1+H*(D1*A2+D2*A3+D3*A4+D4*A5+
     *D5*A6+D6*A7+D7*FZ1)
      Z2=Z1+H*(D1*B2+D2*B3+D3*B4+D4*B5+
     *D5*B6+D6*B7+D7*FZ2)
      ARG=PR+Z2+Z2
      C=DCOS(ARG)
      S=DSIN(ARG)
      S1=(K*C-VDP*S)*DX
      S2=(K*S+VDP*C)*DX
      A8=S1*Y2
      B8=VDP*EC1*DX-S2
      GO TO (20,52,21,50),NQ
   20 AF(I)=Y2
      FF(I)=Z2
      J=I-NA
      IF(J)25,49,49
   49 NQ=2
   52 J=I-NINF
      IF(J)53,54,54
   53 CONTINUE
      GO TO 25
   54 NQ=3
   21 D=DABS(VDP)
      DA=SK+D
      DA=DA*(1.D0+DA+2.D0*D*EC1)+DABS(DV2-DV1)/HM*X
      D=DX*DP*.5D0
      D=DA*D*D-EPS
      IF(D)60,25,25
   60 NQ=4
      I=0
      RP=X
      SUM1=S1
      SUM2=S2
      SUM3=Y2
      SUM4=Z2
      GO TO 25
   50 DV1=DV2
      M=(I-1)/6
      M=I-M*6
      W=CW(M)
      SUM1=SUM1+W*S1
      SUM2=SUM2+W*S2
      SUM3=SUM3+W*Y2
      SUM4=SUM4+W*Z2
      J=I-NV
      IF(J)25,37,37
   37 H3=H*.3D0
      SUM1=(SUM1-S1)*H3
      SUM2=(SUM2-S2)*H3
      SUM3=(SUM3-Y2)*H3
      SUM4=(SUM4-Z2)*H3
      D=X-RP
      AINF=SUM3/(D+SUM2/P2)
      HINF=(SUM4-SUM1/P2)/D
      S=1.D0/AINF
      IF(S)75,75,76
   75 S=-S
      HINF=HINF+PI
   76 CONTINUE
      DO 40 J=1,NS
      DO 40 I=1,NT
      R1(I,J)=R1(I,J)*S
   40 R2(I,J)=R2(I,J)*S
      DO 41 I=1,NA
   41 AF(I)=AF(I)*S
      RETURN
      END
      SUBROUTINE AFVGF(AF,FF,NAR)
       IMPLICIT REAL*8(A-H,O-Z)
C**** pEPEXOd OT AMpliTYdy-fAzy K
C     bOlx{Oj i MAlOj KOMpOHEHTAM
C     AF(NA), FF(NA)-MACCiBy
C     NAR-Ob}EE ~iClO TO~EK (PABHOMEPHyX)
      DIMENSION AF(NAR),FF(NAR)
      COMMON/ERNREM/P,SM,HM,RM,AI,FI
      S=1.D0/SM
      H=HM*P
      X=RM*P
      IZ=1
      M=NAR-1
      IF(M)1,1,2
    2 CONTINUE
      IZ=IZ+1
      DO 3 I=1,NAR
      R=X+FF(I)
      W=AF(I)
      AF(I)=W*DSIN(R)
      FF(I)=W*S*DCOS(R)
    3 X=X+H
    1 CONTINUE
      RETURN
      END
      SUBROUTINE INTEGR(GD,FD,F,NT,NS,NAR,HB,S)
C
       IMPLICIT REAL*8(A-H,O-Z)
C**** pPOgPAMMA iHTEgPiPOBAHiq TPEX fYHKcij
C     GD,FD,F - MACCiBy PAzMEPHOCTx` NT*NS+NAR
C     NT,NS-pAPAMETPy pEPEMEHHOj TAblicy
C     NAR-~iClO TO~EK PABH.Y~ACTKA
C     HB-HA~AlxHyj {Ag
C
C     CM. ERNWR i T.d.
C
      DIMENSION GD(NT),FD(NT),F(NT),CW(6)
      DATA CW/5.D0,1.D0,6.D0,1.D0,5.D0,2.D0/
      NTNS=NT*NS
      S=0.D0
      H=HB
      H1=H*0.3D0
      X=0.D0
    5 J=1
      K=1
      I=1
      S1=0.D0
      SI=0.D0
  306 DO 300 M=1,5
      I=I+1
      K=K+1
      Y=FD(K)*GD(K)*F(K)
  300 SI=SI+CW(M)*Y
      IF(I-NT)302,303,303
  302 I=I+1
      K=K+1
      Y=FD(K)*GD(K)*F(K)
      SI=SI+Y+Y
      GO TO 306
  303 IF(J-NS) 307,308,308
  307 J=J+1
      K=K+1
      Y=FD(K)*GD(K)*F(K)
  309 SI=SI+Y
      H=H+H
      S=S+SI*H1
      H1=H1+H1
      I=1
      SI=Y
      GO TO 306
  308 CONTINUE
  700 M=NAR-1
      IF(M)57,57,58
   57 S2=S+SI*H1
      GO TO 777
   58 H3=10.D0/9.D0
      K=NTNS+2
      Y=FD(K)*GD(K)*F(K)
      SI=SI+Y
      S=S+SI*H1
  600 NAQ=NAR
      SI=Y
      K=2+NTNS
      I=2
  402 DO 401 M=1,6
      K=K+1
      I=I+1
      Y=FD(K)*GD(K)*F(K)
  401 SI=SI+CW(M)*Y
  506 IF(I-NAQ)402,405,405
  405 S=SI*H1+S
      S=S-Y*H1
      S2=S
  777 CONTINUE
      S=S2
      RETURN
      END
      SUBROUTINE POLINT(F,X,N,XR,FR)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION F(N),X(N)
      FR=0.D0
      DO 1 J=1,N
      S=F(J)
      XS=X(J)
      DO 2 I=1,N
      M=I-J
      IF(M)3,2,3
    3 XQ=X(I)
      S=S*(XR-XQ)/(XS-XQ)
    2 CONTINUE
    1 FR=FR+S
      RETURN
      END
      SUBROUTINE PRMAT(IN,AJ1,L1,L,AJ2,L2,P1,P2)
         IMPLICIT REAL*8(A-H,O-Z)
C IN=1  P1=(AJ1,L1//EL//AJ2,L2), P2=(AJ1,L1//MLL//AJ2,L2)
C IN=-1 P1=(AJ1,L1//MLL-1//AJ2.L2), P2=(AJ1,L1//MLL+1//AJ2,L2)
C     HET pPABil OTbOPA: (J1,L1,.5), (J2,L2,.5), (L1,L2,K)
      M=L1+L2+L
      M=(MOD(M,2)*2-1)*IN
      IF(M) 1,100,100
  100 P1=0.D0
      P2=0.D0
      RETURN
    1 A=L
      M=L1+L2-L
      IF(M) 100,10,10
   10 M=L-IABS(L1-L2)
      IF(M) 100,11,11
   11 CONTINUE
      AJP122=AJ2+.5D0
      C=CLEBSH(AJ1,AJ2,0.5D0,-0.5D0,A,0.D0)
      AJP121=AJ1+.5D0
      C1=AJP122/3.1415926535897932D0*.5D0
      E2P=1.D0
      M=AJP122+.1D0
      M=MOD(M,2)
      IF(M)2,3,2
    2 E2P=-1.D0
    3 AJ=AJ1+AJ2+A
      M=AJ+.1D0
      EJ=AJP121
      M=MOD(M,2)
      IF(M)4,5,4
    4 EJ=-AJP121
    5 EJ=EJ+AJP122
      EL2=1.D0
      M=MOD(L2,2)
      IF(M)6,7,6
    6 EL2=-1.D0
    7 IF(IN)8,100,9
    9 P1=-E2P*C*DSQRT(C1)
      P2=0.D0
      IF(A) 100,12,13
   13 CONTINUE
      P2=-C*DSQRT(C1/A/(A+1.D0))*EL2*EJ
   12 CONTINUE
      RETURN
    8 A1=A+1.D0
      A2=A1+A
      C1=C1/A2
      EJ=EJ*EL2
      P1=0.D0
      IF(A.NE.0.D0) P1=DSQRT(C1*A)*C*(E2P-EJ/A)
      P2=DSQRT(C1*A1)*C*(-E2P-EJ/A1)
      RETURN
      END
      SUBROUTINE TBJNL(AK,L,NT,NS,HB,NAZ,NAR,AJ,AN)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C     pPOgPAMMA TAbuliPOBAHiq
C      fuHKcij JL(X) i NL(X)
C
C     X=AK*R
C     BCEgO HA PABHOMEPHOM u~ACTKE NAR
C     TO~EK
C     CM.ERNREL ili AHAlOgi
C     H=H(NS),X(1)=X(NT,NS)
C     NAZ=6*N+2
C     NAR=K*NA+2+N*6
C
C     AJ i AN MACCiBy PAbO~iE
C     PAzMEPHOCTx MAX(NT*NS+NAR) TO~EK
C
      DIMENSION AJ(NT),AN(NT),A(21),B(21)
C
C
    1 CONTINUE
      X=0.D0
      H=HB*AK
      K=1
      IL=L+1
      DO 3 J=1,NS
      DO 4 I=1,NT
      CALL SPLSJN(X,L,A,B)
      AJ(K)=A(IL)
      AN(K)=B(IL)
      K=K+1
      X=X+H
    4 CONTINUE
      H=H+H
    3 CONTINUE
      NTNS=K-1
      IF(NAR.GT.1) GO TO 5
      RETURN
    5 CONTINUE
      H=H*0.5D0
      X=X-H
    7 DO 6 I=1,NAR
      CALL SPLSJN(X,L,A,B)
      AJ(I+NTNS)=A(IL)
      AN(I+NTNS)=B(IL)
    6 X=X+H
      RETURN
      END
C
C
        SUBROUTINE TBSPJ(X,LMAX,AJ)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION AJ(LMAX)
        LMX1=LMAX+1
        DO 1 I=1,LMX1
  1     AJ(I)=0.D0
        IF(X) 2,3,2
  3     AJ(1)=1.D0
        RETURN
  2     S=DSIN(X)
        DX=1.D0/X
        D2X=DX+DX
        IF(LMAX) 4,4,5
  4     AJ(1)=S*DX
        RETURN
  5     CONTINUE
C
C       CALC JL(X)
C
  10    CONTINUE
        IF(X.GT.LMAX) GOTO 11
        S1=1.D0
        QN=0.D0
        QF=LMX1+LMX1+1.D0
        X2=X*X
        BN=1.D0
  13    QN=QN+1.D0
        QF=QF+2.D0
        BN=-BN*0.5D0/QN/QF*X2
        SM=S1+BN
        IF(SM.EQ.S1) GOTO 12
c       IF(abs(bn).lt.1.d-10) GOTO 12
        S1=SM
        GOTO 13
  12    S2=1.D0
        QN=0.D0
        QF=LMAX+LMAX+1.D0
        BN=1.D0
  15    QN=QN+1.D0
        QF=QF+2.D0
        BN=-BN*0.5D0/QN/QF*X2
        SM=S2+BN
        IF(abs(bn).lt.1.d-10) GOTO 14
        IF(SM.EQ.S2) GOTO 14
        S2=SM
        GOTO 15
  14    CONTINUE
        A1=S1/(LMAX+LMAX+3.D0)*X
        A2=S2
        AJ(LMX1)=S2
        Q=(LMAX+LMAX+3.D0)*DX
        J=LMX1
        DO 16 I=1,LMAX
        J=J-1
        Q=Q-D2X
        A3=A2*Q-A1
        AJ(J)=A3
        A1=A2
        A2=A3
        IF(A3.LT.1.D40) GOTO 16
        A1=A1*1.D-4
        A2=A2*1.D-4
        DO 17 K=1,LMX1
  17    AJ(K)=AJ(K)*1.D-4
  16    CONTINUE
C
C
        Q=S*DX/AJ(1)
        DO 18 I=1,LMX1
  18    AJ(I)=AJ(I)*Q
        RETURN
  11    CONTINUE
C
C
        Q=-DX
        C=DCOS(X)
        A1=C*DX
        A2=S*DX
        AJ(1)=A2
        DO 20 I=2,LMX1
        Q=Q+D2X
        A3=A2*Q-A1
        AJ(I)=A3
        A1=A2
        A2=A3
  20    CONTINUE
        RETURN
        END
      FUNCTION RTAB(R,NT,NS,NSI,HM,RM,IN)
        IMPLICIT REAL*8(A-H,O-Z)
C     ByciClEHiE fYHKcii pO TAblicE
C     R-TAblicA (CM.ERDREM)
C     NT,NS-PAzMEPHOCTx TAblicy
C     NSI-AHAlOg NS B ERNREL
C     HM,RM-CM. ERNREL
C     IN=1-pEPBOE ObPA}EHiE
C     IN=2-BCE pOClEdY`}iE
C     |TA fYHKciq ByY1iClqET pOClEdOBA-
C     TElxHO zHA~EHiq B TO~KAX TAblic AF
C     U FF ERNREL.
      DIMENSION R(NT,NS)
      DIMENSION     A(4),X(4)
      GO TO (1,2),IN
    1 L=NSI-NS
      IF(L)3,4,4
    4 NJ=2
      A(4)=R(NT,NS)
    6 RTAB=A(4)
      RETURN
    3 X(1)=RM
      X(2)=RM+HM
      H=HM+HM
      X(3)=X(2)+H
      X(4)=X(3)+H
      A(1)=R(NT,NSI)
      J=NSI+1
      A(2)=R(1,J)
      A(3)=R(2,J)
      A(4)=R(3,J)
      I=3
      XI=X(2)
      ISN=1
      M=0
      NJ=1
      N=2
      RTAB=A(2)
      RETURN
    2 GO TO (5,6),NJ
    5 M=M+1
      XI=XI+HM
      L=M/N
      IF(L)7,7,8
    7 AX=X(1)-XI
      BX=X(2)-XI
      CX=X(3)-XI
      DX=X(4)-XI
      AB=1.D0/(X(1)-X(2))
      AC=1.D0/(X(1)-X(3))
      AD=1.D0/(X(1)-X(4))
      BC=1.D0/(X(2)-X(3))
      BD=1.D0/(X(2)-X(4))
      CD=1.D0/(X(3)-X(4))
      RTAB=-A(1)*BX*AB*CX*AC*DX*AD+
     *A(2)*AX*AB*CX*BC*DX*BD-
     *A(3)*AX*AC*BX*BC*DX*CD+
     *A(4)*AX*AD*BX*BD*CX*CD
      RETURN
    8 M=0
      A(1)=A(2)
      A(2)=A(3)
      A(3)=A(4)
      X(1)=X(2)
      X(2)=X(3)
      X(3)=X(4)
      L=I-NT
      IF(L)9,10,10
    9 I=I+1
      ISN=ISN+1
      L=ISN-NT
      IF(L)11,12,12
   12 N=N+N
      ISN=1
   11 A(4)=R(I,J)
      X(4)=X(3)+H
   15 RTAB=A(2)
      RETURN
   10 L=J-NS
      IF(L)13,14,14
   13 J=J+1
      I=1
      A(4)=R(1,J)
      X(4)=X(3)+H
      H=H+H
      GO TO 15
   14 NJ=2
      RTAB=A(3)
      RETURN
      END
c
c
c
      FUNCTION VTABQM(X)
        IMPLICIT REAL*8(A-H,O-Z)
C     BCpOMOgATElxHAq fYHKciq dlq PAbOTy
C     C ERNREL.
C     OpiCAHiE /BLVTAB/ CM. VTABQV
C     HAdO zAdATx NSI B /BLNSI/
C     NSI=NS B ERNREL.
      COMMON /BLVTAB/U(1020),AV,NT,NS,HB
C     U(NT,NS)-MACCiB U(1,1)=U(0.)
C
     */ERNREM/P,SM,HM,RM,AQW791(2)
     */BLNSI/NSI
      IF(X)1,1,2
    1 I=0
      N=NT+1
      NJ=1
      M=1
      VTABQM=U(1)
      RETURN
    2 GO TO (3,4,5,6),NJ
    3 I=I+1
      GO TO (11,12,13,13,14,15,16,17),I
   11 VTABQM=0.125D0*(3.D0*U(1)+6.D0*U(2)-U(3))
      RETURN
   12 VTABQM=U(2)
      RETURN
   13 VTABQM=0.125D0*(3.D0*U(2)+6.D0*U(3)-U(4))
      RETURN
   14 VTABQM=U(3)
      RETURN
   15 VTABQM=0.125D0*(3.D0*U(3)+6.D0*U(4)-U(5))
      RETURN
   16 VTABQM=U(4)
      RETURN
   17 VTABQM=0.125D0*(3.D0*U(4)+6.D0*U(5)-U(6))
      NJ=2
      I=4
      RETURN
    4 I=I+1
      L=I-N
      IF(L)20,40,21
   20 VTABQM=U(I)
      RETURN
   40 L=M-NSI
      IF(L)20,23,23
   21 L=M-NSI
      IF(L)22,23,23
   22 M=M+1
      N=N+NT
      NJ=3
      VTABQM=U(I-13)
      I1=1
      RETURN
    5 GO TO (31,32),I1
   31 I1=2
      VTABQM=U(I-11)
      RETURN
   32 VTABQM=U(I-9)
      NJ=2
      I=I-1
      RETURN
   23 NJ=4
      HBB=HM
      HBB=HM
      VTABQM=RTAB(U,NT,NS,NSI,HBB,RM,1)
      RETURN
    6 VTABQM=RTAB(U,NT,NS,NSI,HM,RM,2)
      RETURN
      END
      SUBROUTINE SPLSJN(X,LMAX,A,B)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C     AppPOKCiMAciq pO CplAjHAM
C     CfEPi~ECKiX fuHKcij JL i NL
C     iCpOlxzu`TCq PEzulxTATy TBEGSJ
C
      COMMON /BLTBSJ/AJ1(502),AJ2(502),DJ1(502),
     *DJ2(502),XMAX,H,DH,QL,INDJ,LMAXM,
     *N,N1,N2,L1M,LM1M
C
      DIMENSION A(LMAX),B(LMAX)
C***  PAzMEPHOCTx B,B(LMAX+1)
C
C**** LMAX.LE.LMAX1-HE pPOBEPq` ***
C
      L1=LMAX+1
      IF(X)20,20,21
   20 DO 22 I=1,L1
      A(I)=0.D0
   22 B(I)=0.D0
      A(1)=1.D0
      RETURN
   21 CONTINUE
      AX=X*DH+1.D0
      IX=AX
      IF(IX-N1)1,3,3
    3 CALL TBSPJN(X,LMAX,A,B)
      RETURN
C
    1 CONTINUE
      DX1=AX-IX
      DX2=DX1-1.D0
      A1=DX1*DX1*(DX1+DX2-2.D0)
      A3=DX2*DX1*H
      A2=A3*DX2
      A3=A3*DX1
C
C     JL By~iClEHiE
C
      LM1=LMAX-1
      IND=1
      I=IX
      IF(I.LT.INDJ) GO TO 5
      IND=2
      I=I+1
    5 CONTINUE
      I1=I+1
      Y1=AJ1(I)
      Y2=AJ1(I1)
      AQ=Y1+(Y1-Y2)*A1+DJ1(I)*A2+DJ1(I1)*A3
      Y1=AJ2(I)
      Y2=AJ2(I1)
      BQ=Y1+(Y1-Y2)*A1+DJ2(I)*A2+DJ2(I1)*A3
C**
C**
      GO TO (6,14),IND
    6 CONTINUE
      X1=X**LM1M
      BQ=BQ*X1
      AQ=AQ*X*X1
C**
   14 CONTINUE
C**
      J1=L1M
      J2=LMAXM
      J=LM1M
      DX=1.D0/X
      Q=QL*DX
      D2H=DX+DX
      IF(J1.LE.L1) A(J1)=AQ
      IF(J2.LE.L1) A(J2)=BQ
      IF(LM1M.LE.0) GO TO 35
      DO 30 I=1,LM1M
      CQ=Q*BQ-AQ
      AQ=BQ
      BQ=CQ
      IF(J.LE.L1) A(J)=CQ
      J=J-1
   30 Q=Q-D2H
C**
   35 CONTINUE
C**
C
C     TEpEPx NJ(X)
C
      AQ=-AQ
      Q=-DX
      DO 40 I=1,L1
      CQ=Q*BQ-AQ
      B(I)=CQ
      AQ=BQ
      BQ=CQ
      J=J+1
      Q=Q+D2H
   40 CONTINUE
      RETURN
      END
      FUNCTION CLEBSH(AJ,BJ,AM,BM,CJ,CM)
        IMPLICIT REAL*8(A-H,O-Z)
C      CJ,CM
C     C
C      AJ,AM,BJ,BM
      DIMENSION      F(100)
      DATA N/100/, K/0/, F/100*0.D0/, X/2.D0/
C
c       write (*,*) aj,bj,am,bm,cj,cm
      IF(K)1,1,2
    1 K=1
      DO 3 I=3,N
      F(I)=F(I-1)+DLOG(X)
    3 X=X+1.D0
    2 I=AM+BM-CM+.1D0
      IF(I)100,4,100
    4 I1=AJ+BJ-CJ+1.1D0
      IF(I1)100,100,6
    6 I2=AJ-BJ+CJ+1.1D0
      IF(I2)100,100,7
    7 I3=BJ+CJ-AJ+1.1D0
      IF(I3)100,100,8
    8 X=AJ+BJ+CJ+2.1D0
      I4=X
      I=X+.6D0
      I=I4-I
      IF(I)100,5,100
    5 X=AJ+AM+1.1D0
      I5=X
      IF(I5)100,100,9
    9 I=X+.6D0
      I=I-I5
      IF(I)100,10,100
   10 I6=AJ-AM+1.1D0
      IF(I6)100,100,11
   11 X=BJ+BM+1.1D0
      I7=X
      IF(I7)100,100,12
   12 I=X+.6D0
      I=I-I7
      IF(I)100,13,100
   13 I8=BJ-BM+1.1D0
      IF(I8)100,100,14
   14 X=CJ+CM+1.1D0
      I9=X
      IF(I9)100,100,15
   15 I=X+.6D0
      I=I-I9
      IF(I)100,16,100
   16 I10=CJ-CM+1.1D0
      IF(I10)100,100,17
   17 X=F(I1)+F(I2)+F(I3)-F(I4)
      I=I5-I6
      IF(I)18,19,18
   19 I=I7-I8
      IF(I)18,200,18
   18 X=X+F(I5)+F(I6)+F(I7)+F(I8)+F(I9)+F(I10)
      X=X*.5D0
      I10=MIN0(I1,I6,I7)
      I2=I1-I5
      I3=I1-I8
      I9=MAX0(0,I2,I3)+1
      I1=I1+1
      I6=I6+1
      I7=I7+1
      I8=I9/2
      E=1.D0
      I5=I9*.5D0+.6D0
      I8=I8-I5
      IF(I8)20,21,20
   21 E=-1.D0
   20 S=0.D0
      DO 22 I=I9,I10
      C=X-F(I)-F(I1-I)-F(I6-I)-F(I7-I)-
     *F(I-I2)-F(I-I3)
      S=S+E*DEXP(C)
   22 E=1.D0-E-1.D0
      CLEBSH=DSQRT(CJ+CJ+1.D0)*S
      RETURN
  200 I=I4/2
      I5=I4*.5D0+.6D0
      I=I-I5
      IF(I)100,201,100
  201 I6=I5-I6+1
      I7=I5-I8+1
      I8=I5-I10+1
      S=X*0.5D0+F(I5)-F(I6)-F(I7)-F(I8)
      S=DEXP(S)
      I5=I8/2
      I6=I8*.5D0+.6D0
      I5=I5-I6
      IF(I5)202,203,202
  203 S=1.D0-S-1.D0
  202 CLEBSH=S*DSQRT(CJ+CJ+1.D0)
      RETURN
  100 CLEBSH=0.D0
      RETURN
      END
        SUBROUTINE ZRDIRD(G,NT,NS,M,I,J)
        IMPLICIT REAL*8(A-H,O-Z)
C
C       Compute variation of G(NT,NS)
C
        DIMENSION G(NT,NS)
C
C       I,J - coordinate of the latest greate maximum
C
        M=0
        A=0.D0
        PQ=1.D0
        S=0.D0
        D=-1.D0
        DO 1 J1=1,NS
        DO 2 I1=1,NT
        B=G(I1,J1)
        PW=B-A
        IF (PQ*PW.GE.0.D0) GOTO 2
        C=DABS(B)
        IF(C.LT.S*0.1D0) GOTO 3
        S=C
        I=I1
        J=J1
        IF (B*D.GE.0.D0) GOTO 2
        D=-D
        PQ=PW
        M=M+1
  2     A=B
  1     CONTINUE
  3     CONTINUE
        RETURN
        END
      SUBROUTINE TBSPJN(X,LMAX,AJ,AN)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     By~iClEHiE CfEPi~ECKiX fuHKcij bECCElq
C     pO PEKuPEHTHyM COOTHO{EHiqM
C
C     AJ(I)=J(I-1,X), AN(I)=N(I-1,X), I=1,LMAX+1
C
      DIMENSION AJ(LMAX),AN(LMAX)
C
C     By~iClEHiE fuHKcij HEjMAHA
C
      LMX1=LMAX+1
      DO 1 I=1,LMX1
      AJ(I)=0.D0
    1 AN(I)=0.D0
      IF(X)2,3,2
    3 AJ(1)=1.D0
      RETURN
    2 S=DSIN(X)
      C=DCOS(X)
      DX=1.D0/X
      IF(LMAX) 4,4,5
    4 AJ(1)=S*DX
      AN(1)=-C*DX
      RETURN
    5 CONTINUE
C
      D2X=DX+DX
      Q=-DX
      A1=S*DX
      A2=-C*DX
      AN(1)=A2
      DO 6 I=2,LMX1
      Q=Q+D2X
      A3=A2*Q-A1
      AN(I)=A3
      A1=A2
      A2=A3
      IF(A3.LT.-1.D60) GO TO 10
C     ByXOd pO O~EHx MAlOMu X
    6 CONTINUE
C
C     By~iClEHiE JL(X)
C
   10 CONTINUE
      IF(X.GT.LMAX) GO TO 11
C     X.LE.LMAX
C     CuMMiPuEM Pqd
C
      S1=1.D0
      QN=0.D0
      QF=LMX1+LMX1+1.D0
      X2=X*X
      BN=1.D0
   13 QN=QN+1.D0
      QF=QF+2.D0
      BN=-BN*0.5D0/QN/QF*X2
      SM=S1+BN
      IF(SM.EQ.S1) GO TO 12
      S1=SM
      GO TO 13
   12 S2=1.D0
      QN=0.D0
      QF=LMAX+LMAX+1.D0
      BN=1.D0
   15 QN=QN+1.D0
      QF=QF+2.D0
      BN=-BN*0.5D0/(QN*QF)*X2
      SM=S2+BN
      IF(SM.EQ.S2) GO TO 14
      S2=SM
      GO TO 15
   14 CONTINUE
      A1=S1/(LMAX+LMAX+3.D0)*X
      A2=S2
      AJ(LMX1)=S2
      Q=(LMAX+LMAX+3.D0)*DX
      J=LMX1
      DO 16 I=1,LMAX
      J=J-1
      Q=Q-D2X
      A3=A2*Q-A1
      AJ(J)=A3
      A1=A2
      A2=A3
      IF(A3.LT.1.D40) GO TO 16
      A1=A1*1.D-4
      A2=A2*1.D-4
      DO 17 K=J,LMX1
   17 AJ(K)=AJ(K)*1.D-4
   16 CONTINUE
C
C     pEPEHOPMiPOBKA
C
      Q=S*DX/AJ(1)
      DO 18 I=1,LMX1
   18 AJ(I)=AJ(I)*Q
      RETURN
   11 CONTINUE
C     X.GT.LMAX
C     By~iClqEM pO PEKuPEHTHyM COOTHO{EHiqM
      Q=-DX
      A1=C*DX
      A2=S*DX
      AJ(1)=A2
      DO 20 I=2,LMX1
      Q=Q+D2X
      A3=A2*Q-A1
      AJ(I)=A3
      A1=A2
      A2=A3
   20 CONTINUE
      RETURN
      END
      SUBROUTINE ICCATM(HOM,LTR,LB,AJB,EB,
     *NT,NSMAX,NSB,NSC,NA,NAR,HB,W,EPS)
C
C
      IMPLICIT REAL*8(A-H,O-Z)

C
C     By~iClEHiE |lEKTPOHHOgO fAKTOPA
C     BEPOqTHOCTi KOHBEPCii HA OdiH |lEKTPOH
C
C***  BXOd:
C     HOM - Energy of nuclear transition
C     LTR-/LTR/ - L of transition
C                       If LTR.GT.0 EL-transition
C                       If LTR.LT.0 ML-transition
C
C     LB,AJB,EB-L,J,E - quantum number of the conversion shell
C     VFBG,VFBF - the great and small VF of bounded state
C     VFCG,VFCF - the great and small VF of free electron
C     VJL,VNL - J(L) and N(L)
C     VJL1,VNL1 - J(L-1) and N(L-1)
C     NT,NSMAX,HB - the tables parametrs
C     NSB-NS for discrete spectra
C     NSC-NS for continuous spectra
C     NA,NAR - the tables parametrs with equal steps
C     EPS - relative accuracy
C     EXIT:
C               W - electron conversion factor
C
C***  DM(NTQ)-PAbO~ij MACCiB
C     NTQ=MAX(NT*NSMAX,NA)*4
C
C     pEPEd ByzOBOM PAbOTAli: TABEGSJ
C
        PARAMETER (MASDIM=4020)
      COMMON /ICCA1/ VFBG(MASDIM),VFBF(MASDIM)
      COMMON /ICCA2/ VFCG(MASDIM),VFCF(MASDIM)
      COMMON /ICCA3/ VJL(MASDIM),VNL(MASDIM)
      COMMON /ICCA4/ VJL1(MASDIM),VNL1(MASDIM)
      COMMON /NUMBF/ NUMBF
C
C
C
      COMMON /BLVTAB/U(1020),AV,NTU,NSU
C
      COMMON /BLNSI/NSI
C
      COMMON /ERNREM/P,SM,HM,RM
c       3.03.92         Tkalya said to modify
c       Rref    - Radius of sphere
c       Bref    - sqrt from coefficient of Reflection
      common/refl/ rref, bref
      dimension ajomg(10),anomg(10)
C
C
      EXTERNAL VTABQM
C
      DATA CS/137.0388D0/,PI/3.141592653 5897932D0/
C
      EPRED=0.1D0/27.2107D0
C
C
      NSI=NSC
      NTS=NT*NSMAX
      NTQ=NTS
      NTQ1=NTQ+1
      EP=HOM-EB
      IF(EP.GT.EPRED) GO TO 2
    1 W=0.D0
      RETURN
    2 AK=HOM/CS
      LT=IABS(LTR)
      NTQ2=NTQ1+NTQ
      INM=10000
      CALL TBJNL(AK,LT,NT,NSC,HB,NA,NAR,VJL,VNL)
      IND=2
      IF(LTR) 4,3,3
    3 IND=1
      CALL TBJNL(AK,LT-1,NT,NSC,HB,NA,NAR,VJL1,VNL1)
      SL=DSQRT((LT+1.D0)/LT)
    4 CONTINUE
      L1S=AJB+AJB-LB+0.1D0
      W=0.D0
      LM=LB
      IF(IND.EQ.2) LM=L1S
      L2MX=LT+LM+1
      L2MN=IABS(LT-LM)+1
      DO 10 IL2=L2MN,L2MX,2
      L2=IL2-1
      DO 11 IJ=1,2
      AJ2=L2-1.5D0+IJ
      IF(AJ2) 11,11,12
   12 CONTINUE
C
      CALL PRMAT(1,AJ2,L2,LT,AJB,LM,P1,P2)
      GO TO (20,21),IND
   20 IF(P1) 22,11,22
   21 IF(P2) 22,11,22
   22 CALL ERNREL(VTABQM,EP,L2,AJ2,VFCG,VFCF,NT,NSC,
     *            VFCG(NTQ1),VFCF(NTQ1),NAR,EPS,HB)
C
      CALL AFVGF(VFCG(NTQ1),VFCF(NTQ1),NAR)
C
      NSC78=NSC
      NSC=MIN0(NSB,NSC)
C
      GO TO (30,31),IND
   30 CONTINUE
C     EL
      CALL INTEGR(VFBG,VFCG,VJL,NT,NSC,NAR,HB,SGGJ)
      CALL INTEGR(VFBF,VFCF,VJL,NT,NSC,NAR,HB,SFFJ)
C
      CALL INTEGR(VFBG,VFCG,VNL,NT,NSC,NAR,HB,SGGN)
      CALL INTEGR(VFBF,VFCF,VNL,NT,NSC,NAR,HB,SFFN)
C
      CALL INTEGR(VFBG,VFCF,VJL1,NT,NSC,NAR,HB,SGFJ)
      CALL INTEGR(VFBG,VFCF,VNL1,NT,NSC,NAR,HB,SGFN)
C
      CALL INTEGR(VFBF,VFCG,VJL1,NT,NSC,NAR,HB,SFGJ)
      CALL INTEGR(VFBF,VFCG,VNL1,NT,NSC,NAR,HB,SFGN)
C
C     The Old Soldatov's variant
C
       RR=P1*(SGFJ-SFGJ-SGGJ-SFFJ)-P2*SL*
     *(SGFJ+SFGJ)
C
       RI=P1*(SGFN-SFGN-SGGN-SFFN)-P2*SL*
     *(SGFN+SFGN)
C
      GO TO 50
   31 CONTINUE
C     ML
      CALL INTEGR(VFBG,VFCF,VJL,NT,NSC,NAR,HB,SGFJ)
      CALL INTEGR(VFBG,VFCF,VNL,NT,NSC,NAR,HB,SGFN)
C
      CALL INTEGR(VFBF,VFCG,VJL,NT,NSC,NAR,HB,SFGJ)
      CALL INTEGR(VFBF,VFCG,VNL,NT,NSC,NAR,HB,SFGN)
C
c      RR=P2*(SGFJ+SFGJ)
c      RI=P2*(SGFN+SFGN)
      RR=P2*(SGFJ+SFGJ)
      RI=P2*(SGFN+SFGN)
   50 CONTINUE
      NSC=NSC78
C       PRINT 1055,LB,AJB,L2,AJ2,RR,RI
        WRITE(2, 1055) LB,AJB,L2,AJ2,RR,RI
 1055   FORMAT(/1X,'LB=',I3,' AJB=',F5.1,' LF=',I3,' AJF=',F5.1,
     *  1P2E12.4/)
C     PRINT 2001, P1,P2,SGGJ,SFFJ,SGFJ,SFGJ,SGGN,SFFN,SGFN,SFGN
      WRITE(2,2001) P1,P2,SGGJ,SFFJ,SGFJ,SFGJ,SGGN,SFFN,SGFN,SFGN
 2001 FORMAT(1X,'P1 =', 1PE10.3, '  P2 = ', E10.3//
     /16X,'     SGGJ        SFFJ        SGFJ        SFGJ'/16X,
     ,1P4E12.3// 16X,'     SGGN        SFFN        SGFN        SFGN'/
     /16X, 4E12.3/)
C     PRINT 2000
      WRITE(2, 2000)
 2000  FORMAT(/19X,'Wave function and J & N function values'/
     / 19X,39('-')/
     / 4X, 'Radius       Initial state         Final state '/
     /'       x         g(x)       f(x)       g(x)       f(x)',
     ,'       J(x)       N(x)'/)
      CALL PRINTF
      W=W+(AJ2+AJ2+1.D0)*(RR*RR+RI*RI)
   11 CONTINUE
   10 CONTINUE
C
      W=W*32.D0*PI/P*(EP+CS*CS*2.D0)/CS**2
     */(AJB+AJB+1.D0)/(LT+LT+1.D0)*PI
      DO 51 I=1,LT
   51 W=W*AK*AK/(I+I+1.D0)**2
      W=W*AK**2
      RETURN
      END
C
      SUBROUTINE PRINTF
C    Printing wave functions in N first points for E.V.Tkalya
      PARAMETER (MASDIM=4020)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /ICCA1/ VFBG(MASDIM),VFBF(MASDIM)
      COMMON /ICCA2/ VFCG(MASDIM),VFCF(MASDIM)
      COMMON /ICCA3/ VJL(MASDIM),VNL(MASDIM)
      COMMON /ICCA4/ VJL1(MASDIM),VNL1(MASDIM)
      COMMON  /BLVTAB/U(1020),AV,NT,NS,HB
      COMMON /NUMBF/ N
      NBLOCK=INT((N-.5)/NT)+1
      NREST=N-NT*(NBLOCK-1)
      IC=1
      R=0.
      STEP=HB/2.
      DO 1 IBL=1,NBLOCK
       STEP=STEP*2.
       IMAX=NT
       IF(IBL.EQ.NBLOCK) IMAX=NREST
       IF(IMAX.EQ.0) RETURN
       DO 2 I=1,IMAX
        IC=IC+1
        R=R+STEP
C       PRINT 201,R,VFBG(IC)/R,VFBF(IC)/R,VFCG(IC)/R,VFCF(IC)/R,
C    *  VJL(IC), VNL(IC)
        WRITE(2, 201) R,VFBG(IC)/R,VFBF(IC)/R,VFCG(IC)/R,
     *  VFCF(IC)/R, VJL(IC), VNL(IC)
    2  CONTINUE
    1  CONTINUE
  201  FORMAT(1X,1P7E11.2)
       RETURN
       END
       
C      subroutine GETTIM(ihr,imin,isec,i100th)
C       integer(2), intent(out):: ihr, imin, isec, i100th
C       character(8):: sdate
C       character(10):: stime
C       call date_and_time(sdate,stime)
C       read(sTime,"(I2,I2,I2,1x,I3)") ihr, imin, isec, i100th
C       RETURN
C      end
