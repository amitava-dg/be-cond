C=======================================================================
C	PROGRAM
C=======================================================================

	PROGRAM BEC
	
	INTEGER J,K
	REAL*16 MU0HI,MU0LO,MU0MI,TAUX
	REAL*16 INTEGRALH,INTEGRALL,INTEGRALM,E

	OPEN(1,FILE="BEC_DATA.TXT")
	
	TAUX=0.Q0
	REQ=2.3148Q0
	E=0.Q0
	
	WRITE(1,*) 0.Q0,0.Q0,0.Q0
	
	DO 30 J=1,300
		TAUX=TAUX+2Q-2
		MU0HI=-1Q-15
		MU0LO=-20.Q0
		DO 40 K=1,100
			IF(TAUX.LT.1.02) THEN
				MU0MI=0.Q0
				EXIT
			ENDIF
			MU0MI=0.5*(MU0LO+MU0HI)
		CALL SIMP13(INTEGRALH,0.Q0,10.Q0,10000,MU0HI,TAUX,.TRUE.)
		CALL SIMP13(INTEGRALL,0.Q0,10.Q0,10000,MU0LO,TAUX,.TRUE.)
		CALL SIMP13(INTEGRALM,0.Q0,10.Q0,10000,MU0MI,TAUX,.TRUE.)
C			WRITE(*,*) INTEGRALH,INTEGRALM,INTEGRALL
			IF((INTEGRALM-REQ)*(INTEGRALH-REQ).GT.0.Q0) THEN
			MU0HI=MU0MI
			ELSE
			MU0LO=MU0MI
			ENDIF
40 		CONTINUE
		CALL SIMP13(E,1.Q-5,10.Q0,10000,MU0MI,TAUX,.FALSE.)
		WRITE(1,*)TAUX,MU0MI,E
30	CONTINUE
	
	END PROGRAM BEC
	
C=======================================================================
C	SIMPSON ONE THIRD SUBROUTINE
C=======================================================================

	SUBROUTINE SIMP13(INTGRL,XI,XN,N,MU0,TAU,FLAG)
	
	INTEGER I,N
	REAL*16 XI,XN,MU0,TAU
	REAL*16 INTGRL
	REAL*16 H,F12,F32,FUNC
	LOGICAL FLAG
	
	H=(XN-XI)/N
	
	IF(FLAG)THEN
		INTGRL=F12(XN,MU0,TAU)+F12(XI,MU0,TAU)
	ELSE
		INTGRL=F32(XN,MU0,TAU)+F12(XI,MU0,TAU)
	ENDIF
	
	DO 1 I=1,N-1
		IF(FLAG)THEN
			FUNC=F12(XI+I*H,MU0,TAU)
		ELSE
			FUNC=F32(XI+I*H,MU0,TAU)
		ENDIF
		IF (MOD(I,2).EQ.0) THEN
			INTGRL=INTGRL+2.Q0*FUNC
		ELSE
			INTGRL=INTGRL+4.Q0*FUNC
		ENDIF
1	CONTINUE
	INTGRL=INTGRL*H/3.Q0
	
	END SUBROUTINE SIMP13
	
C=======================================================================
C	1/2 INTEGRAL FUNCTION
C=======================================================================

	REAL*16 FUNCTION F12(X,MU0,TAU)
	
	REAL*16 X,MU0,TAU
	
	F12=(X**0.5)/(EXP((X-MU0)/TAU)-1.Q0)
	RETURN
	
	END FUNCTION F12
	
C=======================================================================
C	3/2 INTEGRAL FUNCTION
C=======================================================================

	REAL*16 FUNCTION F32(X,MU0,TAU)
	
	REAL*16 X,MU0,TAU
	
	F32=(X**1.5)/(EXP((X-MU0)/TAU)-1.Q0)
C	WRITE(*,*) X,MU0,TAU
	RETURN
	
	END FUNCTION F32
	
c=======================================================================
C=======================================================================
