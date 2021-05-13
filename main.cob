IDENTIFICATION DIVISION.
PROGRAM-ID. Cobolbrot.
AUTHOR. Ker2x.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 MaxIter CONSTANT 100.
01 sizeX CONSTANT 512.
01 sizeY CONSTANT 512.
01 Rmin CONSTANT -2.0.
01 Rmax CONSTANT 1.0.
01 Imin CONSTANT -1.3.
01 Imax CONSTANT 1.3.
01 OrbitEscape CONSTANT 4.0.

01 screenX PIC 999 value zero.
01 screenY PIC 999 value zero.
01 screenR PIC S99V9(16) value zero.
01 screenI PIC S99V9(16) value zero.
01 iter PIC 9999 value zero.


01 pX PIC S9(4)V9(16) value zero.
01 pY PIC S9(4)V9(16) value zero.
01 tmp PIC S99V9(16) value zero.


PROCEDURE DIVISION.

*> PPM header (P2 = Greyscale; P3 = RGB)
DISPLAY "P2".
DISPLAY sizeX " " sizeY.
DISPLAY "# Max iter : " MaxIter.
DISPLAY MaxIter.

PERFORM VARYING screenX from 0 by 1 until screenX is equal to sizeX
    PERFORM VARYING screenY from 0 by 1 until screenY is equal to sizeY
        MOVE ZERO TO pX
        MOVE ZERO TO pY
        COMPUTE screenR = Rmin + (((Rmax - Rmin) / (sizeX - 1)) * screenX)
        COMPUTE screenI = Imin + (((Imax - Imin) / (sizeY - 1)) * screenY)
*>        DISPLAY screenX " " screenY " : " screenI " " screenR

        PERFORM WITH test AFTER varying iter from 0 by 1 until iter >= maxIter OR pX**2 + pY**2 >= OrbitEscape
            COMPUTE tmp = pX**2 - pY**2 + screenR
            COMPUTE pY = 2.0 * pX * pY + screenI
            MOVE tmp to pX
        END-PERFORM

        IF iter >= maxIter
            DISPLAY maxIter " " WITH NO ADVANCING
        ELSE
            DISPLAY iter " " WITH NO ADVANCING
        END-IF
    END-PERFORM
    DISPLAY " "
END-PERFORM
DISPLAY " "
STOP RUN.

END PROGRAM Cobolbrot.
