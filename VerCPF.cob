      ******************************************************************
      * Author: Ayrton Cavalieri de Almeida
      * Date: 29/08/2019
      * Purpose: This program checks if the CPF is a valid number
      * Tectonics: cobc
      ******************************************************************
      * This program is free software: you can redistribute
      * it and/or modify it under the terms of the GNU General
      * Public License as published by the Free Software Foundation,
      * either version 3 of the License, or (at your option) any
      * later version.

      * This program is distributed in the hope that it will be useful,
      * but WITHOUT ANY WARRANTY; without even the implied warranty of
      * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      * GNU General Public License for more details.

      * You should have received a copy of the GNU
      * General Public License along with this program.
      * If not, see <https://www.gnu.org/licenses/>.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. VER-CPF.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *     CPF = Input string.
           77 CPF PIC X(11) VALUE SPACES.
      *     NCPF = Redefines the string as a digit vector.
           77 NCPF REDEFINES CPF PIC 9(1) OCCURS 11.
      *     Editing clause to display the typed CPF on screen.
           77 DCPF PIC 999.999.999/99.
      *     VCPF = Valid digits of a CPF
           77 VCPF PIC X(9) VALUE SPACES.
      *     Editing clause to show the valid 9 digits.
           77 DVCPF PIC 999.999.999.
      *     ACC = Accumulator.
           77 ACC PIC 9(3) VALUE ZERO.
      *     DIGIT = Verifying digits separated individually.
           77 DIGIT PIC 9(1) OCCURS 2.
      *     I = Iterator.
           77 I PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *    Type CPF.
           DISPLAY "Digite seu CPF: ".
           ACCEPT CPF.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = 10
              COMPUTE ACC = ACC + (NCPF(I) * (11 - I))
           END-PERFORM.
           COMPUTE ACC = FUNCTION MOD(ACC, 11).
           COMPUTE ACC = 11 - ACC.
           COMPUTE ACC = FUNCTION MOD(ACC, 10).
           MOVE ACC TO DIGIT(1).
           MOVE ZEROES TO ACC.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = 10
              COMPUTE ACC = ACC + (NCPF(I) * (12 - I))
           END-PERFORM.
           COMPUTE ACC = ACC + (DIGIT(1) * 2).
           COMPUTE ACC = FUNCTION MOD(ACC, 11).
           COMPUTE ACC = 11 - ACC.
           COMPUTE ACC = FUNCTION MOD(ACC, 10).
           MOVE ACC TO DIGIT(2).
           MOVE CPF TO DCPF.
           DISPLAY "CPF NO: "DCPF.
           IF DIGIT(1) = NCPF(10) AND DIGIT(2) = NCPF(11)
      *    If the CPF is valid, it says it's OK.
               DISPLAY "OK!"
           ELSE
      *    Else, is warns that it's not valid and shows what should be
      *    the valid digits.
               DISPLAY "~OK!"
               MOVE CPF TO VCPF
               MOVE VCPF TO DVCPF
      *        Displays the valid number with digits.
               DISPLAY "NO VÁLIDO: "DVCPF "/" DIGIT(1) DIGIT(2)
           END-IF.
           STOP RUN.
       END PROGRAM VER-CPF.
