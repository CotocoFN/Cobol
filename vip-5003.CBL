       IDENTIFICATION DIVISION.
       PROGRAM-ID.           VIP-5003.
       AUTHOR.               BRUNO.
      ******************************************************************
      *                      CONSULTA-COMPRA                           *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "VIP-COMPRA.SL".
       DATA DIVISION.
       FILE SECTION.
           COPY "VIP-COMPRA.FD".
      ******************************************************************
      *                      WORKING-STORAGE                           *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  STATUS-DATA.
           03 FS-VIP-COMPRA            PIC X(02).
              88 VALID-VIP-COMPRA      VALUE "00" THRU "09".
       01  COMPRA-ID                   PIC 9(04).
       01  COMPRA.
           03 COM-NM-FOR               PIC X(60).
           03 COM-PROD-NM              PIC X(20).
           03 COM-PROD-CD              PIC 9(02).
           03 COM-QT                   PIC ZZ.ZZ9.
           03 COM-VALOR-UNI            PIC $ZZZ.ZZZ,99.
           03 COM-VALOR-TOT            PIC $ZZ.ZZZ.ZZZ.ZZZ,99.
       01  KEYSTATUS                 PIC 9(04) SPECIAL-NAMES CRT STATUS.
           88 ESC-KEY                  VALUE 27.
           88 ENTER-KEY                VALUE 13.
           88 F1-KEY                   VALUE 1.
           88 F2-KEY                   VALUE 2.
           88 F3-KEY                   VALUE 3.
           88 F4-KEY                   VALUE 4.
      ******************************************************************
      *                      SCREEN-SECTION                            *
      ******************************************************************
       SCREEN SECTION.
       01  CONSULTA-COMPRA.
           03 LINE 08 COL 20           VALUE "CONSULTA COMPRA".
           03 LINE 10 COL 20           VALUE "ID:".
           03 LINE 13 COL 20           VALUE "FORNECEDOR:".
           03 LINE 14 COL 20           VALUE "PRODUTO:".
           03 LINE 15 COL 20           VALUE "QUANTIDADE:".
           03 LINE 16 COL 20           VALUE "VALOR UNITARIO:".
           03 LINE 17 COL 20           VALUE "VALOR TOTAL:".

      ******************************************************************
      *                      PROCEDURE DIVISION                        *
      ******************************************************************
       PROCEDURE DIVISION.
           OPEN I-O VIP-COMPRA.
           IF NOT VALID-VIP-COMPRA
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-COMPRA!"
                            X"0A" "FILE STATUS:" FS-VIP-COMPRA
              GO FIM-CONSULTA
           END-IF.
           DISPLAY WINDOW ERASE.
           DISPLAY CONSULTA-COMPRA.
      *-----------------------------------------------------------------
       CONSULTA-COM.
           ACCEPT COMPRA-ID            AT 1024 CONTROL KEY IN KEYSTATUS.
           IF ESC-KEY
              GO FIM-CONSULTA
           END-IF.
      *-----------------------------------------------------------------
       COM-AUTOMATICA.
           IF F1-KEY
              INITIALIZE REG-COMPRA
              START VIP-COMPRA KEY > COMPRA-CHAVE
              READ VIP-COMPRA NEXT WITH NO LOCK
              IF NOT VALID-VIP-COMPRA
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-COMPRA
              END-IF
              PERFORM MOVER-COMPRA-PARA-TELA
              PERFORM RESULTADO
              GO CONSULTA-COM
           END-IF.
           IF F2-KEY
              READ VIP-COMPRA PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-COMPRA
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-COMPRA
              END-IF
              PERFORM MOVER-COMPRA-PARA-TELA
              PERFORM RESULTADO
              GO CONSULTA-COM
           END-IF.

           IF F3-KEY
              READ VIP-COMPRA NEXT WITH NO LOCK
              IF NOT VALID-VIP-COMPRA
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-COMPRA
              END-IF
              PERFORM MOVER-COMPRA-PARA-TELA
              PERFORM RESULTADO
              GO CONSULTA-COM
           END-IF.
           IF F4-KEY
              MOVE HIGH-VALUE          TO COMPRA-CD
              START VIP-COMPRA KEY < COMPRA-CHAVE
              READ VIP-COMPRA PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-COMPRA
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
              END-IF
              PERFORM MOVER-COMPRA-PARA-TELA
              PERFORM RESULTADO
              GO CONSULTA-COM
           END-IF.
      *-----------------------------------------------------------------
       COM-MANUAL.

           INITIALIZE REG-COMPRA.
           MOVE COMPRA-ID              TO COMPRA-CD.
           READ VIP-COMPRA WITH NO LOCK.
           IF NOT VALID-VIP-COMPRA
              DISPLAY MESSAGE BOX "CODIGO INVALIDO"
                            X"0A" "FILE STATUS:" FS-VIP-COMPRA
              GO CONSULTA-COM
           END-IF.
           PERFORM MOVER-COMPRA-PARA-TELA.
           PERFORM RESULTADO.
           GO CONSULTA-COM.
      ******************************************************************
      *                      FUNCTIONS                                 *
      ******************************************************************
       MOVER-COMPRA-PARA-TELA.
           READ VIP-COMPRA WITH NO LOCK.
           IF NOT VALID-VIP-COMPRA
              DISPLAY MESSAGE BOX "CODIGO INVALIDO"
                            X"0A" "FILE STATUS:" FS-VIP-COMPRA
              GO CONSULTA-COM
           ELSE
              MOVE COMPRA-CD           TO COMPRA-ID
              MOVE COMPRA-NM-FOR       TO COM-NM-FOR
              MOVE COMPRA-PROD-NM      TO COM-PROD-NM
              MOVE COMPRA-PROD-CD      TO COM-PROD-CD
              MOVE COMPRA-QT           TO COM-QT
              MOVE COMPRA-VALOR-UNI    TO COM-VALOR-UNI
              MOVE COMPRA-VALOR-TOT    TO COM-VALOR-TOT
           END-IF.
       RESULTADO.
           DISPLAY COMPRA-ID           AT 1024.
           DISPLAY COM-NM-FOR          AT 1331.
           DISPLAY COM-PROD-CD         AT 1428.
           DISPLAY COM-PROD-NM         AT 1431.
           DISPLAY COM-QT              AT 1531.
           DISPLAY COM-VALOR-UNI       AT 1635.
           DISPLAY COM-VALOR-TOT       AT 1732.
      *=================================================================
       FIM-CONSULTA.
           CLOSE VIP-COMPRA.
           EXIT PROGRAM.