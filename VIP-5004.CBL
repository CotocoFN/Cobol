       IDENTIFICATION DIVISION.
       PROGRAM-ID.           VIP-5004.
       AUTHOR.               BRUNO.
      ******************************************************************
      *                      CONSULTA VENDA                            *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "VIP-VENDA.SL".
           COPY "VIP-ESTOQUE.SL".
           COPY "VIP-CLIFOR.SL".
       DATA DIVISION.
       FILE SECTION.
           COPY "VIP-VENDA.FD".
           COPY "VIP-ESTOQUE.FD".
           COPY "VIP-CLIFOR.FD".
      ******************************************************************
      *                      WORKING-STORAGE                           *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  STATUS-DATA.
           03 FS-VIP-VENDA             PIC X(02).
              88 VALID-VIP-VENDA       VALUE "00" THRU "09".
           03 FS-VIP-ESTOQUE           PIC X(02).
              88 VALID-VIP-ESTOQUE     VALUE "00" THRU "09".
           03 FS-VIP-CLIFOR            PIC X(02).
              88 VALID-VIP-CLIFOR      VALUE "00" THRU "09".
       01  ID-CONSULTA                 PIC 9(02).
       01  CD-CLIFOR-CONSULTA          PIC 9(02).
       01  VENDA-CLIFOR.
           03 CD-CLIFOR                PIC ZZZ.ZZ9.9.
           03 NM-CLIFOR                PIC X(60).

       01  VENDA-PROD.
           03 CD-PROD                  PIC ZZZ.ZZZ.9.
           03 NM-PROD                  PIC X(60).
           03 QT-PROD                  PIC ZZZ.ZZZ.ZZZ.ZZZ.Z99.
           03 VALOR-UNI-PROD           PIC ZZZ.ZZZ.ZZZ.ZZZ.Z99.
           03 VALOR-TOT-PROD           PIC ZZZ.ZZZ.ZZZ.ZZZ.Z99.
       01  KEYSTATUS                 PIC 9(04) SPECIAL-NAMES CRT STATUS.
           88 ESC-KEY                  VALUE 27.
           88 ENTER-KEY                VALUE 13.
           88 F1-KEY                   VALUE 1.
           88 F2-KEY                   VALUE 2.
           88 F3-KEY                   VALUE 3.
           88 F4-KEY                   VALUE 4.
      ******************************************************************
      *                      SECREEN-SECTION                           *
      ******************************************************************
       SCREEN SECTION.
       01  CONSULTA-PRINCIPAL.
           03 LINE 08 COL 20           VALUE "CONSULTA VENDA".
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
           OPEN INPUT VIP-VENDA.
           IF NOT VALID-VIP-VENDA
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-VENDA!"
                            X"0A" "FILE STATUS:" FS-VIP-VENDA
              GO FIM-CONSULTA
           END-IF.
           OPEN INPUT VIP-ESTOQUE.
           IF NOT VALID-VIP-ESTOQUE
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-ESTOQUE!"
                            X"0A" "FILE STATUS:" FS-VIP-ESTOQUE
              GO FIM-CONSULTA
           END-IF.
           OPEN INPUT VIP-CLIFOR.
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-CLIFOR!"
                            X"0A" "FILE STATUS:" FS-VIP-CLIFOR
              GO FIM-CONSULTA
           END-IF.
      *-----------------------------------------------------------------
           DISPLAY WINDOW ERASE.
           DISPLAY CONSULTA-PRINCIPAL.
       CONSULTA-VENDA.
           ACCEPT ID-CONSULTA          AT 1024 CONTROL KEY IN KEYSTATUS.
           IF ESC-KEY
              GO FIM-CONSULTA
           END-IF.
      *-----------------------------------------------------------------
           IF F1-KEY
              INITIALIZE REG-VENDA
              START VIP-VENDA KEY > VENDA-CHAVE-1
              READ VIP-VENDA NEXT WITH NO LOCK
              IF NOT VALID-VIP-VENDA
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-VENDA
              ELSE
                 PERFORM MOVER-VENDA-PARA-TELA
                 PERFORM RESULTADO
              END-IF
              GO CONSULTA-VENDA
           END-IF.
           IF F2-KEY
              READ VIP-VENDA PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-VENDA
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-VENDA
              ELSE
                 PERFORM MOVER-VENDA-PARA-TELA
                 PERFORM RESULTADO
              END-IF
              GO CONSULTA-VENDA
           END-IF.
           IF F3-KEY
              READ VIP-VENDA NEXT WITH NO LOCK
              IF NOT VALID-VIP-VENDA
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
                               X"0A" "FILE STATUS:" FS-VIP-VENDA
              ELSE
                 PERFORM MOVER-VENDA-PARA-TELA
                 PERFORM RESULTADO
              END-IF
              GO CONSULTA-VENDA
           END-IF.
           IF F4-KEY
              MOVE HIGH-VALUE          TO VENDA-CD-ESTOQUE
              START VIP-VENDA KEY < VENDA-CHAVE-2
              READ VIP-VENDA PREVIOUS WITH NO LOCK
              IF NOT VALID-VIP-VENDA
                 DISPLAY MESSAGE BOX "REGISTRO NAO ENCONTRADO"
              ELSE
                 PERFORM MOVER-VENDA-PARA-TELA
                 PERFORM RESULTADO
              END-IF
              GO CONSULTA-VENDA
           END-IF.
      *-----------------------------------------------------------------
       VENDA-MANUAL.
           INITIALIZE REG-VENDA.
           MOVE ID-CONSULTA            TO VENDA-ID-VENDA.
           READ VIP-VENDA  WITH NO LOCK.
           IF NOT VALID-VIP-VENDA
              DISPLAY MESSAGE BOX "CODIGO INVALIDO!"
                            X"0A" "FILE STATUS:" FS-VIP-VENDA
              GO CONSULTA-VENDA
           ELSE
              PERFORM MOVER-VENDA-PARA-TELA
              PERFORM RESULTADO
           END-IF.
           GO CONSULTA-VENDA.
      ******************************************************************
      *                      FUNCTIONS                                 *
      ******************************************************************
       MOVER-VENDA-PARA-TELA.
           READ VIP-VENDA WITH NO LOCK.
           IF NOT VALID-VIP-VENDA
              DISPLAY MESSAGE BOX "ID-CONSULTA INVALIDO!"
                            X"0A" "FILE STATUS" FS-VIP-VENDA
              GO CONSULTA-VENDA
           ELSE
              MOVE VENDA-ID-VENDA      TO ID-CONSULTA
              MOVE VENDA-NM-ESTOQUE    TO NM-PROD
              MOVE VENDA-CD-ESTOQUE    TO CD-PROD
              MOVE VENDA-QT-ESTOQUE    TO QT-PROD
              MOVE VENDA-VALOR-UNI-ESTOQUE TO VALOR-UNI-PROD
              MOVE VENDA-VALOR-TOT-ESTOQUE TO VALOR-TOT-PROD

              MOVE VENDA-CD-CLIFOR     TO CLIFOR-CD-CLIFOR
              READ VIP-CLIFOR WITH NO LOCK
              MOVE CLIFOR-NM-CLIFOR    TO NM-CLIFOR
           END-IF.
       RESULTADO.
           DISPLAY ID-CONSULTA         AT 1023.
           MOVE CLIFOR-CD-CLIFOR       TO CD-CLIFOR-CONSULTA.
           DISPLAY CD-CLIFOR-CONSULTA  AT 1331.
           DISPLAY "/"                 AT 1338.
           DISPLAY NM-CLIFOR           AT 1339.
           DISPLAY CD-PROD             AT 1428.
           DISPLAY "/"                 AT 1435.
           DISPLAY NM-PROD             AT 1436.
           DISPLAY QT-PROD             AT 1531.
           DISPLAY VALOR-UNI-PROD      AT 1635.
           DISPLAY VALOR-TOT-PROD      AT 1732.
      *-----------------------------------------------------------------
       FIM-CONSULTA.
           CLOSE VIP-VENDA.
           EXIT PROGRAM.
