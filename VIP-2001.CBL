       IDENTIFICATION DIVISION.
       PROGRAM-ID.           VIP-1005.
       AUTHOR.               BRUNO.
      ******************************************************************
      *                      COMPRA / ENTRADA                          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "VIP-COMPRA.SL".
           COPY "VIP-ESTOQUE.SL".
           COPY "VIP-CLIFOR.SL".
       DATA DIVISION.
       FILE SECTION.
           COPY "VIP-COMPRA.FD".
           COPY "VIP-ESTOQUE.FD".
           COPY "VIP-CLIFOR.FD".
      ******************************************************************
      *                      WORKING-STORAGE                           *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  STATUS-DATA.
           05 FS-VIP-COMPRA            PIC X(02).
              88 VALID-VIP-COMPRA      VALUE "00" THRU "09".
       01  STATUS-DATA.
           05 FS-VIP-ESTOQUE           PIC X(02).
              88 VALID-VIP-ESTOQUE     VALUE "00" THRU "09".
       01  STATUS-DATA.
           05 FS-VIP-CLIFOR            PIC X(02).
              88 VALID-VIP-CLIFOR      VALUE "00" THRU "09".
       01  COMPRA-ID                   PIC 9(02).
       01  WSS-VL-CUSTO-MEDIO          PIC 9(07)V99.

       01  CADASTRO.
           03 NOME-FOR                 PIC X(60).
           03 CD-FOR                   PIC ZZZZZZ9.
           03 PRODUTO-CD               PIC 9(02).
           03 PRODUTO-NM               PIC X(60).
           03 QT-PROD                  PIC 9(15)v9999.
           03 VALOR-UNI                PIC 9(10)v99.
           03 VALOR-TOT                PIC 9(10)v99.
       01  KEYSTATUS                 PIC 9(04) SPECIAL-NAMES CRT STATUS.
           88 ESC-KEY                  VALUE 27.
           88 ENTER-KEY                VALUE 13.
           88 F1-KEY                   VALUE 1.
           88 F2-KEY                   VALUE 2.
           88 F3-KEY                   VALUE 3.
           88 F4-KEY                   VALUE 4.
       01  OPC-CONFIRMA.
           03 CONFIRMA                 PIC X(01).
      ******************************************************************
      *                      SCREEN-SECTION                            *
      ******************************************************************
       SCREEN SECTION.
       01  INICIAL.
           03 LINE 08 COL 25           VALUE "COMPRA/ENTRADA".
           03 LINE 09 COL 25           VALUE "ID:".
           03 LINE 10 COL 20           VALUE "CODIGO PROD:".
           03 LINE 13 COL 20           VALUE "FORNECEDOR:".
           03 LINE 14 COL 20           VALUE "PRODUTO:".
           03 LINE 15 COL 20           VALUE "QUANTIDADE:".
           03 LINE 16 COL 20           VALUE "VALOR UNITARIO:".
           03 LINE 18 COL 20           VALUE "VALOR TOTAL:".
           03 LINE 19 COL 20           VALUE "VALOR MEDIO:".

       01  OPC.
           03 LINE 25 COL 15           VALUE "CONFIRMAR <S>/<N>:".
           03 LINE 26 COL 27           VALUE "OPCAO:".
      ******************************************************************
      *                      PROCEDURE DIVISION                        *
      ******************************************************************
       PROCEDURE DIVISION.
           OPEN I-O VIP-COMPRA.
           IF NOT VALID-VIP-COMPRA
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-COMPRA!"
                            X"0A" "FILES STATUS:" FS-VIP-COMPRA
              GO FIM-PD-COMPRA
           END-IF.
           OPEN I-O VIP-ESTOQUE.
           IF NOT VALID-VIP-ESTOQUE
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-ESTOQUE!"
                            X"0A" "FILES STATUS:" FS-VIP-ESTOQUE
              GO FIM-PD-COMPRA
           END-IF.
           OPEN I-O VIP-CLIFOR.
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "ERRO AO ABRIR ARQUIVO VIP-CLIFOR!"
                            X"0A" "FILES STATUS:" FS-VIP-CLIFOR
              GO FIM-PD-COMPRA
           END-IF.
       PD-COMPRA.
           DISPLAY WINDOW ERASE.
           DISPLAY INICIAL.
      *-----------------------------------------------------------------
       ATRIBUIR-ID.
           INITIALIZE REG-COMPRA
           MOVE HIGH-VALUE             TO COMPRA-CD
           START VIP-COMPRA KEY < COMPRA-CHAVE
           READ VIP-COMPRA PREVIOUS WITH LOCK
           IF NOT VALID-VIP-COMPRA
              MOVE 1                   TO COMPRA-CD
           ELSE
              ADD 1                    TO COMPRA-CD
           END-IF.
           MOVE COMPRA-CD              TO COMPRA-ID.
           DISPLAY COMPRA-ID           AT 0928.
      *-----------------------------------------------------------------
       INICIO-CAD.
           ACCEPT PRODUTO-CD           AT 1032.
           IF ESC-KEY
              GO FIM-PD-COMPRA
           END-IF.
           INITIALIZE REG-ESTOQUE.
           MOVE PRODUTO-CD             TO ESTOQUE-CD-PROD.
           READ VIP-ESTOQUE WITH NO LOCK.
           IF NOT VALID-VIP-ESTOQUE
              DISPLAY MESSAGE BOX "PRODUTO NAO ENCONTRADO!"
                            X"0A" "FILE STATUS:" FS-VIP-ESTOQUE
              GO INICIO-CAD
           ELSE
              MOVE CLIFOR-NM-CLIFOR    TO NOME-FOR
           END-IF.
           PERFORM MOVER-REGISTROS.
           PERFORM MOVER-TELA-PARA-REGISTRO.
           PERFORM RESULTADO-TELA.
      *-----------------------------------------------------------------
           ACCEPT QT-PROD              AT 1531.
           ACCEPT VALOR-UNI            AT 1635.

           PERFORM VALOR-MEDIO.
           PERFORM RESULTADO-TELA.
           DISPLAY OPC.
           ACCEPT OPC-CONFIRMA         AT 2533.
           IF OPC-CONFIRMA EQUAL "S" OR
              OPC-CONFIRMA EQUAL "s"
              INITIALIZE REG-COMPRA
              MOVE HIGH-VALUE          TO COMPRA-CD
              START VIP-COMPRA KEY > COMPRA-CHAVE
              READ VIP-COMPRA NEXT WITH LOCK
              IF NOT VALID-VIP-COMPRA
                 MOVE 1                TO COMPRA-CD
              ELSE
                 ADD  1                TO COMPRA-CD
              END-IF
              PERFORM MOVER-TELA-PARA-REGISTRO
              WRITE REG-COMPRA
              IF NOT VALID-VIP-COMPRA
                 DISPLAY MESSAGE BOX "NAO FOI POSSIVEL GRAVAR!"
                               X"0A" "FILE STATUS:" FS-VIP-COMPRA
              ELSE
                 INITIALIZE REG-ESTOQUE
                 MOVE PRODUTO-CD       TO ESTOQUE-CD-PROD
                 READ VIP-ESTOQUE WITH NO LOCK
                 MOVE COMPRA-VALOR-UNI TO ESTOQUE-VALOR-PROD
                 MOVE WSS-VL-CUSTO-MEDIO TO ESTOQUE-VALOR-CM
                 IF NOT VALID-VIP-ESTOQUE
                    MOVE QT-PROD      TO ESTOQUE-QT-PROD
                    WRITE REG-ESTOQUE
                  ELSE
                    ADD  QT-PROD      TO ESTOQUE-QT-PROD
                    REWRITE REG-ESTOQUE
                  END-IF
                  DISPLAY MESSAGE BOX "GRAVADO COM SUCESSO!"
              END-IF
           END-IF.
      ******************************************************************
      *                      FUNCTIONS                                 *
      ******************************************************************
       MOVER-REGISTROS.
           INITIALIZE REG-ESTOQUE.
           MOVE PRODUTO-CD             TO ESTOQUE-CD-PROD.
           READ VIP-ESTOQUE WITH NO  LOCK.
           IF NOT VALID-VIP-ESTOQUE
              DISPLAY MESSAGE BOX "ERRO"
                            X"0A" "FILE STATUS:" FS-VIP-ESTOQUE
           ELSE
              MOVE ESTOQUE-CD-FOR      TO CD-FOR
              MOVE ESTOQUE-QT-PROD     TO QT-PROD
              MOVE ESTOQUE-NM-PROD     TO PRODUTO-NM
           END-IF.
           INITIALIZE REG-CLIFOR.
           MOVE ESTOQUE-CD-FOR         TO CLIFOR-CD-CLIFOR.
           READ VIP-CLIFOR WITH NO LOCK.
           IF NOT VALID-VIP-CLIFOR
              DISPLAY MESSAGE BOX "ERRO"
                            X"0A" "FILE STATUS" FS-VIP-CLIFOR
           ELSE
              MOVE CLIFOR-NM-CLIFOR    TO NOME-FOR
           END-IF.
       MOVER-TELA-PARA-REGISTRO.
           MOVE COMPRA-ID              TO COMPRA-CD.
           MOVE NOME-FOR               TO COMPRA-NM-FOR.
           MOVE PRODUTO-NM             TO COMPRA-PROD-NM.
           MOVE PRODUTO-CD             TO COMPRA-PROD-CD.
           MOVE QT-PROD                TO COMPRA-QT.
           MOVE VALOR-UNI              TO COMPRA-VALOR-UNI.
           MOVE VALOR-TOT              TO COMPRA-VALOR-TOT.
       VALOR-MEDIO.
      *    INITIALIZE REG-ESTOQUE
           MOVE PRODUTO-CD TO ESTOQUE-CD-PROD.
           READ VIP-ESTOQUE NEXT WITH NO LOCK.
           COMPUTE WSS-VL-CUSTO-MEDIO =
           ((ESTOQUE-QT-PROD * ESTOQUE-VALOR-PROD) +
           (QT-PROD * VALOR-UNI)) /
           (ESTOQUE-QT-PROD + QT-PROD)
           END-COMPUTE.
           DISPLAY WSS-VL-CUSTO-MEDIO AT 1932.
       RESULTADO-TELA.
           DISPLAY NOME-FOR            AT 1339.
           DISPLAY CD-FOR              AT 1331.
           DISPLAY PRODUTO-NM          AT 1429.
           COMPUTE VALOR-TOT           = QT-PROD
                                       * VALOR-UNI
           END-COMPUTE.
           DISPLAY VALOR-TOT           AT 1832.
       FIM-PD-COMPRA.
           CLOSE VIP-ESTOQUE.
           CLOSE VIP-COMPRA.
           CLOSE VIP-CLIFOR.
           EXIT PROGRAM.
           STOP RUN.
