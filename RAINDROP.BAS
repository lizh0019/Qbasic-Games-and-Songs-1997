10 SCREEN 1, 0: COLOR 17, 0: CLS
FOR k = 1 TO 6
LINE (200, 0)-(160, k + 14): LINE -(235, 7 + k \ 2)
LINE -(230, 14 + k): LINE -(275, 7 + k \ 2)
LINE -(275, 20 + k \ 2): LINE -(300, 0)
PAINT (210, 3), 1, 3
CIRCLE (275, 19 + k), 2
LINE (275, 20 + k \ 2)-(290, 0)
LINE (160, 14 + k)-(235, 0)
LINE (230, 14 + k)-(260, 0)
FOR s = 1 TO 500: NEXT s
IF k = 6 GOTO 250
LINE (200, 0)-(160, k + 14), 0
LINE -(235, 7 + k \ 2), 0
LINE -(230, 14 + k), 0: LINE -(275, 7 + k \ 2)
LINE -(275, 20 + k \ 2), 0: LINE -(300, 0), 0
CIRCLE (275, 19 + k), 2, 0
LINE (275, 20 + k \ 2)-(290, 0), 0
LINE (160, 14 + k)-(235, 0), 0
LINE (230, 14 + k)-(260, 0), 0: NEXT
250 FOR i = 60 TO 90
CIRCLE (160, i), 9 - i / 10, 3
FOR j = 1 TO 100: NEXT j
CIRCLE (160, i), 9 - i / 10, 0
NEXT i: SOUND 900, 1
FOR i = 1 TO 40
CIRCLE (160, 130), i / 2, 3, , , .3
FOR q = 1 TO 100: NEXT q
CIRCLE (160, 130), i / .7, 3, , , .3
FOR q = 1 TO 100: NEXT q
CIRCLE (160, 130), i, 3, , , .3
FOR q = 1 TO 100: NEXT q
CIRCLE (160, 130), i / .7, 0, , , .3
CIRCLE (160, 130), i / 2, 0, , , .3
CIRCLE (160, 130), i, 0, , , .3: NEXT i
FOR i = 30 TO 60
CIRCLE (230, i), 6 - i / 10, 3
FOR j = 1 TO 100: NEXT j
CIRCLE (230, i), 6 - i / 10, 0
NEXT i: SOUND 1000, 1
FOR i = 1 TO 40
CIRCLE (230, 120), i / 2, 3, , , .3
FOR q = 1 TO 100: NEXT q
CIRCLE (230, 120), i / .7, 3, , , .3
FOR q = 1 TO 100: NEXT q
CIRCLE (230, 120), i / .7, 3, , , .3
FOR q = 1 TO 100: NEXT q
CIRCLE (230, 120), i / 2, 0, , , .3
CIRCLE (230, 120), i / .7, 0, , , .3
CIRCLE (230, 120), i, 0, , , .3
NEXT i: IF INKEY$ = CHR$(27) THEN CLS : END
GOTO 10


