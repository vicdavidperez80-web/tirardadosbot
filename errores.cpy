000000** Variables con los diferentes mensajes de error del programa.
000010** Se les da un formato adecuado para ser mostrados tanto en una
000020** terminal ANSI como en Telegram:
000030     01 lista-mensajes-error.
000040         05 error-1 PIC X(256)
000050                    VALUE '<err>¡Error!</err> La expresión "' &
000060                    '<expr />" es incorrecta.' & x'0a' &
000070                    'Sólo se admiten <mono>[dados] d [caras]' &
000080                    '</mono>, gracias, con una posible' &
000090                    ' exclamación para indicar dados' &
000100                    ' explosivos. Ejemplos: "1d6", "6d6!", etc.'.
000110         05 error-2 PIC X(256)
000120                    VALUE '<err>¡Error!</err> "<expr />" no' &
000130                    ' es correcto. Obviamente no tiene sentido' &
000140                    ' lanzar 0 dados, o dados de 0 caras.' &
000150                    ' <emoji_meh />'.
000160         05 error-3 PIC X(256)
000170                    VALUE '<err>¡Error!</err> "<expr />" no' &
000180                    ' no es correcto. 99 dados y 99999 caras' &
000190                    ' deberían ser más que suficientes para' &
000200                    ' cualquier juego de rol imaginable.'.
000210         05 error-4 PIC X(256)
000220                    VALUE 'Deben escribirse dados a tirar. Por' &
000230                    ' ejemplo:' & x'0a' &
000240                    '  <mono><exec /> 1d6</mono> (un dado de' &
000250                    ' seis caras)' & x'0a' &
000260                    '  <mono><exec /> 3d6+1</mono>, <mono>' &
000270                    '<exec /> 2d12+1d4</mono> (modificadores' &
000280                    ' y tiradas complejas)'.
000290         05 error-5 PIC X(256)
000300                    VALUE '<err>¡Error!</err> No sé' &
000310                    ' interpretar "<expr />".'.
000290         05 error-6 PIC X(256)
000300                    VALUE '<err>¡Error!</err> "<expr />" es' &
000310                    ' incorrecto, porque dados explosivos (!)' &
000310                    ' y dados "fudge" (f) al mismo tiempo no' &
000310                    ' tiene sentido.'.
000320     01 FILLER REDEFINES lista-mensajes-error.
000330         05 mensaje-error OCCURS 6 TIMES PIC X(256).

