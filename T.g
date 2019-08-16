grammar T;
primera : llamada+;
llamada : 'call' ID ';'
{System.out.println("invoca a f:" + $ID.text);};
ID: ('a'..'z')('a'..'z'|'0'..'9')*
WS: (' '|'\n'|'\r'|'\t')+ {$chanel=HIDDEN;}