
for iS in  1437 1501 2453 2517 3477 3541   469 405 
do

outf='broteR_TT_'$iS'.dat'

./broteR_TT << EOF
$iS
0.01d0
0.99d0
21
0.001d0
0.999d0
0.01d0
$outf
EOF

done
