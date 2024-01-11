
for iS in 2453 #1437 1501 2453 2517 3477 3541   469 405 
do

outf='broteR_TR_'$iS'.dat'

./broteR_TR << EOF
$iS
0.01d0
0.99d0
41
0.d0
0.99d0
0.01d0
$outf
EOF

done
