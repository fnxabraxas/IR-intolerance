
for iS in   1501 1437  1501 2453 2517 3477 3541   469 405 
do

outf='broteT_RT_'$iS'.dat'

./broteT_RT << EOF
$iS
0.01d0
0.99d0
21
0.d0
0.99d0
0.01d0
$outf
EOF

done
