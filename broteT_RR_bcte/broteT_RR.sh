
for iS in  469 # 1501 2453 2517 3477 3541   469 405 
do

outf='broteT_RR_'$iS'.dat'

./broteT_RR << EOF
$iS
0.01d0
0.99d0
21
0.01d0
0.99d0
0.01d0
$outf
EOF

done
