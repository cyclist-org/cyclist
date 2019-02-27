#!/bin/bash

./temporal_ctl_prove.native -s -P ./benchmarks/temporal/jar/exmp1_1.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -F -s -P ./benchmarks/temporal/jar/exmp1_2.ctl
printf "\n";
./temporal_ctl_prove.native -s -P ./benchmarks/temporal/jar/exmp1_3.ctl
printf "\n";
./temporal_ctl_prove.native -s -P ./benchmarks/temporal/jar/exmp1_4.ctl
printf "\n";
./temporal_ctl_prove.native -M 30 -F -s -P ./benchmarks/temporal/jar/exmp1_5.ctl
printf "\n";
./temporal_ctl_prove.native -M 28 -F -s -P ./benchmarks/temporal/jar/exmp1_6.ctl
printf "\n";
./temporal_ctl_prove.native -M 100 -t 10 -s -P ./benchmarks/temporal/jar/exmp1_7.ctl
printf "\n";
./temporal_ctl_prove.native -M 20 -s -P ./benchmarks/temporal/jar/Fin-Lock1.ctl
printf "\n";
./temporal_ctl_prove.native -M 12 -s -P ./benchmarks/temporal/jar/Fin-Lock2.ctl
printf "\n";
./temporal_ctl_prove.native -M 20 -s -P ./benchmarks/temporal/jar/Fin-Lock3.ctl
printf "\n";
./temporal_ctl_prove.native -M 100 -t 10 -s -P ./benchmarks/temporal/jar/Fin-Lock1.ctl
printf "\n";
./temporal_ctl_prove.native -M 16 -s -P ./benchmarks/temporal/jar/Inf-Lock1.ctl
printf "\n";
./temporal_ctl_prove.native -M 16 -s -P ./benchmarks/temporal/jar/Inf-Lock2.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -M 20 -s -P ./benchmarks/temporal/jar/Inf-Lock3.ctl
printf "\n";
./temporal_ctl_prove.native -M 18 -F -s -P ./benchmarks/temporal/jar/Nd-Inf-Lock1.ctl
printf "\n";
./temporal_ctl_prove.native -M 19 -F -s -P ./benchmarks/temporal/jar/Nd-Inf-Lock2.ctl
printf "\n";
./temporal_ctl_prove.native -M 100 -t 10 -s -P ./benchmarks/temporal/jar/Nd-Inf-Lock3.ctl
printf "\n";
./temporal_ctl_prove.native -M 7 -s -P ./benchmarks/temporal/jar/Insert-List1.ctl
printf "\n";
./temporal_ctl_prove.native -M 18 -F -s -P ./benchmarks/temporal/jar/Insert-List2.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -NC -M 28 -F -s -P ./benchmarks/temporal/jar/Insert-List3.ctl
printf "\n";
./temporal_ctl_prove.native -M 5 -s -P ./benchmarks/temporal/jar/Cyclic-List1.ctl
printf "\n";
./temporal_ctl_prove.native -M 6 -s -P ./benchmarks/temporal/jar/Cyclic-List2.ctl
printf "\n";
./temporal_ctl_prove.native -M 6 -s -D examples/bt.def -P ./benchmarks/temporal/jar/Inf-BinTree.ctl
printf "\n";
./temporal_ctl_prove.native -M 6 -t 10 -s -D examples/bt.def -P ./benchmarks/temporal/jar/Inf-BinTree2.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -M 40 -NC -s -P ./benchmarks/temporal/jar/AFEFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 26 -NC -s -P ./benchmarks/temporal/jar/AFAGBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 7 -s -P ./benchmarks/temporal/jar/EGAGBranch.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -M 44 -c -NC -s -P ./benchmarks/temporal/jar/EGAFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -M 30 -NC -s -P ./benchmarks/temporal/jar/EGImpEFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 50 -NC -F -s -P ./benchmarks/temporal/jar/EGImpAFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 14 -NC -s -P ./benchmarks/temporal/jar/AGImpEGBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 24 -NC -s -P ./benchmarks/temporal/jar/AGImpEFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 16 -s -P ./benchmarks/temporal/jar/Acq-rel3.ctl
printf "\n";
./temporal_ctl_prove.native -M 12 -F -s -P ./benchmarks/temporal/jar/Acq-rel4.ctl
printf "\n";
./temporal_ctl_prove.native -M 16 -s -P ./benchmarks/temporal/jar/Acq-rel5.ctl
printf "\n";
./temporal_ctl_prove.native -M 10 -F -s -P ./benchmarks/temporal/jar/Acq-rel6.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/jar/PostgreSQL1.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/jar/PostgreSQL1.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/jar/PostgreSQL2.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/jar/PostgreSQL3.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/jar/PostgreSQL4.ctl
printf "\n";
./temporal_ctl_prove.native -M 31 -s -P ./benchmarks/temporal/jar/WinUpdate1.ctl
printf "\n";
./temporal_ctl_prove.native -M 5 -s -P ./benchmarks/temporal/jar/WinUpdate2.ctl
printf "\n";
./temporal_ctl_prove.native -M 31 -s -P ./benchmarks/temporal/jar/WinUpdate3.ctl
printf "\n";
./temporal_ctl_prove.native -M 31 -F -s -P ./benchmarks/temporal/jar/WinUpdate4.ctl
printf "\n";
./temporal_ctl_prove.native -M 5 -s -P ./benchmarks/temporal/jar/WinUpdate5.ctl
printf "\n";
./temporal_ctl_prove.native -M 31 -F -s -P ./benchmarks/temporal/jar/WinUpdate6.ctl
printf "\n";
