#!/bin/bash

./temporal_ctl_prove.native -s -P ./benchmarks/temporal/cade/exmp1_1.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -F -s -P ./benchmarks/temporal/cade/exmp1_2.ctl
printf "\n";
./temporal_ctl_prove.native -s -P ./benchmarks/temporal/cade/exmp1_3.ctl
printf "\n";
./temporal_ctl_prove.native -s -P ./benchmarks/temporal/cade/exmp1_4.ctl
printf "\n";
./temporal_ctl_prove.native -M 30 -F -s -P ./benchmarks/temporal/cade/exmp1_5.ctl
printf "\n";
./temporal_ctl_prove.native -M 28 -F -s -P ./benchmarks/temporal/cade/exmp1_6.ctl
printf "\n";
./temporal_ctl_prove.native -M 20 -s -P ./benchmarks/temporal/cade/Fin-Lock1.ctl
printf "\n";
./temporal_ctl_prove.native -M 12 -s -P ./benchmarks/temporal/cade/Fin-Lock2.ctl
printf "\n";
./temporal_ctl_prove.native -M 20 -s -P ./benchmarks/temporal/cade/Fin-Lock3.ctl
printf "\n";
./temporal_ctl_prove.native -M 16 -s -P ./benchmarks/temporal/cade/Inf-Lock1.ctl
printf "\n";
./temporal_ctl_prove.native -M 16 -s -P ./benchmarks/temporal/cade/Inf-Lock2.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -M 20 -s -P ./benchmarks/temporal/cade/Inf-Lock3.ctl
printf "\n";
./temporal_ctl_prove.native -M 18 -F -s -P ./benchmarks/temporal/cade/Nd-Inf-Lock1.ctl
printf "\n";
./temporal_ctl_prove.native -M 19 -F -s -P ./benchmarks/temporal/cade/Nd-Inf-Lock2.ctl
printf "\n";
./temporal_ctl_prove.native -M 7 -s -P ./benchmarks/temporal/cade/Insert-List1.ctl
printf "\n";
./temporal_ctl_prove.native -M 18 -F -s -P ./benchmarks/temporal/cade/Insert-List2.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -NC -M 28 -F -s -P ./benchmarks/temporal/cade/Insert-List3.ctl
printf "\n";
./temporal_ctl_prove.native -M 5 -s -P ./benchmarks/temporal/cade/Cyclic-List1.ctl
printf "\n";
./temporal_ctl_prove.native -M 6 -s -P ./benchmarks/temporal/cade/Cyclic-List2.ctl
printf "\n";
./temporal_ctl_prove.native -M 6 -s -D examples/bt.def -P ./benchmarks/temporal/cade/Inf-BinTree.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -M 40 -NC -s -P ./benchmarks/temporal/cade/AFEFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 26 -NC -s -P ./benchmarks/temporal/cade/AFAGBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 7 -s -P ./benchmarks/temporal/cade/EGAGBranch.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -M 44 -c -NC -s -P ./benchmarks/temporal/cade/EGAFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -t 0 -M 30 -NC -s -P ./benchmarks/temporal/cade/EGImpEFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 50 -NC -F -s -P ./benchmarks/temporal/cade/EGImpAFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 14 -NC -s -P ./benchmarks/temporal/cade/AGImpEGBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 24 -NC -s -P ./benchmarks/temporal/cade/AGImpEFBranch.ctl
printf "\n";
./temporal_ctl_prove.native -M 16 -s -P ./benchmarks/temporal/cade/Acq-rel3.ctl
printf "\n";
./temporal_ctl_prove.native -M 12 -F -s -P ./benchmarks/temporal/cade/Acq-rel4.ctl
printf "\n";
./temporal_ctl_prove.native -M 16 -s -P ./benchmarks/temporal/cade/Acq-rel5.ctl
printf "\n";
./temporal_ctl_prove.native -M 10 -F -s -P ./benchmarks/temporal/cade/Acq-rel6.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/cade/PostgreSQL1.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/cade/PostgreSQL1.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/cade/PostgreSQL2.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/cade/PostgreSQL3.ctl
printf "\n";
./temporal_ctl_prove.native -M 40 -s -P ./benchmarks/temporal/cade/PostgreSQL4.ctl
printf "\n";
./temporal_ctl_prove.native -M 31 -s -P ./benchmarks/temporal/cade/WinUpdate1.ctl
printf "\n";
./temporal_ctl_prove.native -M 5 -s -P ./benchmarks/temporal/cade/WinUpdate2.ctl
printf "\n";
./temporal_ctl_prove.native -M 31 -s -P ./benchmarks/temporal/cade/WinUpdate3.ctl
printf "\n";
./temporal_ctl_prove.native -M 31 -F -s -P ./benchmarks/temporal/cade/WinUpdate4.ctl
printf "\n";
./temporal_ctl_prove.native -M 5 -s -P ./benchmarks/temporal/cade/WinUpdate5.ctl
printf "\n";
./temporal_ctl_prove.native -M 31 -F -s -P ./benchmarks/temporal/cade/WinUpdate6.ctl
printf "\n";
