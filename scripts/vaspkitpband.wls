#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[Directory[]];
pbanddata1=Import["PBAND_Mo.dat","Table"];
pbanddata2=Select[pbanddata1,# != {} && Characters[#][[1,1]] != "#"&];
NKPTS=pbanddata1[[2,5]];
NBANDS=pbanddata1[[2,6]];
PBAND=Array[pband,NBANDS];
Table[pband[i+1]=Sort[pbanddata2[[i*NKPTS+1;;(i+1)*NKPTS]],#1[[1]]<#2[[1]]&],{i,0,NBANDS-1}];
orb=<|Table[pbanddata1[[1]][[n]]->n,{n,1,Length[pbanddata1[[1]]]}]|>;


kpoints=Select[Import["KPOINTS","Table"],# != {} && Characters[#][[1,1]] != "#" &];
KPTS=NKPTS/kpoints[[2,1]];
Array[K,KPTS];
Table[K[i]=Transpose[pband[1]][[1,20*i]],{i,1,KPTS}];
KK=Table[{K[i],kpoints[[4+2*i,4]]},{i,1,KPTS}];
PrependTo[KK,{K[0]=Transpose[pband[1]][[1,1]],kpoints[[5,4]]}];
KK=KK/.{"GAMMA"->"\[CapitalGamma]"}


pbnd[orbitals_,color_:Red,legend_:"orbitals",scaling_:10]:=Legended[
Graphics[Table[{PointSize[Sum[pband[j][[i,orbitals[[k]]]],{k,Length[orbitals]}]/scaling],color,Tooltip[Point[{pband[j][[i,1]],pband[j][[i,2]]}]]},{j,1,NBANDS},{i,1,NKPTS}],
Axes->False,GridLines->{Transpose[KK][[1]],{0}},GridLinesStyle->Directive[Orange, Dashed],
Frame->True,FrameTicks->{{Automatic,None},{KK,None}},FrameStyle->Directive[Purple],FrameLabel->{"Wave vector","Energy(eV)"},
AspectRatio->2/3,PlotLabel->Style["Projected Band-Structure of Orbitals",16,Blue],
PlotRange->{{K[0],K[KPTS]},{-8,8}},PlotRangeClipping->True],
Placed[PointLegend[{color},{legend}],Right]
];


pbandfig=Show[
pbnd[{orb["s"]},Green,"s",30],
pbnd[{orb["py"]},Purple,"\!\(\*SubscriptBox[\(p\), \(y\)]\)",30],pbnd[{orb["pz"]},Orange,"\!\(\*SubscriptBox[\(p\), \(z\)]\)",30],pbnd[{orb["px"]},Cyan,"\!\(\*SubscriptBox[\(p\), \(x\)]\)",30],
pbnd[{orb["dxy"]},Brown,"\!\(\*SubscriptBox[\(d\), \(xy\)]\)",30],pbnd[{orb["dyz"]},Red,"\!\(\*SubscriptBox[\(d\), \(yz\)]\)",30],pbnd[{orb["dz2"]},Blue,"\!\(\*SubscriptBox[\(d\), SuperscriptBox[\(z\), \(2\)]]\)",30],pbnd[{orb["dxz"]},Yellow,"\!\(\*SubscriptBox[\(d\), \(xz\)]\)",30],pbnd[{orb["x2-y2"]},Pink,"\!\(\*SubscriptBox[\(d\), \(\*SuperscriptBox[\(x\), \(2\)] - \*SuperscriptBox[\(y\), \(2\)]\)]\)",30]
];


Export["pband.pdf",pbandfig,ImageResolution->500];
figpath=ExpandFileName[%];
Print["orbitals:",orb]
Print["Number of k-points on each band:",NKPTS]
Print["Total band number:", NBANDS]
Print["The path of the figure:",figpath]



