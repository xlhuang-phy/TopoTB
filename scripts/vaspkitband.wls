#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[Directory[]];
banddata1=Import["BAND.dat","Table"];
banddata2=Select[banddata1,# != {} && Characters[#][[1,1]] != "#" &];
NKPTS=banddata1[[2,5]];
NBANDS=banddata1[[2,6]];
BAND=Array[band,NBANDS];
Table[band[i+1]=Sort[banddata2[[i*NKPTS+1;;(i+1)*NKPTS]],#1[[1]]<#2[[1]]&],{i,0,NBANDS-1}];


kpoints=Select[Import["KPOINTS","Table"],# != {} && Characters[#][[1,1]] != "#" &];
KPTS=NKPTS/kpoints[[2,1]];
Array[K,KPTS];
Table[K[i]=Transpose[band[1]][[1,20*i]],{i,1,KPTS}];
KK=Table[{K[i],kpoints[[4+2*i,4]]},{i,1,KPTS}];
PrependTo[KK,{K[0]=Transpose[band[1]][[1,1]],kpoints[[5,4]]}];
KK=KK/.{"GAMMA"->"\[CapitalGamma]"}


bandfig=ListLinePlot[Table[Tooltip[band[i]],{i,1,NBANDS}],
Axes->False,
GridLines->{Transpose[KK][[1]], {0}},
GridLinesStyle->Directive[Orange, Dashed],
(*PlotLabels->Table[i,{i,1,NBANDS}],*)
PlotStyle->Directive[Blue,Thick],
Frame->True,
FrameTicks->{{Automatic,None},{KK,None}},
FrameStyle->Directive[Purple],
FrameLabel->{"Wave vector","Energy(eV)"},
(*PlotTheme->"Scientific",*)
AspectRatio->2/3,
PlotRange->{{K[0],K[KPTS]},{-5,5}}];


Export["band.png",bandfig,ImageResolution->500];
figpath=ExpandFileName[%];
Print["Number of k-points on each band:",NKPTS]
Print["Total band number:", NBANDS]
