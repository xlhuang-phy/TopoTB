(* ::Package:: *)

BeginPackage["TopoTB`Plots`"]


BandPlot::usage="BandPlot[BandData_,KLabel_,color_:Blue,thickness_:0.003,aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]

Input:
BandData: Input BandData.
KLabel: Input KLabel.
color: Control the plot style, with default values after \":\"
thickness: Control the plot style, with default values after \":\"
aspectRatio: Control the plot style, with default values after \":\"
yRange: Control the plot style, with default values after \":\"

Output\:ff1a
BandFigure: Band structure.";

BandIndexPlot::usage="BandIndexPlot[BandData_,KLabel_,color_:{Blue},thickness_:0.003,aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]

Input:
BandData: Input BandData.
KLabel: Input KLabel.
color: Control the plot style, with default values after \":\"
thickness: Control the plot style, with default values after \":\"
aspectRatio: Control the plot style, with default values after \":\"
yRange: Control the plot style, with default values after \":\"

Output\:ff1a
BandFigure: Band structure\:ff08\:80fd\:5e26\:56fe\:ff09";

BandLabelsPlot::usage="BandLabelsPlot[BandData_,KLabel_,color_:Blue,thickness_:0.003,aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]

Input:
BandData: Input BandData.
KLabel: Input KLabel.
color: Control the plot style, with default values after \":\"
thickness: Control the plot style, with default values after \":\"
aspectRatio: Control the plot style, with default values after \":\"
yRange: Control the plot style, with default values after \":\"

Output\:ff1a
BandFigure: Band structure.";

PBandPlot::usage="PBandPlot[PBandData_,KLabel_,orbitals_,pcolor_:Red,legend_:\"orbitals\",scaling_:10,aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]

Input:
PBandData: Input PBandData.
KLabel: Input KLabel.
orbitals: Input projected orbitals.
pcolor: Control the plot style, with default values after \":\"
legend: Control the plot style, with default values after \":\"
scaling: Control the plot style, with default values after \":\"
aspectRatio: Control the plot style, with default values after \":\"
yRange: Control the plot style, with default values after \":\"

Output\:ff1a
PBandFigure: PBand structure.";

PBandSpinPlot::usage="PBandSpinPlot[PBandData_,KLabel_,orbitals_,legend_:\"orbitals\",aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]

Input:
PBandData: Input PBandData.
KLabel: Input KLabel.
orbitals: Input projected orbitals.
legend: Control the plot style, with default values after \":\"
aspectRatio: Control the plot style, with default values after \":\"
yRange: Control the plot style, with default values after \":\"

Output\:ff1a
PBandSpinFigure: PBand Spin structure.";


Begin["Private`"]


BandPlot[BandData_,KLabel_,color_:Blue,thickness_:0.003,aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]:=Module[
	{tband,KK,bandFig},
	tband=BandData;
	KK=KLabel; 
	bandFig=ListLinePlot[tband,
		Axes->False,GridLines->{Transpose[KK][[1]],{0}},
		GridLinesStyle->Directive[Orange, Dashed],
		AspectRatio->aspectRatio,
		Frame->True,FrameTicks->{{Automatic,None},{KK,None}},
		FrameStyle->Directive[Purple],
		FrameLabel->{Style["Wave vector",FontFamily->"Times New Roman",16],Style["Energy(eV)",FontFamily->"Times New Roman",16]},
		PlotStyle->Directive[Thickness[thickness],color],PlotRange->{{First[KK][[1]],Last[KK][[1]]},yRange},
		Background->White];
	(*Output*)
	(*Dataset[Association[{"BandFigure"->bandFig}],DatasetTheme->{"Serif"}]*)
	bandFig
	]

BandIndexPlot[BandData_,KLabel_,color_:{Blue},thickness_:0.003,aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]:=Module[
	{tband,KK,style,bandFig},
	tband=BandData;
	KK=KLabel; 
	style=Table[Directive[Thickness[thickness],i],{i,color}];
	bandFig=ListLinePlot[tband,
		Axes->False,GridLines->{Transpose[KK][[1]],{0}},
		GridLinesStyle->Directive[Orange, Dashed],
		AspectRatio->aspectRatio,
		Frame->True,FrameTicks->{{Automatic,None},{KK,None}},
		FrameStyle->Directive[Purple],
		FrameLabel->{Style["Wave vector",FontFamily->"Times New Roman",16],Style["Energy(eV)",FontFamily->"Times New Roman",16]},
		PlotStyle->style,PlotRange->{{First[KK][[1]],Last[KK][[1]]},yRange},
		Background->White];
	(*Output*)
	(*Dataset[Association[{"BandFigure"->bandFig}],DatasetTheme->{"Serif"}]*)
	bandFig
	]
	
BandLabelsPlot[BandData_,KLabel_,color_:Blue,thickness_:0.003,aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]:=Module[
	{tband,KK,temp1,temp2,bandFig},
	tband=BandData;
	KK=KLabel; 
	temp1=Table[Transpose[tband[[i]]][[2]]//Min,{i,1,Length[tband]}];
	temp2=Flatten[Position[temp1,_?(# < yRange[[2]] &)]];
	bandFig=ListLinePlot[Table[tband[[i]],{i,temp2}],
		Axes->False,GridLines->{Transpose[KK][[1]],{0}},
		GridLinesStyle->Directive[Orange, Dashed],
		AspectRatio->aspectRatio,
		Frame->True,FrameTicks->{{Automatic,None},{KK,None}},
		FrameStyle->Directive[Purple],
		FrameLabel->{Style["Wave vector",FontFamily->"Times New Roman",16],Style["Energy(eV)",FontFamily->"Times New Roman",16]},
		PlotStyle->Directive[Thickness[thickness],color],PlotRange->{{First[KK][[1]],Last[KK][[1]]},yRange},
		PlotLabels->Range[1,Length[tband]],
		Background->White];
	(*Output*)
	(*Dataset[Association[{"BandFigure"->bandFig}],DatasetTheme->{"Serif"}]*)
	bandFig
	]
	
PBandPlot[PBandData_,KLabel_,orbitals_,pcolor_:Red,legend_:"orbitals",scaling_:10,aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]:=Module[
	{tpband,KK,nbands,nkpts,pbandFig},
	tpband=PBandData;
	KK=KLabel;
	nbands=Dimensions[tpband][[1]];
	nkpts=Dimensions[tpband][[2]];
	pbandFig=Legended[
		Graphics[Table[
			{PointSize[Sum[tpband[[j]][[i,orbitals[[k]]]],{k,Length[orbitals]}]/scaling],
			(*Opacity[Sum[tpband[[j]][[i,orbitals[[k]]]],{k,Length[orbitals]}]/scaling],*)
			pcolor,
			Point[{tpband[[j]][[i,1]],tpband[[j]][[i,2]]}]},{j,1,nbands},{i,1,nkpts}],
			Axes->False,GridLines->{Transpose[KK][[1]],{0}},GridLinesStyle->Directive[Orange, Dashed],
			Frame->True,FrameTicks->{{Automatic,None},{KK,None}},
			FrameStyle->Directive[Purple],
			FrameLabel->{Style["Wave vector",FontFamily->"Times New Roman",16],Style["Energy(eV)",FontFamily->"Times New Roman",16]},
			AspectRatio->aspectRatio,
			PlotRange->{{First[KK][[1]],Last[KK][[1]]},yRange},PlotRangeClipping->True],
		Placed[PointLegend[{pcolor},{legend}],Top]
	];
	pbandFig
	]

PBandSpinPlot[PBandData_,KLabel_,orbitals_,legend_:"orbitals",aspectRatio_:1/GoldenRatio,yRange_:{-10,10}]:=Module[
	{tpband,KK,nbands,nkpts,pbandSpinFig},
	tpband=PBandData;
	KK=KLabel;
	nbands=Dimensions[tpband][[1]];
	nkpts=Dimensions[tpband][[2]];
	pbandSpinFig=Legended[
		Graphics[Table[
			{ColorData["Rainbow"][Rescale[Sum[tpband[[j]][[i,orbitals[[k]]]],{k,Length[orbitals]}],{-1,1}]],
			PointSize[0.005],
			Point[{tpband[[j]][[i,1]],tpband[[j]][[i,2]]}]},{j,1,nbands},{i,1,nkpts}],
			Axes->False,GridLines->{Transpose[KK][[1]],{0}},GridLinesStyle->Directive[Orange, Dashed],
			Frame->True,FrameTicks->{{Automatic,None},{KK,None}},
			FrameStyle->Directive[Purple],
			FrameLabel->{Style["Wave vector",FontFamily->"Times New Roman",16],Style["Energy(eV)",FontFamily->"Times New Roman",16]},
			AspectRatio->aspectRatio,
			PlotRange->{{First[KK][[1]],Last[KK][[1]]},yRange},PlotRangeClipping->True],
		Placed[BarLegend[{"Rainbow",{-1,1}},LegendMarkerSize->150,(*LegendFunction\[Rule](Framed[#,RoundingRadius\[Rule]5]&),*)LegendLabel->legend],Right]
	];
	pbandSpinFig
	]


End[]


EndPackage[]
