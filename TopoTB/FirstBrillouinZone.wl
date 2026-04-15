(* ::Package:: *)

BeginPackage["TopoTB`FirstBrillouinZone`"]


FBZ3D::usage="FBZ3D[lattice_,axes_:False,DLColor_:Blue,RLColor_:Red,range_:3]

Input:
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
axes: Whether the coordinate axes are displayed. The default value is False.
DLColor: Color of the Wigner\[Dash]Seitz cell for the direct lattice. The default value is Blue.
RLColor: Color of the Wigner\[Dash]Seitz cell for the reciprocal lattice. The default value is Red.
range: When there are issues with the Wigner\[Dash]Seitz cell, the value can be increased. The default value is 3.

Output:
LatticeVector: Lattice Vectors (Ang).
ReciprocalSpaceVector: Reciprocal-Space Vectors (Ang^-1).
DirectLatticeWS: The Wigner\[Dash]Seitz cells of the direct lattice.
ReciprocalLatticeWS(FirstBrillouinZone): The Wigner\[Dash]Seitz cells of the direct lattice and the reciprocal lattice (or first Brillouin zone).

Example\:ff1a
lat={{\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{0,0,3}};
FBZ3D[lat,False,Blue,Purple,3]";

FBZ2D::usage="FBZ2D[lattice_,axes_:False,DLColor_:Blue,RLColor_:Red,range_:3]

Input:
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
axes: Whether the coordinate axes are displayed. The default value is False.
DLColor: Color of the Wigner\[Dash]Seitz cell for the direct lattice. The default value is Blue.
RLColor: Color of the Wigner\[Dash]Seitz cell for the reciprocal lattice. The default value is Red.
range: When there are issues with the Wigner\[Dash]Seitz cell, the value can be increased. The default value is 3.

Output:
LatticeVector: Lattice Vectors (Ang).
ReciprocalSpaceVector: Reciprocal-Space Vectors (Ang^-1).
DirectLatticeWS: The Wigner\[Dash]Seitz cells of the direct lattice.
ReciprocalLatticeWS(FirstBrillouinZone): The Wigner\[Dash]Seitz cells of the direct lattice and the reciprocal lattice (or first Brillouin zone).

Example\:ff1a
lat={{\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\)},{-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\)}};
FBZ2D[lat,False,Blue,Purple,4]";


Begin["Private`"]


FBZ3D[lattice_,axes_:False,DLColor_:Blue,RLColor_:Red,range_:3]:=Module[
	{a1,a2,a3,b1,b2,b3,DL,RL,\[CapitalOmega],targetPoint,voronoiMesh3DA,selectedRegionsA,voronoiMesh3DB,selectedRegionsB,DLFig,RLFig,LatticeVector,ReciprocalSpaceVector},
	(*direct lattice (DL)*)
	{a1,a2,a3}=lattice;
	LatticeVector={"a1"->a1,"a2"->a2,"a3"->a3};
	DL=Tuples[Range[-range,range],3] . {a1,a2,a3};
	
	(*reciprocal lattice (RL)*)
	\[CapitalOmega]=a1 . Cross[a2,a3];
	{b1,b2,b3}={2\[Pi]*Cross[a2,a3]/\[CapitalOmega],2\[Pi]*Cross[a3,a1]/\[CapitalOmega],2\[Pi]*Cross[a1,a2]/\[CapitalOmega]};
	ReciprocalSpaceVector={"b1"->b1,"b2"->b2,"b3"->b3};
	RL=Tuples[Range[-range,range],3] . {b1,b2,b3};
	
	(*Define the target point*)
	targetPoint={0,0,0};
	(*Generate the three-dimensional Voronoi diagram*)
	voronoiMesh3DA=VoronoiMesh[DL,MeshCellLabel->None];
	voronoiMesh3DB=VoronoiMesh[RL,MeshCellLabel->None];
	(*Select polygons associated with the target point*)
	selectedRegionsA=Select[MeshPrimitives[voronoiMesh3DA,3],RegionMember[#,targetPoint]&];
	selectedRegionsB=Select[MeshPrimitives[voronoiMesh3DB,3],RegionMember[#,targetPoint]&];
	
	(*Display the results*)
	DLFig=Show[
		(*Graphics3D[{Black,PointSize[Large],Opacity[0.1],Point[DL]}],*)
		Graphics3D[{EdgeForm[{Thickness[0.003],DLColor}],FaceForm[None],selectedRegionsA}],
		Graphics3D[{Thickness[0.003],DLColor,
			Arrow[{{0,0,0},a1}],Arrow[{{0,0,0},a2}],Arrow[{{0,0,0},a3}],
			Text[Style["\!\(\*SubscriptBox[\(a\), \(1\)]\)",DLColor,Italic,20],1.1 a1],Text[Style["\!\(\*SubscriptBox[\(a\), \(2\)]\)",DLColor,Italic,20],1.1 a2],Text[Style["\!\(\*SubscriptBox[\(a\), \(3\)]\)",DLColor,Italic,20],1.1 a3]},
			Boxed->False],
			Boxed->False,Axes->axes,AxesLabel->{"x","y","z"},AxesOrigin->{0,0,0}];
	RLFig=Show[
		Graphics3D[{EdgeForm[{Thickness[0.003],RLColor}],FaceForm[None],selectedRegionsB}],
		Graphics3D[{Thickness[0.003],RLColor,
			Arrow[{{0,0,0},b1}],Arrow[{{0,0,0},b2}],Arrow[{{0,0,0},b3}],
			Text[Style["\!\(\*SubscriptBox[\(b\), \(1\)]\)",RLColor,Italic,20],1.1 b1],Text[Style["\!\(\*SubscriptBox[\(b\), \(2\)]\)",RLColor,Italic,20],1.1 b2],Text[Style["\!\(\*SubscriptBox[\(b\), \(3\)]\)",RLColor,Italic,20],1.1 b3]},
			Boxed->False],
			Boxed->False,Axes->axes,AxesLabel->{"x","y","z"},AxesOrigin->{0,0,0}];
	Dataset[Association[{"LatticeVector"->LatticeVector,"ReciprocalSpaceVector"->ReciprocalSpaceVector,"DirectLatticeWS"->DLFig,"ReciprocalLatticeWS(FirstBrillouinZone)"->RLFig}],DatasetTheme->{"Serif"}]
	]

FBZ2D[lattice_,axes_:False,DLColor_:Blue,RLColor_:Red,range_:3]:=Module[
	{a1,a2,a3,b1,b2,b3,DL,RL,\[CapitalOmega],targetPoint,voronoiMesh3DA,selectedRegionsA,voronoiMesh3DB,selectedRegionsB,DLFig,RLFig,LatticeVector,ReciprocalSpaceVector},
	(*direct lattice (DL)*)
	{a1,a2}=lattice;
	LatticeVector={"a1"->a1,"a2"->a2};
	AppendTo[a1,0];
	AppendTo[a2,0];
	a3={0,0,1};
	DL=Tuples[Range[-range,range],2] . {a1[[1;;2]],a2[[1;;2]]};
	
	(*reciprocal lattice (RL)*)
	\[CapitalOmega]=a1 . Cross[a2,a3];
	{b1,b2,b3}={2\[Pi]*Cross[a2,a3]/\[CapitalOmega],2\[Pi]*Cross[a3,a1]/\[CapitalOmega],2\[Pi]*Cross[a1,a2]/\[CapitalOmega]};
	ReciprocalSpaceVector={"b1"->b1[[1;;2]],"b2"->b2[[1;;2]]};
	RL=Tuples[Range[-range,range],2] . {b1[[1;;2]],b2[[1;;2]]};
	
	(*Define the target point*)
	targetPoint={0,0};
	(*Generate the three-dimensional Voronoi diagram*)
	voronoiMesh3DA=VoronoiMesh[DL,MeshCellLabel->None];
	voronoiMesh3DB=VoronoiMesh[RL,MeshCellLabel->None];
	(*Select polygons associated with the target point*)
	selectedRegionsA=Select[MeshPrimitives[voronoiMesh3DA,2],RegionMember[#,targetPoint]&];
	selectedRegionsB=Select[MeshPrimitives[voronoiMesh3DB,2],RegionMember[#,targetPoint]&];
	
	(*Display the results*)
	DLFig=Show[
		(*Graphics[{Black,PointSize[Large],Opacity[0.1],Point[DL]}],*)
		Graphics[{EdgeForm[{Thickness[0.003],DLColor}],FaceForm[None],selectedRegionsA}],
		Graphics[{Thickness[0.003],DLColor,
			Arrow[{{0,0},a1[[1;;2]]}],Arrow[{{0,0},a2[[1;;2]]}],
			Text[Style["\!\(\*SubscriptBox[\(a\), \(1\)]\)",DLColor,Italic,20],1.1 a1[[1;;2]]],Text[Style["\!\(\*SubscriptBox[\(a\), \(2\)]\)",DLColor,Italic,20],1.1 a2[[1;;2]]]}],
			Boxed->False,Axes->axes,AxesLabel->{"x","y"},AxesOrigin->{0,0}];
	RLFig=Show[
		Graphics[{EdgeForm[{Thickness[0.003],RLColor}],FaceForm[None],selectedRegionsB}],
		Graphics[{Thickness[0.003],RLColor,
			Arrow[{{0,0},b1[[1;;2]]}],Arrow[{{0,0},b2[[1;;2]]}],
			Text[Style["\!\(\*SubscriptBox[\(b\), \(1\)]\)",RLColor,Italic,20],1.1 b1[[1;;2]]],Text[Style["\!\(\*SubscriptBox[\(b\), \(2\)]\)",RLColor,Italic,20],1.1 b2[[1;;2]]]}],
			Boxed->False,Axes->axes,AxesLabel->{"x","y"},AxesOrigin->{0,0}];
	Dataset[Association[{"LatticeVector"->LatticeVector,"ReciprocalSpaceVector"->ReciprocalSpaceVector,"DirectLatticeWS"->DLFig,"ReciprocalLatticeWS(FirstBrillouinZone)"->RLFig}],DatasetTheme->{"Serif"}]
	]


End[]


EndPackage[]
