(* ::Package:: *)

BeginPackage["TopoTB`BrillouinZone2D`"]


BrillouinZone2D::usage="BrillouinZone2D[lattice_,surfaceLattice_,range_]

Input:
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
surfaceLattice: The two lattice vectors on the surface that you need to study.
range: The Brillouin zone range of output. The default value is {-2,2}.

Output:
LatticeVector: Lattice Vectors (Ang).
LatticeInformation: Lattice Information (Ang).
\[CapitalOmega]: Unit Cell Volume.
BravaisLattice: Bravais Lattice.
ReciprocalSpaceVector: Reciprocal-Space Vectors (Ang^-1).
WSPC2D: Wigner-Seitz primitive cell on the surface.
BZ2D: Wigner-Seitz primitive cell in reciprocal space of the surface (2D Brillouin zone).
KPOINTS: high symmetry points.

Example:
{a,b,c}={{1,0,0},{\!\(\*FractionBox[\(1\), \(3\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{0,0,1}};
lat={a,b,c};
surf001={a,b};
BrillouinZone2D[lat,surf001,{-1,1}]";


Begin["Private`"]


BrillouinZone2D[lattice0_,surfaceLattice0_,range0_:{-2,2}]:=Module[
	{lattice=lattice0,surfaceLattice=surfaceLattice0,range=range0,a,b,c,\[Alpha],\[Beta],\[Gamma],LatticeVector,LatticeInformation,o,ill,abc,BravaisLattice,g1,g2,g3,\[CapitalOmega],ReciprocalSpaceVector,
		aa,bb,r1,r2,\[Theta],aaNew,bbNew,ccNew,gg1New,gg2New,gg3New,a1,a2,b1,b2,b3,b4,b5,\[Alpha]1,\[Alpha]2,\[Alpha]3,\[Alpha]4,pa,pb,WSPC2D,k11,k21,k12,k22,k1,k2,K0,K1,K2,K3,K4,K5,KK0,KK1,KK2,KK3,KK4,KK5,
		bz,BZ,point,BZ2D,KPOINTS},
	(*Lattice Vectors (Ang)*)
	a=lattice[[1]];b=lattice[[2]];c=lattice[[3]];
	\[Alpha]=VectorAngle[b,c];\[Beta]=VectorAngle[c,a];\[Gamma]=VectorAngle[a,b];
	(*LatticeVector=Association[{"a"->a,"b"->b,"c"->c}];*)
	LatticeVector={"a"->a,"b"->b,"c"->c};
	
	(*Lattice Information (Ang)*)
	(*LatticeInformation=Association[{"a"->Norm[a],"b"->Norm[b],"c"->Norm[c],"\[Alpha]"->\[Alpha],"\[Beta]"->\[Beta],"\[Gamma]"->\[Gamma]}];*)
	LatticeInformation={"a"->Norm[a],"b"->Norm[b],"c"->Norm[c],"\[Alpha]"->\[Alpha],"\[Beta]"->\[Beta],"\[Gamma]"->\[Gamma]};
	
	(*Unit Cell Volume*)
	\[CapitalOmega]=a . Cross[b,c];
	
	(*Bravais Lattice*)
	o={0,0,0};
	ill={Opacity[1,Blue],PointSize[Large],Point[o],Arrowheads[Medium],Thick,Arrow[{o,a}],Arrow[{o,b}],Arrow[{o,c}]};
	abc=Graphics3D[{Opacity[0.2],Yellow,Parallelepiped[o,{a,b,c}],ill},Axes->True,AxesLabel->{"x","y","z"},AxesStyle->{Orange,Blue,Red}];
	BravaisLattice=Show[abc];
	
	(*Reciprocal-Space Vectors (Ang^-1)*)
	g1=2\[Pi]*Cross[b,c]/\[CapitalOmega];g2=2\[Pi]*Cross[c,a]/\[CapitalOmega];g3=2\[Pi]*Cross[a,b]/\[CapitalOmega];
	(*ReciprocalSpaceVector=Association[{"g1"->g1,"g2"->g2,"g3"->g3}];*)
	ReciprocalSpaceVector={"g1"->g1,"g2"->g2,"g3"->g3};
	
	(*Choose two lattice vectors on the surface*)
	aa=surfaceLattice[[1]];bb=surfaceLattice[[2]];
	(*Range of Brillouin zone*)
	r1=range[[1]];r2=range[[2]];
	(*Transform aa and bb to the Oxy plane*)
	\[Theta]=VectorAngle[aa,bb];
	aaNew=CoordinateTransform["Spherical"->"Cartesian",{Norm[aa],\[Pi]/2,0}];
	bbNew=CoordinateTransform["Spherical"->"Cartesian",{Norm[bb],\[Pi]/2,\[Theta]}];
	(*Calculation after transformation*)
	ccNew=Cross[aaNew,bbNew];
	gg1New=2\[Pi]*Cross[bbNew,ccNew]/\[CapitalOmega];gg2New=2\[Pi]*Cross[ccNew,aaNew]/\[CapitalOmega];gg3New=2\[Pi]*Cross[aaNew,bbNew]/\[CapitalOmega];
	(*Change 3D coordinates to 2D coordinates*)
	a1=aaNew[[1;;2]];a2=bbNew[[1;;2]];
	b1=gg1New[[1;;2]];b2=gg2New[[1;;2]];
	\[Alpha]1=VectorAngle[b1,b2];
	(*Using geometric relations to find the coordinates of high symmetry points*)
	If[
	\[Alpha]1>=\[Pi]/2,
	{
	(*Wigner-Seitz primitive cell on the surface*)
	a={a1,a2},
	pa=Tuples[Range[r1,r2],2] . a,
	WSPC2D=Show[VoronoiMesh[pa,PlotTheme->"Classic"],Epilog->{Red,Point[pa],Arrow[{{0,0},a1}],Arrow[{{0,0},a2}]}],
	
	(*Wigner-Seitz primitive cell in reciprocal space of the surface(2D Brillouin zone)*)
	b3=b1+b2,(*The circumcenter in triangle*)
	b4=TriangleCenter[{{0,0},b3,b1},"Circumcenter"],
	\[Alpha]2=\[Pi]-\[Alpha]1,
	\[Alpha]3=VectorAngle[b1,b4],
	k11=0.5*Norm[b1]*Tan[\[Alpha]3]/Tan[\[Alpha]2]/Norm[b1]+0.5,(*Coordinates of the Circumcenter on the b1-axis*)
	k21=0.5*Norm[b1]*Tan[\[Alpha]3]/Sin[\[Alpha]2]/Norm[b2],(*Coordinates of the Circumcenter on the b2-axis*)
	b5=TriangleCenter[{{0,0},b3,b2},"Circumcenter"],
	\[Alpha]4=VectorAngle[b2,b5],
	k12=0.5*Norm[b2]*Tan[\[Alpha]4]/Sin[\[Alpha]2]/Norm[b1],
	k22=0.5*Norm[b2]*Tan[\[Alpha]4]/Tan[\[Alpha]2]/Norm[b2]+0.5,
	K0={0,0},K1={0.5,0},K2={0,0.5},K3={k11,k21},K4={0.5,0.5},K5={k12,k22},
	KK0={0,0},KK1=K1[[1]]*b1+K1[[2]]*b2,KK2=K2[[1]]*b1+K2[[2]]*b2,KK3=K3[[1]]*b1+K3[[2]]*b2,KK4=K4[[1]]*b1+K4[[2]]*b2,KK5=K5[[1]]*b1+K5[[2]]*b2,
	bz={b1,b2},
	pb=Tuples[Range[r1,r2],2] . bz,
	BZ=Show[VoronoiMesh[pb,PlotTheme->"Classic"],Epilog->{Blue,Point[pb],Arrow[{{0,0},b1}],Arrow[{{0,0},b2}]}],
	point=ListPlot[{KK0,KK1,KK2,KK3,KK4,KK5}->{"K0","K1","K2","K3","K4","K5"},PlotStyle->{Red,PointSize[Large]}],
	BZ2D=Show[BZ,point],
	(*KPOINTS=Association[{"K0"->K0,"K1"->K1,"K2"->K2,"K3"->K3,"K4"->K4,"K5"->K5}]*)
	KPOINTS={"K0"->K0,"K1"->K1,"K2"->K2,"K3"->K3,"K4"->K4,"K5"->K5}
	},
	{
	a={a1,a2},
	pa=Tuples[Range[r1,r2],2] . a,
	WSPC2D=Show[VoronoiMesh[pa,PlotTheme->"Classic"],Epilog->{Red,Point[pa],Arrow[{{0,0},a1}],Arrow[{{0,0},a2}]}],
	
	b4=TriangleCenter[{{0,0},b1,b2},"Circumcenter"],
	\[Alpha]3=VectorAngle[b2,b4],
	k1=0.5*Norm[b2]*Tan[\[Alpha]3]/Sin[\[Alpha]1]/Norm[b1],
	k2=0.5-0.5*Norm[b2]*Tan[\[Alpha]3]/Tan[\[Alpha]1]/Norm[b2],
	K0={0,0},K1={0.5,0},K2={0,0.5},K3={k1,k2},
	KK0={0,0},KK1=K1[[1]]*b1+K1[[2]]*b2,KK2=K2[[1]]*b1+K2[[2]]*b2,KK3=K3[[1]]*b1+K3[[2]]*b2,
	bz={b1,b2},
	pb=Tuples[Range[r1,r2],2] . bz,
	BZ=Show[VoronoiMesh[pb,PlotTheme->"Classic"],Epilog->{Blue,Point[pb],Arrow[{{0,0},b1}],Arrow[{{0,0},b2}]}],
	point=ListPlot[{KK0,KK1,KK2,KK3}->{"K0","K1","K2","K3"},PlotStyle->{Red,PointSize[Large]}],
	BZ2D=Show[BZ,point],
	(*KPOINTS=Association[{"K0"->K0,"K1"->K1,"K2"->K2,"K3"->K3}]*)
	KPOINTS={"K0"->K0,"K1"->K1,"K2"->K2,"K3"->K3}
	}];
	(*Association[{"LatticeVector"->LatticeVector,"LatticeInformation"->LatticeInformation,"\[CapitalOmega]"->\[CapitalOmega],"BravaisLattice"->BravaisLattice,"ReciprocalSpaceVector"->ReciprocalSpaceVector,
	"WSPC2D"->WSPC2D,"BZ2D"->BZ2D,"KPOINTS"->KPOINTS}]*)
	
	(*Dataset[Association[{"LatticeVector"->Normal[LatticeVector],"LatticeInformation"->LatticeInformation,"\[CapitalOmega]"->\[CapitalOmega],"BravaisLattice"->BravaisLattice,"ReciprocalSpaceVector"->Normal[ReciprocalSpaceVector],
	"WSPC2D"->WSPC2D,"BZ2D"->BZ2D,"KPOINTS"->Normal[KPOINTS]}]]*)
	
	Dataset[Association[{"LatticeVector"->LatticeVector,"LatticeInformation"->LatticeInformation,"\[CapitalOmega]"->\[CapitalOmega],"BravaisLattice"->BravaisLattice,"ReciprocalSpaceVector"->ReciprocalSpaceVector,
	"WSPC2D"->WSPC2D,"BZ2D"->BZ2D,"KPOINTS"->KPOINTS}],DatasetTheme->{"Serif"}]
	
	(*Association[{"BZ2D"->BZ2D,"KPOINTS"->KPOINTS}]*)
	]


End[]


EndPackage[]
