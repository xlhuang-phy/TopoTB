(* ::Package:: *)

BeginPackage["TopoTB`HamiltonianCalculation`"]


HSPBand::usage="HSPBand[H_,lattice_,kpt2kpt_,klabel_,EFermi_:0,bandIndex_:All,color_:RandomColor[]]

Input:
H: The format of the Hamiltonian is H[{kx, ky, kz}, parameter...].
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
kpt2kpt: High-symmetry point coordinates and their corresponding point numbers are formatted as {{A, B, num}, {B, C, num}, ...}.
klabel: Name of high-symmetry point.\:ff09
EFermi: Fermi level. The default value is 0.
bandIndex: Calculate the band of the bandIndex band.
color: The color of the band. The default value is RandomColor[].

Output:
HSPPathCoordinate: The coordinates of points on a high-symmetry path.
HSPPathDistance: Accumulated distance of points on a high-symmetry path.
HSPLabel: The position of high-symmetry points on a high-symmetry paths.
HSPBandData: With the option to use this data for plotting or exporting it for use with other software.
HSPBandFigure: Band structure.

Example\:ff1a
h[{kx_,ky_,kz_},t_:1,\[Lambda]R_:0.05,\[Lambda]SO_:0.06,\[Lambda]v_:0.1]=H;
lat={{\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{0,0,3}};
G0={0,0,0};K0={\!\(\*FractionBox[\(2\), \(3\)]\),\!\(\*FractionBox[\(1\), \(3\)]\),0};M0={\!\(\*FractionBox[\(1\), \(2\)]\),0,0};
HSPBand[h,lat,{{G0,M0,40},{M0,K0,20},{K0,G0,40}},klabel,0]";

HCalc2D::usage="HCalc2D[H_,lattice_,bandIndex_:{1},b1b2points_:{50,50},b1b2ranges_:{2,2},spinTexture:(1|2):1,OSorSO:(1|2):1,EFermi_:0]

Input:
H: The format of the Hamiltonian is H[{kx, ky, kz}, parameter...].
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
bandIndex: Calculate the 3D band, Fermi surface, and spin texture of the bandIndex band.
b1b2points: The number of points along the direction of two reciprocal lattice vectors. The default value is {50, 50}.
b1b2ranges: The range of points along the direction of two reciprocal lattice vectors. The default value is {2,2}.
spinTexture: Whether to calculate spin texture, set to 1 and 2 respectively. The default value is 1.
OSorSO: The calculation formula for spin texture is S=\[LeftAngleBracket]\[CapitalPsi]\[VerticalSeparator]\[CapitalOmega]\[VerticalSeparator]\[CapitalPsi]\[RightAngleBracket], where OSorSO=\[CapitalOmega]. If the basis of the Hamiltonian is \"up down up down...\", then OSorSO=1. If the basis of the Hamiltonian is \"up up down down...\", then OSorSO=2.
EFermi: Fermi level. The default value is 0.

Output:
KPoints: The coordinates of points in reciprocal space.
Eigenvalues: Eigenvalues corresponding to each point in reciprocal space.
Eigenstates: Eigenstates corresponding to each point in reciprocal space.
Band3DFigure: Three dimensional band structure.
FermiSurfaceFigure: Fermi surface.
SpinTextureFigure: Spin texture.

Example:
h[{kx_,ky_,kz_},t_:1,\[Lambda]R_:0.05,\[Lambda]SO_:0.06,\[Lambda]v_:0.1]=H;
lat={{\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{0,0,3}};
ds=HCalc2D[h,lat,{1,2,3,4},{50,50},{2,2},2,1,0]";

DOS2D::usage="DOS2D[H_,lattice_,b1b2points_:{100,100},round_:0.05,EFermi_:0,bandIndex_:All,color_:RandomColor[]]

Input:
H: The format of the Hamiltonian is H[{kx, ky, kz}, parameter...].
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
b1b2points: The number of points along the direction of two reciprocal lattice vectors. The default value is {100, 100}.
round: Rounded values. The default value is 0.05.
EFermi: Fermi level. The default value is 0.
bandIndex: Calculate the DOS of the bandIndex band.
color: The color of the DOS. The default value is RandomColor[].

Output:
DOSData: Density of states data.
DOSXYFigure: The horizontal axis represents energy, and the vertical axis represents the number of energy states.
DOSYXFigure: The horizontal axis represents the number of energy states, and the vertical axis represents energy.

Example:
h[{kx_,ky_,kz_},t_:1,\[Lambda]R_:0.05,\[Lambda]SO_:0.06,\[Lambda]v_:0.1]=H;
lat={{\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{0,0,3}};
DOS2D[h,lat,{120,120},0.08,1]";

FermiSurfaceAtEnergy2D::usage="FermiSurfaceAtEnergy2D[H_,lattice_,Elevel_,bandIndex_,color_:RandomColor[],deltaE_:0.006,di_:1,b1b2points_:{200,200},b1b2ranges_:{1,1},EFermi_:0]

Input:
H: The format of the Hamiltonian is H[{kx, ky, kz}, parameter...].
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
Elevel: The Fermi surface at E=Elevel.
bandIndex: Calculate the Fermi surface of the bandIndex band.
color: The color of the Fermi surface. The default value is RandomColor[].
deltaE: Calculate the energy interval used for the Fermi surface. The default value is 0.006.
di: Step size is di, increase this value when there are too many points. The default value is 1.
b1b2points: The number of points along the direction of two reciprocal lattice vectors. The default value is {200, 200}.
b1b2ranges: The range of points along the direction of two reciprocal lattice vectors. The default value is {1,1}.
EFermi: Fermi level. The default value is 0.

Output:
FermiSurfaceAtEnergyData: The data of the Fermi surface at E=Elevel.
FermiSurfaceAtEnergyFigure: The figure of the Fermi surface at E=Elevel.

Example:
h[{kx_,ky_,kz_},t_:1,\[Lambda]R_:0.05,\[Lambda]SO_:0.06,\[Lambda]v_:0.1]=H;
lat={{\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{0,0,3}};
FermiSurfaceAtEnergy2D[h,lat,2,3,Red]";

SpinTextureAtEnergy2D::usage="SpinTextureAtEnergy2D[H_,lattice_,Elevel_,bandIndex_,color_:RandomColor[],OSorSO_:1,deltaE_:0.006,di_:1,b1b2points_:{200,200},b1b2ranges_:{1,1},EFermi_:0]

Input:
H: The format of the Hamiltonian is H[{kx, ky, kz}, parameter...].
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
Elevel: The spin texture at E=Elevel.
bandIndex: Calculate the spin texture of the bandIndex band.
color: The color of the spin texture. The default value is RandomColor[].
OSorSO: The calculation formula for spin texture is S=\[LeftAngleBracket]\[CapitalPsi]\[VerticalSeparator]\[CapitalOmega]\[VerticalSeparator]\[CapitalPsi]\[RightAngleBracket], where OSorSO=\[CapitalOmega]. If the basis of the Hamiltonian is \"up down up down...\", then OSorSO=1. If the basis of the Hamiltonian is \"up up down down...\", then OSorSO=2.
di: Step size is di, increase this value when there are too many points. The default value is 1.
b1b2points: The number of points along the direction of two reciprocal lattice vectors. The default value is {200, 200}.
b1b2ranges: The range of points along the direction of two reciprocal lattice vectors. The default value is {1,1}.
EFermi: Fermi level. The default value is 0.

Output:
SpinTextureAtEnergyData: The data of the spin texture at E=Elevel.
SpinTextureAtEnergyFigure: The figure of the spin texture at E=Elevel.

Example:
h[{kx_,ky_,kz_},t_:1,\[Lambda]R_:0.05,\[Lambda]SO_:0.06,\[Lambda]v_:0.1]=H;
lat={{\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{0,0,3}};
SpinTextureAtEnergy2D[h,lat,2,3,Red,1,0.006,2]";


Begin["Private`"]


HSPBand[H_,lattice_,kpt2kpt_,klabel_,EFermi_:0,bandIndex_:All,color_:{RandomColor[]}]:=Module[
	{a1,a2,a3,\[CapitalOmega],b1,b2,b3,ktrans,kpoints,kpath1,kpath2,kpath3,kk1,kk2,KK,energy,nbands,nkpts,tband,bandFig},
	{a1,a2,a3}=lattice;
	\[CapitalOmega]=a1 . Cross[a2,a3];
	b1=2\[Pi] Cross[a2,a3]/\[CapitalOmega];b2=2\[Pi] Cross[a3,a1]/\[CapitalOmega];b3=2\[Pi] Cross[a1,a2]/\[CapitalOmega];
	ktrans[x_]:=x[[1]]*b1+x[[2]]*b2+x[[3]]*b3;
	kpoints[start_,end_,npoints_]:=Table[start+i/npoints (end-start),{i,0,npoints-1}];
	kpath1=Flatten[Table[kpoints[ktrans[kpt2kpt[[j,1]]],ktrans[kpt2kpt[[j,2]]],kpt2kpt[[j,3]]],{j,1,Length[kpt2kpt]}],1]//N;
	AppendTo[kpath1,ktrans[Last[kpt2kpt][[2]]]];
	kpath2=Table[Norm[kpath1[[i+1]]-kpath1[[i]]],{i,1,Length[kpath1]-1}];
	kpath3=Join[{0},Accumulate[kpath2]];
	kk1=klabel;
	kk2=Table[kpt2kpt[[j,3]],{j,1,Length[kpt2kpt]}];
	PrependTo[kk2,1];
	kk2=Accumulate[kk2];
	kk2=Table[kpath3[[i]],{i,kk2}];
	KK=Transpose[{kk2,kk1}];
	energy=Table[Sort[Eigenvalues[H[kpath1[[i]]]]][[bandIndex]],{i,1,Length[kpath1]}]-EFermi;
	(*nbands=Length[H[kpath1[[1]]]];*)
	nbands=Length[energy[[1]]];
	nkpts=Length[kpath1];
	tband=Table[Transpose[{kpath3,energy[[All,i]]}],{i,1,nbands}];
	bandFig=ListLinePlot[tband,
		Axes->False,GridLines->{Transpose[KK][[1]],{0}},
		GridLinesStyle->Directive[Orange, Dashed],
		Frame->True,FrameTicks->{{Automatic,None},{KK,None}},
		FrameStyle->Directive[Purple],
		FrameLabel->{Style["Wave vector",FontFamily->"Times New Roman",16],Style["Energy(eV)",FontFamily->"Times New Roman",16]},
		AspectRatio->1/GoldenRatio,
		PlotStyle->Table[Directive[Thickness[0.003],i],{i,color}],
		(*ColorFunction\[Rule]"Rainbow",*)
		PlotRange->{{First[KK][[1]],Last[KK][[1]]},{All,All}}];
	(*Output*)
	Dataset[Association[{"HSPPathCoordinate"->kpath1,"HSPPathDistance"->kpath3,"HSPLabel"->KK,"HSPBandData"->tband},"HSPBandFigure"->bandFig],DatasetTheme->{"Serif"}]
	]
	
HCalc2D[H_,lattice_,bandIndex_:{1},b1b2points_:{50,50},b1b2ranges_:{2,2},spinTexture:(1|2):1,OSorSO:(1|2):1,EFermi_:0]:=Module[
	{a1,a2,a3,\[CapitalOmega],b1,b2,b3,nx,ny,BZb1,BZb2,BZ,kk,eigensystem,vals,vecs,nband,Band3DData,Band3DFigure,FermiSurfaceFigure,OSorSOx,OSorSOy,spintextureKx,spintextureKy,SpinTextureData,SpinTextureFigure,ds},
	{a1,a2,a3}=lattice;
	\[CapitalOmega]=a1 . Cross[a2,a3];
	b1=2\[Pi] Cross[a2,a3]/\[CapitalOmega];b2=2\[Pi] Cross[a3,a1]/\[CapitalOmega];b3=2\[Pi] Cross[a1,a2]/\[CapitalOmega];
	{nx,ny}=b1b2points;
	BZb1=b1b2ranges[[1]]*b1;
	BZb2=b1b2ranges[[2]]*b2;
	BZ[nb1_,nb2_]:=nb1*BZb1+nb2*BZb2;(*Define functions in units of reciprocal lattice BZb1 and BZb2*)
	kk=Table[BZ[(i-1)/nx,(j-1)/ny]-BZ[1/2,1/2],{i,1,nx+1},{j,1,ny+1}];
	eigensystem=Table[Transpose[Eigensystem[H[kk[[i,j]]]]//N//Chop]//Sort,{i,1,nx+1},{j,1,ny+1}];
	vals=Table[eigensystem[[i,j]][[All,1]],{i,1,nx+1},{j,1,ny+1}]-EFermi;(*Eigenvalues*)
	vecs=Table[eigensystem[[i,j]][[All,2]],{i,1,nx+1},{j,1,ny+1}];(*Eigenstates*)
	nband=Length[H[kk[[1,1]]]];
	(*3D band*)
	Band3DData=Table[Flatten[Table[Append[kk[[i,j]][[1;;2]],vals[[i,j,k]]],{i,1,nx+1},{j,1,ny+1}],1],{k,bandIndex}];
	Band3DFigure=ListPlot3D[Band3DData,Mesh->None,ColorFunction->"Rainbow",BoxRatios->Automatic,
		AxesLabel->{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)",FontFamily->"Times New Roman",16],Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)",FontFamily->"Times New Roman",16],Style["Energy(eV)",FontFamily->"Times New Roman",16]}];
	(*Fermi surface*)
	FermiSurfaceFigure=Table[ListContourPlot[Band3DData[[i]],AspectRatio->Automatic,FrameLabel->{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)",FontFamily->"Times New Roman",16],Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)",FontFamily->"Times New Roman",16]}],{i,bandIndex}];
	Switch[spinTexture,
		1,
		{ds=Dataset[Association["KPoints"->kk,"Eigenvalues"->vals,"Eigenstates"->vecs,"Band3DFigure"->Band3DFigure,"FermiSurfaceFigure"->FermiSurfaceFigure],DatasetTheme->{"Serif"}]},
		2,
		{
		{OSorSOx,OSorSOy}=Switch[OSorSO,
			1,{KroneckerProduct[IdentityMatrix[nband/2],PauliMatrix[1]],KroneckerProduct[IdentityMatrix[nband/2],PauliMatrix[2]]},
			2,{KroneckerProduct[PauliMatrix[1],IdentityMatrix[nband/2]],KroneckerProduct[PauliMatrix[2],IdentityMatrix[nband/2]]}];
		spintextureKx=Table[Diagonal[Conjugate[vecs[[i,j]]] . OSorSOx . Transpose[vecs[[i,j]]]],{i,1,nx+1},{j,1,ny+1}];
		spintextureKy=Table[Diagonal[Conjugate[vecs[[i,j]]] . OSorSOy . Transpose[vecs[[i,j]]]],{i,1,nx+1},{j,1,ny+1}];
		SpinTextureData=Table[Table[{kk[[i,j]][[1;;2]],{spintextureKx[[i,j]][[k]],spintextureKy[[i,j]][[k]]}},{i,1,nx+1},{j,1,ny+1}],{k,bandIndex}];
		SpinTextureFigure=Table[ListVectorPlot[SpinTextureData[[i]],VectorColorFunction->"Rainbow",AspectRatio->Automatic,
		VectorPoints->All,FrameLabel->{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)",FontFamily->"Times New Roman",16],Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)",FontFamily->"Times New Roman",16]}],{i,bandIndex}];
		ds=Dataset[Association["KPoints"->kk,"Eigenvalues"->vals,"Eigenstates"->vecs,"Band3DFigure"->Band3DFigure,"FermiSurfaceFigure"->FermiSurfaceFigure,"SpinTextureFigure"->SpinTextureFigure],DatasetTheme->{"Serif"}]
		}];
	(*Output*)
	ds
	]
	
DOS2D[H_,lattice_,b1b2points_:{100,100},round_:0.05,EFermi_:0,bandIndex_:All,color_:RandomColor[]]:=Module[
	{a1,a2,a3,\[CapitalOmega],b1,b2,b3,nx,ny,BZb1,BZb2,BZ,kk,eigenvalues,DOSData,DOSXYFigure,DOSYXFigure},
	{a1,a2,a3}=lattice;
	\[CapitalOmega]=a1 . Cross[a2,a3];
	b1=2\[Pi] Cross[a2,a3]/\[CapitalOmega];b2=2\[Pi] Cross[a3,a1]/\[CapitalOmega];b3=2\[Pi] Cross[a1,a2]/\[CapitalOmega];
	{nx,ny}=b1b2points;
	BZb1=b1;
	BZb2=b2;
	BZ[nb1_,nb2_]:=nb1*BZb1+nb2*BZb2;
	kk=Table[BZ[(i-1)/nx,(j-1)/ny]-BZ[1/2,1/2],{i,1,nx+1},{j,1,ny+1}];
	eigenvalues=Table[(Eigenvalues[H[kk[[i,j]]]]//N//Chop//Sort)[[bandIndex]],{i,1,nx+1},{j,1,ny+1}]-EFermi;
	DOSData=Tally[Round[Sort[Flatten[eigenvalues]],round]];
	DOSXYFigure=ListLinePlot[DOSData,InterpolationOrder->2,PlotRange->All,PlotStyle->Directive[Thickness[0.008],color],(*Filling->Axis,*)
		Axes->False,Frame->True,FrameStyle->Directive[Purple],
		FrameLabel->{Style["Energy(eV)",FontFamily->"Times New Roman",16],Style["DOS",FontFamily->"Times New Roman",16]}];
	DOSYXFigure=ListLinePlot[Transpose@RotateLeft@Transpose@DOSData,InterpolationOrder->2,PlotRange->All,PlotStyle->Directive[Thickness[0.008],color],
		Axes->False,Frame->True,FrameStyle->Directive[Purple],FrameTicks->{{None,None},{None,None}},AspectRatio->5/2,
		FrameLabel->{Style["DOS",FontFamily->"Times New Roman",16],None}];
	(*Output*)
	Dataset[Association["DOSData"->DOSData,"DOSXYFigure"->DOSXYFigure,"DOSYXFigure"->DOSYXFigure],DatasetTheme->{"Serif"}]
	]
	
FermiSurfaceAtEnergy2D[H_,lattice_,Elevel_,bandIndex_,color_:RandomColor[],deltaE_:0.006,di_:1,b1b2points_:{200,200},b1b2ranges_:{1,1},EFermi_:0]:=Module[
	{a1,a2,a3,\[CapitalOmega],b1,b2,b3,nx,ny,BZb1,BZb2,BZ,kk,eigenvalues,FermiSurfaceAtEnergyData,FermiSurfaceAtEnergyFigure},
	{a1,a2,a3}=lattice;
	\[CapitalOmega]=a1 . Cross[a2,a3];
	b1=2\[Pi] Cross[a2,a3]/\[CapitalOmega];b2=2\[Pi] Cross[a3,a1]/\[CapitalOmega];b3=2\[Pi] Cross[a1,a2]/\[CapitalOmega];
	{nx,ny}=b1b2points;
	BZb1=b1b2ranges[[1]]*b1;
	BZb2=b1b2ranges[[2]]*b2;
	BZ[nb1_,nb2_]:=nb1*BZb1+nb2*BZb2;
	kk=Table[BZ[(i-1)/nx,(j-1)/ny]-BZ[1/2,1/2],{i,1,nx+1},{j,1,ny+1}];
	eigenvalues=Table[Eigenvalues[H[kk[[i,j]]]]//N//Chop//Sort,{i,1,nx+1},{j,1,ny+1}]-EFermi;
	FermiSurfaceAtEnergyData=Reap[Do[If[Abs[eigenvalues[[i,j]][[bandIndex]]-Elevel]<deltaE,
		Sow[kk[[i,j]][[1;;2]]~Join~{eigenvalues[[i,j]][[bandIndex]]}]],{i,1,nx+1,di},{j,1,ny+1,di}]][[2,1]];
	FermiSurfaceAtEnergyFigure=ListContourPlot[FermiSurfaceAtEnergyData,ContourStyle->color,ContourShading->False,
		PlotRange->{MinMax[Transpose[Flatten[kk,1]][[1]]],MinMax[Transpose[Flatten[kk,1]][[2]]]},
		PlotLabel->Style["Fermi surface at E = "<>ToString[Elevel]<>" eV",FontFamily->"Times New Roman",16],
		AspectRatio->Automatic,FrameLabel->{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)",FontFamily->"Times New Roman",16],Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)",FontFamily->"Times New Roman",16]}];
	(*Output*)
	Dataset[Association["FermiSurfaceAtEnergyData"->FermiSurfaceAtEnergyData,"FermiSurfaceAtEnergyFigure"->FermiSurfaceAtEnergyFigure],DatasetTheme->{"Serif"}]
	]
	
SpinTextureAtEnergy2D[H_,lattice_,Elevel_,bandIndex_,color_:RandomColor[],OSorSO_:1,deltaE_:0.006,di_:1,b1b2points_:{200,200},b1b2ranges_:{1,1},EFermi_:0]:=Module[
	{a1,a2,a3,\[CapitalOmega],b1,b2,b3,nx,ny,BZb1,BZb2,BZ,kk,eigensystem,vals,vecs,nband,OSorSOx,OSorSOy,spintextureKx,spintextureKy,SpinTextureAtEnergyData,SpinTextureAtEnergyFigure},
	{a1,a2,a3}=lattice;
	\[CapitalOmega]=a1 . Cross[a2,a3];
	b1=2\[Pi] Cross[a2,a3]/\[CapitalOmega];b2=2\[Pi] Cross[a3,a1]/\[CapitalOmega];b3=2\[Pi] Cross[a1,a2]/\[CapitalOmega];
	{nx,ny}=b1b2points;
	BZb1=b1b2ranges[[1]]*b1;
	BZb2=b1b2ranges[[2]]*b2;
	BZ[nb1_,nb2_]:=nb1*BZb1+nb2*BZb2;
	kk=Table[BZ[(i-1)/nx,(j-1)/ny]-BZ[1/2,1/2],{i,1,nx+1},{j,1,ny+1}];
	eigensystem=Table[Transpose[Eigensystem[H[kk[[i,j]]]]//N//Chop]//Sort,{i,1,nx+1},{j,1,ny+1}]-EFermi;
	vals=Table[eigensystem[[i,j]][[All,1]],{i,1,nx+1},{j,1,ny+1}];
	vecs=Table[eigensystem[[i,j]][[All,2]],{i,1,nx+1},{j,1,ny+1}];
	nband=Length[H[kk[[1,1]]]];
	(*spin texture*)
	{OSorSOx,OSorSOy}=Switch[OSorSO,
		1,{KroneckerProduct[IdentityMatrix[nband/2],PauliMatrix[1]],KroneckerProduct[IdentityMatrix[nband/2],PauliMatrix[2]]},
		2,{KroneckerProduct[PauliMatrix[1],IdentityMatrix[nband/2]],KroneckerProduct[PauliMatrix[2],IdentityMatrix[nband/2]]}];
	spintextureKx=Table[Diagonal[Conjugate[vecs[[i,j]]] . OSorSOx . Transpose[vecs[[i,j]]]],{i,1,nx+1},{j,1,ny+1}];
	spintextureKy=Table[Diagonal[Conjugate[vecs[[i,j]]] . OSorSOy . Transpose[vecs[[i,j]]]],{i,1,nx+1},{j,1,ny+1}];
	SpinTextureAtEnergyData=Reap[Do[If[Abs[vals[[i,j]][[bandIndex]]-Elevel]<deltaE,
		Sow[{kk[[i,j]][[1;;2]],{spintextureKx[[i,j]][[bandIndex]],spintextureKy[[i,j]][[bandIndex]]}}]],{i,1,nx+1,di},{j,1,ny+1,di}]][[2,1]];
	SpinTextureAtEnergyFigure=ListVectorPlot[SpinTextureAtEnergyData,VectorStyle->color,VectorColorFunction->None,
		PlotRange->{MinMax[Transpose[Flatten[kk,1]][[1]]],MinMax[Transpose[Flatten[kk,1]][[2]]]},
		PlotLabel->Style["Spin texture at E = "<>ToString[Elevel]<>" eV",FontFamily->"Times New Roman",16],
		AspectRatio->Norm[MinMax[Transpose[Flatten[kk,1]][[2]]]]/Norm[MinMax[Transpose[Flatten[kk,1]][[1]]]],
		VectorPoints->All,FrameLabel->{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)",FontFamily->"Times New Roman",16],Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)",FontFamily->"Times New Roman",16]}];
	(*Output*)
	Dataset[Association["SpinTextureAtEnergyData"->SpinTextureAtEnergyData,"SpinTextureAtEnergyFigure"->SpinTextureAtEnergyFigure],DatasetTheme->{"Serif"}]
	]


End[]


EndPackage[]
