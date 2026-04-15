(* ::Package:: *)

BeginPackage["TopoTB`TopologicalNumber`"]


BerryPhaseCalc::usage="BerryPhaseCalc[H_,lattice_,bandIndex_,loop_]

Input:
H: The format of the Hamiltonian is H[{kx, ky, kz}, parameter...].
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
bandIndex: Calculate the Berry phase of the bandIndex band.
loop: Calculate the loop of Berry phase.

Output:
KPoints: The coordinates of points in reciprocal space.
kPointFigure: K-point schematic diagram.
BerryPhase: Berry phase.

Example:
h[{kx_,ky_,kz_},v_:0.5,w_:1.0]=H;
lat={{1,0,0},{0,1,0},{0,0,1}};
bnd=Table[i,{i,1,1}];
loop=Subdivide[{0,0,0},{1.,0,0},20]
BerryPhaseCalc[h,lat,bnd,loop]";

ChernNumberCalc::usage="ChernNumberCalc[H_,lattice_,bandIndex_,b1b2points_:{50,50},b1b2ranges_:{2,2}]

Input:
H: The format of the Hamiltonian is H[{kx, ky, kz}, parameter...].
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
bandIndex: Calculate the Chern number of the bandIndex band.
b1b2points: The number of points along the direction of two reciprocal lattice vectors. The default value is {50, 50}.
b1b2ranges: The range of points along the direction of two reciprocal lattice vectors. The default value is {2,2}.

Output:
KPoints: The coordinates of points in reciprocal space.
kPointFigure: K-point schematic diagram.
ChernNumber: Chern number.
BerryCurvatureData: Berry curvature data.
BerryCurvatureFigure3D: Plot with the ListPlot3D function.
BerryCurvatureFigureContour: Plot with the ListContourPlot function.
BerryCurvatureFigureDensity: Plot with the ListDensityPlot function.

Example:
h[{kx_,ky_,kz_},M_:0.1,t1_:1,t2_:0.2,\[Phi]_:0.2*\[Pi]]=H;
lat={{-\!\(\*SqrtBox[\(3\)]\),0,0},{\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),-\!\(\*FractionBox[\(3\), \(2\)]\),0},{0,0,1}};
bnd=Table[i,{i,1,1}];
ChernNumberCalc[h,lat,bnd]";

Z2NumberCalcByWilsonLoop::usage="Z2NumberCalcByWilsonLoop[H_,lattice_,bandIndex_,plane:(1|2|3|4|5|6):5,referenceLine_:-1,dikx_:10^-2.,diky_:10^-2.,deltaky_:10^-4.]

Input:
H: The format of the Hamiltonian is H[{kx, ky, kz}, parameter...].
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
bandIndex: Calculate the \!\(\*SubscriptBox[\(Z\), \(2\)]\) number of the bandIndex band.
plane: The calculated plane corresponds to: (1,2,3,4,5,6)->(\!\(\*SubscriptBox[\(x\), \(0\)]\),\!\(\*SubscriptBox[\(x\), \(\[Pi]\)]\),\!\(\*SubscriptBox[\(y\), \(0\)]\),\!\(\*SubscriptBox[\(y\), \(\[Pi]\)]\),\!\(\*SubscriptBox[\(z\), \(0\)]\),\!\(\*SubscriptBox[\(z\), \(\[Pi]\)]\)). The default value is 5.
referenceLine: Position of Reference Lines. The default value is -1.
dikx: Set the step size in the kx direction within the [0,1] range. The default value is 10^-2..
diky: Set the step size in the ky direction within the [0,0.5] range. The default value is 10^-2..
deltaky: The offset when taking the derivative in the ky direction. The default value is 10^-4.

Output:
KPoints: The coordinates of points in reciprocal space.
kPointFigure: K-point schematic diagram.
Z2Number: \!\(\*SubscriptBox[\(Z\), \(2\)]\) number.
WannierCenterData: Wannier Center Data.
WannierCenterFigureList: Plot with the ListPlot function.

Example:
h[{kx_,ky_,kz_},t_:1,\[Lambda]R_:0.05,\[Lambda]SO_:0.06,\[Lambda]v_:0.1]=H;
lat={{\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{0,0,3}};
Z2NumberCalcByWilsonLoop[h,lat,{1,2}]";

Z2NumberCalc::usage="Z2NumberCalc[H_,lattice_,bandIndex_,UT_,plane:(1|2|3|4|5|6):5]

Input:
H: The format of the Hamiltonian is H[{kx, ky, kz}, parameter...].
lattice: The primitive vectors of Bravais Lattices (Lattice Vectors).
bandIndex: Calculate the \!\(\*SubscriptBox[\(Z\), \(2\)]\) number of the bandIndex band.
UT: Time reversal operator.
plane: The calculated plane corresponds to: (1,2,3,4,5,6)->(\!\(\*SubscriptBox[\(x\), \(0\)]\),\!\(\*SubscriptBox[\(x\), \(\[Pi]\)]\),\!\(\*SubscriptBox[\(y\), \(0\)]\),\!\(\*SubscriptBox[\(y\), \(\[Pi]\)]\),\!\(\*SubscriptBox[\(z\), \(0\)]\),\!\(\*SubscriptBox[\(z\), \(\[Pi]\)]\)). The default value is 5.

Output:
KPoints: The coordinates of points in reciprocal space.
kPointFigure: K-point schematic diagram.
Z2Number: \!\(\*SubscriptBox[\(Z\), \(2\)]\) number.

Example:
h[{kx_,ky_,kz_},t_:1,\[Lambda]R_:0.0,\[Lambda]SO_:0.06,\[Lambda]v_:0.]=H;
lat={{\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),0},{0,0,3}};
UT=KroneckerProduct[PauliMatrix[0],I*PauliMatrix[2]];
Z2NumberCalc[h,lat,{1,2},UT]";


Begin["Private`"]


BerryPhaseCalc[H_,lattice_,bandIndex_,loop_]:=Module[
	{a1,a2,a3,\[CapitalOmega],b1,b2,b3,BZ,kk,kPointFigure,vectors,num,VEC1,VEC2,uab,BerryPhase},
	{a1,a2,a3}=lattice;
	\[CapitalOmega]=a1 . Cross[a2,a3];
	b1=2\[Pi] Cross[a2,a3]/\[CapitalOmega];b2=2\[Pi] Cross[a3,a1]/\[CapitalOmega];b3=2\[Pi] Cross[a1,a2]/\[CapitalOmega];
	BZ[{k1_,k2_,k3_}]:=k1*b1+k2*b2+k3*b3;
	kk=BZ/@loop;
	kPointFigure=Show[
		Graphics3D[{Thickness[0.003],Red,Arrow[{{0,0,0},b1}],Arrow[{{0,0,0},b2}],Arrow[{{0,0,0},b3}],
		Text[Style["\!\(\*SubscriptBox[\(b\), \(1\)]\)",Red,Italic,20],1.1 b1],Text[Style["\!\(\*SubscriptBox[\(b\), \(2\)]\)",Red,Italic,20],1.1 b2],Text[Style["\!\(\*SubscriptBox[\(b\), \(3\)]\)",Red,Italic,20],1.1 b3]}],
		ListPointPlot3D[kk,PlotStyle->Blue],
		BoxRatios->Automatic,AxesOrigin->{0,0,0}];
	vectors[{kx_,ky_,kz_}]:=Module[{eigensystem},
		eigensystem=Transpose[Eigensystem[H[{kx,ky,kz}]//N]//Chop]//Sort;
		Table[{k,eigensystem[[bandIndex[[k]],2]]},{k,1,Length[bandIndex]}]
		];
	(*VEC1=Table[vectors[kk[[i]]],{i,1,Length[kk]}];*)
	VEC1=MapThread[vectors,{kk}];
	VEC2=RotateLeft[VEC1];
	uab[vec1_,vec2_]:=Module[{k1,k2},
		Det[Table[Conjugate[vec1[[k1,2]]] . vec2[[k2,2]],{k1,1,Length[bandIndex]},{k2,1,Length[bandIndex]}]]
		];
	BerryPhase=-Arg[Times@@Table[uab[VEC1[[i]],VEC2[[i]]],{i,1,Length[kk]}]];
	(*BerryPhase=-Im[Log[Times@@Table[uab[VEC1[[i]],VEC2[[i]]],{i,1,Length[kk]}]]];*)
	(*Output*)
	Dataset[Association["KPoints"->kk,"kPointFigure"->kPointFigure,"BerryPhase"->BerryPhase],DatasetTheme->{"Serif"}]
	]

ChernNumberCalc[H_,lattice_,bandIndex_,b1b2points_:{50,50},b1b2ranges_:{2,2}]:=Module[
	{a1,a2,a3,\[CapitalOmega],b1,b2,b3,nx,ny,BZb1,BZb2,BZ,kk,kPointFigure,vectors,vec,uab,u12,u23,u34,u41,Fk,phi,
		BerryCurvatureData,ChernNumber,BerryCurvatureFigure3D,BerryCurvatureFigureContour,BerryCurvatureFigureDensity},
	{a1,a2,a3}=lattice;
	\[CapitalOmega]=a1 . Cross[a2,a3];
	b1=2\[Pi] Cross[a2,a3]/\[CapitalOmega];b2=2\[Pi] Cross[a3,a1]/\[CapitalOmega];b3=2\[Pi] Cross[a1,a2]/\[CapitalOmega];
	{nx,ny}=b1b2points;
	BZb1=b1b2ranges[[1]]*b1;
	BZb2=b1b2ranges[[2]]*b2;
	BZ[nb1_,nb2_]:=nb1*BZb1+nb2*BZb2;
	kk=Table[BZ[(i-1)/nx,(j-1)/ny]-BZ[1/2,1/2],{i,1,nx+1},{j,1,ny+1}];
	kPointFigure=Show[
		Graphics3D[{Thickness[0.003],Red,Arrow[{{0,0,0},b1}],Arrow[{{0,0,0},b2}],Arrow[{{0,0,0},b3}],
			Text[Style["\!\(\*SubscriptBox[\(b\), \(1\)]\)",Red,Italic,20],1.1 b1],Text[Style["\!\(\*SubscriptBox[\(b\), \(2\)]\)",Red,Italic,20],1.1 b2],Text[Style["\!\(\*SubscriptBox[\(b\), \(3\)]\)",Red,Italic,20],1.1 b3]}],
		ListPointPlot3D[Flatten[kk,1],PlotStyle->Blue],
		BoxRatios->Automatic,AxesOrigin->{0,0,0}];
	(*Eigenvectors*)
	vectors[{kx_,ky_,kz_},nband_]:=Module[{eigensystem},
		eigensystem=Transpose[Eigensystem[H[{kx,ky,kz}]//N]//Chop]//Sort;
		Table[{k,eigensystem[[nband[[k]],2]]},{k,1,Length[nband]}]
		];
	(*Creating an index*)
	vec=ConstantArray[0,{nx+1,ny+1}];
	(*Uab*)
	Table[vec[[i,j]]=vectors[kk[[i,j]],bandIndex],{i,1,nx+1},{j,1,ny+1}];
	(*F(k) & A(k)*)
	uab[vec1_,vec2_,nband_]:=Module[{k1,k2},
		Det[Table[Conjugate[vec1[[k1,2]]] . vec2[[k2,2]],{k1,1,Length[nband]},{k2,1,Length[nband]}]]
		];
	u12=Table[uab[vec[[i,j]],vec[[i+1,j]],bandIndex],{i,1,nx},{j,1,ny}];
	u23=Table[uab[vec[[i+1,j]],vec[[i+1,j+1]],bandIndex],{i,1,nx},{j,1,ny}];
	u34=Table[uab[vec[[i+1,j+1]],vec[[i,j+1]],bandIndex],{i,1,nx},{j,1,ny}];
	u41=Table[uab[vec[[i,j+1]],vec[[i,j]],bandIndex],{i,1,nx},{j,1,ny}];
	Fk=-Table[Im[Log[u12[[i,j]]*u23[[i,j]]*u34[[i,j]]*u41[[i,j]]]],{i,1,nx},{j,1,ny}];
	(*Berry curvature*)
	phi=Fk;
	BerryCurvatureData=Flatten[Table[Append[kk[[i,j]][[1;;2]],phi[[i,j]]],{i,1,nx},{j,1,ny}],1];
	(*Chern number*)
	(*ChernNumber=Total[Flatten[Fk]]/(2\[Pi]);*)
	ChernNumber=Round[Total[Flatten[Fk]]/(2\[Pi])/(Times@@b1b2ranges)];
	(*Plots*)
	BerryCurvatureFigure3D=ListPlot3D[BerryCurvatureData,PlotRange->Full,PlotLegends->Automatic,ColorFunction->Hue,AspectRatio->Automatic,
		AxesLabel->{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)",FontFamily->"Times New Roman",16],Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)",FontFamily->"Times New Roman",16],Style["\[CapitalOmega]",FontFamily->"Times New Roman",16]}];
	BerryCurvatureFigureContour=ListContourPlot[BerryCurvatureData,PlotRange->Full,PlotLegends->Automatic,ColorFunction->"Rainbow",AspectRatio->Automatic,
		FrameLabel->{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)",FontFamily->"Times New Roman",16],Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)",FontFamily->"Times New Roman",16]}];
	BerryCurvatureFigureDensity=ListDensityPlot[BerryCurvatureData,PlotRange->Full,PlotLegends->Automatic,ColorFunction->"Rainbow",AspectRatio->Automatic,
		FrameLabel->{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)",FontFamily->"Times New Roman",16],Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)",FontFamily->"Times New Roman",16]}];
	(*Output*)
	Dataset[Association["KPoints"->kk,"kPointFigure"->kPointFigure,"ChernNumber"->ChernNumber,"BerryCurvatureData"->SparseArray[BerryCurvatureData],"BerryCurvatureFigure3D"->BerryCurvatureFigure3D,"BerryCurvatureFigureContour"->BerryCurvatureFigureContour,"BerryCurvatureFigureDensity"->BerryCurvatureFigureDensity],
		DatasetTheme->{"Serif"}]
	]
	
Z2NumberCalcByWilsonLoop[H_,lattice_,bandIndex_,plane:(1|2|3|4|5|6):5,referenceLine_:-1,dikx_:10^-2.,diky_:10^-2.,deltaky_:10^-4.]:=Module[
	{a1,a2,a3,\[CapitalOmega],b1,b2,b3,nx,ny,BZ,nc,kk,kyphi,kyNum,eigensystem,DotProductMatrix,Wk,ky0num,ky1num,phi0,phi1,label,WannierCenterFigureList,PHI0,PHI1,M,Z2Number,kPointFigure},
	{a1,a2,a3}=lattice;
	\[CapitalOmega]=a1 . Cross[a2,a3];
	b1=2\[Pi] Cross[a2,a3]/\[CapitalOmega];b2=2\[Pi] Cross[a3,a1]/\[CapitalOmega];b3=2\[Pi] Cross[a1,a2]/\[CapitalOmega];
	BZ[{k1_,k2_,k3_}]:=k1*b1+k2*b2+k3*b3;
	nc=Length[bandIndex];
	(*Phi at each ky point*)
	kyphi[H0_][ny0_]:=Module[
		{},
		(*The Z2 invariant in 3D system is defined as Z2=(\[Nu]0,\[Nu]1,\[Nu]2,\[Nu]3)=(x0+x\[Pi],x\[Pi],y\[Pi],z\[Pi])*)
		(*Time reversal invariant planes = TRIP*)
		kyNum=Switch[plane,
			1,Table[BZ[{0.,nx,ny0}],{nx,0,1,dikx}],(*x0*)
			2,Table[BZ[{0.5,nx,ny0}],{nx,0,1,dikx}],(*x\[Pi]*)
			3,Table[BZ[{ny0,0.,nx}],{nx,0,1,dikx}],(*y0*)
			4,Table[BZ[{ny0,0.5,nx}],{nx,0,1,dikx}],(*y\[Pi]*)
			5,Table[BZ[{nx,ny0,0.}],{nx,0,1,dikx}],(*z0*)
			6,Table[BZ[{nx,ny0,0.5}],{nx,0,1,dikx}]];(*z\[Pi]*)
		eigensystem=Sort/@Transpose/@Eigensystem/@MapThread[H0,{kyNum}];
		DotProductMatrix[vec1_, vec2_]:=Table[vec1[[i]] . vec2[[j]],{i,Length[vec1]},{j,Length[vec2]}];
		Wk=Module[
			{VEC1,VEC2,WkF},
			VEC1=Conjugate[eigensystem[[All,1;;nc,2]]];
			VEC2=RotateLeft[eigensystem[[All,1;;nc,2]]];
			WkF=Dot@@MapThread[DotProductMatrix,{VEC1,VEC2}]
			];
		Sort[Arg[Eigenvalues[Wk]]]
		];
	(*phi0=Table[kyphi[H],[ny],{ny,0,0.5,diky}];*)
	ky0num=Table[ny,{ny,0,0.5,diky}];
	phi0=Map[kyphi[H],ky0num];	
	(*Plots*)
	(*The evolution lines of Wannier centers*)
	label=Switch[plane,
		1,{Style["\!\(\*SubscriptBox[\(k\), \(z\)]\)/2\[Pi]",FontFamily->"Times New Roman",16],Style["\[Phi]/2\[Pi]",FontFamily->"Times New Roman",16]},
		2,{Style["\!\(\*SubscriptBox[\(k\), \(z\)]\)/2\[Pi]",FontFamily->"Times New Roman",16],Style["\[Phi]/2\[Pi]",FontFamily->"Times New Roman",16]},
		3,{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)/2\[Pi]",FontFamily->"Times New Roman",16],Style["\[Phi]/2\[Pi]",FontFamily->"Times New Roman",16]},
		4,{Style["\!\(\*SubscriptBox[\(k\), \(x\)]\)/2\[Pi]",FontFamily->"Times New Roman",16],Style["\[Phi]/2\[Pi]",FontFamily->"Times New Roman",16]},
		5,{Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)/2\[Pi]",FontFamily->"Times New Roman",16],Style["\[Phi]/2\[Pi]",FontFamily->"Times New Roman",16]},
		6,{Style["\!\(\*SubscriptBox[\(k\), \(y\)]\)/2\[Pi]",FontFamily->"Times New Roman",16],Style["\[Phi]/2\[Pi]",FontFamily->"Times New Roman",16]}];
	WannierCenterFigureList=ListPlot[Table[Transpose[phi0][[i]],{i,1,nc}],
		Axes->False,AspectRatio->3/4,
		Frame->True,FrameStyle->Directive[Purple],
		FrameLabel->label,
		FrameTicks->{{{{-\[Pi],-0.5},{0,0},{\[Pi],0.5}},None},{{{1,0},{Length[phi0],0.5}},None}},
		PlotStyle->Blue,PlotRange->{{1,Length[phi0]},{-\[Pi],\[Pi]}},
		Epilog->{Red,Dashed,Line[{{1,referenceLine},{Length[phi0],referenceLine}}]}];	
	(*Winding number*)
	(*phi1=Table[kyphi[H][ny+deltaky],{ny,0,0.5,diky}];*)
	ky1num=Table[ny+deltaky,{ny,0,0.5,diky}];
	phi1=Map[kyphi[H],ky1num];
	PHI0=Transpose[phi0];
	PHI1=Transpose[phi1];
	M=Total[Table[(Total[(PHI1[[i]]-PHI0[[i]])/deltaky*diky]-(Last[PHI0[[i]]]-First[PHI0[[i]]]))/(2\[Pi]),{i,1,nc}]];
	Z2Number=Mod[Round[M],2];	
	(*k-points*)
	kk=Switch[plane,
		1,Table[BZ[{0.,nx,ny}],{nx,0,1,dikx},{ny,0,0.5,diky}],
		2,Table[BZ[{0.5,nx,ny}],{nx,0,1,dikx},{ny,0,0.5,diky}],
		3,Table[BZ[{ny,0.,nx}],{nx,0,1,dikx},{ny,0,0.5,diky}],
		4,Table[BZ[{ny,0.5,nx}],{nx,0,1,dikx},{ny,0,0.5,diky}],
		5,Table[BZ[{nx,ny,0.}],{nx,0,1,dikx},{ny,0,0.5,diky}],
		6,Table[BZ[{nx,ny,0.5}],{nx,0,1,dikx},{ny,0,0.5,diky}]];
	kPointFigure=Show[
		Graphics3D[{Thickness[0.003],Red,Arrow[{{0,0,0},b1}],Arrow[{{0,0,0},b2}],Arrow[{{0,0,0},b3}],
			Text[Style["\!\(\*SubscriptBox[\(b\), \(1\)]\)",Red,Italic,20],1.1 b1],Text[Style["\!\(\*SubscriptBox[\(b\), \(2\)]\)",Red,Italic,20],1.1 b2],Text[Style["\!\(\*SubscriptBox[\(b\), \(3\)]\)",Red,Italic,20],1.1 b3]}],
		ListPointPlot3D[Flatten[kk,1],PlotStyle->Blue],
		BoxRatios->Automatic,AxesOrigin->{0,0,0}];
	(*Output*)
	Dataset[Association["KPoints"->kk,"kPointFigure"->kPointFigure,"Z2Number"->Z2Number,"WannierCenterData"->SparseArray[phi0],"WannierCenterFigureList"->WannierCenterFigureList],
		DatasetTheme->{"Serif"}]
	]

Z2NumberCalc[H_,lattice_,bandIndex_,UT_,plane:(1|2|3|4|5|6):5]:=Module[
	{a1,a2,a3,\[CapitalOmega],b1,b2,b3,BZ,BZFigure,nx,ny,kk1,kk2,kk3,kk4,kk5,kk6,kk,Gamma00,Gamma0Pi,GammaPi0,GammaPiPi,ky0,kyPi,ky02kyPiFigure,kPointFigure,
		wk,skewMatrix,G00,G0Pi,GPi0,GPiPi,Pf00,Pf0Pi,PfPi0,PfPiPi,vectors,vec,DetUdaggerU,Uky0,P0,UkyPi,PPi,u12,u23,u34,u41,Fk,Z2Number},
	{a1,a2,a3}=lattice;
	\[CapitalOmega]=a1 . Cross[a2,a3];
	b1=2\[Pi] Cross[a2,a3]/\[CapitalOmega];b2=2\[Pi] Cross[a3,a1]/\[CapitalOmega];b3=2\[Pi] Cross[a1,a2]/\[CapitalOmega];
	BZ[{k1_,k2_,k3_}]:=k1*b1+k2*b2+k3*b3;
	BZFigure=Graphics3D[{Thickness[0.003],Red,Arrow[{{0,0,0},b1}],Arrow[{{0,0,0},b2}],Arrow[{{0,0,0},b3}],
		Text[Style["\!\(\*SubscriptBox[\(b\), \(1\)]\)",Red,Italic,20],1.1 b1],Text[Style["\!\(\*SubscriptBox[\(b\), \(2\)]\)",Red,Italic,20],1.1 b2],Text[Style["\!\(\*SubscriptBox[\(b\), \(3\)]\)",Red,Italic,20],1.1 b3]}];
	nx=10;
	ny=nx/2;
	(*Time reversal invariant planes = TRIP*)
	kk1=Table[BZ[{0,(i-1)/nx,(j-1)/ny/2}],{j,1,ny+1},{i,1,nx+1}];(*x0*)
	kk2=Table[BZ[{1/2,(i-1)/nx,(j-1)/ny/2}],{j,1,ny+1},{i,1,nx+1}];(*x\[Pi]*)
	kk3=Table[BZ[{(i-1)/nx,0,(j-1)/ny/2}],{j,1,ny+1},{i,1,nx+1}];(*y0*)
	kk4=Table[BZ[{(i-1)/nx,1/2,(j-1)/ny/2}],{j,1,ny+1},{i,1,nx+1}];(*y\[Pi]*)
	kk5=Table[BZ[{(i-1)/nx,(j-1)/ny/2,0}],{j,1,ny+1},{i,1,nx+1}];(*z0*)
	kk6=Table[BZ[{(i-1)/nx,(j-1)/ny/2,1/2}],{j,1,ny+1},{i,1,nx+1}];(*z\[Pi]*)
	kk=Switch[plane,1,kk1,2,kk2,3,kk3,4,kk4,5,kk5,6,kk6];
	(*time reversal invariant points*)
	(*-Subscript[\[CapitalGamma], i]=Subscript[\[CapitalGamma], i]+Subscript[n, i]G -> Subscript[\[CapitalGamma], i]=Subscript[n, i]G/2,H(Subscript[\[CapitalGamma], i])=\[CapitalTheta]H(Subscript[\[CapitalGamma], i])\[CapitalTheta]^-1, where Subscript[n, i]=0 or 1*)
	Gamma00=kk[[1,1]];(*(0,0)*)
	Gamma0Pi=kk[[ny+1,1]];(*(0,\[Pi])*)
	GammaPi0=kk[[1,ny+1]];(*(\[Pi],0)*)
	GammaPiPi=kk[[ny+1,ny+1]];(*(\[Pi],\[Pi])*)
	(*ky=0 line*)
	ky0=kk[[1,1;;ny+1]];
	(*ky=\[Pi] line*)
	kyPi=kk[[ny+1,1;;ny+1]];
	(*Schematic diagram*)
	ky02kyPiFigure=Show[BZFigure,
		ListPointPlot3D[{ky0,kyPi},AspectRatio->1,PlotStyle->Blue],
		BoxRatios->Automatic,AxesOrigin->{0,0,0}];
	kPointFigure=Show[BZFigure,
		ListPointPlot3D[kk,AspectRatio->1,PlotStyle->Blue],
		BoxRatios->Automatic,AxesOrigin->{0,0,0}];
	(*(0,0),(0,\[Pi]),(\[Pi],0),(\[Pi],\[Pi])*)
	(*unitary matrix w(k)*)
	wk[{kxo_,kyo_,kzo_}]:=Module[{kx=kxo,ky=kyo,kz=kzo,vecs1,vecs2,band,matrix},
		vecs1=Transpose[Eigensystem[H[{kx,ky,kz}]//N//Chop]]//Sort;
		(*vecs2=Transpose[Eigensystem[H[-{kx,ky,kz}]//N//Chop]]//Sort;*)
		vecs2=vecs1;
		band=Length[bandIndex];
		matrix=IdentityMatrix[band];
		Table[matrix[[k1,k2]]=Conjugate[vecs2[[k1,2]]] . UT . Conjugate[vecs1[[k2,2]]],{k1,1,band},{k2,1,band}]//Chop
		];
	(*skew-symmetric matrix*)
	skewMatrix[x_]:=-Transpose[UpperTriangularize[x]]+UpperTriangularize[x];
	{G00,G0Pi,GPi0,GPiPi}=skewMatrix/@{wk[Gamma00],wk[Gamma0Pi],wk[GammaPi0],wk[GammaPiPi]};
	(*Pfaffian*)
	(*https://resources.wolframcloud.com/FunctionRepository/resources/Pfaffian*)
	Pf00=ResourceFunction["Pfaffian"][G00];
	Pf0Pi=ResourceFunction["Pfaffian"][G0Pi];
	PfPi0=ResourceFunction["Pfaffian"][GPi0];
	PfPiPi=ResourceFunction["Pfaffian"][GPiPi];
	(*Eigenvalues and Eigenstates*)
	vectors[{kx_,ky_,kz_}]:=Module[{eigensystem},
		eigensystem=Transpose[Eigensystem[H[{kx,ky,kz}]//N//Chop]]//Sort;
		Table[{k,eigensystem[[bandIndex[[k]],2]]},{k,1,Length[bandIndex]}]
		];
	(*vec=Table[vectors[kk[[i,j]]],{i,1,ny+1},{j,1,nx+1}];*)
	vec=Table[MapThread[vectors,{kk[[i,All]]}],{i,1,ny+1}];
	DetUdaggerU[Udagger_,U_,nband_]:=Det[Table[Conjugate[Udagger[[k1,2]]] . U[[k2,2]],{k1,1,Length[nband]},{k2,1,Length[nband]}]];
	(*P(\[CapitalGamma]y)*)
	Uky0=Table[DetUdaggerU[vec[[1,i+1]],vec[[1,i]],bandIndex],{i,1,ny}];
	P0=Arg[(Times@@Uky0)*(Pf00/PfPi0)]/(2\[Pi]);
	UkyPi=Table[DetUdaggerU[vec[[ny+1,i+1]],vec[[ny+1,i]],bandIndex],{i,1,ny}];
	PPi=Arg[(Times@@UkyPi)*(Pf0Pi/PfPiPi)]/(2\[Pi]);
	(*F(k)*)
	u12=Table[DetUdaggerU[vec[[i,j]],vec[[i+1,j]],bandIndex],{i,1,ny},{j,1,nx}];
	u23=Table[DetUdaggerU[vec[[i+1,j]],vec[[i+1,j+1]],bandIndex],{i,1,ny},{j,1,nx}];
	u34=Table[DetUdaggerU[vec[[i+1,j+1]],vec[[i,j+1]],bandIndex],{i,1,ny},{j,1,nx}];
	u41=Table[DetUdaggerU[vec[[i,j+1]],vec[[i,j]],bandIndex],{i,1,ny},{j,1,nx}];
	Fk=Table[Arg[u12[[i,j]]*u23[[i,j]]*u34[[i,j]]*u41[[i,j]]],{i,1,ny},{j,1,nx}];
	(*Z2 invariant*)
	Z2Number=Mod[Round[Total[Flatten[Fk]]/(2\[Pi])-2*P0+2*PPi],2];
	(*Output*)
	Dataset[Association["KPoints"->kk,"kPointFigure"->kPointFigure,"Z2Number"->Z2Number],DatasetTheme->{"Serif"}]
	]


End[]


EndPackage[]
