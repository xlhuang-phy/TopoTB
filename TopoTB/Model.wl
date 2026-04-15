(* ::Package:: *)

BeginPackage["TopoTB`Model`"]


KaneMeleModel::usage="KaneMeleModel[kx_,ky_,t_,\[Lambda]v_,\[Lambda]R_,\[Lambda]SO_]

Lattice vectors: \!\(\*SubscriptBox[\(a\), \(1\)]\)={\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\)}, \!\(\*SubscriptBox[\(a\), \(2\)]\)={-\!\(\*FractionBox[\(1\), \(2\)]\),\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\)}

Time reversal operator: \[CapitalTheta] = \!\(\*SubscriptBox[\(\[Sigma]\), \(0\)]\) \[CircleTimes] \!\(\*SubscriptBox[\(i\[Sigma]\), \(y\)]\) K

References:
[1] Kane C L, Mele E J. Quantum spin Hall effect in graphene[J]. Physical review letters, 2005, 95(22): 226801.
[2] Kane C L, Mele E J. Z 2 topological order and the quantum spin Hall effect[J]. Physical review letters, 2005, 95(14): 146802.";

BHZModel::usage="BHZModel[kx_,ky_,A_,B_,C_,D_,M_]

Lattice vectors: \!\(\*SubscriptBox[\(a\), \(1\)]\)={1,0}, \!\(\*SubscriptBox[\(a\), \(2\)]\)={0,1}

Time reversal operator: \[CapitalTheta] = \!\(\*SubscriptBox[\(i\[Sigma]\), \(y\)]\) \[CircleTimes] \!\(\*SubscriptBox[\(\[Sigma]\), \(0\)]\) K

References:
[1] Bernevig B A, Hughes T L, Zhang S C. Quantum spin Hall effect and topological phase transition in HgTe quantum wells[J]. science, 2006, 314(5806): 1757-1761.";

HaldaneModel::usage="HaldaneModel[kx_,ky_,t1_,t2_,M_,\[Phi]_]

Lattice vectors: \!\(\*SubscriptBox[\(a\), \(1\)]\)={-\!\(\*SqrtBox[\(3\)]\),0}, \!\(\*SubscriptBox[\(a\), \(1\)]\)={\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\),-\!\(\*FractionBox[\(3\), \(2\)]\)}

References:
[1] Haldane F D M. Model for a quantum Hall effect without Landau levels: Condensed-matter realization of the \" parity anomaly\"[J]. Physical review letters, 1988, 61(18): 2015.";

SSHModel::usage="SSHModel[kx_,v_,w_]

Lattice vectors: \!\(\*SubscriptBox[\(a\), \(1\)]\)={1,0}

References:
[1] Su W P, Schrieffer J R, Heeger A J. Solitons in polyacetylene[J]. Physical review letters, 1979, 42(25): 1698.
[2] Asb\[OAcute]th J K, Oroszl\[AAcute]ny L, P\[AAcute]lyi A. A short course on topological insulators[J]. Lecture notes in physics, 2016, 919: 166.";

IQHEModel::usage="IQHEModel[kx_,ky_,A_,B_,\[CapitalDelta]_]

Lattice vectors: \!\(\*SubscriptBox[\(a\), \(1\)]\)={1,0}, \!\(\*SubscriptBox[\(a\), \(2\)]\)={0,1}

References:
[1] SHUN-QING. SHEN. Topological Insulators: Dirac Equation in Condensed Matter[M]. Springer, 2018.";

QSHEModel::usage="QSHEModel[kx_,ky_,A_,B_,\[CapitalDelta]_]

Lattice vectors: \!\(\*SubscriptBox[\(a\), \(1\)]\)={1,0}, \!\(\*SubscriptBox[\(a\), \(2\)]\)={0,1}

Time reversal operator: \[CapitalTheta] = \!\(\*SubscriptBox[\(\[Sigma]\), \(y\)]\) \[CircleTimes] \!\(\*SubscriptBox[\(i\[Sigma]\), \(y\)]\) K

References:
[1] SHUN-QING. SHEN. Topological Insulators: Dirac Equation in Condensed Matter[M]. Springer, 2018.";


Begin["Private`"]


(*Kane-Mele Model*)
KaneMeleModel[kx_,ky_,t_,\[Lambda]v_,\[Lambda]R_,\[Lambda]SO_]:=Module[
	{\[Sigma]0,\[Sigma]x,\[Sigma]y,\[Sigma]z,s0,sx,sy,sz,\[CapitalGamma]1,\[CapitalGamma]2,\[CapitalGamma]3,\[CapitalGamma]4,\[CapitalGamma]5,\[CapitalGamma]ab,d1,d2,d3,d4,d12,d15,d23,d24},
	{\[Sigma]0,\[Sigma]x,\[Sigma]y,\[Sigma]z}={s0,sx,sy,sz}=Map[PauliMatrix,{0,1,2,3}];
	{\[CapitalGamma]1,\[CapitalGamma]2,\[CapitalGamma]3,\[CapitalGamma]4,\[CapitalGamma]5}={KroneckerProduct[\[Sigma]x,s0],KroneckerProduct[\[Sigma]z,s0],KroneckerProduct[\[Sigma]y,sx],KroneckerProduct[\[Sigma]y,sy],KroneckerProduct[\[Sigma]y,sz]};
	\[CapitalGamma]ab[x_,y_]:=(x . y-y . x)/2/I;
	d1=t(1+2 Cos[kx/2]Cos[Sqrt[3]ky/2]);
	d2=\[Lambda]v;
	d3=\[Lambda]R(1-Cos[kx/2]Cos[Sqrt[3]ky/2]);
	d4=-Sqrt[3]\[Lambda]R Sin[kx/2]Sin[Sqrt[3]ky/2];
	d12=-2t Cos[kx/2]Sin[Sqrt[3]ky/2];
	d15=\[Lambda]SO(2Sin[kx]-4Sin[kx/2]Cos[Sqrt[3]ky/2]);
	d23=-\[Lambda]R Cos[kx/2]Sin[Sqrt[3]ky/2];
	d24=Sqrt[3]\[Lambda]R Sin[kx/2]Cos[Sqrt[3]ky/2];
	\[CapitalGamma]1*d1+\[CapitalGamma]2*d2+\[CapitalGamma]3*d3+\[CapitalGamma]4*d4+\[CapitalGamma]ab[\[CapitalGamma]1,\[CapitalGamma]2]*d12+\[CapitalGamma]ab[\[CapitalGamma]1,\[CapitalGamma]5]*d15+\[CapitalGamma]ab[\[CapitalGamma]2,\[CapitalGamma]3]*d23+\[CapitalGamma]ab[\[CapitalGamma]2,\[CapitalGamma]4]*d24
	]
	
(*BHZ Model*)
BHZModel[kx_,ky_,A_,B_,C_,D_,M_]:=Module[
	{s0,sx,sy,sz,d1,d2,d3,\[Epsilon],H},
	s0=PauliMatrix[0];
	sx=PauliMatrix[1];
	sy=PauliMatrix[2];
	sz=PauliMatrix[3];
	d1=A Sin[kx];
	d2=A Sin[ky];
	d3=-2 B (2-(M/(2 B))-Cos[kx]-Cos[ky]);
	\[Epsilon]=C - 2 D * (2-Cos[kx]-Cos[ky]);
	H=d1*sx+d2*sy+d3*sz+\[Epsilon]*s0//FullSimplify;
	ArrayFlatten[{{H,0},{0,ComplexExpand[Conjugate[H]/.{kx->-kx,ky->-ky}]}}]//FullSimplify
	]
	
(*Haldane Model*)
HaldaneModel[kx_,ky_,t1_,t2_,M_,\[Phi]_]:=Module[
	{k,a,a1,a2,a3,b1,b2,b3,\[Sigma]0,\[Sigma]1,\[Sigma]2,\[Sigma]3,\[Epsilon],dx,dy,dz},
	k={kx,ky};
	a=1;
	a1={0,a};a2={-(Sqrt[3]/2)a,-(a/2)};a3={Sqrt[3]/2 a,-(a/2)};
	b1=a2-a3;b2=a3-a1;b3=a1-a2;
	{\[Sigma]0,\[Sigma]1,\[Sigma]2,\[Sigma]3}={PauliMatrix[0],PauliMatrix[1],PauliMatrix[2],PauliMatrix[3]};
	\[Epsilon]=2*t2*Cos[\[Phi]]*(Cos[k . b1]+Cos[k . b2]+Cos[k . b3]);
	dx=t1*(Cos[k . a1]+Cos[k . a2]+Cos[k . a3]);
	dy=t1*(Sin[k . a1]+Sin[k . a2]+Sin[k . a3]);
	dz=M-2*t2*Sin[\[Phi]]*(Sin[k . b1]+Sin[k . b2]+Sin[k . b3]);
	\[Epsilon]*\[Sigma]0+dx*\[Sigma]1+dy*\[Sigma]2+dz*\[Sigma]3
	]
	
(*SSH Model*)
SSHModel[kx_,v_,w_]:=Module[
	{},
	{{0,v+w*Exp[-I kx]},
	{v+w*Exp[I kx],0}}
	]
	
(*Two-Dimensional Lattice Model: Integer Quantum Hall Effect*)
IQHEModel[kx_,ky_,A_,B_,\[CapitalDelta]_]:=Module[
	{s0,sx,sy,sz,dx,dy,dz},
	s0=PauliMatrix[0];
	sx=PauliMatrix[1];
	sy=PauliMatrix[2];
	sz=PauliMatrix[3];
	dx=A*Sin[kx];
	dy=A*Sin[ky];
	dz=\[CapitalDelta]-4*B*Sin[kx/2]^2-4*B*Sin[ky/2]^2;
	dx*sx+dy*sy+dz*sz
	]
	
(*Two-Dimensional Lattice Model: Quantum Spin Hall Effect*)
QSHEModel[kx_,ky_,A_,B_,\[CapitalDelta]_]:=Module[
	{s0,sx,sy,sz,dx,dy,dz},
	s0=PauliMatrix[0];
	sx=PauliMatrix[1];
	sy=PauliMatrix[2];
	sz=PauliMatrix[3];
	dx=A*Sin[kx];
	dy=A*Sin[ky];
	dz=\[CapitalDelta]-4*B*Sin[kx/2]^2-4*B*Sin[ky/2]^2;
	KroneckerProduct[dx*sx,s0]+KroneckerProduct[dy*sy,s0]+KroneckerProduct[dz*sz,sz]
	]


End[]


EndPackage[]
