(* ::Package:: *)

BeginPackage["TopoTB`VASP`"]


VASPBand::usage="VASPBand[POSCAR_,KPOINTS_,EIGENVAL_,EFermi_:0,ISPIN:(1|2):1,SOC:(1|2):1,ExportFileName_:\"BAND.dat\"]

Input:
POSCAR: Import POSCAR files.
KPOINTS: Import KPOINTS files.
EIGENVAL: Import EIGENVAL files.
EFermi: Input Fermi level. The default value is 0.
ISPIN: Corresponding to spin polarization in VASP. The default value is 1.
SOC: SOC=1, not considering SOC; SOC=2, considering SOC.
ExportFileName: Export file name. The default value is \"BAND.dat\".

Output\:ff1a
DirectLatticeVector: Lattice Vectors (Ang).
ReciprocalLatticeVector: Reciprocal-Space Vectors (Ang^-1).
TheNumberOfBands: Number of bands.
TheNumberOfKPoints: Number of K-points.
KLabel: The position of high-symmetry points along high-symmetry paths.
BandDataToColumns: Provide data explanation.
BandData: Band data.
SavePath: Save Path.
ExportData: Export data.

Example:
SetDirectory[FileNameTake[NotebookFileName[],{1,-2}]];
poscar=Import[\"POSCAR\",\"Table\"];
kpoints=Import[\"KPOINTS\",\"Table\"]; 
eigenval=Import[\"EIGENVAL\",\"Table\"];
efermi=-1.7400;
VASPBand[poscar,kpoints,eigenval,efermi,1,\"Band.dat\"]";

VASPPBand::usage="VASPPBand[POSCAR_,KPOINTS_,PROCAR_,EFermi_:0,ISPIN:(1|2):1,UpOrDown:(1|2):1,SOC:(1|{2,0}|{2,1}|{2,2}|{2,3}):1,selectAtom_:{0},ExportFileName_:\"PBAND.dat\"]

Input:
POSCAR: Import POSCAR files.
KPOINTS: Import KPOINTS files.
PROCAR: Import PROCAR files.
EFermi: Input Fermi level. The default value is 0.
ISPIN: Corresponding to spin polarization in VASP. The default value is 1.
UpOrDown: UpOrDown=1, calculate spin up; UpOrDown=2, calculate spin down.
SOC: SOC=1, not considering SOC; SOC={2,0}, considering the total SOC; SOC={2,1}, considering the spin polarization Sx of SOC; SOC={2,2}, considering the spin polarization Sy of SOC; SOC={2,3}, considering the spin polarization Sz of SOC. The default value is 1.
selectAtom: Choose which atom's orbital projection to calculate. The default value is selectAtom={0}, calculate total atom's orbital projection.
ExportFileName: Export file name. The default value is \"PBAND.dat\".

Output\:ff1a
DirectLatticeVector: Lattice Vectors (Ang).
ReciprocalLatticeVector: Reciprocal-Space Vectors (Ang^-1).
TheNumberOfBands: Number of bands.
TheNumberOfKPoints: Number of K-points.
KLabel: The position of high-symmetry points along high-symmetry paths.
PBandDataToColumns: Provide data explanation.
PBandData: PBand data.
SavePath: Save Path.
ExportData: Export data.

Example:
SetDirectory[FileNameTake[NotebookFileName[],{1,-2}]];
poscar=Import[\"POSCAR\",\"Table\"];
kpoints=Import[\"KPOINTS\",\"Table\"]; 
procar=Import[\"PROCAR\",\"Table\"];
efermi=-1.7400;
VASPPBand[poscar,kpoints,procar,efermi,1,{3},\"PBand.dat\"]";


Begin["Private`"]


VASPBand[POSCAR_,KPOINTS_,EIGENVAL_,EFermi_:0,ISPIN:(1|2):1,SOC:(1|2):1,ExportFileName_:"BAND.dat"]:=Module[
	{poscar,kpoints,eigenval,efermi,ispin,nkpts,nbands,PKPTS,KPTS,kpt1,kpt2,kpt3,kpt4,kpt5,a,b,c,abc,\[CapitalOmega],BZa,BZb,BZc,BZabc,
		kpt,dd,kptband,column,band,tband,KPT,KK,data0,banddata,path,ds},
	poscar=POSCAR;
	kpoints=Select[KPOINTS,# != {} && Characters[#][[1,1]] != "#" &];
	eigenval=Select[EIGENVAL,# != {}&];
	efermi=EFermi;
	Switch[SOC,
		1,ispin=ISPIN,
		2,ispin=1];
	nkpts=eigenval[[6,2]];
	nbands=eigenval[[6,3]];
	PKPTS=kpoints[[2,1]];
	KPTS=nkpts/kpoints[[2,1]];
	kpt1=Transpose[kpoints[[5;;All]]][[4]]/.{"GAMMA"->"\[CapitalGamma]"};
	kpt2=Join[{kpt1[[1]]},
		Table[ToString@kpt1[[2*i]]<>"|"<>ToString@kpt1[[2*i+1]],{i,1,KPTS-1}],{kpt1[[2*KPTS]]}];
	kpt3=Quiet[StringReplace[kpt2,(x:LetterCharacter..)~~"|"~~(x:LetterCharacter..)->x]];
	kpt4=StringCount[kpt3,RegularExpression["\\W\\D"]];
	kpt5=Flatten[Position[kpt4,1],1];
	a=poscar[[3]];b=poscar[[4]];c=poscar[[5]];
	abc={a,b,c};
	\[CapitalOmega]=a . Cross[b,c];
	BZa=2\[Pi]*Cross[b,c]/\[CapitalOmega];BZb=2\[Pi]*Cross[c,a]/\[CapitalOmega];BZc=2\[Pi]*Cross[a,b]/\[CapitalOmega];
	BZabc={BZa,BZb,BZc};
	(*Data*)
	kpt[k_]:=
		eigenval[[7+(nbands+1)*(k-1)]][[1]]*BZa+
		eigenval[[7+(nbands+1)*(k-1)]][[2]]*BZb+
		eigenval[[7+(nbands+1)*(k-1)]][[3]]*BZc;
	kpt[0]=kpt[1];
	dd=Table[Norm[kpt[k]-kpt[k-1]],{k,1,nkpts}];
	Table[dd[[1+(kpt5[[i]]-1)*PKPTS]]=0,{i,1,Length[kpt5]}]/;kpt5!={};
	kptband=Table[Total[dd[[1;;k]]],{k,1,nkpts}];
	If[ispin==2,column={2,3},column={2}];
	Array[band,nbands];
	tband=Table[band[i]=Table[Flatten[{kptband[[j]],eigenval[[7+i+(nbands+1)*(j-1),column]]-efermi},1],
		{j,1,nkpts}],{i,1,nbands}];
	(*KPOINTS*)
	Array[KPT,KPTS];
	Table[KPT[i]=Transpose[band[1]][[1,kpoints[[2]]*i]][[1]],{i,1,KPTS}];
	KK=Table[{KPT[i],kpt3[[i+1]]},{i,1,KPTS}];
	PrependTo[KK,{KPT[0]=Transpose[band[1]][[1,1]],kpt3[[1]]}];
	If[
		ispin==2,
		data0=PositionIndex[{"K-Path(1/\[Angstrom])","Energy(spin up)(eV)","Energy(spin down)(eV)"}],
		data0=PositionIndex[{"K-Path(1/\[Angstrom])","Energy(eV)"}]
		];
	(*BandData*)
	path=SetDirectory[FileNameTake[NotebookFileName[],{1,-2}]];
	banddata=Flatten[Table[band[i],{i,1,nbands}],1];
	(*Table[banddata=Insert[banddata,{},{nkpts+1}*n],{n,1,nbands-1}];*)
	Table[banddata=Insert[banddata,{"# Band Index   "<>ToString[n+1]},{nkpts+1}*n],{n,1,nbands-1}];
	PrependTo[banddata,{"# Band Index   "<>ToString[1]}];
	PrependTo[banddata,{"# High Symmetry Points   "<>ToString[KK]}];
	PrependTo[banddata,{"# The Number Of KPoints   "<>ToString[nkpts]}];
	PrependTo[banddata,{"# The Number Of Bands   "<>ToString[nbands]}];
	PrependTo[banddata,StringSplit["# "<>StringDelete[StringReplace[ToString[Keys[data0]],","->"   "],{"{","}"}],"   "]];
	(*The code below supplements the 0 at the end of the data*)
	banddata=Map[NumberForm[#,{16,14},NumberPadding->{" ", "0"}]&,N[banddata,14],{-1}];
	(*Output*)
	Dataset[Association["DirectLatticeVector"->abc,"ReciprocalLatticeVector"->BZabc,"TheNumberOfBands"->nbands,"TheNumberOfKPoints"->nkpts,
		"KLabel"->KK,"BandDataToColumns"->data0,"BandData"->tband,
		"SavePath"->path,"ExportData"->Export[ExportFileName,banddata]],
		DatasetTheme->{"Serif"}]
	]

VASPPBand[POSCAR_,KPOINTS_,PROCAR_,EFermi_:0,ISPIN:(1|2):1,UpOrDown:(1|2):1,SOC:(1|{2,0}|{2,1}|{2,2}|{2,3}):1,selectAtom_:{0},ExportFileName_:"PBAND.dat"]:=Module[
	{poscar,kpoints,procar,efermi,pRowNum,ispin,nkpts,nbands,atoms,PKPTS,KPTS,kpt1,kpt2,kpt3,kpt4,kpt5,a,b,c,abc,\[CapitalOmega],BZa,BZb,BZc,BZabc,selectAtom0,
		kpt,dd,kptband,energy,energyup,energydown,projected,projectedup,projecteddown,row,pband,tpband,KPT,KK,data0,pbanddata,path},
	poscar=POSCAR;
	kpoints=Select[KPOINTS,# != {} && Characters[#][[1,1]] != "#" &];
	procar=Select[PROCAR,# != {}&]/.s_String:>Sequence@@ToExpression[StringReplace["{"<>s<>"}",{x:DigitCharacter~~"-"~~y:DigitCharacter:>x~~",-"~~y,{":","=","#","occ."}:>"999"}]];
	efermi=EFermi;
	If[Total[poscar[[7]]]==1,
		Switch[SOC,
			1,{pRowNum=Total[poscar[[7]]],ispin=ISPIN,selectAtom0=If[selectAtom[[1]]==0,{1},selectAtom]},
			{2,0},{pRowNum=(Total[poscar[[7]]])*4,ispin=1,selectAtom0=If[selectAtom[[1]]==0,{1},selectAtom+0]},
			{2,1},{pRowNum=(Total[poscar[[7]]])*4,ispin=1,selectAtom0=If[selectAtom[[1]]==0,{2},selectAtom+1]},
			{2,2},{pRowNum=(Total[poscar[[7]]])*4,ispin=1,selectAtom0=If[selectAtom[[1]]==0,{3},selectAtom+2]},
			{2,3},{pRowNum=(Total[poscar[[7]]])*4,ispin=1,selectAtom0=If[selectAtom[[1]]==0,{4},selectAtom+3]}
			],
		Switch[SOC,
			1,{pRowNum=Total[poscar[[7]]]+1,ispin=ISPIN,selectAtom0=If[selectAtom[[1]]==0,{pRowNum},selectAtom]},
			{2,0},{pRowNum=(Total[poscar[[7]]]+1)*4,ispin=1,selectAtom0=If[selectAtom[[1]]==0,{(Total[poscar[[7]]]+1)*1},selectAtom+(Total[poscar[[7]]]+1)*0]},
			{2,1},{pRowNum=(Total[poscar[[7]]]+1)*4,ispin=1,selectAtom0=If[selectAtom[[1]]==0,{(Total[poscar[[7]]]+1)*2},selectAtom+(Total[poscar[[7]]]+1)*1]},
			{2,2},{pRowNum=(Total[poscar[[7]]]+1)*4,ispin=1,selectAtom0=If[selectAtom[[1]]==0,{(Total[poscar[[7]]]+1)*3},selectAtom+(Total[poscar[[7]]]+1)*2]},
			{2,3},{pRowNum=(Total[poscar[[7]]]+1)*4,ispin=1,selectAtom0=If[selectAtom[[1]]==0,{(Total[poscar[[7]]]+1)*4},selectAtom+(Total[poscar[[7]]]+1)*3]}
			]
		];
	nkpts=procar[[2,4]];
	nbands=procar[[2,8]];
	atoms=procar[[2,12]];
	PKPTS=kpoints[[2,1]];
	KPTS=nkpts/kpoints[[2,1]];
	kpt1=Transpose[kpoints[[5;;All]]][[4]]/.{"GAMMA"->"\[CapitalGamma]"};
	kpt2=Join[{kpt1[[1]]},
		Table[ToString@kpt1[[2*i]]<>"|"<>ToString@kpt1[[2*i+1]],{i,1,KPTS-1}],{kpt1[[2*KPTS]]}];
	kpt3=Quiet[StringReplace[kpt2,(x:LetterCharacter..)~~"|"~~(x:LetterCharacter..)->x]];
	kpt4=StringCount[kpt3,RegularExpression["\\W\\D"]];
	kpt5=Flatten[Position[kpt4,1],1];
	a=poscar[[3]];b=poscar[[4]];c=poscar[[5]];
	abc={a,b,c};
	\[CapitalOmega]=a . Cross[b,c];
	BZa=2\[Pi]*Cross[b,c]/\[CapitalOmega];BZb=2\[Pi]*Cross[c,a]/\[CapitalOmega];BZc=2\[Pi]*Cross[a,b]/\[CapitalOmega];
	BZabc={BZa,BZb,BZc};
	(*Data*)
	kpt[k_]:=
		procar[[3+(nbands*(2+pRowNum)+1)*(k-1),4;;6]][[1]]*BZa+
		procar[[3+(nbands*(2+pRowNum)+1)*(k-1),4;;6]][[2]]*BZb+
		procar[[3+(nbands*(2+pRowNum)+1)*(k-1),4;;6]][[3]]*BZc;
	kpt[0]=kpt[1];
	dd=Table[Norm[kpt[k]-kpt[k-1]],{k,1,nkpts}];
	Table[dd[[1+(kpt5[[i]]-1)*PKPTS]]=0,{i,1,Length[kpt5]}]/;kpt5!={};
	kptband=Table[Total[dd[[1;;k]]],{k,1,nkpts}];
	If[
		ispin==2,
		{energyup[i_,j_]:=procar[[4+(i-1)*(2+pRowNum)+(nbands*(2+pRowNum)+1)*(j-1),5]]-efermi,
		projectedup[i_,j_,atom_]:=procar[[5+atom+(i-1)*(2+pRowNum)+(nbands*(2+pRowNum)+1)*(j-1),2;;]],
		row=5+pRowNum+(nbands-1)*(2+pRowNum)+(nbands*(2+pRowNum)+1)*(nkpts-1),
		energydown[i_,j_]:=procar[[row+3+(i-1)*(2+pRowNum)+(nbands*(2+pRowNum)+1)*(j-1),5]]-efermi,
		projecteddown[i_,j_,atom_]:=procar[[row+4+atom+(i-1)*(2+pRowNum)+(nbands*(2+pRowNum)+1)*(j-1),2;;]]},
		{energy[i_,j_]:=procar[[4+(i-1)*(2+pRowNum)+(nbands*(2+pRowNum)+1)*(j-1),5]]-efermi,
		projected[i_,j_,atom_]:=procar[[5+atom+(i-1)*(2+pRowNum)+(nbands*(2+pRowNum)+1)*(j-1),2;;]]}
	];
	Switch[ispin,
		1,
		{Array[pband,nbands];
		tpband=
			Table[pband[i]=Table[
				Flatten[{kptband[[j]],energy[i,j],Sum[projected[i,j,s],{s,selectAtom0}]},1],
				{j,1,nkpts}],{i,1,nbands}]},
		2,
		Switch[UpOrDown,
			1,
			{Array[pband,nbands],
			tpband=
				Table[pband[i]=Table[
					Flatten[{kptband[[j]],energyup[i,j],Sum[projectedup[i,j,s],{s,selectAtom0}]},1],
					{j,1,nkpts}],{i,1,nbands}]},
			2,
			{Array[pband,nbands],
			tpband=
				Table[pband[i]=Table[
					Flatten[{kptband[[j]],energydown[i,j],Sum[projecteddown[i,j,s],{s,selectAtom0}]},1],
					{j,1,nkpts}],{i,1,nbands}]}
		]
	];
	(*KPOINTS*)
	Array[KPT,KPTS];
	Table[KPT[i]=Transpose[pband[1]][[1,kpoints[[2]]*i]][[1]],{i,1,KPTS}];
	KK=Table[{KPT[i],kpt3[[i+1]]},{i,1,KPTS}];
	PrependTo[KK,{KPT[0]=Transpose[pband[1]][[1,1]],kpt3[[1]]}];
	data0=PositionIndex[Flatten[Append[{"K-Path(1/\[Angstrom])","Energy(eV)"},procar[[5,2;;]]]]];	
	(*BandData*)
	path=SetDirectory[FileNameTake[NotebookFileName[],{1,-2}]];
	pbanddata=Flatten[Table[pband[i],{i,1,nbands}],1];
	(*Table[banddata=Insert[banddata,{},{nkpts+1}*n],{n,1,nbands-1}];*)
	Table[pbanddata=Insert[pbanddata,{"# Band Index   "<>ToString[n+1]},{nkpts+1}*n],{n,1,nbands-1}];
	PrependTo[pbanddata,{"# Band Index   "<>ToString[1]}];
	PrependTo[pbanddata,{"# High Symmetry Points   "<>ToString[KK]}];
	PrependTo[pbanddata,{"# The Number Of KPoints   "<>ToString[nkpts]}];
	PrependTo[pbanddata,{"# The Number Of Bands   "<>ToString[nbands]}];
	PrependTo[pbanddata,StringSplit["# "<>StringDelete[StringReplace[ToString[Keys[data0]],","->"   "],{"{","}"}],"   "]];
	(*The code below supplements the 0 at the end of the data*)
	pbanddata=Map[NumberForm[#,{16,14},NumberPadding->{" ", "0"}]&,N[pbanddata,14],{-1}];
	(*Output*)
	Dataset[Association["DirectLatticeVector"->abc,"ReciprocalLatticeVector"->BZabc,"TheNumberOfBands"->nbands,"TheNumberOfKPoints"->nkpts,
		"KLabel"->KK,"PBandDataToColumns"->data0,"PBandData"->tpband,
		"SavePath"->path,"ExportData"->Export[ExportFileName,pbanddata]],
		DatasetTheme->{"Serif"}]
	]


End[]


EndPackage[]
