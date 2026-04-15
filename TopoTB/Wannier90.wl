(* ::Package:: *)

BeginPackage["TopoTB`Wannier90`"]


Wannier90hr::usage="Wannier90hr[wannier90hr_,lattice_,k_,LatticeGaugeOrAtomGauge:(1|2):1,wfcentre_:{}]

Input:
wannier90hr: Import wannier90_hr.dat files.
lattice: Import lattice vectors, which can be manually inputted or obtained from files such as POSCAR.
k: Enter the k-space vector, for example {kx, ky, kz}.
LatticeGaugeOrAtomGauge: Choose lattice gauge or atomic gauge. Input 1 for lattice gauge (The default value is 1), input 2 for atomic gauge.
wfcentre: If choosing atomic gauge, the WF center after the Final State in the wannier90.out file is required.

Output:
TheNumberOfBands: Number of bands.
TheNumberOfHoppings: Number of hoppings.
Degeneracy: Degeneracy of each point.
Hoppings: Hopping direction.
HRealSpace: Real space Hamiltonian corresponding to each hopping direction.
HKSpace: The k-space Hamiltonian corresponding to each hopping direction.
HKTotal: Total k-space Hamiltonian.

Example:
SetDirectory[FileNameTake[NotebookFileName[],{1,-2}]];
wannier90hr=Select[Import[\"Graphene_wannier90_hr.dat\",\"Table\"],#\[NotEqual]{}&&Characters[#][[1,1]]\[NotEqual]\"#\"&];
poscar=Import[\"Graphene_POSCAR\",\"Table\"];
wfcentre=Import[\"Graphene_WFcentre.txt\",\"Table\"];
efermi=-1.7611;
a=poscar[[3]];b=poscar[[4]];c=poscar[[5]];
lat={a,b,c};
dsLattice=Wannier90hr[wannier90hr,lat,{kx,ky,kz}]
dsAtom=Wannier90hr[wannier90hr,lat,{kx,ky,kz},2,wfcentre]";


Begin["Private`"]


Wannier90hr[wannier90hr_,lattice_,k_,LatticeGaugeOrAtomGauge:(1|2):1,wfcentre_:{}]:=Module[
	{a1,a2,a3,wannier90hr0,nbands,nhopping,degeneracy,TBparameters,tbhopping,hopping,temp,HRealSpace,h,WFcentre,HKSpace,HKTotal},
	{a1,a2,a3}=lattice;
	wannier90hr0=Select[wannier90hr,#!={}&&Characters[#][[1,1]]!="#"&];
	nbands=wannier90hr0[[2,1]];(*The number of Wannier function = num_wann*)
	nhopping=wannier90hr0[[3,1]];(*The number of Wigner-Seitz grid-points = nrpts*)
	degeneracy=Flatten[wannier90hr0[[4;;3+Ceiling[nhopping/15]]]];(*the degeneracy of each Wigner-Seitz grid-points*)
	TBparameters=wannier90hr0[[4+Ceiling[nhopping/15];;]];(*TB parameters*)
	(*Group by hopping*)
	tbhopping=Table[TBparameters[[i;;i+nbands^2-1]],{i,1,Length[TBparameters],nbands^2}];
	hopping=Table[tbhopping[[i,1,1;;3]],{i,1,nhopping}];
	HRealSpace=SparseArray[ConstantArray[0,{nhopping,nbands,nbands}]];
	(*HRealSpace=ConstantArray[0,nhopping];*)
	Table[{temp=IdentityMatrix[nbands],
			Table[temp[[tbhopping[[i,j,4]],tbhopping[[i,j,5]]]]=tbhopping[[i,j,6]]+I*tbhopping[[i,j,7]],{j,1,nbands^2}],
			HRealSpace[[i]]=temp/degeneracy[[i]]},{i,1,nhopping}];
	h[i_]:=Exp[I k . (hopping[[i,1]]*a1+hopping[[i,2]]*a2+hopping[[i,3]]*a3)]*HRealSpace[[i]];
	(*lattice gauge or atomic guage*)
	HKSpace=SparseArray[ConstantArray[0,{nhopping,nbands,nbands}]];
	(*HKSpace=ConstantArray[0,nhopping];*)
	Which[
		LatticeGaugeOrAtomGauge==1,
			{Table[HKSpace[[i]]=Chop[h[i]],{i,1,nhopping}],HKTotal=Sum[HKSpace[[i]],{i,1,nhopping}]},
		LatticeGaugeOrAtomGauge==2,
			{WFcentre=Table[Exp[I k . (-wfcentre[[i]]+wfcentre[[j]])],{i,1,nbands},{j,1,nbands}],Table[HKSpace[[i]]=Chop[h[i]*WFcentre],{i,1,nhopping}],HKTotal=Sum[Chop[h[i]],{i,1,nhopping}]*WFcentre}
		];
	(*Output*)
	Dataset[Association["TheNumberOfBands"->nbands,"TheNumberOfHoppings"->nhopping,"Degeneracy"->degeneracy,"Hoppings"->hopping,"HRealSpace"->HRealSpace,"HKSpace"->HKSpace,"HKTotal"->HKTotal],
		DatasetTheme->{"Serif"}]
	]


End[]


EndPackage[]
