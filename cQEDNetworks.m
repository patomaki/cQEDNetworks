(* ::Package:: *)

 BeginPackage["cQEDNetworks`"];
MatchVector::usage="Returns a vector of zeros (for non-matching elements of v1, v2) and ones (for matching elements of v1, v2).\nInput (2): vector; vector (they must be of equal length)";
UnsrtCountMatch::usage="Returns the number of list1-elements, which also are list2-elements.\nInput (2): list1 (reference); list2.";
UnsrtMatchQ::usage="Returns true if (unsorted) list1 and list2 contain the same elements, false otherwise.\nInput (2):list1; list2.";
UnsrtSubsetQ::usage="Returns true if (unsorted) list2 is a subset of list2.\nInput (2): list1; list2";
OrientEdge::usage="Return +1 if the edge (ei) is oriented in the same direction as the cycle, and -1 if the edge is oriented to the opposite direction.\nInput (4): incidence matrix; list of cycle nodes; list of cycle edges; edge index";
PossibleEdges::usage="Finds a list of edges which connect to a connected sequence of edges, and associated nodes, within a list of edges.\nInput (3): graph-edge-list; a node-sequence; an edge-sequence.";
AppendSequence::usage="Adds new nodes and edges (i.e. branches) to a connected sequence of nodes (s0), and associated edges (se0), according to possible edges (pe0), returns the appended sequence {seq,sedges}.\nInput (4): list of edges; node sequence; edge sequence; list of edges that are connected to the sequence (see PossibleEdges)";
FindCycles::usage="Returns a list of cycles (i.e. loops) in the form {{c1nodes,c1edges},{c2nodes,c2edges},...}. The list includes all the cycles of the graph.\nInput (7): list of nodes; list of edges; list of current nodes in a sequence (can be empty); list of current edges in a sequence (can be empty); list of nodes of found cycles (can be empty); list of edges of found cycles (can be empty); depth (one or larger)";
FindAllCycles::usage="Return a list of all cycles {cnodes,cedges} of a graph.\nInput (1): graph object";
FindSpanningTree::usage="Returns a list of spanning trees (lists of branches), starting from branch bstart. Note that this functions does not find all the spanning trees of the graph.\nInput (3): the index of the starting branch; graph object; list of all cycles (elements in the form {cn,ce})";
CospanningTree::usage="Returns a co-spanning tree corresponding to spanning tree.\nInput (2): spanning tree; branch-list";
FundamentalCycles::usage="Find fundamental cycles, given a spanning tree (stree).\nInput (3): spanning tree; co-spanning tree; graph object";
ClassifyNodes::usage="Classify a set of nodes in a graph as either passive, active or ground. Ground nodes are recognized from branch name (b), passive nodes are only connected to capacitive edges (btype). The rest of the nodes are active.\nInput (4): branch-list; branch-type-list (types: C (capacitive),L (inductive),J (josephson junction,inductive),W (wire)); node list; incidence matrix";
ReduceBranchVariables::usage="Return a list of solutions to Kirchoff's voltage equations.\nInput (6): list of branch variables; redundant branch variables (to solve for); incidence matrix; spanning tree; graph object; and all cycles (elements in the form {cnodes,cedges})";
SolveNodeVariables::usage="Return a list of solutions to Kirchhoff's current equations\nInput (4): branch-variable-list; dynamical-branch-variable-list (to solve for); node-variable-list ; indicence matrix";
CapacitiveEnergy::usage="Return capacitive energy.\nInput (2): branch flux; capacitance at the branch (all components are assumed to be two-port)";
Begin["Private`"];
MatchVector[v1_List,v2_List]:=Module[{vmatch},
  If[Length[v1]!=Length[v2],
    Print["Error: Length of input vectors is not equal. Exiting."];
    Return[{}];,
	vmatch=Table[0,{i,1,Length[v1]}];
    Do[
	  If[v1[[i]]==v2[[i]],
	    vmatch[[i]]=1;
	  ];
    ,{i,1,Length[v1]}];
    Return[vmatch];
  ];
];
UnsrtCountMatch[list10_List,list20_List]:=Module[{positions,list1=list10,list2=list20,debug=False},
(*list1 is the reference set*)
(*list2 is the trial set*)
  If[Length[list2]>Length[list1],
    If[debug,Print["Error: ",list1," has to be longer or equal to the length of ",list2,". Exiting."]];
    Return[{{}}],
  positions={};
  Do[
    If[Length[Position[list2,list1[[i]]]]>0,
      positions=Append[positions,{i}];
    ];
  ,{i,1,Length[list1]}];
  Return[Length[positions]]
 ];
];
UnsrtMatchQ[list1_List,list2_List]:=Module[{},
  If[UnsrtCountMatch[list1,list2]==Length[list1],True,False]
];
UnsrtSubsetQ[list1_,list2_]:=Module[{},
  If[UnsrtCountMatch[list1,list2]==Length[list2],True,False]
];
OrientEdge[IM_,cn_,ce_,ei_]:=Module[{epos,ni,oi},
  epos=Position[ce,ei][[1]][[1]];
  ni=cn[[epos]];
  If[IM[[ni,ei]]==-1,
    oi=+1,
    oi=-1;
  ];
  oi
];
PossibleEdges[edges_,sn0_List,se0_List]:=Module[
{sn=sn0,se=se0,j,pedges={},n1,n2},
Do[
  j=If[i==Length[edges]
    ,Length[edges]
    ,Max[Mod[i,Length[edges]],1]
  ];(*index modulo Length[edges] with Mathematica indexing 1...Length[edges]*)
  n1=edges[[j]][[1]];
  n2=edges[[j]][[2]];
  If[sn[[-1]]==n1&&FreeQ[sn[[2;;-1]],n2]&&FreeQ[se,j],
    pedges=Append[pedges,j],
    If[sn[[-1]]==n2&&FreeQ[sn[[2;;-1]],n1]&&FreeQ[se,j],
    pedges=Append[pedges,j]
    ]
  ]
,{i,1,Length[edges]}];
pedges
];
AppendSequence[edges_,sn0_List,se0_List,pe0_List]:=Module[
{sn,se,pedges=pe0,n1,n2,snodes={},sedges={},debug=False},
Do[
n1=edges[[e]][[1]];
n2=edges[[e]][[2]];
If[debug,Print["e=",e," n1=",n1," n2=",n2]];
sn=sn0;
se=se0;
If[sn[[-1]]==n1&&FreeQ[sn[[2;;-1]],n2]&&FreeQ[se,e],
  sn=Append[sn,n2];
  se=Append[se,e];,
  If[sn[[-1]]==n2&&FreeQ[sn[[2;;-1]],n1]&&FreeQ[se,e],
    sn=Append[sn,n1];
    se=Append[se,e];
  ];
];
If[debug,Print["sn=",sn," se=",se]];
snodes=Append[snodes,sn];
sedges=Append[sedges,se];
,{e,pedges}];
{snodes,sedges}
];
FindCycles[nodes_,edges_,snodes0_List,sedges0_List,cnodes0_List,cedges0_List,depth_]:=
Module[{maxdepth,snodes=snodes0,sedges=sedges0,sn,se,sn2,se2,pe,cnodes=cnodes0,cedges=cedges0,Nsnodes,ls,j,debug=False},
maxdepth=Length[nodes];
If[debug,Print["\ndepth=",depth]];
Nsnodes=Length[snodes];
ls=Length[snodes[[1]]];
If[debug,Print["Loop through current sequences"]];
Do[
  If[debug,Print["i=",i]];
  Clear[sn2,se2];
  sn=snodes[[i]];
  se=sedges[[i]];
  pe=PossibleEdges[edges,sn,se];
  If[debug,
    Print["sn=",sn];
    Print["se=",se];
    Print["pe=",pe]
  ];
  {sn2,se2}=AppendSequence[edges,sn,se,pe];
  Do[
    snodes=Append[snodes,sn2[[j]]];
    sedges=Append[sedges,se2[[j]]];
  ,{j,1,Length[sn2]}];
,{i,1,Nsnodes}];

If[debug,Print["Trim seqs,sedges"]];
j=0;
Do[(*Delete elements of length ls*)
  If[Length[snodes[[i-j]]]==ls,
  snodes=Delete[snodes,i-j];
  sedges=Delete[sedges,i-j];
  j=j+1];
,{i,1,Nsnodes}];
If[debug,
  Print["snodes=",snodes];
  Print["sedges=",sedges];
  Print["Append to ready cycles"]
];
Nsnodes=Length[snodes];
j=0;
Do[
  sn=snodes[[i-j]];
  se=sedges[[i-j]];
  If[sn[[1]]==sn[[-1]],
    cnodes=Append[cnodes,sn];
    cedges=Append[cedges,se];
    snodes=Delete[snodes,i-j];
    sedges=Delete[sedges,i-j];
    j=j+1;
  ];
,{i,1,Nsnodes}];
If[debug,
  Print["snodes=",snodes];
  Print["sedges=",sedges];
  Print["cnodes=",cnodes];
  Print["cedges=",cedges]
];

(*Recursive loop*)
If[depth>=maxdepth||snodes=={},
  Return[{snodes,sedges,cnodes,cedges}],
  FindCycles[nodes,edges,snodes,sedges,cnodes,cedges,depth+1]
]
];
FindAllCycles[g_]:=Module[
{edges,allcycles,allcnodes,allcedges,snodes0,sedges0,maxlen,sdepth,cnodes,cedges,
seqnodes,seqedges,cyclenodes,cycleedges,ncycles,k,dupl,nodes},
edges=EdgeList[g];
nodes=VertexList[g];
allcnodes={};
allcedges={};
allcycles={};
Do[
  snodes0={{edges[[e]][[1]],edges[[e]][[2]]}};
  sedges0={{e}};
  maxlen=Length[n];
  sdepth=1;
  cnodes={};
  cedges={};
  {seqnodes,seqedges,cyclenodes,cycleedges}=FindCycles[nodes,edges,snodes0,sedges0,cnodes,cedges,sdepth];
  allcnodes=Append[allcnodes,cyclenodes];
  allcedges=Append[allcedges,cycleedges];
  Do[
    allcycles=Append[allcycles,{cyclenodes[[i]],cycleedges[[i]]}];
  ,{i,1,Length[cyclenodes]}];
,{e,1,Length[edges]}];

(*Remove duplicates*)
ncycles=Length[allcycles];
k=0;
Do[
  dupl={};
  If[i<=Length[allcycles],
    Do[
	  If[i<j&&UnsrtMatchQ[allcycles[[All,1,1;;-2]][[i]],allcycles[[All,1,1;;-2]][[j]]]&&\
      UnsrtMatchQ[allcycles[[All,2]][[i]],allcycles[[All,2]][[j]]],
	    dupl=Append[dupl,{j}];
	  ];
    ,{j,i,Length[allcycles]}];
	allcycles=Delete[allcycles,dupl];
    ,
    Break[]
  ];
,{i,1,ncycles}];
allcycles
];
FindSpanningTree[bstart_,g_,allcycles_]:=Module[{b,n,IM,len,boffset,tree,ntree,containsc,e1,e2,v,z,secondn,firstn,debug=False},
b =EdgeList[g];(*list of node-pairs*)
n =VertexList[g];
IM=Normal[IncidenceMatrix[g]];
len=Length[b]-Length[n]+1;
boffset=Table[If[i+bstart-1==Length[b],Length[b],Mod[i+bstart-1,Length[b]]],{i,1,Length[b]}];(*list of indices*)
tree={};
ntree={};
Do[
  containsc=False;
  If[Length[tree]==0,
    tree=Append[tree,bi];
	ntree=Append[ntree,b[[bi]]];,
    If[Length[tree]<Length[b]-len,
	If[debug,Print["branch=",bi]];
      Do[
		If[UnsrtMatchQ[allcycles[[All,2]][[i]],Append[tree,bi]]||UnsrtSubsetQ[Append[tree,bi],allcycles[[All,2]][[i]]],
          If[debug,
		    Print["appended tree=",Append[tree,bi]];
		    Print["cycle=",allcycles[[All,2]][[i]]]
		  ];
		  containsc=True;
		  Break[]
		];
      ,{i,1,Length[allcycles]}];
      If[!containsc,
		tree=Append[tree,bi];
		ntree=Append[ntree,b[[bi]]];
	  ];
    ];
  ];
,{bi,boffset}];
If[Length[tree]==Length[b]-len,
  If[debug,Print["Found a spanning tree with the edges: tree=",tree]];
  Return[{ntree,tree}]
];
];
CospanningTree[stree_,b_]:=Module[{costree,bi},
costree={};
bi=Table[i,{i,1,Length[b]}];
Do[
  If[FreeQ[stree,i],
    costree=Append[costree,i];
  ];
,{i,bi}];
costree
];
FundamentalCycles[stree_,costree_,g_]:=Module[{allcycles,b,n,nKirchhoffV,fcycles,nc},
allcycles=FindAllCycles[g];
b=EdgeList[g];
n=VertexList[g];
nKirchhoffV=Length[b]-Length[n]+1;
nc=0;
fcycles={};
Do[
  Do[
  (*If[nc>=nKirchhoffV,
    Break[],*)
    If[UnsrtSubsetQ[Append[stree,costree[[j]]],allcycles[[All,2]][[i]]]&&Length[fcycles]<=nKirchhoffV,
      nc=nc+1;
      fcycles=Append[fcycles,{allcycles[[All,1]][[i]],allcycles[[All,2]][[i]]}];
    ];
  (*];*)
  ,{j,1,Length[costree]}];
,{i,1,Length[allcycles]}];
fcycles
];
ClassifyNodes[b_,btype_,n_,IM_]:=Module[{gndnodes,anodes,pnodes},
Clear[gndnodes,anodes,pnodes];
gndnodes={}; anodes={}; pnodes={};
Do[
  If[!StringFreeQ[n[[i]],"gnd"],
    gndnodes=Append[gndnodes,i],
    Do[
    If[IM[[i,j]]!=0&&StringFreeQ[btype[[j]],"C"]&&FreeQ[anodes,i],
      anodes=Append[anodes,i]
    ];
    ,{j,1,Length[b]}];    
    If[FreeQ[gndnodes,i]&&FreeQ[anodes,i],
      pnodes=Append[pnodes,i];
    ];
  ];
,{i,1,Length[n]}];
{gndnodes,anodes,pnodes}
];
ReduceBranchVariables[Phi_,rPhi_,IM_,stree_,g_,allcycles_]:=Module[{b,n,costree,fcycles,nKirchhoffV,Vequations,cn,ce,sum,sols},
b=EdgeList[g];
n=VertexList[g];
nKirchhoffV=Length[b]-Length[n]+1;
costree=CospanningTree[stree,b];
fcycles=FundamentalCycles[stree,costree,g];
Vequations={};
Do[
  cn=fcycles[[All,1]][[i]];
  ce=fcycles[[All,2]][[i]];
  sum=0;
  Do[
    sum=sum+Phi[[j]]*OrientEdge[IM,cn,ce,j];
  ,{j,ce}];
  Vequations=Append[Vequations,{sum==0}];
,{i,1,Length[fcycles]}];
(*Solve the system of equation for the dynamical degrees of freedom*)
sols=Solve[Flatten[Vequations],rPhi];
Return[sols]
];
SolveNodeVariables[Phi_,dPhi_,phi_,IM_]:=Module[{Iequations,sum,nlist,o,sols},
Iequations={};
Do[
  If[Length[Position[dPhi,Phi[[bi]]]]>0,
    sum=0;
    (*Find the nodes that are connected to this branchs*)
	nlist=Flatten[{Position[IM[[All,bi]],-1],Position[IM[[All,bi]],+1]}];
	Do[
	  If[IM[[nlist[[ni]],bi]]==-1,
		o=-1,
		o=1;
	  ];
	  sum=sum+phi[[nlist[[ni]]]]*o;
	,{ni,1,Length[nlist]}];
	Iequations=Append[Iequations,{sum==Phi[[bi]]}];
  ];
,{bi,1,Length[Phi]}];
sols=Solve[Flatten[Iequations],dPhi];
sols
];
CapacitiveEnergy[Phi_,bC_]:=bC*Phi'*Phi'/2;
End[];
EndPackage[];
