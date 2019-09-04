(* ::Package:: *)

(* ::Title:: *)
(*SpinorHelicity6D*)


BeginPackage["SpinorHelicity6D`"]


(* ::Section:: *)
(*Messages*)


$Shortcuts::usage="This is a list comprising all the available shortcuts and their output"


MinusSignQ::usage="Tells you if its argument is negative"


overbar::usage="Auxiliary function for OverBar."


SpinorDot::usage="This is the generic tilde 4d spinor. It serves both for the \[Lambda] and the \[Mu], it takes four arguents SpinorDot[momlabel][type][upper][lower].
The first labels the momentum, the second is either $lam or $mu and distinguishes between \[Lambda] and \[Mu], the firsd is for the upper index and the fourth is for the lower index. If no index
of a certain kind is present the argument to be given is Null. Do not worry, you are not supposed to use this functions. The shortcuts directly give you what you are seeking."


SpinorUndot::usage="This is the generic 4d spinor. It serves both for the \[Lambda] and the \[Mu], it takes four arguents SpinorUndot[momlabel][type][upper][lower].
The first labels the momentum, the second is either $lam or $mu and distinguishes between \[Lambda] and \[Mu], the firsd is for the upper index and the fourth is for the lower index. If no index
of a certain kind is present the argument to be given is Null. Do not worry, you are not supposed to use this functions. The shortcuts directly give you what you are seeking."


Spinorundot::usage="Auxiliary function for SpinorUndot"


$lam::usage="Protected label for the \[Lambda] spinors"


$mu::usage="Protected label for the \[Mu] spinors"


Spinordot::usage="Auxiliary function for SpinorDot"


SpinorDot6D::usage="SpinorDot6D[momlabel][Loretzindex][Null][lgindex] is the 6D spinor with momentum momlabel, Lorentz index Lorentzindex and the downstairs little group index lgindex"


SpinorUndot6D::usage="SpinorUndot6D[momlabel][Loretzindex][Null][lgindex] is the 6D tilde spinor with momentum momlabel, Lorentz index Lorentzindex and the downstairs little group index lgindex"


SpinorUndotPure::usage="The equivaqlent of SpinorUndot but without index structure"
SpinorDotPure::usage="The equivalent of SpinorDot but without index structure"


extramass::usage="Mass given by the extra dimensional part of the momenta."
extramasstilde::usage="auxiliary function encorporating all the properties of the mass \[CapitalMu] tilde"
extramasss::usage="auxiliary function for extramass"
extramassstilde::usage="auxiliary function for extramasstilde"


KillMasses::usage="KillMasses[{a,b,c,...}] sets \[CapitalMu][a],\[CapitalMu][b],\[CapitalMu][c] and so on to zero (as well as \[CapitalMu] tilde)"
Momenta4D::usage="This a variable which tells you which momenta have been specified to be 4 dimensional. It takes no argument of course."
NewProcess::usage="NewProcess deletes all the downvalues of any function fixed during the computation of a given process. For example masses of particles that were previously set to zero (being 4D) are restore using this function. Also conservation relations stored in AllIdentities6D are cleared."
ClearDownValues::usage="ClearDownValues[f] clears the downvalues of the function f given as an argument"


SquAngInvariant::usage="SquAngInvariant[n,m][adot,b] is the invariant [n_adot m_b > with adot and b little group indices and n,m labels of the particles. The associated shortcut is esc + sai + esc"
AngSquInvariant::usage="AngSquInvariant[n,m][a,bdot] is the invariant < n_a m_bdot ] with a and bdot littlegroup indices and n,m particle labels. The associated shortcut is esc + asi + esc"
AngAngInvariant::usage="AngAngInvariant[m,n,l,k][a,b,c,d] is the invariant < m_a n_b l_c k_d > where m,n,l,k are particle labels and a,b,c,d are little group indices. The shotcut is esc + aa + esc"
SquSquInvariant::usage="BraBrainvariant[m,n,l,k][a,b,c,d] is the invariant [ m_a n_b l_c k_d ] where m,n,l,k are particle labels and a,b,c,d are the little group indices"


SpinorAngleBracket::usage = "SpinorAngleBracket[\*StyleBox[\"x\", \"TI\"], \*StyleBox[\"y\", \"TI\"]] represents the contraction of undotted spinors \!\(\*SubscriptBox[\(\[Lambda]\), \(x\)]\) and \!\(\*SubscriptBox[\(\[Lambda]\), \(y\)]\)."
SpinorSquareBracket::usage = "SpinorAngleBracket[\*StyleBox[\"x\", \"TI\"], \*StyleBox[\"y\", \"TI\"]] represents the contraction of undotted spinors" (*SubscriptBox[OverscriptBox[\"\[Lambda]\", \"~\"], #]*)
Spinoranglebracket::usage="Auxiliary function for SpinorAngleBracket"
Spinorsquarebracket::usage="Auxiliary function for SpinorSquareBracket"


levicivita2up::usage="levicivita2[a,b] is the element (a,b) (with a,b integers) of the two dimensional Levi-Civita tensor with upper indices and following the conventions in 1010.0494"
levicivita2down::usage="levicivita2[a,b] is the element (a,b) (with a,b integers) of the two dimensional Levi-Civita tensor with lower indices and following the conventions in 1010.0494"
levicivita2Up::usage="This is an auxiliary function needed in order to define the good contractions properties for the Levi Civita tensor with the spinors. The function levicivita2up[a,b] assumes this value when a and b are not both integers."
levicivita2Down::usage="This is an auxiliary function needed in order to define the good contractions properties for the Levi Civita tensor with the spinors. The function levicivita2down[a,b] assumes this value when a and b are not both integers."


$contractvariable::usage="Internal variable used in some contractions."
$contractvariable1::usage="Internal variable used in some contractions."
$contractvariable2::usage="Internal variable used in some contractions."
$contractvariable3::usage="Internal variable used in some contractions."
$contractvariable4::usage="Internal variable used in some contractions."


Momenta::usage = "Momenta[\*StyleBox[\"expr\", \"TI\"]] extracts the name of the momenta appearing in brackes in \*StyleBox[\"expr\", \"TI\"]."
AllMomenta::usage="Same as Momenta but distinguishes between the \[Lambda] from the \[Mu]"


SpinorReplace::usage="SpinorReplace[exp,subs] applies the replacements subs in exp. Notice that subs must be given in terms of pure spinors without index structure"
SpinorReplaceSequential::usage="SpinorReplaceSequential[exp,subs] works like SpinorReplace but does the replacements sequentially, one after another in the given order."
SubCounter::usage="Auxiliary function used inside SpinorReplace"
$crep::usage="Auxiliary global variable needed in SubCounter"


ConvenientMu::usage="ConvenientMu[exp] return a convenient substitution of the first encountered \[Mu] spinor in terms of other spinors, in order to kill as much terms in exp as possible."


MuReplace::usage="MuReplace[exp] replaces the \[Mu] and \[Mu] tilde in exp with the most convenient ones in order to kill as much terms in exp as possible. It admits the option DisplayReplacements->True to show the replacements."
DisplayReplacements::usage="DisplayReplacements is an option for MuReplace. If set to True the replacements chosen by MuReplace are shown along with the result. Default is False."
AllowMutoMu::usage="AllowMutoMu is an option for MuReplace. If set to True replecements of \[Mu] with another \[Mu] become allowed. Default is False."
GlobalReplacements::usage="GlobalReplacements is an option for MuReplace. If set to True it makes the replecemnts globally defined, and stores the in FixedSpinors. Default is False."


FixedSpinors::usage="FixedSpinors is a global variable in which all the arbitrary constraints imposed on the spinors are stored."
ClearSpinors::usage="ClearSpinors[] clears all the arbitrary constraints enforced on the spinors."
FixSpinors::usage="FixSpinors[{\[Mu][a]->\[Lambda][b]}] fixes \[Mu][a] to be equal to \[Lambda][b]. FixSpinors needs a list of pure spinor replacements as input. In order to clear the given definitions use ClearSpinors."


SchoutenSimplify::usage = "SchoutenSimplify[\*StyleBox[\"expr\", \"TI\"]] simplifies the expression \*StyleBox[\"expr\", \"TI\"] by using Schouten identities."


Mom4D::usage="Mom4D[label][type][undotted][dotted] produces expression of the 4D massive momentum label with indices undotted and dotted, which are upper or lower indices based on type, which should be either $up or $down"
$up::usage="Variable needed to specify upper or lower indices"
$down::usage="Variable needed to specify upper or lower indices"


Infycheck::usage="Infycheck[exp] returns True if the expression is divergent and False otherwise."


S6::usage="S6[p1,p2] is the six dimensional mandelstam invariant s=(p1+p2)^2 expressed in terms of the foru dimensional pieces"
S6many::usage="S6many[p1,p2,...,pn] is teh six dimensional mandelstam invariant s=(p1+p2+...+pn)^2 in terms of the 4d objects"


CompleteDenominators::usage="CompleteDenominators[exp] returns exp over a single denominator where spinor products have been completed to Mandelstam invariants."
CompleteMandelstam::usage="CompleteMandelstam[exp] returns exp where as many spinor products as possible have been replaced with the corresponding four-dimensional Mandelstam invariants. CompleteMandelstam does only gather existing spinor products without adding new terms."
ToChain::usage="ToChain[exp] returns exp where angle and square brackets are gathered together in chains of invariants."
Chain::usage="Chain[type1,first,momList,last,type2] is the invariant obtained chaining together angle and square brackest. Type1 and type2 assume values $angle or $bracket and represent the type of bracket with which the invariants start or end. First is the first momentum in the invariant, last is the last and momList are all the intermediate ones."
chain::usage="chain is an auxiliary function for Chain. It has all the contraction properties one would expect from Chain."
S::usage="Global variable used to Label six-dimensional Mandelstam invariants."
S4::usage="Global variable used to Label four-dimensional Mandelstam invariants."


$angle::usage="Global variable of SpinorHelicity6D, labels angle brackets."
$square::usage="Global variable of SpinorHelicity6D, labels square brackets."


mp::usage="mp[p_a,p_b] is the scalar product fo momenta p_a and p_b. mp[] has the attribute Orderless."
eps::usage="eps[p_a,p_b,p_c,p_d] is the Levi-Civita tensor contracted with the four momenta pa_,p_b,p_c and p_d."
TrG::usage="TrG[mom_List] is the trace over the list of slashed momenta mom."
TrG5::usage="TrG5[mom_List] is the trace over the slashed momenta of the list mom but has also a Gamma 5 matrix inserted in the first position of the trace."
ToTrace::usage="ToTrace[exp] converts all closed chains in exp into traces and evaluates them. It admits the Option KillEpsilon whose default is False. If KillEpsilon is set to True the terms in the traces containing an epsilon tensor are set to zero."
ScalProdToS::usage="ScalProdToS[exp] convert all the four-dimensional scalar products in exp to six dimensional mandelstam invariants S taking into account possible masses."


Relabel::usage="Relabel[exp_,{LabelRep->{lab1->lab2,...},ScalarProduct->funtion}] allows to perform relabelings of the momenta inside the functions S[], S4[] and mp[]. These relabelings are specified by the optional argument LabelRep. Furthermore it allows to replace the definition of the scalar product given in SpinorHelicity6D with any given function (also optional feature). This is intended to be used when interfacing the results of SpinorHelicity6D with other Mathematica package. For example by setting ScalarProduct->sp will allow direct input of the results into LiteRed for integral reductions."
LabelRep::usage="Option for the function Relabel. LabelRep specifies the label and functional replacements to be performed in a given expression. For example LabelRep->{1->p1,2->p2,S[1,2]->s12,...}."
ScalarProduct::usage="Option for the function Relabel which specifies if the scalar product has to be redefined to a new function. For example ScalarProduct->sp sets the scalar product to be the function sp (used in LiteRed for example)."


SumContracted::usage="SumContracted[index__][exp_] performs the sum over the contracted little group indices index in the expression exp."
CompleteToMassive::usage="CompleteToMassive[exp_,reps_] restores the \[Mu] dependence in exp after this has been removed by choosing specific values for them. It uses the replacements reps to reinsert the \[Mu] backwards. The replacemnt argument is optional, if no replacement is given the lements of FixedSpinors are used by default."


MDelta::usage="MDelta[dim][up1,up2][down1,down2] is the Kronecker delta in dimension dim with upper indices up1 and up2 and lower indices down1 down2. Not present indices are replaced by Null"


MDeltaBox::usage="Just the boxing function for MDelta"
$MDimension::usage="Global variable setting the dimension of MDelta. Default is 4."


Antisymmetrize::usage="Antisymmetrize[exp,{A1,...,An},{B1,...,Bm},...] returns exp antisymmetrized on the indices in the index lists. Be careful, the indices in the list need to be in the same order as in exp for all the signs to be correct in the output."


Fstrength::usage="Fstrength[momlabel][A,B][C,D][lg,lgdot] is the field strength of mmentum momlabel, with A,B Lorentz indices transforming in the fundamental representation and C,D in th eantifundamental, and lg,lgdot little group indices."
FstrengthBox::usage="FstrengthBox is just the boxing function for Fstrength."


ContractReplace::usage="ContractReplace[exp] performs simplifications in exp by recognizing terms which are contracted with an \[Epsilon] tensor and differ by a simple relabelling of indices. Pay attention, it works only on <1_a,2_b] invariants and could be rather inefficient in terms of time...Will hopefully be inproved in later versions of the package."


Fst::usage="Fst[mom][mu,nu][Null,Null] is the field strength tensor with standard Lorentz Indices mu nu and momentum label mom"
Eta::usage="Eta[mu,nu][$up/$down] is the flat metric tensor with upper/lower indices mu nu"
Polar::usage="Polar[mom,refmom][mu][Null] is the polarization vector for the particle with momentum mom and reference momentum refmom, with upper index mu"
PolarPure::usage="PolarPure[mom,refmom] is the polarization vector for the particle with momentum mom and reference momentum refmom with Lorentz indices stripped off"
Mom::usage="Mom[mom][mu][Null] represents the momentum for the particle with momentum mom and upper Lorentz index mu"
MomPure::usage="MomPure[mom] represents the momentum of the particle with momentum mom with Lorentz index stripped off"


ToFile::usage="ToFile[exp,filename] writes to the file filename.txt located in the documents directory all the frontend oprinted output appearing in exp, just returning the output of exp."


SpinorPalette::usage="Opens the palette associated to the package SpinorHelicity6D."


(* ::Section:: *)
(*Private: 6D spinor helicity definitions*)


Begin["`Private`"]


(*We define a private variable needed for the package to decide whether to run shortcuts and the palette or not. This is related to the availability of a frontend.*)


frontend=If[TrueQ[$FrontEnd==Null],0,1];


(* ::Subsection:: *)
(*Infycheck*)


Attributes[Infycheck]={HoldAll};
Infycheck[x_]:=TrueQ[Quiet[Check[x,$Failed,{PowerMod::ninv,Power::infy,Infinity::indet,Power::indet}],{PowerMod::ninv,Power::infy,Infinity::indet,Power::indet}]==$Failed];


(* ::Subsection:: *)
(*overbar*)


overbar[Times[-1,x_]]:=-1*OverBar[x];
overbar[x_Integer]:=If[x>=0,OverBar[x],-OverBar[-x]];
overbar[n_]:=OverBar[n];


(* ::Subsection:: *)
(*MinuSignQ*)


(*MinusSignQ[x_]:=If[StringContainsQ[ToString[x],"-"],True,False];*)
MinusSignQ[-x_]:=True;
MinusSignQ[x_?Negative]:=True;
MinusSignQ[x_]:=False


(* ::Subsection:: *)
(*Extra mass*)


extramasssBox[x_]:=TemplateBox[{x},"extramass",
DisplayFunction->(RowBox[{"\[CapitalMu]","[",#,"]"}]&),
InterpretationFunction->(RowBox[{"extramasss[",#,"]"}]&)
];

extramass[x_]:=Module[{input},
input=x/.OverBar->overbar;
If[MinusSignQ[input],Return[-extramasss[-input]],Return[extramasss[input]]];
];

extramasss /: MakeBoxes[extramasss[x_],StandardForm|TraditionalForm]:=extramasssBox[ToBoxes[x]];


(* ::Subsection:: *)
(*Extra Mass tilde*)


extramassstildeBox[x_]:=TemplateBox[{x},"extramasstilde",
DisplayFunction->(RowBox[{OverscriptBox["\[CapitalMu]","~"],"[",#,"]"}]&),
InterpretationFunction->(RowBox[{"extramassstilde[",#,"]"}]&)
];

extramasstilde[x_]:=Module[{input},
input=x/.OverBar->overbar;
If[MinusSignQ[input],Return[-extramassstilde[-input]],Return[extramassstilde[input]]];
];

extramassstilde /: MakeBoxes[extramassstilde[x_],StandardForm|TraditionalForm]:=extramassstildeBox[ToBoxes[x]];


(* ::Subsection:: *)
(*KillMasses*)


KillMasses[x_List]:=(
Unprotect[extramasss,extramassstilde,Momenta4D];
Do[extramasss[i]=0;extramassstilde[i]=0,{i,x}];
Momenta4D=x;
Protect[extramasss,extramassstilde,Momenta4D];
);


(* ::Subsection:: *)
(*Momenta4D*)


Momenta4D={};


(* ::Subsection:: *)
(*ClearDownValues*)


ClearDownValues[f_]:=DownValues[f]=DeleteCases[DownValues[f],_?(FreeQ[First[#],Pattern]&)];


(* ::Subsection:: *)
(*NewProcess*)


NewProcess:=(Unprotect[extramasss,extramassstilde,Momenta4D];ClearDownValues[extramasss];ClearDownValues[extramassstilde];Momenta4D={};Protect[extramasss,extramassstilde,Momenta4D];);


(* ::Subsection:: *)
(*SpinorUndot*)


(*Define all the boxing functions for the different cases, \[Lambda], \[Mu] with both up and down indices*)
SpinorLaUpBox[mom_,index_]:=TemplateBox[{mom,index},"SpinorLaUp",
DisplayFunction->(RowBox[{SuperscriptBox["\[Lambda]",#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Spinorundot","[",#1,"]","[","$lam","]","[",#2,"]","[","Null","]"}]&)
];
SpinorLaDownBox[mom_,index_]:=TemplateBox[{mom,index},"SpinorLaDown",
DisplayFunction->(RowBox[{SubscriptBox["\[Lambda]",#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Spinorundot","[",#1,"]","[","$lam","]","[","Null","]","[",#2,"]"}]&)
];
SpinorMuUpBox[mom_,index_]:=TemplateBox[{mom,index},"SpinorMuUp",
DisplayFunction->(RowBox[{SuperscriptBox["\[Mu]",#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Spinorundot","[",#1,"]","[","$mu","]","[",#2,"]","[","Null","]"}]&)
];
SpinorMuDownBox[mom_,index_]:=TemplateBox[{mom,index},"SpinorMuDown",
DisplayFunction->(RowBox[{SubscriptBox["\[Mu]",#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Spinorundot","[",#1,"]","[","$mu","]","[","Null","]","[",#2,"]"}]&)
];


(*Define the property of Spinorundot to get an i in front for negative arguments of the momentum label. Pay attention to the difference between SpinorUndot and Spinorundot!*)
Spinorundot[momlabel_][type_][upper_][lower_]:=If[MinusSignQ[momlabel],I*SpinorUndot[-momlabel][type][upper][lower],SpinorUndot[momlabel][type][upper][lower]];


(*Define the action of Makeboxes on our functions*)
SpinorUndot /: MakeBoxes[SpinorUndot[mom_][$lam][upper_][Null],StandardForm|TraditionalForm]:=SpinorLaUpBox[ToBoxes[mom],ToBoxes[upper]];
SpinorUndot /: MakeBoxes[SpinorUndot[mom_][$mu][upper_][Null],StandardForm|TraditionalForm]:=SpinorMuUpBox[ToBoxes[mom],ToBoxes[upper]];
SpinorUndot /: MakeBoxes[SpinorUndot[mom_][$lam][Null][lower_],StandardForm|TraditionalForm]:=SpinorLaDownBox[ToBoxes[mom],ToBoxes[lower]];
SpinorUndot /: MakeBoxes[SpinorUndot[mom_][$mu][Null][lower_],StandardForm|TraditionalForm]:=SpinorMuDownBox[ToBoxes[mom],ToBoxes[lower]];


(*Define all the shortcuts*)
If[frontend==1,
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "lu" -> SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ld" -> SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "muu" -> SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mud" -> SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
];


(*Finally define the contraction properties of the Undotted spinors among themselves when repeated indices are encoutered:*)
SpinorUndot /: Times[SpinorUndot[a_][$mu][d_][Null],SpinorUndot[b_][$mu][Null][d_]]:=SpinorAngleBracket[OverBar[a],OverBar[b]];
SpinorUndot /: Times[SpinorUndot[a_][$lam][d_][Null],SpinorUndot[b_][$mu][Null][d_]]:=SpinorAngleBracket[a,OverBar[b]];
SpinorUndot /: Times[SpinorUndot[a_][$mu][d_][Null],SpinorUndot[b_][$lam][Null][d_]]:=SpinorAngleBracket[OverBar[a],b];
SpinorUndot /: Times[SpinorUndot[a_][$lam][d_][Null],SpinorUndot[b_][$lam][Null][d_]]:=SpinorAngleBracket[a,b];
SpinorUndot /: Times[levicivita2Down[a_,b_],SpinorUndot[mom_][type_][b_][Null]]:=SpinorUndot[mom][type][Null][a];
SpinorUndot /: Times[levicivita2Down[a_,b_],SpinorUndot[mom_][type_][a_][Null]]:=-SpinorUndot[mom][type][Null][b];
SpinorUndot /: Times[levicivita2Up[a_,b_],SpinorUndot[mom_][type_][Null][b_]]:=SpinorUndot[mom][type][a][Null];
SpinorUndot /: Times[levicivita2Up[a_,b_],SpinorUndot[mom_][type_][Null][a_]]:=-SpinorUndot[mom][type][b][Null];


(* ::Subsection:: *)
(*SpinorUndotPure*)


SpinorUndotPureLBox[label_]:=TemplateBox[{label},"SpinorUndotPureL",
DisplayFunction->(RowBox[{"\[Lambda]","[",#,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorUndotPure","[",#,"]","[","$lam","]"}]&)
];
SpinorUndotPureMBox[label_]:=TemplateBox[{label},"SpinorUndotPureM",
DisplayFunction->(RowBox[{"\[Mu]","[",#,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorUndotPure","[",#,"]","[","$mu","]"}]&)
];

SpinorUndotPure /: MakeBoxes[SpinorUndotPure[label_][$lam],TraditionalForm|StandardForm]:=SpinorUndotPureLBox[ToBoxes[label]];
SpinorUndotPure /: MakeBoxes[SpinorUndotPure[label_][$mu],TraditionalForm|StandardForm]:=SpinorUndotPureMBox[ToBoxes[label]];

If[frontend==1,
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "lp" -> SpinorUndotPureLBox["\[SelectionPlaceholder]"]]];
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mp" -> SpinorUndotPureMBox["\[SelectionPlaceholder]"]]];
];


(* ::Subsection:: *)
(*SpinorUndot6D*)


SpinorUndot6D[momlabel_][index_][Null][1]:={-extramass[momlabel]/Spinoranglebracket[momlabel,overbar[momlabel]]*Spinorundot[momlabel][$mu][Null][index],Spinordot[momlabel][$lam][index][Null]};
SpinorUndot6D[momlabel_][index_][Null][2]:={Spinorundot[momlabel][$lam][Null][index],-extramasstilde[momlabel]/Spinorsquarebracket[momlabel,overbar[momlabel]]*Spinordot[momlabel][$mu][index][Null]};

(*Define boxing functions for the SpinorUndot6D*)

SpinorUndot6DBox[momlabel_,index_,lgindex_]:=TemplateBox[{momlabel,index,lgindex},"SpinorUndot6D",
DisplayFunction->(RowBox[{SubscriptBox[SuperscriptBox["\[CapitalLambda]",#2],#3],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorUndot6D","[",#1,"]","[",#2,"]","[","Null","]","[",#3,"]"}]&)
];

SpinorUndot6DBox2[mom_,A_,bdot_]:=TemplateBox[{mom,A,bdot},"SpinorUndot6D2",
DisplayFunction->(RowBox[{SuperscriptBox["\[CapitalLambda]",RowBox[{#2,#3}]],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorUndot6D","[",#1,"]","[",#2,"]","[",#3,"]","[","Null","]"}]&)
];

(*Define the action of MakeBoxes on SpinorUndot6D*)

SpinorUndot6D /: MakeBoxes[SpinorUndot6D[mom_][index_][Null][lgindex_],StandardForm|TraditionalForm]:=SpinorUndot6DBox[ToBoxes[mom],ToBoxes[index],ToBoxes[lgindex]];
SpinorUndot6D /: MakeBoxes[SpinorUndot6D[mom_][index_][lgindex_][Null],StandardForm|TraditionalForm]:=SpinorUndot6DBox2[ToBoxes[mom],ToBoxes[index],ToBoxes[lgindex]];

(*Define shortcut*)

If[frontend==1,
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Ld" -> SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
];
(*Define contraction properties with the epsilon tensor*)

(*SpinorUndot6D /: Times[levicivita2Up[a_,b_],SpinorUndot6D[mom_][A_][Null][b_]]:=SpinorUndot6D[mom][A][a][Null];
SpinorUndot6D /: Times[levicivita2Up[b_,a_],SpinorUndot6D[mom_][A_][Null][b_]]:=-SpinorUndot6D[mom][A][a][Null];*)


(* ::Subsection:: *)
(*SpinorDot*)


(*For an explaination of the following functions see the section SpinorUndot, these work just in the same way*)
SpinorLatUpBox[mom_,index_]:=TemplateBox[{mom,index},"SpinorLatUp",
DisplayFunction->(RowBox[{SuperscriptBox[OverscriptBox["\[Lambda]","~"],#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Spinordot","[",#1,"]","[","$lam","]","[",#2,"]","[","Null","]"}]&)
];
SpinorLatDownBox[mom_,index_]:=TemplateBox[{mom,index},"SpinorLatDown",
DisplayFunction->(RowBox[{SubscriptBox[OverscriptBox["\[Lambda]","~"],#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Spinordot","[",#1,"]","[","$lam","]","[","Null","]","[",#2,"]"}]&)
];
SpinorMutUpBox[mom_,index_]:=TemplateBox[{mom,index},"SpinorMutUp",
DisplayFunction->(RowBox[{SuperscriptBox[OverscriptBox["\[Mu]","~"],#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Spinordot","[",#1,"]","[","$mu","]","[",#2,"]","[","Null","]"}]&)
];
SpinorMutDownBox[mom_,index_]:=TemplateBox[{mom,index},"SpinorMutDown",
DisplayFunction->(RowBox[{SubscriptBox[OverscriptBox["\[Mu]","~"],#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Spinordot","[",#1,"]","[","$mu","]","[","Null","]","[",#2,"]"}]&)
];


Spinordot[momlabel_][type_][upper_][lower_]:=If[MinusSignQ[momlabel],I*SpinorDot[-momlabel][type][upper][lower],SpinorDot[momlabel][type][upper][lower]];


SpinorDot /: MakeBoxes[SpinorDot[mom_][$lam][upper_][Null],StandardForm|TraditionalForm]:=SpinorLatUpBox[ToBoxes[mom],ToBoxes[upper]];
SpinorDot /: MakeBoxes[SpinorDot[mom_][$mu][upper_][Null],StandardForm|TraditionalForm]:=SpinorMutUpBox[ToBoxes[mom],ToBoxes[upper]];
SpinorDot /: MakeBoxes[SpinorDot[mom_][$lam][Null][lower_],StandardForm|TraditionalForm]:=SpinorLatDownBox[ToBoxes[mom],ToBoxes[lower]];
SpinorDot /: MakeBoxes[SpinorDot[mom_][$mu][Null][lower_],StandardForm|TraditionalForm]:=SpinorMutDownBox[ToBoxes[mom],ToBoxes[lower]];


If[frontend==1,
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ltu" -> SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ltd" -> SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mtu" -> SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mtd" -> SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
];


(*Finally define the contraction properties of the Undotted spinors among themselves when repeated indices are encoutered:*)
SpinorDot /: Times[SpinorDot[a_][$mu][d_][Null],SpinorDot[b_][$mu][Null][d_]]:=SpinorSquareBracket[OverBar[b],OverBar[a]];
SpinorDot /: Times[SpinorDot[a_][$lam][d_][Null],SpinorDot[b_][$mu][Null][d_]]:=SpinorSquareBracket[OverBar[b],a];
SpinorDot /: Times[SpinorDot[a_][$mu][d_][Null],SpinorDot[b_][$lam][Null][d_]]:=SpinorSquareBracket[b,OverBar[a]];
SpinorDot /: Times[SpinorDot[a_][$lam][d_][Null],SpinorDot[b_][$lam][Null][d_]]:=SpinorSquareBracket[b,a];
SpinorDot /: Times[levicivita2Down[a_,b_],SpinorDot[mom_][type_][b_][Null]]:=SpinorDot[mom][type][Null][a];
SpinorDot /: Times[levicivita2Down[a_,b_],SpinorDot[mom_][type_][a_][Null]]:=-SpinorDot[mom][type][Null][b];
SpinorDot /: Times[levicivita2Up[a_,b_],SpinorDot[mom_][type_][Null][b_]]:=SpinorDot[mom][type][a][Null];
SpinorDot /: Times[levicivita2Up[a_,b_],SpinorDot[mom_][type_][Null][a_]]:=-SpinorDot[mom][type][b][Null];


(* ::Subsection:: *)
(*SpinorDotPure*)


SpinorDotPureLBox[label_]:=TemplateBox[{label},"SpinorDotPureL",
DisplayFunction->(RowBox[{OverscriptBox["\[Lambda]","~"],"[",#,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorDotPure","[",#,"]","[","$lam","]"}]&)
];
SpinorDotPureMBox[label_]:=TemplateBox[{label},"SpinorDotPureM",
DisplayFunction->(RowBox[{OverscriptBox["\[Mu]","~"],"[",#,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorDotPure","[",#,"]","[","$mu","]"}]&)
];

SpinorDotPure /: MakeBoxes[SpinorDotPure[label_][$lam],TraditionalForm|StandardForm]:=SpinorDotPureLBox[ToBoxes[label]];
SpinorDotPure /: MakeBoxes[SpinorDotPure[label_][$mu],TraditionalForm|StandardForm]:=SpinorDotPureMBox[ToBoxes[label]];

If[frontend==1,
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ltp" -> SpinorDotPureLBox["\[SelectionPlaceholder]"]]];
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "mtp" -> SpinorDotPureMBox["\[SelectionPlaceholder]"]]];
];


(* ::Subsection:: *)
(*SpinorDot6D*)


SpinorDot6D[momlabel_][index_][Null][1]:={extramasstilde[momlabel]/Spinoranglebracket[momlabel,overbar[momlabel]]*Spinorundot[momlabel][$mu][index][Null],-Spinordot[momlabel][$lam][Null][index]};
SpinorDot6D[momlabel_][index_][Null][2]:={Spinorundot[momlabel][$lam][index][Null],-extramass[momlabel]/Spinorsquarebracket[momlabel,overbar[momlabel]]*Spinordot[momlabel][$mu][Null][index]};

(*Define boxing functions*)

SpinorDot6DBox[momlabel_,index_,lgindex_]:=TemplateBox[{momlabel,index,lgindex},"SpinorDot6D",
DisplayFunction->(RowBox[{SubscriptBox[SubscriptBox[OverscriptBox["\[CapitalLambda]","~"],#2],#3],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorDot6D","[",#1,"][",#2,"]","[","Null","]","[",#3,"]"}]&)
];

SpinorDot6DBox2[momlabel_,index_,lgindex_]:=TemplateBox[{momlabel,index,lgindex},"SpinorDot6D2",
DisplayFunction->(RowBox[{SuperscriptBox[SubscriptBox[OverscriptBox["\[CapitalLambda]","~"],#2],#3],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorDot6D","[",#1,"][",#2,"][",#3,"]","[","Null","]"}]&)
];

(*Define action of MakeBoxes*)

SpinorDot6D /: MakeBoxes[SpinorDot6D[mom_][index_][Null][lgindex_],StandardForm|TraditionalForm]:=SpinorDot6DBox[ToBoxes[mom],ToBoxes[index],ToBoxes[lgindex]];
SpinorDot6D /: MakeBoxes[SpinorDot6D[mom_][index_][lgindex_][Null],StandardForm|TraditionalForm]:=SpinorDot6DBox2[ToBoxes[mom],ToBoxes[index],ToBoxes[lgindex]];

(*Define Shortcut*)

If[frontend==1,
SetOptions[EvaluationNotebook[],InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "Ltd" -> SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
];

(*Define contraction with SPinorUndot6D and epsilon tensor*)

SpinorDot6D /: Times[SpinorDot6D[label1_][L_][Null][lg1_],SpinorUndot6D[label2_][L_][Null][lg2_]]:=AngSquInvariant[label2,label1][lg2,lg1];

(*SpinorDot6D /: Times[levicivita2Up[adot_,bdot_],SpinorDot6D[mom_][A_][Null][adot_]]:=-SpinorDot6D[mom][A][bdot][Null];
SpinorDot6D /: Times[levicivita2Up[bdot_,adot_],SpinorDot6D[mom_][A_][Null][adot_]]:=SpinorDot6D[mom][A][bdot][Null];*)


(* ::Subsection:: *)
(*Levi-Civita Tensor up*)


(*levicivita2upBox[a_,b_]:=TemplateBox[{a,b},"levicivita2up",
DisplayFunction->(SuperscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"levicivita2up","[",#1,",",#2,"]"}]&)
];

levicivita2up /: MakeBoxes[levicivita2up[a_,b_],TraditionalForm|StandardForm]:=levicivita2upBox[ToBoxes[a],ToBoxes[b]];
(*Boxing of levicivita2Up, actually I just realized that no auxiliary function is needed...*)
levicivita2Up /: MakeBoxes[levicivita2Up[a_,b_],TraditionalForm|StandardForm]:=TemplateBox[{ToBoxes[a],ToBoxes[b]},"levicivita2Up",
DisplayFunction->(SuperscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"levicivita2Up","[",#1,",",#2,"]"}]&)];*)


(*levicivita2up[a_Integer,b_Integer]:=LeviCivitaTensor[2][[a,b]];
levicivita2up[a_,b_]:=levicivita2Up[a,b];
levicivita2Up[a_, b_] /; (a == b) := 0
levicivita2Up[a_, b_] /; \[Not]OrderedQ[{a,b}] := -levicivita2Up[b, a];

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "lcup" -> levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]]*)


levicivita2upBox[a_,b_]:=TemplateBox[{a,b},"levicivita2up",
DisplayFunction->(SuperscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"levicivita2up","[",#1,",",#2,"]"}]&)
];

levicivita2up /: MakeBoxes[levicivita2up[a_,b_],TraditionalForm|StandardForm]:=levicivita2upBox[ToBoxes[a],ToBoxes[b]];
(*Boxing of levicivita2Up, actually I just realized that no auxiliary function is needed...*)
levicivita2Up /: MakeBoxes[levicivita2Up[a_,b_],TraditionalForm|StandardForm]:=TemplateBox[{ToBoxes[a],ToBoxes[b]},"levicivita2Up",
DisplayFunction->(SuperscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"levicivita2Up","[",#1,",",#2,"]"}]&)];


levicivita2up[a_,b_]:=levicivita2Up[a,b];
levicivita2Up[a_, b_] /; (a == b) := 0;
levicivita2Up[a_, b_] /; \[Not]OrderedQ[{a,b}] := -levicivita2Up[b, a];
levicivita2Up[a_Integer,b_Integer]:=LeviCivitaTensor[2][[a,b]];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "lcup" -> levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection:: *)
(*Levi-Civita Tensor down*)


(*levicivita2downBox[a_,b_]:=TemplateBox[{a,b},"levicivita2down",
DisplayFunction->(SubscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"levicivita2down","[",#1,",",#2,"]"}]&)
];

levicivita2down /: MakeBoxes[levicivita2down[a_,b_],TraditionalForm|StandardForm]:=levicivita2downBox[ToBoxes[a],ToBoxes[b]];
(*Boxing of levicivita2Down, actually I just realized that no auxiliary function is needed...*)
levicivita2Down /: MakeBoxes[levicivita2Down[a_,b_],TraditionalForm|StandardForm]:=TemplateBox[{ToBoxes[a],ToBoxes[b]},"levicivita2Down",
DisplayFunction->(SubscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"levicivita2Down","[",#1,",",#2,"]"}]&)];*)


(*levicivita2down[a_Integer,b_Integer]:=-LeviCivitaTensor[2][[a,b]];
levicivita2down[a_,b_]:=levicivita2Down[a,b];
levicivita2Down[a_, b_] /; (a == b) := 0
levicivita2Down[a_, b_] /; \[Not]OrderedQ[{a,b}] := -levicivita2Down[b, a];

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "lcd" -> levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]]*)


levicivita2downBox[a_,b_]:=TemplateBox[{a,b},"levicivita2down",
DisplayFunction->(SubscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"levicivita2down","[",#1,",",#2,"]"}]&)
];

levicivita2down /: MakeBoxes[levicivita2down[a_,b_],TraditionalForm|StandardForm]:=levicivita2downBox[ToBoxes[a],ToBoxes[b]];
(*Boxing of levicivita2Down, actually I just realized that no auxiliary function is needed...*)
levicivita2Down /: MakeBoxes[levicivita2Down[a_,b_],TraditionalForm|StandardForm]:=TemplateBox[{ToBoxes[a],ToBoxes[b]},"levicivita2Down",
DisplayFunction->(SubscriptBox["\[Epsilon]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"levicivita2Down","[",#1,",",#2,"]"}]&)];


levicivita2down[a_,b_]:=levicivita2Down[a,b];
levicivita2Down[a_, b_] /; (a == b) := 0
levicivita2Down[a_, b_] /; \[Not]OrderedQ[{a,b}] := -levicivita2Down[b, a];
levicivita2Down[a_Integer,b_Integer]:=-LeviCivitaTensor[2][[a,b]];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "lcd" -> levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection:: *)
(*Spinor Angle Bracket*)


SpinorAngleBracket[a_, b_] /; (a == b) := 0
SpinorAngleBracket[a_, b_] /; \[Not]OrderedQ[{a,b}] := -SpinorAngleBracket[b, a];

SpinorAngleBracketBox[a_, b_] :=
    TemplateBox[{a, b}, "SpinorAngleBracket",
        DisplayFunction -> (RowBox[{"\[LeftAngleBracket]",RowBox[{#1,"\[MediumSpace]",#2}],"\[RightAngleBracket]"}]&),
        InterpretationFunction -> (RowBox[{"Spinoranglebracket","[",RowBox[{#1,",",#2}],"]"}]&)]

Spinoranglebracket[x_,y_]:=Module[{input1,input2,bool1,bool2},
input1=x/.OverBar->overbar;
bool1=MinusSignQ[input1];
input2=y/.OverBar->overbar;
bool2=MinusSignQ[input2];
Which[bool1&&bool2,
Return[-SpinorAngleBracket[-input1,-input2]],
bool1,
Return[I*SpinorAngleBracket[-input1,input2]],
bool2,
Return[I*SpinorAngleBracket[input1,-input2]],
True,
Return[SpinorAngleBracket[input1,input2]]
];
];

SpinorAngleBracket /: MakeBoxes[SpinorAngleBracket[a_, b_], StandardForm | TraditionalForm] := SpinorAngleBracketBox[ToBoxes[a], ToBoxes[b]]
If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ab" -> SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"]]];
    ];


(* ::Subsection:: *)
(*Spinor Square Bracket*)


SpinorSquareBracket[a_, b_] /; (a == b) := 0
SpinorSquareBracket[a_, b_] /; \[Not]OrderedQ[{a, b}] := -SpinorSquareBracket[b, a]

SpinorSquareBracketBox[a_, b_] :=
    TemplateBox[{a, b}, "SpinorSquareBracket",
        DisplayFunction -> (RowBox[{"[",RowBox[{#1,"\[MediumSpace]",#2}],"]"}]&),
        InterpretationFunction -> (RowBox[{"Spinorsquarebracket","[",RowBox[{#1,",",#2}],"]"}]&)]

Spinorsquarebracket[x_,y_]:=Module[{input1,input2,bool1,bool2},
input1=x/.OverBar->overbar;
bool1=MinusSignQ[input1];
input2=y/.OverBar->overbar;
bool2=MinusSignQ[input2];
Which[bool1&&bool2,
Return[-SpinorSquareBracket[-input1,-input2]],
bool1,
Return[I*SpinorSquareBracket[-input1,input2]],
bool2,
Return[I*SpinorSquareBracket[input1,-input2]],
True,
Return[SpinorSquareBracket[input1,input2]]
];
];

SpinorSquareBracket /: MakeBoxes[SpinorSquareBracket[a_, b_], StandardForm | TraditionalForm] := SpinorSquareBracketBox[ToBoxes[a], ToBoxes[b]]

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sb" -> SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"]]];
    ];


(* ::Subsection:: *)
(*Square-angle invariant*)


SquAngInvariant[a_, b_][x_,y_] /; (a == b) := 0;

SquAngInvariant[n_,m_][a_Integer,b_Integer]:=SpinorDot6D[n][$contractvariable][Null][a].SpinorUndot6D[m][$contractvariable][Null][b];

SquAngInvariantBox[n_,m_,adot_,b_]:=TemplateBox[{n,m,adot,b},"SquAngInvariant",
DisplayFunction->(RowBox[{"[",SubscriptBox[#1,#3], "," ,SubscriptBox[#2,#4],"\[RightAngleBracket]"}]&),
InterpretationFunction->(RowBox[{"SquAngInvariant","[",#1,",",#2,"]","[",#3,",",#4,"]"}]&)
];

SquAngInvariant /: MakeBoxes[SquAngInvariant[n_,m_][adot_,b_],StandardForm|TraditionalForm]:=SquAngInvariantBox[ToBoxes[n],ToBoxes[m],ToBoxes[adot],ToBoxes[b]];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sai" -> SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection:: *)
(*Angle-square invariant*)


AngSquInvariant[a_, b_][x_,y_] /; (a == b) := 0;

AngSquInvariant[n_,m_][a_Integer,b_Integer]:=SpinorUndot6D[n][$contractvariable][Null][a].SpinorDot6D[m][$contractvariable][Null][b];

AngSquInvariantBox[n_,m_,adot_,b_]:=TemplateBox[{n,m,adot,b},"AngSquInvariant",
DisplayFunction->(RowBox[{"\[LeftAngleBracket]",SubscriptBox[#1,#3], "," ,SubscriptBox[#2,#4],"]"}]&),
InterpretationFunction->(RowBox[{"AngSquInvariant","[",#1,",",#2,"]","[",#3,",",#4,"]"}]&)
];

AngSquInvariant /: MakeBoxes[AngSquInvariant[n_,m_][adot_,b_],StandardForm|TraditionalForm]:=AngSquInvariantBox[ToBoxes[n],ToBoxes[m],ToBoxes[adot],ToBoxes[b]];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "asi" -> AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection:: *)
(*Angle-angle invariant*)


AngAngInvariantBox[m_,n_,l_,k_,a_,b_,c_,d_]:=TemplateBox[{m,n,l,k,a,b,c,d},"AngAngInvariant",
DisplayFunction->(RowBox[{"\[LeftAngleBracket]",SubscriptBox[#1,#5],",",SubscriptBox[#2,#6],",",SubscriptBox[#3,#7],",",SubscriptBox[#4,#8],"\[RightAngleBracket]"}]&),
InterpretationFunction->(RowBox[{"AngAngInvariant","[",#1,",",#2,",",#3,",",#4,"]","[",#5,",",#6,",",#7,",",#8,"]"}]&)
];

AngAngInvariant /: MakeBoxes[AngAngInvariant[m_,n_,l_,k_][a_,b_,c_,d_],TraditionalForm|StandardForm]:=AngAngInvariantBox[ToBoxes[m],ToBoxes[n],ToBoxes[l],ToBoxes[k],ToBoxes[a],ToBoxes[b],ToBoxes[c],ToBoxes[d]];

AngAngInvariant[i_,j_,k_,l_][a_Integer,b_Integer,c_Integer,d_Integer]:=
First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[j][$contractvariable2][Null][b]]Last[SpinorUndot6D[k][$contractvariable3][Null][c]]Last[SpinorUndot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[k][$contractvariable2][Null][c]]Last[SpinorUndot6D[l][$contractvariable3][Null][d]]Last[SpinorUndot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[l][$contractvariable2][Null][d]]Last[SpinorUndot6D[j][$contractvariable3][Null][b]]Last[SpinorUndot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[k][$contractvariable1][Null][c]]First[SpinorUndot6D[l][$contractvariable2][Null][d]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[l][$contractvariable1][Null][d]]First[SpinorUndot6D[j][$contractvariable2][Null][b]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[j][$contractvariable1][Null][b]]First[SpinorUndot6D[k][$contractvariable2][Null][c]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "aa" -> AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection:: *)
(*Square square invariant*)


SquSquInvariantBox[m_,n_,l_,k_,a_,b_,c_,d_]:=TemplateBox[{m,n,l,k,a,b,c,d},"SquSquInvariant",
DisplayFunction->(RowBox[{"[",SubscriptBox[#1,#5],",",SubscriptBox[#2,#6],",",SubscriptBox[#3,#7],",",SubscriptBox[#4,#8],"]"}]&),
InterpretationFunction->(RowBox[{"SquSquInvariant","[",#1,",",#2,",",#3,",",#4,"]","[",#5,",",#6,",",#7,",",#8,"]"}]&)
];

SquSquInvariant /: MakeBoxes[SquSquInvariant[m_,n_,l_,k_][a_,b_,c_,d_],StandardForm|TraditionalForm]:=SquSquInvariantBox[ToBoxes[m],ToBoxes[n],ToBoxes[l],ToBoxes[k],ToBoxes[a],ToBoxes[b],ToBoxes[c],ToBoxes[d]];

SquSquInvariant[i_,j_,k_,l_][a_Integer,b_Integer,c_Integer,d_Integer]:=
Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[j][$contractvariable2][Null][b]]First[SpinorDot6D[k][$contractvariable3][Null][c]]First[SpinorDot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[k][$contractvariable2][Null][c]]First[SpinorDot6D[l][$contractvariable3][Null][d]]First[SpinorDot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[l][$contractvariable2][Null][d]]First[SpinorDot6D[j][$contractvariable3][Null][b]]First[SpinorDot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[k][$contractvariable1][Null][c]]Last[SpinorDot6D[l][$contractvariable2][Null][d]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[l][$contractvariable1][Null][d]]Last[SpinorDot6D[j][$contractvariable2][Null][b]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[j][$contractvariable1][Null][b]]Last[SpinorDot6D[k][$contractvariable2][Null][c]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "bb" -> SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection:: *)
(*SpinorReplace*)


$crep=1;
SubCounter:=($crep=$crep+1;\[Alpha]*IntegerPart[$crep/2]);
SpinorReplace[exp_,subs_List]:=Block[{locexp,locsubs,$a,$b},
locexp=exp/.{
SpinorAngleBracket[x_,y_]/;TrueQ[Head[x]==OverBar&&Head[y]==OverBar]-> HoldForm[SpinorUndot[x][$mu][SubCounter][Null]*SpinorUndot[y][$mu][Null][SubCounter]],SpinorSquareBracket[x_,y_]/;TrueQ[Head[x]==OverBar&&Head[y]==OverBar]->HoldForm[SpinorDot[x][$mu][Null][SubCounter]*SpinorDot[y][$mu][SubCounter][Null]],
SpinorAngleBracket[x_,y_]/;TrueQ[Head[y]==OverBar]-> HoldForm[SpinorUndot[x][$lam][SubCounter][Null]*SpinorUndot[y][$mu][Null][SubCounter]],SpinorSquareBracket[x_,y_]/;TrueQ[Head[y]==OverBar]->HoldForm[SpinorDot[x][$lam][Null][SubCounter]*SpinorDot[y][$mu][SubCounter][Null]],SpinorAngleBracket[x_,y_]-> HoldForm[SpinorUndot[x][$lam][SubCounter][Null]*SpinorUndot[y][$lam][Null][SubCounter]],SpinorSquareBracket[x_,y_]->HoldForm[SpinorDot[x][$lam][Null][SubCounter]*SpinorDot[y][$lam][SubCounter][Null]]};
locexp=locexp/.{SpinorUndot[OverBar[x_]][$mu][y_][z_]->SpinorUndot[x][$mu][y][z],SpinorDot[OverBar[x_]][$mu][y_][z_]->SpinorDot[x][$mu][y][z]};
locsubs=subs/.{SpinorUndotPure->SpinorUndot,SpinorDotPure->SpinorDot};
(*Do[
locsubs[[i,1]]=locsubs[[i,1]][$a_][$b_];
locsubs[[i,2]]=locsubs[[i,2]][$a][$b];
,{i,Length[subs]}];*)
locexp=locexp/.locsubs;
$crep=1;
Return[locexp//ReleaseHold];
];


(* ::Subsection:: *)
(*Momenta*)


Momenta[expr_] := Join[Cases[{expr}, HoldPattern[SpinorAngleBracket[a_, b_] | SpinorSquareBracket[a_, b_]] :> Sequence[a, b], \[Infinity]],
Cases[{expr}, HoldPattern[extramass[a_] | extramasstilde[a_]|\[CapitalMu][a_]|OverTilde[\[CapitalMu]][a_]|SpinorUndot[a_][_][_][_]|SpinorDot[a_][_][_][_]] :> a, \[Infinity]]]/.OverBar[x_]->x // DeleteDuplicates


(* ::Subsection:: *)
(*AllMomenta*)


AllMomenta[expr_] := Join[Cases[{expr}, HoldPattern[SpinorAngleBracket[a_, b_] | SpinorSquareBracket[a_, b_]] :> Sequence[a, b], \[Infinity]],
Cases[{expr}, HoldPattern[extramass[a_] | extramasstilde[a_]|\[CapitalMu][a_]|OverTilde[\[CapitalMu]][a_]|SpinorUndot[a_][_][_][_]|SpinorDot[a_][_][_][_]] :> a, \[Infinity]]] // DeleteDuplicates


(* ::Subsection:: *)
(*ConvenientMu*)


(*ConvenientMu[exp_,mu_]:=Block[{momlist,mu2,count,tabu,localexp,locmomlist,$SquarePowerCount,$AnglePowerCount,anglecounts,squarecounts,angletarget,squaretarget,anglereplace,squarereplace,locexp},
(*Extract all the momenta appearing in expression*)
momlist=AllMomenta[exp];
(*remove mu from the list*)
momlist=DeleteCases[momlist,x_/;x==mu];

(*tabu is the list of values it cannot assume since it would set the denominator to zero.*)
(*tabu["angle"]={};
tabu["square"]={};*)
tabu["angle"]={mu}/.OverBar[x_]->x;
tabu["square"]={mu}/.OverBar[x_]->x;
localexp=Denominator[Together[exp]];
localexp=localexp//.{SpinorAngleBracket[x_,y_]/;TrueQ[x==mu]==False&&TrueQ[y==mu]==False->1,SpinorSquareBracket[x_,y_]->1};
debugPrint["localexp= ",localexp];
AppendTo[tabu["angle"],DeleteCases[AllMomenta[localexp],x_/;x==mu]];
tabu["angle"]=tabu["angle"]//Flatten;
debugPrint["tabu[angle]= ",tabu["angle"]];

(*Now determine the tabu list in the square bracket*)
localexp=Denominator[Together[exp]];
localexp=localexp//.{SpinorSquareBracket[x_,y_]/;TrueQ[x==mu]==False&&TrueQ[y==mu]==False->1,SpinorAngleBracket[x_,y_]->1};
debugPrint["localexp= ",localexp];
AppendTo[tabu["square"],DeleteCases[AllMomenta[localexp],x_/;x==mu]];
tabu["square"]=tabu["square"]//Flatten;
debugPrint["tabu[square]= ",tabu["square"]];

(*Now pick local expression and remove all the tabu contractions in order not to count them*)
locexp=exp//.{SpinorSquareBracket[x_,y_]/;MemberQ[tabu["square"],x]||MemberQ[tabu["square"],y]->1,SpinorAngleBracket[x_,y_]/;MemberQ[tabu["angle"],x]||MemberQ[tabu["angle"],y]->1};
debugPrint["localexp= ",localexp];

(*count how many times the chosen barred momentum appears in expression contracted with each single one of the other momenta*)
(*counting for the angle bracket*)
Do[
(*now count*)
localexp=locexp//.{SpinorAngleBracket[mu,i]->$AnglePowerCount,SpinorAngleBracket[i,mu]->$AnglePowerCount};
debugPrint["localexp= ",localexp];
count[i,"angle"]=Expand[D[localexp,$AnglePowerCount]];
debugPrint["count[",i,",angle]= ",count[i,"angle"]];
(*If the resulting expression is not a sum then the immediate counting could go wrong, so we have to test the Head*)
Which[TrueQ[Head[count[i,"angle"]]==Plus],
debugPrint["sum in ",i];
count[i,"angle"]=Length[count[i,"angle"]],
TrueQ[count[i,"angle"]==0],
debugPrint["zero in ",i],
True,
debugPrint["not a sum in ",i];
count[i,"angle"]=1;
];
debugPrint["count[",i,",angle]= ",count[i,"angle"]];
,{i,momlist}];

(*Now we just need to identify the momentum with the highest counter*)
debugPrint["momlist= ",momlist];
anglecounts=Table[count[i,"angle"],{i,momlist}];
debugPrint["anglecounts= ",anglecounts];
angletarget=Position[anglecounts,Max[anglecounts]]//Flatten//First;
angletarget=momlist[[angletarget]];
debugPrint["Selected substitution angle= ",angletarget];

(*Now recast the replacement into a form amiable for later use*)
mu2=mu/.OverBar[x_]->x;
If[TrueQ[Head[angletarget]==OverBar],
angletarget=angletarget/.OverBar[x_]->x;
anglereplace={SpinorUndotPure[mu2][$mu]->SpinorUndotPure[angletarget][$mu]},
anglereplace={SpinorUndotPure[mu2][$mu]->SpinorUndotPure[angletarget][$lam]};
];

(*Repeat the procedure for the squares, upon performing the angle substitutions*)
(*Pick local expression and remove all the tabu contractions in order not to count them*)
locexp=SpinorReplace[exp,anglereplace];
locexp=locexp//.{SpinorSquareBracket[x_,y_]/;MemberQ[tabu["square"],x]||MemberQ[tabu["square"],y]->1,SpinorAngleBracket[x_,y_]/;MemberQ[tabu["angle"],x]||MemberQ[tabu["angle"],y]->1};
debugPrint["locexp= ",locexp];

(*Counting the square brackets*)
Do[
(*now count*)
localexp=locexp//.{SpinorSquareBracket[mu,i]->$SquarePowerCount,SpinorSquareBracket[i,mu]->$SquarePowerCount};
(*count[i,"square"]=Exponent[localexp,$SquarePowerCount];*)
debugPrint["localexp= ",localexp];
count[i,"square"]=Expand[D[localexp,$SquarePowerCount]];
debugPrint["count[",i,",square]= ",count[i,"square"]];
(*If the resulting expression is not a sum then the immediate counting could go wrong, so we have to test the Head*)
Which[TrueQ[Head[count[i,"square"]]==Plus],
debugPrint["sum in ",i];
count[i,"square"]=Length[count[i,"square"]],
TrueQ[count[i,"square"]==0],
debugPrint["zero in ",i],
True,
debugPrint["not a sum in ",i];
count[i,"square"]=1;
];
debugPrint["count[",i,",square]= ",count[i,"square"]];
,{i,momlist}];

(*Now we just need to identify the momentum with the highest counter*)
debugPrint["momlist= ",momlist];
squarecounts=Table[count[i,"square"],{i,momlist}];
debugPrint["squarecounts= ",squarecounts];
squaretarget=Position[squarecounts,Max[squarecounts]]//Flatten//First;
squaretarget=momlist[[squaretarget]];
debugPrint["Selected subsitution square= ",squaretarget];
If[TrueQ[Head[squaretarget]==OverBar],
squaretarget=squaretarget/.OverBar[x_]->x;
squarereplace={SpinorDotPure[mu2][$mu]->SpinorDotPure[squaretarget][$mu]},
squarereplace={SpinorDotPure[mu2][$mu]->SpinorDotPure[squaretarget][$lam]};
];
Return[Join[anglereplace,squarereplace]];
];*)


ConvenientMu[exp_,mu_,tabulist_,bool_]:=Module[{momlist,mu2,count,tabu,localexp,locmomlist,$SquarePowerCount,$AnglePowerCount,anglecounts,squarecounts,angletarget,squaretarget,anglereplace,squarereplace,locexp,localtabulist,angletargetlist,squaretargetlist},
(*Extract all the momenta appearing in expression*)
momlist=AllMomenta[exp];
(*remove mu from the list*)
momlist=DeleteCases[momlist,x_/;x==mu];

(*tabu is the list of values it cannot assume since it would set the denominator to zero.*)
(*tabu["angle"]={};
tabu["square"]={};*)
tabu["angle"]={mu}/.OverBar[x_]->x;
tabu["square"]={mu}/.OverBar[x_]->x;
localexp=Denominator[Together[exp]];
localexp=localexp//.{SpinorAngleBracket[x_,y_]/;TrueQ[x==mu]==False&&TrueQ[y==mu]==False->1,SpinorSquareBracket[x_,y_]->1};
debugPrint["localexp= ",localexp];
AppendTo[tabu["angle"],DeleteCases[AllMomenta[localexp],x_/;x==mu]];
tabu["angle"]=tabu["angle"]//Flatten;
debugPrint["tabu[angle]= ",tabu["angle"]];

(*Now determine the tabu list in the square bracket*)
localexp=Denominator[Together[exp]];
localexp=localexp//.{SpinorSquareBracket[x_,y_]/;TrueQ[x==mu]==False&&TrueQ[y==mu]==False->1,SpinorAngleBracket[x_,y_]->1};
debugPrint["localexp= ",localexp];
AppendTo[tabu["square"],DeleteCases[AllMomenta[localexp],x_/;x==mu]];
tabu["square"]=tabu["square"]//Flatten;
debugPrint["tabu[square]= ",tabu["square"]];

(*Now pick local expression and remove all the tabu contractions in order not to count them*)
locexp=exp//.{SpinorSquareBracket[x_,y_]/;MemberQ[tabu["square"],x]||MemberQ[tabu["square"],y]->1,SpinorAngleBracket[x_,y_]/;MemberQ[tabu["angle"],x]||MemberQ[tabu["angle"],y]->1};
If[!bool,
(*We want the \[Mu] to be replaced by a \[Lambda] not another \[Mu], so we set to one all the contractions of two \[Mu]*)
locexp=locexp/.{SpinorAngleBracket[x_,y_]/;TrueQ[Head[x]==OverBar]&&TrueQ[Head[y]==OverBar]->1,SpinorSquareBracket[x_,y_]/;TrueQ[Head[x]==OverBar]&&TrueQ[Head[y]==OverBar]->1};
];
debugPrint["locexp= ",locexp];

(*count how many times the chosen barred momentum appears in expression contracted with each single one of the other momenta*)
(*counting for the angle bracket*)
Do[
(*now count*)
localexp=locexp//.{SpinorAngleBracket[mu,i]->$AnglePowerCount,SpinorAngleBracket[i,mu]->$AnglePowerCount};
debugPrint["localexp= ",localexp];
count[i,"angle"]=Expand[D[localexp,$AnglePowerCount]];
debugPrint["count[",i,",angle]= ",count[i,"angle"]];
(*If the resulting expression is not a sum then the immediate counting could go wrong, so we have to test the Head*)
Which[TrueQ[Head[count[i,"angle"]]==Plus],
debugPrint["sum in ",i];
count[i,"angle"]=Length[count[i,"angle"]],
TrueQ[count[i,"angle"]==0],
debugPrint["zero in ",i],
True,
debugPrint["not a sum in ",i];
count[i,"angle"]=1;
];
debugPrint["count[",i,",angle]= ",count[i,"angle"]];
,{i,momlist}];

(*Now we just need to identify the momentum with the highest counter*)
debugPrint["momlist= ",momlist];
anglecounts=Table[count[i,"angle"],{i,momlist}];
debugPrint["anglecounts= ",anglecounts];
(*Take into account the fact that there might be either no allowed substitution or no mu to be replaced, which would result in anglecounts being a list of zeros*)
(*Also account for the fact that if we already replaced \[Mu][i]\[Rule]\[Mu][j] we cannot replace \[Mu][j]\[Rule]\[Lambda][i], this is what tabulist is needed for.*)
If[Max[anglecounts]==0,
angletarget=mu,
angletargetlist=Position[anglecounts,Max[anglecounts]]//Flatten;
angletargetlist=Table[momlist[[i]],{i,angletargetlist}];
debugPrint["angletargetlist= ",angletargetlist];
If[bool,
localtabulist=Table[i[[1]],{i,tabulist}]/.{SpinorDotPure[x_][$mu]:>0,SpinorUndotPure[x_][$mu]:>x}//DeleteDuplicates;
localtabulist=Join[DeleteCases[localtabulist,0],OverBar/@DeleteCases[localtabulist,0]];
debugPrint["localtabulist for angles= ",localtabulist];
angletargetlist=DeleteCases[angletargetlist,x_/;MemberQ[localtabulist,x]];
];
If[Length[angletargetlist]>0,
angletarget=First[angletargetlist],
angletarget=mu;
];
(*angletarget=momlist[[angletarget]];*)
];
debugPrint["Selected substitution angle= ",angletarget];

(*Now recast the replacement into a form amiable for later use*)
mu2=mu/.OverBar[x_]->x;
If[TrueQ[Head[angletarget]==OverBar],
angletarget=angletarget/.OverBar[x_]->x;
anglereplace={SpinorUndotPure[mu2][$mu]->SpinorUndotPure[angletarget][$mu]},
anglereplace={SpinorUndotPure[mu2][$mu]->SpinorUndotPure[angletarget][$lam]};
];

(*Repeat the procedure for the squares, upon performing the angle substitutions*)
(*Pick local expression and remove all the tabu contractions in order not to count them*)
locexp=SpinorReplace[exp,anglereplace];
locexp=locexp//.{SpinorSquareBracket[x_,y_]/;MemberQ[tabu["square"],x]||MemberQ[tabu["square"],y]->1,SpinorAngleBracket[x_,y_]/;MemberQ[tabu["angle"],x]||MemberQ[tabu["angle"],y]->1};
debugPrint["locexp= ",locexp];

(*Counting the square brackets*)
Do[
(*now count*)
localexp=locexp//.{SpinorSquareBracket[mu,i]->$SquarePowerCount,SpinorSquareBracket[i,mu]->$SquarePowerCount};
(*count[i,"square"]=Exponent[localexp,$SquarePowerCount];*)
debugPrint["localexp= ",localexp];
count[i,"square"]=Expand[D[localexp,$SquarePowerCount]];
debugPrint["count[",i,",square]= ",count[i,"square"]];
(*If the resulting expression is not a sum then the immediate counting could go wrong, so we have to test the Head*)
Which[TrueQ[Head[count[i,"square"]]==Plus],
debugPrint["sum in ",i];
count[i,"square"]=Length[count[i,"square"]],
TrueQ[count[i,"square"]==0],
debugPrint["zero in ",i],
True,
debugPrint["not a sum in ",i];
count[i,"square"]=1;
];
debugPrint["count[",i,",square]= ",count[i,"square"]];
,{i,momlist}];

(*Now we just need to identify the momentum with the highest counter*)
debugPrint["momlist= ",momlist];
squarecounts=Table[count[i,"square"],{i,momlist}];
debugPrint["squarecounts= ",squarecounts];
(*Take into account the fact that there might be either no allowed substitution or no mu to be replaced, which would result in anglecounts being a list of zeros*)
If[Max[squarecounts]==0,
squaretarget=mu,
squaretargetlist=Position[squarecounts,Max[squarecounts]]//Flatten;
squaretargetlist=Table[momlist[[i]],{i,squaretargetlist}];
debugPrint["squaretargetlist= ",squaretargetlist];
If[bool,
localtabulist=Table[i[[1]],{i,tabulist}]/.{SpinorDotPure[x_][$mu]:>x,SpinorUndotPure[x_][$mu]:>0}//DeleteDuplicates;
localtabulist=Join[DeleteCases[localtabulist,0],OverBar/@DeleteCases[localtabulist,0]];
debugPrint["localtabulist for squares= ",localtabulist];
squaretargetlist=DeleteCases[squaretargetlist,x_/;MemberQ[localtabulist,x]];
];
If[Length[squaretargetlist]>0,
squaretarget=First[squaretargetlist],
squaretarget=mu;
];

];
debugPrint["Selected subsitution square= ",squaretarget];
If[TrueQ[Head[squaretarget]==OverBar],
squaretarget=squaretarget/.OverBar[x_]->x;
squarereplace={SpinorDotPure[mu2][$mu]->SpinorDotPure[squaretarget][$mu]},
squarereplace={SpinorDotPure[mu2][$mu]->SpinorDotPure[squaretarget][$lam]};
];
Return[Join[anglereplace,squarereplace]];
];


(* ::Subsection:: *)
(*SpinorReplaceSequential*)


SpinorReplaceSequential[exp_,subs_]:=Catch[Block[{locexp},
locexp=exp;
Do[If[Infycheck[locexp=SpinorReplace[locexp,{subs[[i]]}]],Throw["The substitution "<>ToString[subs[[i]]]<>" is not allowed"]],{i,Length[subs]}];
Return[locexp];
]];


(* ::Subsection:: *)
(*MuReplace*)


(*Options[MuReplace]={DisplayReplacements->False};
MuReplace[exp_,OptionsPattern[]]:=Block[{localexp,momlist,replacements,totalreplacements,mu},
localexp=exp;
momlist:=AllMomenta[localexp];
totalreplacements={};
While[MemberQ[momlist,x_/;TrueQ[Head[x]==OverBar]],
mu=First[Select[momlist,TrueQ[Head[#]==OverBar]&,1]];
replacements=ConvenientMu[localexp,mu];
totalreplacements=Join[totalreplacements,replacements];
localexp=SpinorReplace[localexp,replacements];
debugPrint["localexp= ",localexp];
];
Which[OptionValue[DisplayReplacements],
Return[{localexp,totalreplacements}],
OptionValue[DisplayReplacements]==False,
Return[localexp],
True,
Return["Unknown option assigned for DisplayReplacements, please choose among True or False"];
];
];*)


Options[MuReplace]={DisplayReplacements->False,AllowMutoMu->False,GlobalReplacements->False};
MuReplace[exp_,OptionsPattern[]]:=Module[{localexp,momlist,replacements,totalreplacements,mu,mu2,exceptions,candidates,candidatetest,scores,$count,tabulist},
localexp=exp;
momlist:=AllMomenta[localexp];
totalreplacements={};
exceptions={};
While[Length[candidates=DeleteCases[Select[momlist,TrueQ[Head[#]==OverBar]&],x_/;MemberQ[exceptions,x]]]>0,
(*Now among the candidates we want to choose the one which appears more often*)
scores={};
Do[
candidatetest=localexp/.{SpinorAngleBracket[x_,y_]/;TrueQ[x==i]||TrueQ[y==i]->$count,SpinorSquareBracket[x_,y_]/;TrueQ[x==i]||TrueQ[y==i]->$count};
candidatetest=D[candidatetest,$count]//Expand;
AppendTo[scores,Length[candidatetest]];
,{i,candidates}];
debugPrint["The candidates ",candidates," scored ",scores];
debugPrint["Chosen ",candidates[[Position[scores,Max[scores],1]//Flatten//First]]];
mu=candidates[[Position[scores,Max[scores],1]//Flatten//First]];
tabulist={};
If[OptionValue[AllowMutoMu],
mu2=mu/.OverBar[x_]:>x;
tabulist=DeleteCases[totalreplacements,x_/;FreeQ[x[[2]],SpinorDotPure[mu2][$mu]]&&FreeQ[x[[2]],SpinorUndotPure[mu2][$mu]]];
debugPrint["candidate= ",mu];
debugPrint["tabulist= ",tabulist];
];
replacements=ConvenientMu[localexp,mu,tabulist,OptionValue[AllowMutoMu]];
(*If ConvenientMu did not find an appropriate replacement for mu we leave it as it is, so we need to add it to the list of exceptions*)
If[!FreeQ[Table[{i[[1]],i[[2]]},{i,replacements}],{x_,x_}],
AppendTo[exceptions,mu];
];
totalreplacements=Join[totalreplacements,replacements];
localexp=SpinorReplace[localexp,replacements];
debugPrint["localexp= ",localexp];
];
(*Store in global variable fixed spinors if the given option is set to True. We only want to store the real replacements not \[Mu] going to itrself, so we have to remove such elements from our list before saving it.*)
If[OptionValue[GlobalReplacements],
replacements=Table[{i[[1]],i[[2]]},{i,totalreplacements}];
replacements=DeleteCases[Table[{i[[1]],i[[2]]},{i,replacements}],{x_,x_}];
replacements=Table[{i[[1]]->i[[2]]},{i,replacements}]//Flatten;
FixSpinors[replacements];
];
Which[OptionValue[DisplayReplacements],
Return[{localexp,totalreplacements}],
OptionValue[DisplayReplacements]==False,
Return[localexp],
True,
Return["Unknown option assigned for DisplayReplacements, please choose among True or False"];
];
];


(* ::Subsection:: *)
(*ClearSpinors*)


FixedSpinors={};
ClearSpinors[]:=(Unprotect[SpinorUndot,SpinorDot,FixedSpinors];SubValues[SpinorUndot]=DeleteCases[SubValues[SpinorUndot],_?(FreeQ[First[#],HoldPattern@SpinorUndot[Pattern]]&)];
SubValues[SpinorDot]=DeleteCases[SubValues[SpinorDot],_?(FreeQ[First[#],HoldPattern@SpinorDot[Pattern]]&)];
FixedSpinors={};
Protect[SpinorUndot,SpinorDot,FixedSpinors];)


(* ::Subsection:: *)
(*ClearSpinorsPrivate*)


FixedSpinors={};
ClearSpinorsPrivate[]:=(Unprotect[SpinorUndot,SpinorDot];SubValues[SpinorUndot]=DeleteCases[SubValues[SpinorUndot],_?(FreeQ[First[#],HoldPattern@SpinorUndot[Pattern]]&)];
SubValues[SpinorDot]=DeleteCases[SubValues[SpinorDot],_?(FreeQ[First[#],HoldPattern@SpinorDot[Pattern]]&)];
Protect[SpinorUndot,SpinorDot];)


(* ::Subsection:: *)
(*FixSpinors*)


FixSpinors[x__List]:=Module[{len,reps,duplicates,clean,auxlist,auxUndot,auxDot},
len=Length[x];
reps=x;
duplicates=Table[reps[[j,1]],{j,len}];

(*Before assigning the spinor equalities we add the new definitions to the list of fixed spinors, but to do so we need first to remove possible old overlapping definitions*)

Unprotect[FixedSpinors];
Do[FixedSpinors=DeleteCases[FixedSpinors,y_/;MemberQ[duplicates,y[[1]]]],{j,len}];
FixedSpinors=Join[FixedSpinors,x]//Sort;
Protect[FixedSpinors];

(*Now we clear all the definitions of the spinors*)
ClearSpinorsPrivate[];

(*Now fix the spinors to the desired values, also the ones which were already fixed*)
Unprotect[SpinorUndot,SpinorDot];
reps=FixedSpinors/.{SpinorUndotPure->SpinorUndot,SpinorDotPure->SpinorDot};
reps=reps/.Rule->Set;
Protect[SpinorUndot,SpinorDot];
];


(* ::Subsection:: *)
(*SchoutenSimplify*)


(* Rules for Schouten simplification *)
SchoutenRules =
    {k_. SpinorAngleBracket[a_,c_] SpinorAngleBracket[b_,d_] - k_. SpinorAngleBracket[a_,d_] SpinorAngleBracket[b_,c_] :> k SpinorAngleBracket[a,b] SpinorAngleBracket[c,d],
     k_. SpinorAngleBracket[a_,b_] SpinorAngleBracket[c_,d_] + k_. SpinorAngleBracket[a_,d_] SpinorAngleBracket[b_,c_] :> k SpinorAngleBracket[a,c] SpinorAngleBracket[b,d],
     k_. SpinorAngleBracket[a_,c_] SpinorAngleBracket[b_,d_] - k_. SpinorAngleBracket[a_,b_] SpinorAngleBracket[c_,d_] :> k SpinorAngleBracket[a,d] SpinorAngleBracket[b,c],
     k_. SpinorSquareBracket[a_,c_] SpinorSquareBracket[b_,d_] - k_. SpinorSquareBracket[a_,d_] SpinorSquareBracket[b_,c_] :> k SpinorSquareBracket[a,b] SpinorSquareBracket[c,d],
     k_. SpinorSquareBracket[a_,b_] SpinorSquareBracket[c_,d_] + k_. SpinorSquareBracket[a_,d_] SpinorSquareBracket[b_,c_] :> k SpinorSquareBracket[a,c] SpinorSquareBracket[b,d],
     k_. SpinorSquareBracket[a_,c_] SpinorSquareBracket[b_,d_] - k_. SpinorSquareBracket[a_,b_] SpinorSquareBracket[c_,d_] :> k SpinorSquareBracket[a,d] SpinorSquareBracket[b,c]}

(* Generates all possibile Schouten identities associated with a given set of momenta. *)
SchoutenIdentities[momenta_List] :=
    ({a,b,c,d} \[Function]
        Sequence@@
            {SpinorAngleBracket[a,b] SpinorAngleBracket[c,d] + SpinorAngleBracket[b,c] SpinorAngleBracket[a,d] + SpinorAngleBracket[c,a] SpinorAngleBracket[b,d] == 0,
             SpinorSquareBracket[a,b] SpinorSquareBracket[c,d] + SpinorSquareBracket[b,c] SpinorSquareBracket[a,d] + SpinorSquareBracket[c,a] SpinorSquareBracket[b,d] == 0})@@@
        Subsets[momenta,{4}]

(* SchoutenSimplify*)
SchoutenSimplify[expr_] :=
    Simplify[expr,
        Assumptions -> SchoutenIdentities[Momenta[expr]],
        TransformationFunctions -> {Automatic, (e \[Function] e /. SchoutenRules)}]


(* ::Subsection:: *)
(*Mom4D*)


Mom4D[l1_][$up][a_][b_]:=Spinorundot[l1][$lam][a][Null]Spinordot[l1][$lam][b][Null]+extramass[l1]extramasstilde[l1]/(Spinoranglebracket[l1,OverBar[l1]]Spinorsquarebracket[OverBar[l1],l1])Spinorundot[l1][$mu][a][Null]Spinordot[l1][$mu][b][Null];
Mom4D[l1_][$down][a_][b_]:=Spinorundot[l1][$lam][Null][a]Spinordot[l1][$lam][Null][b]+extramass[l1]extramasstilde[l1]/(Spinoranglebracket[l1,OverBar[l1]]Spinorsquarebracket[OverBar[l1],l1])Spinorundot[l1][$mu][Null][a]Spinordot[l1][$mu][Null][b];


(* ::Subsection:: *)
(*S6*)


S6[x_,y_]:=-extramass[x]extramasstilde[y]-extramass[y]extramasstilde[x]+Mom4D[x][$up][$contractvariable1][$contractvariable2]Mom4D[y][$down][$contractvariable1][$contractvariable2]//Expand;


(* ::Subsection:: *)
(*S*)


SetAttributes[S,Orderless];


(* ::Subsection:: *)
(*S4*)


SetAttributes[S4,{Orderless,Protected}];


(* ::Subsection:: *)
(*S6many*)


S6many[labels__]:=Block[{locvar,locexp},
locvar={labels};
locvar=DeleteDuplicates[Sort/@Tuples[locvar,2]];
locvar=DeleteCases[locvar,x_/;Length[DeleteDuplicates[x]]==1];
debugPrint["locvar= ",locvar];
locexp=Plus@@Apply[S6[#1,#2]&,locvar,{1}];
Return[locexp];
];


(* ::Subsection:: *)
(*CompleteDenominators*)


(*CompleteDenominators[exp_]:=Module[{localexp,numtot,dentot,numden},
localexp=Together[exp];
numtot=Numerator[localexp];
dentot=Denominator[localexp];
dentot=dentot/.{SpinorAngleBracket[x_,y_]:>S[x,y]/SpinorSquareBracket[y,x]};
numden=Numerator[dentot]/.{SpinorSquareBracket[x_,y_]:>S[x,y]/SpinorAngleBracket[y,x]};
dentot=numden/Denominator[dentot];
Return[numtot/dentot];
];*)


(* ::Subsection:: *)
(*CompleteDenominators*)


CompleteDenominators[exp_]:=Module[{localexp,numtot,dentot,numden},
localexp=Together[exp];
numtot=Numerator[localexp];
dentot=Denominator[localexp];
dentot=dentot/.{SpinorAngleBracket[x_,y_]:>S4[x,y]/SpinorSquareBracket[y,x]};
numden=Numerator[dentot]/.{SpinorSquareBracket[x_,y_]:>S4[x,y]/SpinorAngleBracket[y,x]};
dentot=numden/Denominator[dentot];
Return[numtot/dentot];
];


(* ::Subsection:: *)
(*CompleteMandelstam*)


(*CompleteMandelstam[exp_]:=Module[{localexp,numtot,dentot,numnum,dennum,denden,numden,numi},
localexp=Together[exp];
numtot={Numerator[localexp]}/.{Plus->List}//Flatten;
dentot=Denominator[localexp];

Do[
numi=numtot[[i]];
numi=numi/.{SpinorAngleBracket[x_,y_]:>S[x,y]/SpinorSquareBracket[y,x]};
numnum=Numerator[numi];
dennum=Denominator[numi]/.{SpinorSquareBracket[x_,y_]:>S[x,y]/SpinorAngleBracket[y,x]};
numi=numnum/dennum;
numtot[[i]]=numi;
,{i,Length[numtot]}];
numtot=Plus@@numtot;

dentot=dentot/.{SpinorAngleBracket[x_,y_]:>S[x,y]/SpinorSquareBracket[y,x]};
numden=Numerator[dentot];
denden=Denominator[dentot]/.{SpinorSquareBracket[x_,y_]:>S[x,y]/SpinorAngleBracket[y,x]};
dentot=numden/denden;

Return[numtot/dentot];
];*)


(* ::Subsection:: *)
(*CompleteMandelstam*)


CompleteMandelstam[exp_]:=Module[{localexp,numtot,dentot,numnum,dennum,denden,numden,numi},
localexp=Together[exp];
numtot={Numerator[localexp]}/.{Plus->List}//Flatten;
dentot=Denominator[localexp];

Do[
numi=numtot[[i]];
numi=numi/.{SpinorAngleBracket[x_,y_]:>S4[x,y]/SpinorSquareBracket[y,x]};
numnum=Numerator[numi];
dennum=Denominator[numi]/.{SpinorSquareBracket[x_,y_]:>S4[x,y]/SpinorAngleBracket[y,x]};
numi=numnum/dennum;
numtot[[i]]=numi;
,{i,Length[numtot]}];
numtot=Plus@@numtot;

dentot=dentot/.{SpinorAngleBracket[x_,y_]:>S4[x,y]/SpinorSquareBracket[y,x]};
numden=Numerator[dentot];
denden=Denominator[dentot]/.{SpinorSquareBracket[x_,y_]:>S4[x,y]/SpinorAngleBracket[y,x]};
dentot=numden/denden;

Return[numtot/dentot];
];


(* ::Subsection:: *)
(*ScalProdToS*)


ScalProdToS[exp_]:=exp/.{mp[i_,j_]:>S[i,j]/2+(extramass[i]extramasstilde[j]+extramass[j]extramasstilde[i])/2};


(* ::Subsection:: *)
(*chain*)


(*Contraction properties of the chains*)
(*SquareAngle to AngleSquare*)
chain[$square,x_,k_List,y_,$angle]:=(-1)^(Length[k]+1)chain[$angle,y,k,x,$square]
(*AngleAngle with SquareSquare*)
chain /: Times[chain[$angle,x_,k_List,y_,$angle],chain[$square,y_,q_List,z_,$square]]:=chain[$angle,x,Join[k,{y},q],z,$square];
chain /: Times[chain[$angle,x_,k_List,y_,$angle],chain[$square,z_,q_List,y_,$square]]:=(-1)^(Length[q]+1)chain[$angle,x,Join[k,{y},Reverse[q]],z,$square];
chain /: Times[chain[$angle,y_,k_List,x_,$angle],chain[$square,y_,q_List,z_,$square]]:=(-1)^(Length[k]+1)chain[$angle,x,Join[Reverse[k],{y},q],z,$square];
(*AngleSquare with SquareSquare*)
(*chain /: Times[chain[$square,x_,k_List,y_,$angle],chain[$square,y_,q_List,z_,$square]]:=chain[$square,x,Join[k,{y},q],z,$square];
chain /: Times[chain[$square,x_,k_List,y_,$angle],chain[$square,z_,q_List,y_,$square]]:=(-1)^(Length[q]+1)chain[$square,x,Join[k,{y},q],z,$square];*)
chain /: Times[chain[$angle,y_,k_List,x_,$square],chain[$square,y_,q_List,z_,$square]]:=(-1)^(Length[k]+1)chain[$square,x,Join[Reverse[k],{y},q],z,$square];
chain /: Times[chain[$angle,y_,k_List,x_,$square],chain[$square,z_,q_List,y_,$square]]:=chain[$square,z,Join[q,{y},k],x,$square];
(*SquareAngle with AngleAngle*)
chain /: Times[chain[$angle,x_,k_List,y_,$square],chain[$angle,y_,q_List,z_,$angle]]:=chain[$angle,x,Join[k,{y},q],z,$angle];
chain /: Times[chain[$angle,x_,k_List,y_,$square],chain[$angle,z_,q_List,y_,$angle]]:=(-1)^(Length[q]+1)chain[$angle,x,Join[k,{y},Reverse[q]],z,$angle];
(*chain /: Times[chain[$square,y_,k_List,x_,$angle],chain[$angle,y_,q_List,z_,$angle]]:=(-1)^(Length[k]+1)chain[$angle,x,Join[k,{y},q],z,$angle];
chain /: Times[chain[$square,y_,k_List,x_,$angle],chain[$angle,z_,q_List,y_,$angle]]:=chain[$angle,z,Join[q,{y},k],x,$angle];*)
(*AngleSquare with AngleSquare*)
chain /: Times[chain[$angle,x_,k_List,y_,$square],chain[$angle,y_,q_List,z_,$square]]:=chain[$angle,x,Join[k,{y},q],z,$square];
(*Vanishing chains*)
chain[type_,x_,{x_,y___},z_,type2_]:=0;
chain[type_,x_,{y___,z_},z_,type2_]:=0;
(*chain[type_,x_,{x_},z_,type2_]:=0;
chain[type_,x_,{z_},z_,type2_]:=0;*)

(*Display of the chains*)
AngleSquarechainBox[beginning_,args_,end_]:=TemplateBox[{beginning,args,end},"AngleSquarechain",
DisplayFunction->(RowBox[{"\[LeftAngleBracket]",#1,#2,#3,"]"}]&),
InterpretationFunction->(RowBox[{"chain","[","$angle",",",#1,",",#2,",",#3,",","$square","]"}]&)
];
SquareAnglechainBox[beginning_,args_,end_]:=TemplateBox[{beginning,args,end},"SquareAnglechain",
DisplayFunction->(RowBox[{"[",#1,#2,#3,"\[RightAngleBracket]"}]&),
InterpretationFunction->(RowBox[{"chain","[","$square",",",#1,",",#2,",",#3,",","$angle","]"}]&)
];
AngleAnglechainBox[beginning_,args_,end_]:=TemplateBox[{beginning,args,end},"AngleAnglechain",
DisplayFunction->(RowBox[{"\[LeftAngleBracket]",#1,#2,#3,"\[RightAngleBracket]"}]&),
InterpretationFunction->(RowBox[{"chain","[","$angle",",",#1,",",#2,",",#3,",","$angle","]"}]&)
];
SquareSquarechainBox[beginning_,args_,end_]:=TemplateBox[{beginning,args,end},"SquareSquarechain",
DisplayFunction->(RowBox[{"[",#1,#2,#3,"]"}]&),
InterpretationFunction->(RowBox[{"chain","[","$square",",",#1,",",#2,",",#3,",","$square","]"}]&)
];
chain /: MakeBoxes[chain[$angle,x_,y_List,z_,$square],StandardForm|TraditionalForm] /;OddQ[Length[y]+2]:=AngleSquarechainBox[ToBoxes[x],ToBoxes[y],ToBoxes[z]];
chain /: MakeBoxes[chain[$angle,x_,y_List,z_,$angle],StandardForm|TraditionalForm] /;EvenQ[Length[y]+2]:=AngleAnglechainBox[ToBoxes[x],ToBoxes[y],ToBoxes[z]];
chain /: MakeBoxes[chain[$square,x_,y_List,z_,$angle],StandardForm|TraditionalForm] /;OddQ[Length[y]+2]:=SquareAnglechainBox[ToBoxes[x],ToBoxes[y],ToBoxes[z]];
chain /: MakeBoxes[chain[$square,x_,y_List,z_,$square],StandardForm|TraditionalForm] /;EvenQ[Length[y]+2]:=SquareSquarechainBox[ToBoxes[x],ToBoxes[y],ToBoxes[z]];


(* ::Subsection:: *)
(*Chain*)


(*(*Contraction properties of the chains*)
(*SquareAngle to AngleSquare*)
Chain[$square,x_,k_List,y_,$angle]:=(-1)^(Length[k]+1)Chain[$angle,y,k,x,$square]
(*AngleAngle with SquareSquare*)
Chain /: Times[Chain[$angle,x_,k_List,y_,$angle],Chain[$square,y_,q_List,z_,$square]]:=Chain[$angle,x,Join[k,{y},q],z,$square];
Chain /: Times[Chain[$angle,x_,k_List,y_,$angle],Chain[$square,z_,q_List,y_,$square]]:=(-1)^(Length[q]+1)Chain[$angle,x,Join[k,{y},q],z,$square];
Chain /: Times[Chain[$angle,y_,k_List,x_,$angle],Chain[$square,y_,q_List,z_,$square]]:=(-1)^(Length[k]+1)Chain[$angle,x,Join[k,{y},q],z,$square];
(*AngleSquare with SquareSquare*)
(*Chain /: Times[Chain[$square,x_,k_List,y_,$angle],Chain[$square,y_,q_List,z_,$square]]:=Chain[$square,x,Join[k,{y},q],z,$square];
Chain /: Times[Chain[$square,x_,k_List,y_,$angle],Chain[$square,z_,q_List,y_,$square]]:=(-1)^(Length[q]+1)Chain[$square,x,Join[k,{y},q],z,$square];*)
Chain /: Times[Chain[$angle,y_,k_List,x_,$square],Chain[$square,y_,q_List,z_,$square]]:=(-1)^(Length[k]+1)Chain[$square,x,Join[k,{y},q],z,$square];
Chain /: Times[Chain[$angle,y_,k_List,x_,$square],Chain[$square,z_,q_List,y_,$square]]:=Chain[$square,z,Join[q,{y},k],x,$square];
(*SquareAngle with AngleAngle*)
Chain /: Times[Chain[$angle,x_,k_List,y_,$square],Chain[$angle,y_,q_List,z_,$angle]]:=Chain[$angle,x,Join[k,{y},q],z,$angle];
Chain /: Times[Chain[$angle,x_,k_List,y_,$square],Chain[$angle,z_,q_List,y_,$angle]]:=(-1)^(Length[q]+1)Chain[$angle,x,Join[k,{y},q],z,$angle];
(*Chain /: Times[Chain[$square,y_,k_List,x_,$angle],Chain[$angle,y_,q_List,z_,$angle]]:=(-1)^(Length[k]+1)Chain[$angle,x,Join[k,{y},q],z,$angle];
Chain /: Times[Chain[$square,y_,k_List,x_,$angle],Chain[$angle,z_,q_List,y_,$angle]]:=Chain[$angle,z,Join[q,{y},k],x,$angle];*)
(*AngleSquare with AngleSquare*)
Chain /: Times[Chain[$angle,x_,k_List,y_,$square],Chain[$angle,y_,q_List,z_,$square]]:=Chain[$angle,x,Join[k,{y},q],z,$square];
*)

(*Display of the chains*)
AngleSquareChainBox[beginning_,args_,end_]:=TemplateBox[{beginning,args,end},"AngleSquareChain",
DisplayFunction->(RowBox[{"\[LeftAngleBracket]",#1,#2,#3,"]"}]&),
InterpretationFunction->(RowBox[{"Chain","[","$angle",",",#1,",",#2,",",#3,",","$square","]"}]&)
];
SquareAngleChainBox[beginning_,args_,end_]:=TemplateBox[{beginning,args,end},"SquareAngleChain",
DisplayFunction->(RowBox[{"[",#1,#2,#3,"\[RightAngleBracket]"}]&),
InterpretationFunction->(RowBox[{"Chain","[","$square",",",#1,",",#2,",",#3,",","$angle","]"}]&)
];
AngleAngleChainBox[beginning_,args_,end_]:=TemplateBox[{beginning,args,end},"AngleAngleChain",
DisplayFunction->(RowBox[{"\[LeftAngleBracket]",#1,#2,#3,"\[RightAngleBracket]"}]&),
InterpretationFunction->(RowBox[{"Chain","[","$angle",",",#1,",",#2,",",#3,",","$angle","]"}]&)
];
SquareSquareChainBox[beginning_,args_,end_]:=TemplateBox[{beginning,args,end},"SquareSquareChain",
DisplayFunction->(RowBox[{"[",#1,#2,#3,"]"}]&),
InterpretationFunction->(RowBox[{"Chain","[","$square",",",#1,",",#2,",",#3,",","$square","]"}]&)
];
Chain /: MakeBoxes[Chain[$angle,x_,y_List,z_,$square],StandardForm|TraditionalForm] /;OddQ[Length[y]+2]:=AngleSquareChainBox[ToBoxes[x],ToBoxes[y],ToBoxes[z]];
Chain /: MakeBoxes[Chain[$angle,x_,y_List,z_,$angle],StandardForm|TraditionalForm] /;EvenQ[Length[y]+2]:=AngleAngleChainBox[ToBoxes[x],ToBoxes[y],ToBoxes[z]];
Chain /: MakeBoxes[Chain[$square,x_,y_List,z_,$angle],StandardForm|TraditionalForm] /;OddQ[Length[y]+2]:=SquareAngleChainBox[ToBoxes[x],ToBoxes[y],ToBoxes[z]];
Chain /: MakeBoxes[Chain[$square,x_,y_List,z_,$square],StandardForm|TraditionalForm] /;EvenQ[Length[y]+2]:=SquareSquareChainBox[ToBoxes[x],ToBoxes[y],ToBoxes[z]];


(* ::Subsection:: *)
(*ToChain*)


(*ToChain[exp_]:=Block[{localexp,MyPower,counter},
localexp=exp/.Power->MyPower;
(*First we need to group things properly in order to apply the contraction properties, since Mathematica is not able to recognize things inside powers as contractible with things outside that power...*)
If[FreeQ[localexp,MyPower]==False,
localexp=localexp/.{A_*MyPower[B_,x_]/;FreeQ[A,MyPower]:>MyPower[A,1]*MyPower[B,x]};
localexp=localexp//.MyPower[A_,x_]*MyPower[B_,x_]:>MyPower[A*B,x];
localexp=localexp/.MyPower[A_,x_?Negative]:>1/MyPower[A,-x];
localexp=localexp//.{MyPower[x_*A_,n_]*MyPower[B_,m_]/;n>m&&FreeQ[B,x]:>MyPower[x*A,n-m]MyPower[x*A*B,m]};
localexp=localexp//.{MyPower[x_,n_]*MyPower[B_,m_]/;n>m&&FreeQ[B,x]:>MyPower[x,n-m]MyPower[x*B,m]};
];

(*Now introduce the Chains and let them contract*)
localexp=localexp/.{SpinorAngleBracket[x_,y_]:>chain[$angle,x,{},y,$angle],SpinorSquareBracket[x_,y_]:>chain[$square,x,{},y,$square]};

(*Now replace the chain with Chain, where the first has all the contraction properties whereas the second does not and thus expressions stay as they are*)

localexp=localexp/.{chain->Chain};

(*Back to mathematica's Power*)
localexp=localexp/.{MyPower->Power};

(*Replace the single chains back with angle and square brackets*)
localexp=localexp/.{Chain[$angle,x_,{},y_,$angle]:>SpinorAngleBracket[x,y],Chain[$square,x_,{},y_,$square]:>SpinorSquareBracket[x,y]};

Return[localexp];
];*)


(* ::Subsection:: *)
(*ToChain*)


ToChain[exp_]:=Block[{localexp,MyPower,counter},
localexp=exp/.Power->MyPower;
(*First we need to group things properly in order to apply the contraction properties, since Mathematica is not able to recognize things inside powers as contractible with things outside that power...*)
If[FreeQ[localexp,MyPower]==False,
localexp=localexp/.{A_*MyPower[B_,x_]/;FreeQ[A,MyPower]:>MyPower[A,1]*MyPower[B,x]};
localexp=localexp//.MyPower[A_,x_]*MyPower[B_,x_]:>MyPower[A*B,x];
localexp=localexp/.MyPower[A_,x_?Negative]:>1/MyPower[A,-x];
localexp=localexp//.{MyPower[x_*A_,n_]*MyPower[B_,m_]/;n>m&&FreeQ[B,x]:>MyPower[x*A,n-m]MyPower[x*A*B,m]};
localexp=localexp//.{MyPower[x_,n_]*MyPower[B_,m_]/;n>m&&FreeQ[B,x]:>MyPower[x,n-m]MyPower[x*B,m]};
];

(*Now introduce the Chains and let them contract*)
localexp=localexp/.{SpinorAngleBracket[x_,y_]:>chain[$angle,x,{},y,$angle],SpinorSquareBracket[x_,y_]:>chain[$square,x,{},y,$square]};

(*Back to mathematica's Power*)
localexp=localexp/.{MyPower->Power};

(*Now replace the chain with Chain, where the first has all the contraction properties whereas the second does not and thus expressions stay as they are*)
localexp=localexp/.{chain->Chain};

(*Replace the single chains back with angle and square brackets*)
localexp=localexp/.{Chain[$angle,x_,{},y_,$angle]:>SpinorAngleBracket[x,y],Chain[$square,x_,{},y_,$square]:>SpinorSquareBracket[x,y]};

Return[localexp];
];


(* ::Subsection:: *)
(*mp*)


mpBox[x_,y_]:=TemplateBox[{x,y},"ScalarProduct",
DisplayFunction->(RowBox[{"(",#1,"\[CenterDot]",#2,")"}]&),
InterpretationFunction->(RowBox[{"mp","[",#1,",",#2,"]"}]&)];
mp /: MakeBoxes[mp[x_,y_],StandardForm|TraditionalForm]:=mpBox[ToBoxes[x],ToBoxes[y]];
SetAttributes[mp,{Orderless,Protected}];


(* ::Subsection:: *)
(*eps*)


epsBox[a_,b_,c_,d_]:=TemplateBox[{a,b,c,d},"eps",
DisplayFunction->(RowBox[{"\[Epsilon]","[",#1,",",#2,",",#3,",",#4,"]"}]&),
InterpretationFunction->(RowBox[{"eps","[",#1,",",#2,",",#3,",",#4,"]"}]&)
];
eps /: MakeBoxes[eps[a_,b_,c_,d_],StandardForm|TraditionalForm]:=epsBox[ToBoxes[a],ToBoxes[b],ToBoxes[c],ToBoxes[d]];

(*Contraction with twice the same vector vanishes*)
eps[x___,y_,z___,y_,k___]:=0;

SetAttributes[eps,Protected];


(* ::Subsection:: *)
(*TrG*)


TrG[x_List]/;OddQ[Length[x]]:=0;
TrG[{}]:=4;
TrG[x_List]:=Sum[(-1)^i*mp[x[[1]],x[[i]]]TrG[Delete[x,{{1},{i}}]],{i,2,Length[x]}];


(* ::Subsection:: *)
(*TrG5*)


TrG5[x_List]/;Length[x]<4:=0;
TrG5[x_List]/;OddQ[Length[x]]:=0;
TrG5[x_List]:=mp[x[[-3]],x[[-2]]]*TrG5[Delete[x,{{-3},{-2}}]]+mp[x[[-2]],x[[-1]]]*TrG5[x[[;;-3]]]-mp[x[[-3]],x[[-1]]]*TrG5[Delete[x,{{-3},{-1}}]]-I*Sum[(-1)^i*eps[x[[-i]],x[[-3]],x[[-2]],x[[-1]]]*TrG[Delete[x[[;;-4]],{-(i-3)}]],{i,4,Length[x]}];


(* ::Subsection:: *)
(*ToTrace*)


Options[ToTrace]={KillEpsilon->False}
{KillEpsilon->False}
ToTrace[exp_,OptionsPattern[]]:=Block[{eps,localexp},
Which[OptionValue[KillEpsilon],
eps[x__]:=0,
OptionValue[KillEpsilon]==False,
Null,
True,
Print["Undefined value for the option KillEpsilon. Only True or False are allowed. Proceed assuming default value False."];
];
(*Convert the chains to traces*)
localexp=exp/.Chain[$angle,a_,b_List,a_,$square]:>(TrG[Join[{a},b]]-TrG5[Join[{a},b]])/2;

(*Return output*)
Return[localexp];
];

SetAttributes[ToTrace,Protected];


(* ::Subsection:: *)
(*SumContracted*)


SumContracted[x__][exp_]:=Block[{out,vars},
vars=Sequence@@Table[{i,2},{i,{x}}];
Return[Sum[exp,Evaluate[vars]]];
];


(* ::Subsection:: *)
(*CompleteToMassive*)


(*CompleteToMassive[exp_,reps_:FixedSpinors]:=Block[{$crep2,locvar,SubCounter2,localexp,locreps,moms,SpinorUndot,SpinorDot,out,MyPower,numer,firstrep,MomLeft,MomRight,MomMassive},
(*Auxiliary function needed for the replacements later on*)
$crep2=2;
SubCounter2:=locvar*IntegerPart[$crep2++/2];

(*Pick only the relevant replacements, i.e. those concerning the \[Mu]s*)
locreps=Select[reps,(!FreeQ[#,SpinorUndotPure[_][$mu]->SpinorUndotPure[_][$lam]]||!FreeQ[#,SpinorDotPure[_][$mu]->SpinorDotPure[_][$lam]])&];

(*Extract the momenta whose \[Mu]s need to be restored*)
moms=Table[i[[1]],{i,locreps}];
moms=DeleteDuplicates[moms//.{SpinorUndotPure[x_][_]:>x,SpinorDotPure[x_][_]:>x}];

(*Convert the whole expression into single spinors, making also use of the auxiliary function SubCounter defined along with SpinorReplace. Before doing this we split powers into single objects to get a fully correct replacement.*)
localexp=exp//.Power[A_,n_?Positive]:>Product[MyPower[A,i],{i,n}];

localexp=localexp/.{
SpinorAngleBracket[x_,y_]/;TrueQ[Head[x]==OverBar&&Head[y]==OverBar]:> SpinorUndot[x][$mu][SubCounter2][Null]*SpinorUndot[y][$mu][Null][SubCounter2],SpinorSquareBracket[x_,y_]/;TrueQ[Head[x]==OverBar&&Head[y]==OverBar]:>SpinorDot[x][$mu][Null][SubCounter2]*SpinorDot[y][$mu][SubCounter2][Null],
SpinorAngleBracket[x_,y_]/;TrueQ[Head[y]==OverBar]:> SpinorUndot[x][$lam][SubCounter2][Null]*SpinorUndot[y][$mu][Null][SubCounter2],SpinorSquareBracket[x_,y_]/;TrueQ[Head[y]==OverBar]:>SpinorDot[x][$lam][Null][SubCounter2]*SpinorDot[y][$mu][SubCounter2][Null],SpinorAngleBracket[x_,y_]:> SpinorUndot[x][$lam][SubCounter2][Null]*SpinorUndot[y][$lam][Null][SubCounter2],SpinorSquareBracket[x_,y_]:>SpinorDot[x][$lam][Null][SubCounter2]*SpinorDot[y][$lam][SubCounter2][Null]};
localexp=localexp/.{SpinorUndot[OverBar[x_]][$mu][y_][z_]:>SpinorUndot[x][$mu][y][z],SpinorDot[OverBar[x_]][$mu][y_][z_]:>SpinorDot[x][$mu][y][z]};
(*Now we can safely remove MyPower*)
localexp=localexp//.MyPower[A_,x_]:>A;

(*Now replace the massless momenta |p>[p| with a massive momentum plus the \[Mu] piece. The massive momentum cannot be written any more in terms of spinors so will appear only inside a chain object.*)

(*localexp=localexp//Expand;*)
(*Now we define a bunch of objects. These will allow us to do the replecements in a more efficient way*)
MomLeft /: Times[MomLeft[mom_][x_][y_],MomRight[mom_][z_][k_]]:=MomMassive[{mom},$angle,$square][x,z][y,k]+extramass[mom]*extramasstilde[mom]*SpinorUndot[mom][$mu][x][y]*SpinorDot[mom][$mu][z][k]/(SpinorAngleBracket[mom,overbar[mom]]SpinorSquareBracket[mom,overbar[mom]]);
(*Now we define the contraction properties of MomMassive with itself*)
MomMassive /: Times[MomMassive[x_List,$angle,$square][a_,adot_][Null,bdot_],MomMassive[y_List,$angle,$square][Null,cdot_][a_,ddot_]]:=MomMassive[Join[x,y],$square,$square][adot,cdot][bdot,ddot];
MomMassive /: Times[MomMassive[x_List,$angle,$square][a_,Null][b_,bdot_],MomMassive[y_List,$angle,$square][c_,bdot_][d_,Null]]:=MomMassive[Join[x,y],$angle,$angle][a,c][b,d];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][a1_,a2_][b1_,Null],MomMassive[y_List,$angle,type_][Null,cdot_][a2_,ddot_]]:=MomMassive[Join[x,y],$angle,type][a1,cdot][b1,ddot];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][Null,a2_][c_,b2_],MomMassive[y_List,type_,$angle][cdot_,c_][ddot_,Null]]:=MomMassive[Join[y,x],type,$angle][cdot,a2][ddot,b2];
MomMassive /: Times[MomMassive[x_List,$square,$square][a1_,a2_][b1_,Null],MomMassive[y_List,$square,type_][Null,cdot_][a2_,ddot_]]:=-MomMassive[Join[x,y],$square,type][a1,cdot][b1,ddot];
MomMassive /: Times[MomMassive[x_List,$square,$square][Null,a2_][c_,b2_],MomMassive[y_List,type_,$square][cdot_,c_][ddot_,Null]]:=-MomMassive[Join[y,x],type,$square][cdot,a2][ddot,b2];

Do[
localexp=localexp/.{SpinorUndot[i][$lam][x_][y_]:>MomLeft[i][x][y],SpinorDot[i][$lam][x_][y_]:>MomRight[i][x][y]};
localexp=localexp/.{MomLeft[i][x_][y_]:>SpinorUndot[i][$lam][x][y],MomRight[i][x_][y_]:>SpinorDot[i][$lam][x][y]};
,{i,moms}];


(*Now we replace the resulting \[Mu]s with the chosen \[Lambda]s again*)
localexp=SpinorReplaceSequential[localexp,locreps];

(*Finally we define the contraction properties of MomMassive with angle and square spinors*)

MomMassive /: Times[MomMassive[x_List,$angle,$angle][a1_,a2_][Null,Null],SpinorUndot[lab_][$lam][Null][a1_],SpinorUndot[lab2_][$lam][Null][a2_]]:=-chain[$angle,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][Null,Null][a1_,a2_],SpinorUndot[lab_][$lam][a1_][Null],SpinorUndot[lab2_][$lam][a2_][Null]]:=-chain[$angle,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][a1_,Null][Null,a2_],SpinorUndot[lab_][$lam][Null][a1_],SpinorUndot[lab2_][$lam][a2_][Null]]:=chain[$angle,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][Null,a2_][a1_,Null],SpinorUndot[lab_][$lam][a1_][Null],SpinorUndot[lab2_][$lam][Null][a2_]]:=chain[$angle,lab,x,lab2,$angle];

MomMassive /: Times[MomMassive[x_List,$square,$square][a1_,a2_][Null,Null],SpinorDot[lab_][$lam][Null][a1_],SpinorDot[lab2_][$lam][Null][a2_]]:=-chain[$square,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$square,$square][Null,Null][a1_,a2_],SpinorDot[lab_][$lam][a1_][Null],SpinorDot[lab2_][$lam][a2_][Null]]:=-chain[$square,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$square,$square][a1_,Null][Null,a2_],SpinorDot[lab_][$lam][Null][a1_],SpinorDot[lab2_][$lam][a2_][Null]]:=chain[$square,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$square,$square][Null,a2_][a1_,Null],SpinorDot[lab_][$lam][a1_][Null],SpinorDot[lab2_][$lam][Null][a2_]]:=chain[$square,lab,x,lab2,$square];

MomMassive /: Times[MomMassive[x_List,$square,$angle][a1_,a2_][Null,Null],SpinorDot[lab_][$lam][Null][a1_],SpinorUndot[lab2_][$lam][Null][a2_]]:=+chain[$square,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$square,$angle][Null,Null][a1_,a2_],SpinorDot[lab_][$lam][a1_][Null],SpinorUndot[lab2_][$lam][a2_][Null]]:=+chain[$square,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$square,$angle][a1_,Null][Null,a2_],SpinorDot[lab_][$lam][Null][a1_],SpinorUndot[lab2_][$lam][a2_][Null]]:=-chain[$square,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$square,$angle][Null,a2_][a1_,Null],SpinorDot[lab_][$lam][a1_][Null],SpinorUndot[lab2_][$lam][Null][a2_]]:=-chain[$square,lab,x,lab2,$angle];

MomMassive /: Times[MomMassive[x_List,$angle,$square][a1_,a2_][Null,Null],SpinorUndot[lab_][$lam][Null][a1_],SpinorDot[lab2_][$lam][Null][a2_]]:=+chain[$angle,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$angle,$square][Null,Null][a1_,a2_],SpinorUndot[lab_][$lam][a1_][Null],SpinorDot[lab2_][$lam][a2_][Null]]:=+chain[$angle,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$angle,$square][a1_,Null][Null,a2_],SpinorUndot[lab_][$lam][Null][a1_],SpinorDot[lab2_][$lam][a2_][Null]]:=-chain[$angle,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$angle,$square][Null,a2_][a1_,Null],SpinorUndot[lab_][$lam][a1_][Null],SpinorDot[lab2_][$lam][Null][a2_]]:=-chain[$angle,lab,x,lab2,$square];

(*Just to make sure all the properties have been applied*)
out=localexp//Expand;

(*Return the output*)
Return[out];
];*)


(* ::Subsection:: *)
(*CompleteToMassive*)


(*MomMassive is an auxiliary function for CompleteToMassive*)

(*Now we define the contraction properties of MomMassive with itself*)
MomMassive /: Times[MomMassive[x_List,$angle,$square][a_,adot_][Null,bdot_],MomMassive[y_List,$angle,$square][Null,cdot_][a_,ddot_]]:=MomMassive[Join[x,y],$square,$square][adot,cdot][bdot,ddot];
MomMassive /: Times[MomMassive[x_List,$angle,$square][a_,Null][b_,bdot_],MomMassive[y_List,$angle,$square][c_,bdot_][d_,Null]]:=MomMassive[Join[x,y],$angle,$angle][a,c][b,d];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][a1_,a2_][b1_,Null],MomMassive[y_List,$angle,type_][Null,cdot_][a2_,ddot_]]:=MomMassive[Join[x,y],$angle,type][a1,cdot][b1,ddot];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][Null,a2_][c_,b2_],MomMassive[y_List,type_,$angle][cdot_,c_][ddot_,Null]]:=MomMassive[Join[y,x],type,$angle][cdot,a2][ddot,b2];
MomMassive /: Times[MomMassive[x_List,$square,$square][a1_,a2_][b1_,Null],MomMassive[y_List,$square,type_][Null,cdot_][a2_,ddot_]]:=-MomMassive[Join[x,y],$square,type][a1,cdot][b1,ddot];
MomMassive /: Times[MomMassive[x_List,$square,$square][Null,a2_][c_,b2_],MomMassive[y_List,type_,$square][cdot_,c_][ddot_,Null]]:=-MomMassive[Join[y,x],type,$square][cdot,a2][ddot,b2];

(*Finally we define the contraction properties of MomMassive with angle and square spinors*)

MomMassive /: Times[MomMassive[x_List,$angle,$angle][a1_,a2_][Null,Null],SpinorUndot[lab_][$lam][Null][a1_],SpinorUndot[lab2_][$lam][Null][a2_]]:=-chain[$angle,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][Null,Null][a1_,a2_],SpinorUndot[lab_][$lam][a1_][Null],SpinorUndot[lab2_][$lam][a2_][Null]]:=-chain[$angle,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][a1_,Null][Null,a2_],SpinorUndot[lab_][$lam][Null][a1_],SpinorUndot[lab2_][$lam][a2_][Null]]:=chain[$angle,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$angle,$angle][Null,a2_][a1_,Null],SpinorUndot[lab_][$lam][a1_][Null],SpinorUndot[lab2_][$lam][Null][a2_]]:=chain[$angle,lab,x,lab2,$angle];

MomMassive /: Times[MomMassive[x_List,$square,$square][a1_,a2_][Null,Null],SpinorDot[lab_][$lam][Null][a1_],SpinorDot[lab2_][$lam][Null][a2_]]:=-chain[$square,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$square,$square][Null,Null][a1_,a2_],SpinorDot[lab_][$lam][a1_][Null],SpinorDot[lab2_][$lam][a2_][Null]]:=-chain[$square,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$square,$square][a1_,Null][Null,a2_],SpinorDot[lab_][$lam][Null][a1_],SpinorDot[lab2_][$lam][a2_][Null]]:=chain[$square,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$square,$square][Null,a2_][a1_,Null],SpinorDot[lab_][$lam][a1_][Null],SpinorDot[lab2_][$lam][Null][a2_]]:=chain[$square,lab,x,lab2,$square];

MomMassive /: Times[MomMassive[x_List,$square,$angle][a1_,a2_][Null,Null],SpinorDot[lab_][$lam][Null][a1_],SpinorUndot[lab2_][$lam][Null][a2_]]:=+chain[$square,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$square,$angle][Null,Null][a1_,a2_],SpinorDot[lab_][$lam][a1_][Null],SpinorUndot[lab2_][$lam][a2_][Null]]:=+chain[$square,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$square,$angle][a1_,Null][Null,a2_],SpinorDot[lab_][$lam][Null][a1_],SpinorUndot[lab2_][$lam][a2_][Null]]:=-chain[$square,lab,x,lab2,$angle];
MomMassive /: Times[MomMassive[x_List,$square,$angle][Null,a2_][a1_,Null],SpinorDot[lab_][$lam][a1_][Null],SpinorUndot[lab2_][$lam][Null][a2_]]:=-chain[$square,lab,x,lab2,$angle];

MomMassive /: Times[MomMassive[x_List,$angle,$square][a1_,a2_][Null,Null],SpinorUndot[lab_][$lam][Null][a1_],SpinorDot[lab2_][$lam][Null][a2_]]:=+chain[$angle,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$angle,$square][Null,Null][a1_,a2_],SpinorUndot[lab_][$lam][a1_][Null],SpinorDot[lab2_][$lam][a2_][Null]]:=+chain[$angle,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$angle,$square][a1_,Null][Null,a2_],SpinorUndot[lab_][$lam][Null][a1_],SpinorDot[lab2_][$lam][a2_][Null]]:=-chain[$angle,lab,x,lab2,$square];
MomMassive /: Times[MomMassive[x_List,$angle,$square][Null,a2_][a1_,Null],SpinorUndot[lab_][$lam][a1_][Null],SpinorDot[lab2_][$lam][Null][a2_]]:=-chain[$angle,lab,x,lab2,$square];

CompleteToMassive[exp_,reps_:FixedSpinors]:=Block[{$crep2,locvar,SubCounter2,localexp,locreps,moms,numtot,dentot,out,MyPower,SpinorUndot2,SpinorDot2,numer,firstrep,MomLeft,MomRight},

(*Auxiliary function needed for the replacements later on*)
$crep2=2;
SubCounter2:=locvar*IntegerPart[$crep2++/2];

(*Pick only the relevant replacements, i.e. those concerning the \[Mu]s*)
locreps=Select[reps,(!FreeQ[#,SpinorUndotPure[_][$mu]->SpinorUndotPure[_][$lam]]||!FreeQ[#,SpinorDotPure[_][$mu]->SpinorDotPure[_][$lam]])&];

(*Extract the momenta whose \[Mu]s need to be restored*)
moms=Table[i[[1]],{i,locreps}];
moms=DeleteDuplicates[moms//.{SpinorUndotPure[x_][_]:>x,SpinorDotPure[x_][_]:>x}];

(*Now we are going to separate numerator and denominator*)

localexp=Together[exp]//CompleteMandelstam;
numtot={Numerator[localexp]//Expand}/.{Plus->List}//Flatten;
dentot=Denominator[localexp];

(*Convert the whole expression into single spinors, making also use of the auxiliary function SubCounter defined along with SpinorReplace. Before doing this we split powers into single objects to get a fully correct replacement.*)
numtot=numtot//.Power[A_,n_?Positive]:>Product[MyPower[A,i],{i,n}];

numtot=numtot/.{
SpinorAngleBracket[x_,y_]/;TrueQ[Head[x]==OverBar&&Head[y]==OverBar]:> SpinorUndot2[x][$mu][SubCounter2][Null]*SpinorUndot2[y][$mu][Null][SubCounter2],SpinorSquareBracket[x_,y_]/;TrueQ[Head[x]==OverBar&&Head[y]==OverBar]:>SpinorDot2[x][$mu][Null][SubCounter2]*SpinorDot2[y][$mu][SubCounter2][Null],
SpinorAngleBracket[x_,y_]/;TrueQ[Head[y]==OverBar]:> SpinorUndot2[x][$lam][SubCounter2][Null]*SpinorUndot2[y][$mu][Null][SubCounter2],SpinorSquareBracket[x_,y_]/;TrueQ[Head[y]==OverBar]:>SpinorDot2[x][$lam][Null][SubCounter2]*SpinorDot2[y][$mu][SubCounter2][Null],SpinorAngleBracket[x_,y_]:> SpinorUndot2[x][$lam][SubCounter2][Null]*SpinorUndot2[y][$lam][Null][SubCounter2],SpinorSquareBracket[x_,y_]:>SpinorDot2[x][$lam][Null][SubCounter2]*SpinorDot2[y][$lam][SubCounter2][Null]};
numtot=numtot/.{SpinorUndot2[OverBar[x_]][$mu][y_][z_]:>SpinorUndot2[x][$mu][y][z],SpinorDot2[OverBar[x_]][$mu][y_][z_]:>SpinorDot2[x][$mu][y][z]};
(*Now we can safely remove MyPower*)
numtot=numtot//.MyPower[A_,x_]:>A;

(*Now replace the massless momenta |p>[p| with a massive momentum plus the \[Mu] piece. The massive momentum cannot be written any more in terms of spinors so will appear only inside a chain object.*)

(*Now we define a bunch of objects. These will allow us to do the replecements in a more efficient way*)
MomLeft /: Times[MomLeft[mom_][x_][y_],MomRight[mom_][z_][k_]]:=MomMassive[{mom},$angle,$square][x,z][y,k]+extramass[mom]*extramasstilde[mom]*SpinorUndot[mom][$mu][x][y]*SpinorDot[mom][$mu][z][k]/(SpinorAngleBracket[mom,overbar[mom]]SpinorSquareBracket[mom,overbar[mom]]);

Do[
numtot=numtot/.{SpinorUndot2[i][$lam][x_][y_]:>MomLeft[i][x][y],SpinorDot2[i][$lam][x_][y_]:>MomRight[i][x][y]};
numtot=numtot/.{MomLeft[i][x_][y_]:>SpinorUndot2[i][$lam][x][y],MomRight[i][x_][y_]:>SpinorDot2[i][$lam][x][y]};
,{i,moms}];


(*Now we replace the four-dimensional Mandelstam invariants with six-dimensional ones*)

numtot=numtot/.{S4[x1_,x2_]:>S[x1,x2]+extramass[x2]*extramasstilde[x1]+extramass[x1]*extramasstilde[x2]-(extramass[x1]*extramasstilde[x1] Spinoranglebracket[x2,\!\(\*OverscriptBox[\(x1\), \(_\)]\)] Spinorsquarebracket[x2,\!\(\*OverscriptBox[\(x1\), \(_\)]\)])/(Spinoranglebracket[x1,\!\(\*OverscriptBox[\(x1\), \(_\)]\)] Spinorsquarebracket[x1,\!\(\*OverscriptBox[\(x1\), \(_\)]\)])-(extramass[x2]*extramasstilde[x2] Spinoranglebracket[x1,\!\(\*OverscriptBox[\(x2\), \(_\)]\)] Spinorsquarebracket[x1,\!\(\*OverscriptBox[\(x2\), \(_\)]\)])/(Spinoranglebracket[x2,\!\(\*OverscriptBox[\(x2\), \(_\)]\)] Spinorsquarebracket[x2,\!\(\*OverscriptBox[\(x2\), \(_\)]\)])+(extramass[x1]*extramasstilde[x1]extramass[x2]*extramasstilde[x2] Spinoranglebracket[\!\(\*OverscriptBox[\(x1\), \(_\)]\),\!\(\*OverscriptBox[\(x2\), \(_\)]\)] Spinorsquarebracket[\!\(\*OverscriptBox[\(x1\), \(_\)]\),\!\(\*OverscriptBox[\(x2\), \(_\)]\)])/(Spinoranglebracket[x1,\!\(\*OverscriptBox[\(x1\), \(_\)]\)] Spinoranglebracket[x2,\!\(\*OverscriptBox[\(x2\), \(_\)]\)] Spinorsquarebracket[x1,\!\(\*OverscriptBox[\(x1\), \(_\)]\)] Spinorsquarebracket[x2,\!\(\*OverscriptBox[\(x2\), \(_\)]\)])};

(*Now we replace the resulting \[Mu]s with the chosen \[Lambda]s again*)
numtot=SpinorReplaceSequential[numtot,locreps];

(*Just to make sure all the properties have been applied*)
numtot=numtot/.{SpinorUndot2->SpinorUndot,SpinorDot2->SpinorDot}//Expand;

(*Make numtot a sum again*)
If[Length[numtot]==1,
numtot=First[numtot],
numtot=Plus@@numtot;
];

(*Return the output*)
Return[numtot/dentot];
];


(* ::Subsection:: *)
(*Relabel*)


Options[Relabel]={LabelRep->{},ScalarProduct->True};

Relabel[exp_,OptionsPattern[]]:=Module[{localexp,funrep,labrep,labrepneg,sp},
(*Divide the replacements in two cathegories: the ines acting on labesl and the ones acting on the entire functions S[__] or mp[_,_]*)

funrep=Select[OptionValue[LabelRep],(!FreeQ[#,S[__]]||!FreeQ[#,mp[_,_]]||!FreeQ[#,S4[__]])&];

(*Label replacements are harder to deal with since they may be simple numbers,so they need to be separated and treated more carefully. In order to do these replacemenst properly we transfor the labels of all the functions into strings. This allows us to distinguish for example the label 2 from the number 2 in 2*l1 where l1 is the label. This has the drawback that we also need to duplicate the replacements by introducing a set of negative labels as well. This seems to be the safest way of doing things.*)

labrep=Select[OptionValue[LabelRep],(FreeQ[#,S[__]]&&FreeQ[#,mp[_,_]]&&FreeQ[#,S4[__]])&];
labrepneg=Table[-i[[1]]->-i[[2]],{i,labrep}];
labrep=Table[ToString[i[[1]]]->ToString[i[[2]]],{i,labrep}];
labrepneg=Table[ToString[i[[1]]]->ToString[i[[2]]],{i,labrepneg}];

(*Convert all the labels to strings*)
localexp=exp/.{S[x__]:>S[Sequence@@ToString/@{x}],S4[x__]:>S4[Sequence@@ToString/@{x}],mp[x__]:>mp[Sequence@@ToString/@{x}]};
(*Apply the label replacements*)
(*localexp=localexp/.f_[x__]\[RuleDelayed]f[StringReplace[#,Join[labrep,labrepneg]]&/@{x}];*)
localexp=localexp/.Join[labrep,labrepneg];
(*Convert labels back to symbols*)
localexp=localexp/.{S[x__]:>S[Sequence@@ToExpression/@{x}],S4[x__]:>S4[Sequence@@ToExpression/@{x}],mp[x__]:>mp[Sequence@@ToExpression/@{x}]};
(*Apply the functional replacements*)
localexp=localexp/.funrep;

(*Finally convert the scalar product if a new one has been defined*)

If[!TrueQ[OptionValue[ScalarProduct]],
sp=OptionValue[ScalarProduct];
localexp=localexp/.{mp[x_,y_]:>sp[x,y],S4[x_,y_]:>sp[x+y,x+y],S[x_,y_]:>sp[x+y,x+y]};
];

Return[localexp];
];


(* ::Section:: *)
(*Symbolic calculus*)


(* ::Subsection:: *)
(*Fstrength*)


Fstrength[label_][A_,B_][C_,D_][lg1_Symbol,lg2_Symbol]:=2*Antisymmetrize[MDelta[4][A ][C]SpinorUndot6D[label ][B ][Null][lg1]SpinorDot6D[label ][D ][Null][lg2],{A,B},{C,D}];

(*Boxing function*)
FstrengthBox[mom_,AA_,BB_,CC_,DD_,lg1_,lg2_]:=TemplateBox[{mom,AA,BB,CC,DD,lg1,lg2},"Fstrength",
DisplayFunction->(RowBox[{SubscriptBox[SuperscriptBox["F",RowBox[{#2,#3}]],RowBox[{#4,#5}]],"[",SubscriptBox[#1,RowBox[{#6,#7}]],"]"}]&),
InterpretationFunction->(RowBox[{"Fstrength[",#1,"][",#2,",",#3,"][",#4,",",#5,"][",#6,",",#7,"]"}]&)
];

Fstrength /: MakeBoxes[Fstrength[mom_][AA_,BB_][CC_,DD_][lg1_Symbol,lg2_Symbol],StandardForm|TraditionalForm]:=FstrengthBox[ToBoxes[mom],ToBoxes[AA],ToBoxes[BB],ToBoxes[CC],ToBoxes[DD],ToBoxes[lg1],ToBoxes[lg2]];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "fst" -> FstrengthBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection:: *)
(*MDelta*)


(*Properties of MDelta*)
(*Trace*)
MDelta[dim_Integer][up1_][down1_] /;TrueQ[up1==down1]:=dim;

(*Kronecker delta for integers*)
MDelta[dim_Integer][up1_Integer][down1_Integer]:=If[TrueQ[up1==down1],1,0];

(*Contraction of two deltas*)
MDelta /: Times[MDelta[dim_][up1_][down1_],MDelta[dim_][down1_][down2_]]:=MDelta[dim][up1][down2];

(*Define contractions with SpinorDot*)

MDelta /: Times[MDelta[4][x_][y_],SpinorDot6D[label_][y_][lg1_][lg2_]]:=SpinorDot6D[label][x][lg1][lg2];
MDelta /: Times[MDelta[4][x_][y_],SpinorDot6D[label_][x_][lg1_][lg2_]]:=SpinorDot6D[label][y][lg1][lg2];
MDelta /: Times[MDelta[4][x_][y_],SpinorUndot6D[label_][y_][lg1_][lg2_]]:=SpinorUndot6D[label][x][lg1][lg2];
MDelta /: Times[MDelta[4][x_][y_],SpinorUndot6D[label_][x_][lg1_][lg2_]]:=SpinorUndot6D[label][y][lg1][lg2];

(*Proper visualization through custom box function*)
MDeltaBox[dim_,up_,down_]:=TemplateBox[{up,down,dim},"MDelta",
DisplayFunction->(RowBox[{SubscriptBox[SuperscriptBox["\[Delta]",#1],#2]}]&),
InterpretationFunction->(RowBox[{"MDelta","[",#3,"][",#1,"][",#2,"]"}]&)
];

(*Define the action of MakeBoxes on MDelta*)
MDelta /: MakeBoxes[MDelta[dim_][up_][down_],StandardForm|TraditionalForm]:=MDeltaBox[ToBoxes[dim],ToBoxes[up],ToBoxes[down]];

(*Definion of the default dimension of the delta*)
$MDimension=4;

(*Shortcuts*)
If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "md" -> MDeltaBox[$MDimension,"\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection:: *)
(*Antisymmetrize*)


SetAttributes[Antisymmetrize,HoldFirst];

Antisymmetrize[exp_,indices__List]:=Block[{canonicalsign,len,combos,localexp,output,localindices,nsubs},
(*Start by counting the number of different lists given as input*)
localindices={indices};
nsubs=Length[localindices];

output=HoldForm[exp];

(*Now we loop over every single list of antisymmetrized indices*)
Do[
(*Since mathematica has its own canonical ordering we need to take into account that what we started from may already have a minus sign. So we simply use the starting lists sign as normalization*)
canonicalsign=Signature[localindices[[k]]];

(*Now generate all the possible reshuffelings of the indices in list*)
len=Length[localindices[[k]]];
combos=Select[DeleteDuplicates[DeleteDuplicates/@Tuples[Table[localindices[[k]],{j,len}]]],(Length[#]==len)&];
debugPrint[combos];

(*Now do the replacements in the expression*)
localexp=output;
output={};
Do[AppendTo[output,(Signature[combos[[i]]]/canonicalsign)*localexp/.Table[localindices[[k,j]]->combos[[i,j]],{j,len}]],{i,Length[combos]}];
output=(1/(len!))*Plus@@output//Expand;
,{k,nsubs}];

(*Before returning the result release the hold on the expression*)
output=ReleaseHold[output];
Return[output];
];


(* ::Subsection:: *)
(*ContractReplace*)


ContractReplace[exp_]:=Block[{locexp,angsqurep,squangrep,angsqurepdot,squangrepdot,backangsqu,backsquang},
(*Just an enourmous and probably unefficient list of replacements coming with appropriate minus signs...*)
angsqurep={Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;OrderedQ[{x,z}]&&x!=z:>AngSquInvariant[x,y][{a,b},adot]*AngSquInvariant[z,k][{a,b},bdot],Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;OrderedQ[{x,z}]==False:>-AngSquInvariant[x,y][{a,b},adot]*AngSquInvariant[z,k][{a,b},bdot],Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;TrueQ[x==z]&&OrderedQ[{y,k}]&&y!=k:>AngSquInvariant[x,y][{a,b},adot]*AngSquInvariant[z,k][{a,b},bdot],
Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;TrueQ[x==z]&&OrderedQ[{y,k}]==False:>-AngSquInvariant[x,y][{a,b},adot]*AngSquInvariant[z,k][{a,b},bdot],
Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;TrueQ[x==z]&&TrueQ[y==k]&&OrderedQ[{adot,bdot}]:>AngSquInvariant[x,y][{a,b},adot]*AngSquInvariant[z,k][{a,b},bdot],
Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;TrueQ[x==z]&&TrueQ[y==k]&&OrderedQ[{adot,bdot}]==False:>-AngSquInvariant[x,y][{a,b},adot]*AngSquInvariant[z,k][{a,b},bdot]};
angsqurepdot={Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;OrderedQ[{y,k}]&&y!=k:>AngSquInvariant[x,y][a,{adot,bdot}]*AngSquInvariant[z,k][b,{adot,bdot}],
Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;OrderedQ[{y,k}]==False:>-AngSquInvariant[x,y][a,{adot,bdot}]*AngSquInvariant[z,k][b,{adot,bdot}],
Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;TrueQ[y==k]&&OrderedQ[{x,z}]&&x!=z:>AngSquInvariant[x,y][a,{adot,bdot}]*AngSquInvariant[z,k][b,{adot,bdot}],
Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;TrueQ[y==k]&&OrderedQ[{x,z}]==False:>-AngSquInvariant[x,y][a,{adot,bdot}]*AngSquInvariant[z,k][b,{adot,bdot}],
Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;TrueQ[y==k]&&TrueQ[x==z]&&OrderedQ[{a,b}]:>AngSquInvariant[x,y][a,{adot,bdot}]*AngSquInvariant[z,k][b,{adot,bdot}],
Times[AngSquInvariant[x_,y_][a_,adot_],AngSquInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;TrueQ[y==k]&&TrueQ[x==z]&&OrderedQ[{a,b}]==False:>-AngSquInvariant[x,y][a,{adot,bdot}]*AngSquInvariant[z,k][b,{adot,bdot}]};
squangrep={Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;OrderedQ[{x,z}]&&x!=z:>SquAngInvariant[x,y][{a,b},adot]*SquAngInvariant[z,k][{a,b},bdot],Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;OrderedQ[{x,z}]==False:>-SquAngInvariant[x,y][{a,b},adot]*SquAngInvariant[z,k][{a,b},bdot],Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;TrueQ[x==z]&&OrderedQ[{y,k}]&&y!=k:>SquAngInvariant[x,y][{a,b},adot]*SquAngInvariant[z,k][{a,b},bdot],
Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;TrueQ[x==z]&&OrderedQ[{y,k}]==False:>-SquAngInvariant[x,y][{a,b},adot]*SquAngInvariant[z,k][{a,b},bdot],
Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;TrueQ[x==z]&&TrueQ[y==k]&&OrderedQ[{adot,bdot}]:>SquAngInvariant[x,y][{a,b},adot]*SquAngInvariant[z,k][{a,b},bdot],
Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[a_,b_]]/;TrueQ[x==z]&&TrueQ[y==k]&&OrderedQ[{adot,bdot}]==False:>-SquAngInvariant[x,y][{a,b},adot]*SquAngInvariant[z,k][{a,b},bdot]};
squangrepdot={Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;OrderedQ[{y,k}]&&y!=k:>SquAngInvariant[x,y][a,{adot,bdot}]*SquAngInvariant[z,k][b,{adot,bdot}],
Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;OrderedQ[{y,k}]==False:>-SquAngInvariant[x,y][a,{adot,bdot}]*SquAngInvariant[z,k][b,{adot,bdot}],
Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;TrueQ[y==k]&&OrderedQ[{x,z}]&&x!=z:>SquAngInvariant[x,y][a,{adot,bdot}]*SquAngInvariant[z,k][b,{adot,bdot}],
Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;TrueQ[y==k]&&OrderedQ[{x,z}]==False:>-SquAngInvariant[x,y][a,{adot,bdot}]*SquAngInvariant[z,k][b,{adot,bdot}],
Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;TrueQ[y==k]&&TrueQ[x==z]&&OrderedQ[{a,b}]:>SquAngInvariant[x,y][a,{adot,bdot}]*SquAngInvariant[z,k][b,{adot,bdot}],
Times[SquAngInvariant[x_,y_][a_,adot_],SquAngInvariant[z_,k_][b_,bdot_],levicivita2Up[adot_,bdot_]]/;TrueQ[y==k]&&TrueQ[x==z]&&OrderedQ[{a,b}]==False:>-SquAngInvariant[x,y][a,{adot,bdot}]*SquAngInvariant[z,k][b,{adot,bdot}]};
locexp=exp//.Join[angsqurep,squangrep,angsqurepdot,squangrepdot];
(*Now the expression will be simplified and then we backsubstitute*)
backangsqu={Times[AngSquInvariant[x_,y_][{a_,b_},adot_],AngSquInvariant[z_,k_][{a_,b_},bdot_]]:>AngSquInvariant[x,y][a,adot]*AngSquInvariant[z,k][b,bdot]*levicivita2Up[a,b],Times[AngSquInvariant[x_,y_][a_,{adot_,bdot_}],AngSquInvariant[z_,k_][b_,{adot_,bdot_}]]:>AngSquInvariant[x,y][a,adot]*AngSquInvariant[z,k][b,bdot]*levicivita2Up[adot,bdot]};
backsquang={Times[SquAngInvariant[x_,y_][{a_,b_},adot_],SquAngInvariant[z_,k_][{a_,b_},bdot_]]:>SquAngInvariant[x,y][a,adot]*SquAngInvariant[z,k][b,bdot]*levicivita2Up[a,b],Times[SquAngInvariant[x_,y_][a_,{adot_,bdot_}],SquAngInvariant[z_,k_][b_,{adot_,bdot_}]]:>SquAngInvariant[x,y][a,adot]*SquAngInvariant[z,k][b,bdot]*levicivita2Up[adot,bdot]};
locexp=Expand[Simplify[locexp]];
locexp=locexp//.Join[backsquang,backangsqu];
Return[locexp];

];


(* ::Subsection:: *)
(*Computing F with Lorentz indices*)


FstBoxup[mom_,mu_,nu_]:=TemplateBox[{mom,mu,nu},"Fstup",
DisplayFunction->(RowBox[{SuperscriptBox["F",RowBox[{#2,#3}]],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Fst","[",#1,"][",#2,",",#3,"][Null,Null]"}]&)
];
FstBoxdown[mom_,mu_,nu_]:=TemplateBox[{mom,mu,nu},"Fstdown",
DisplayFunction->(RowBox[{SubscriptBox["F",RowBox[{#2,#3}]],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Fst","[",#1,"][Null,Null][",#2,",",#3,"]"}]&)
];
Fst /: MakeBoxes[Fst[mom_][mu_,nu_][Null,Null],StandardForm|TraditionalForm]:=FstBoxup[ToBoxes[mom],ToBoxes[mu],ToBoxes[nu]];
Fst /: MakeBoxes[Fst[mom_][Null,Null][mu_,nu_],StandardForm|TraditionalForm]:=FstBoxdown[ToBoxes[mom],ToBoxes[mu],ToBoxes[nu]];
Fst[mom_][mu_,nu_][Null,Null]:=Mom[mom][mu][Null]Polar[mom,ToExpression["ref"<>ToString[mom]]][nu][Null]-Mom[mom][nu][Null]Polar[mom,ToExpression["ref"<>ToString[mom]]][mu][Null];

EtaBoxup[mu_,nu_]:=TemplateBox[{mu,nu},"etaup",
DisplayFunction->(SuperscriptBox["\[Eta]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"Eta","[",#1,",",#2,"][$up]"}]&)
];
EtaBoxdown[mu_,nu_]:=TemplateBox[{mu,nu},"etadown",
DisplayFunction->(SubscriptBox["\[Eta]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"Eta","[",#1,",",#2,"][down$]"}]&)
];
Eta /: MakeBoxes[Eta[mu_,nu_][$up],StandardForm|TraditionalForm]:=EtaBoxup[ToBoxes[mu],ToBoxes[nu]];
Eta /: MakeBoxes[Eta[mu_,nu_][$down],StandardForm|TraditionalForm]:=EtaBoxdown[ToBoxes[mu],ToBoxes[nu]];

PolarBoxup[mom_,refmom_,mu_]:=TemplateBox[{mom,refmom,mu},"Polarup",
DisplayFunction->(RowBox[{SuperscriptBox["\[CurlyEpsilon]",#3],"[",#1,",",#2,"]"}]&),
InterpretationFunction->(RowBox[{"Polar","[",#1,",",#2,"]["#3,"][Null]"}]&)
];
PolarBoxdown[mom_,refmom_,mu_]:=TemplateBox[{mom,refmom,mu},"Polardown",
DisplayFunction->(RowBox[{SubscriptBox["\[CurlyEpsilon]",#3],"[",#1,",",#2,"]"}]&),
InterpretationFunction->(RowBox[{"Polar","[",#1,",",#2,"][Null["#3,"]]"}]&)
];
PolarPureBox[mom_,refmom_]:=TemplateBox[{mom,refmom},"PolarPure",
DisplayFunction->(RowBox[{"\[CurlyEpsilon]","[",#1,",",#2,"]"}]&),
InterpretationFunction->(RowBox[{"PolarPure","[",#1,","#2,"]"}]&)
];
Polar /: MakeBoxes[Polar[mom_,refmom_][mu_][Null],TraditionalForm|StandardForm]:=PolarBoxup[ToBoxes[mom],ToBoxes[refmom],ToBoxes[mu]];
Polar /: MakeBoxes[Polar[mom_,refmom_][Null][mu_],TraditionalForm|StandardForm]:=PolarBoxdown[ToBoxes[mom],ToBoxes[refmom],ToBoxes[mu]];
PolarPure /: MakeBoxes[PolarPure[mom_,refmom_],StandardForm|TraditionalForm]:=PolarPureBox[ToBoxes[mom],ToBoxes[refmom]];

MomBoxup[mom_,mu_]:=TemplateBox[{mom,mu},"Momup",
DisplayFunction->(RowBox[{SuperscriptBox["p",#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Mom","[",#1,"][",#2,"][Null]"}]&)
];
MomBoxdown[mom_,mu_]:=TemplateBox[{mom,mu},"Momdown",
DisplayFunction->(RowBox[{SubscriptBox["p",#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Mom","[",#1,"][Null][",#2,"]"}]&)
];
MomPureBox[mom_]:=TemplateBox[{mom},"MomPure",
DisplayFunction->(RowBox[{"p","[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"MomPure","[",#1,"]"}]&)
];
Mom /: MakeBoxes[Mom[mom_][mu_][Null],StandardForm|TraditionalForm]:=MomBoxup[ToBoxes[mom],ToBoxes[mu]];
Mom /: MakeBoxes[Mom[mom_][Null][mu_],StandardForm|TraditionalForm]:=MomBoxdown[ToBoxes[mom],ToBoxes[mu]];
MomPure /: MakeBoxes[MomPure[mom_],StandardForm|TraditionalForm]:=MomPureBox[ToBoxes[mom]];

Eta /: Times[Eta[mu_,nu_][$down],f_[lab__][nu_][Null]]:=f[lab][Null][mu];
Eta /: Times[Eta[mu_,nu_][$up],f_[lab__][Null][nu_]]:=f[lab][mu][Null];
Mom /: Times[Mom[lab1_][mu_][Null],f_[lab2__][Null][mu_]]:=CenterDot[MomPure[lab1],ToExpression[ToString[f]<>"Pure"][lab2]];
Polar /: Times[Polar[lab1_,lab2_][mu_][Null],f_[lab3__][Null][mu_]]:=CenterDot[PolarPure[lab1,lab2],ToExpression[ToString[f]<>"Pure"][lab3]];
PolarPure /: CenterDot[PolarPure[x_,ref_],PolarPure[y_,ref_]]:=0;


(* ::Subsection:: *)
(*ToFile*)


SetAttributes[ToFile,HoldFirst];
ToFile[exp_,filename_]:=Block[{Print,olddir,stream,out},
olddir=Directory[];
SetDirectory[$UserDocumentsDirectory];
stream=OpenWrite[ToString[filename]<>".txt"];
Print[x_]:=Write[stream,x];
out=exp//ReleaseHold;
Close[stream];
SetDirectory[olddir];
Return[out];
];


(* ::Section:: *)
(*Shortcuts*)


(* ::Subsection:: *)
(*$Shortcuts*)


If[frontend==1,
$Shortcuts={RawBoxes[RowBox[{SuperscriptBox["\[Lambda]","\[Alpha]"],"[p]"}]] -> "esc + lu + esc",
			RawBoxes[RowBox[{SubscriptBox["\[Lambda]","\[Alpha]"],"[p]"}]] -> "esc + ld + esc",
			RawBoxes[RowBox[{SuperscriptBox[OverscriptBox["\[Lambda]","~"],"\[Alpha]"],"[p]"}]] -> "esc + ltu + esc",
			RawBoxes[RowBox[{SubscriptBox[OverscriptBox["\[Lambda]","~"],"\[Alpha]"],"[p]"}]] -> "esc + ltd + esc",
			RawBoxes[RowBox[{SuperscriptBox["\[Mu]","\[Alpha]"],"[p]"}]] -> "esc + muu + esc",
			RawBoxes[RowBox[{SubscriptBox["\[Mu]","\[Alpha]"],"[p]"}]] -> "esc + mud + esc",
			RawBoxes[RowBox[{SuperscriptBox[OverscriptBox["\[Mu]","~"],"\[Alpha]"],"[p]"}]] -> "esc + mtu + esc",
			RawBoxes[RowBox[{SubscriptBox[OverscriptBox["\[Mu]","~"],"\[Alpha]"],"[p]"}]] -> "esc + mtd + esc",
			RawBoxes[RowBox[{"\[Lambda][p]"}]]-> "esc + lp + esc",
			RawBoxes[RowBox[{"\[Mu][p]"}]]-> "esc + mp + esc",
			RawBoxes[RowBox[{OverscriptBox["\[Lambda]","~"],"[p]"}]]-> "esc + ltp + esc",
			RawBoxes[RowBox[{OverscriptBox["\[Mu]","~"],"[p]"}]]-> "esc + mtp + esc",
			RawBoxes[RowBox[{SubscriptBox[SuperscriptBox["\[CapitalLambda]","A"],"a"],"[p]"}]]-> "esc + Ld + esc",
			RawBoxes[RowBox[{SubscriptBox[SubscriptBox[OverscriptBox["\[CapitalLambda]","~"],"A"],"a"],"[p]"}]]-> "esc + Ltd + esc",
			RawBoxes[SuperscriptBox["\[Epsilon]",RowBox[{"a","b"}]]]->"esc + lcup + esc",
			RawBoxes[SubscriptBox["\[Epsilon]",RowBox[{"a","b"}]]]->"esc + lcd + esc",
			RawBoxes[SubscriptBox[SuperscriptBox["\[Delta]","A"],"B"]]-> "esc + md + esc",
			RawBoxes[RowBox[{"\[LeftAngleBracket]",RowBox[{"1 , 2"}],"\[RightAngleBracket]"}]] -> "esc + ab + esc",
			RawBoxes[RowBox[{"[",RowBox[{"1 , 2"}],"]"}]]-> "esc + sb + esc",
			RawBoxes[RowBox[{"\[LeftAngleBracket]",RowBox[{SubscriptBox["1","a"],",",SubscriptBox["2","b"]}],"]"}]] -> "esc + asi + esc",
			RawBoxes[RowBox[{"[",RowBox[{SubscriptBox["1","a"],",",SubscriptBox["2","b"]}],"\[RightAngleBracket]"}]] -> "esc + sai + esc",
			RawBoxes[RowBox[{"\[LeftAngleBracket]",SubscriptBox["1","a"],",",SubscriptBox["2","b"],",",SubscriptBox["3","c"],",",SubscriptBox["4","d"],"\[RightAngleBracket]"}]]-> "esc + aa + esc",
			RawBoxes[RowBox[{"[",SubscriptBox["1","a"],",",SubscriptBox["2","b"],",",SubscriptBox["3","c"],",",SubscriptBox["4","d"],"]"}]]-> "esc + bb + esc",
			RawBoxes[RowBox[{SubscriptBox[SuperscriptBox["F",RowBox[{"A","B"}]],RowBox[{"C","D"}]],"[",SubscriptBox["p",RowBox[{"a","b"}]],"]"}]] -> "esc + fst + esc"}//MatrixForm;
			];


(* ::Section:: *)
(*Attributes*)


SetAttributes[{overbar,MinusSignQ,SpinorDot,SpinorUndot,SpinorLaDownBox,SpinorLaUpBox,SpinorLatDownBox,
SpinorLatUpBox,SpinorMuDownBox,SpinorMuUpBox,SpinorMutDownBox,SpinorMutUpBox,Spinordot,Spinorundot,$lam,$mu,
extramass,extramasstilde,extramasstildeBox,extramassBox,extramasssBox,extramassstildeBox,extramassstilde,extramasss,
KillMasses,Momenta4D,SpinorAngleBracket,SpinorAngleBracketBox,SpinorSquareBracket,SpinorSquareBracketBox,
NewProcess,ClearDownValues,levicivita2up,levicivita2down,levicivita2Up,levicivita2Down,levicivita2up,levicivita2down,
SpinorDot6D,SpinorDot6DBox,SpinorUndot6D,SpinorUndot6DBox,AngAngInvariant,AngAngInvariantBox,SquSquInvariant,SquSquInvariantBox,
SquAngInvariant,SquAngInvariantBox,AngSquInvariant,AngSquInvariantBox,Momenta,AllMomenta,
SpinorReplace,SubCounter,ConvenientMu,SchoutenSimplify,Mom4D,S6,S6many,MDelta,MDeltaBox,Fstrength,FstrengthBox,\[Lambda],\[Mu],\[CapitalLambda],\[CapitalMu],\[Epsilon],\[Delta],FixedSpinors,ClearSpinors,FixSpinors,CompleteDenominators,CompleteMandelstam,ToChain,Chain,chain,$angle,$square,S},Protected]


(* ::Section:: *)
(*Create a palette*)


If[frontend==1,
SpinorPalette:=CreatePalette[Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],SpinorUndotPureMBox["\[SelectionPlaceholder]"],SpinorDotPureMBox["\[SelectionPlaceholder]"],SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[RowBox[{"\[CapitalMu][","\[SelectionPlaceholder]","]"}]],Defer[extramass[\[SelectionPlaceholder]]]],PasteButton[RawBoxes[RowBox[{OverscriptBox["\[CapitalMu]","~"],"[","\[SelectionPlaceholder]","]"}]],Defer[extramasstilde[\[SelectionPlaceholder]]]]}}],Spacings->Automatic],WindowTitle->"SpinorHelicity6D"];
];


If[frontend==1,
CreatePalette[Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],SpinorUndotPureMBox["\[SelectionPlaceholder]"],SpinorDotPureMBox["\[SelectionPlaceholder]"],SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[RowBox[{"\[CapitalMu][","\[SelectionPlaceholder]","]"}]],Defer[extramass[\[SelectionPlaceholder]]]],PasteButton[RawBoxes[RowBox[{OverscriptBox["\[CapitalMu]","~"],"[","\[SelectionPlaceholder]","]"}]],Defer[extramasstilde[\[SelectionPlaceholder]]]]}}],Spacings->Automatic],WindowTitle->"SpinorHelicity6D"];
];


Print["===============SpinorHelicity6D================"];
Print["Authors: Manuel Accettulli Huber (QMUL)"];
Print["         Stefano De Angelis (QMUL)"];
Print["Please report any bug to:"];
Print["m.accettullihuber@qmul.ac.uk"];
Print["Version 1.1 , last update 14/08/2019"];
Print["==============================================="];


If[TrueQ[$MachineID=="6239-87290-05914"],Speak["Ubi maior minor cessat"]];
If[TrueQ[$MachineID=="5113-13572-95048"],Speak["Tricche tracche bombe a mano"]];


End[]

EndPackage[]
