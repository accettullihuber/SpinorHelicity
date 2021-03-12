(* ::Package:: *)

(* ::Title:: *)
(*SpinorHelicity6D*)


BeginPackage["SpinorHelicity6D`"]


(* ::Section:: *)
(*Messages*)


$Shortcuts::usage="This is a list comprising all the available shortcuts and their output"


MinusSignQ::usage="Tells you if its argument is negative"


overbar::usage="Auxiliary function for OverBar."


InverseDot::usage="InverseDot[x_List,y_List] performs the matrix product between the vector x and covector y returning a matrix."


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
Extramass::usage="auxiliary function for extramass"
Extramasstilde::usage="auxiliary function for extramasstilde"


KillMasses::usage="KillMasses[{a,b,c,...}] defines particles a,b,c,... as four dimensional and massless, thus \[CapitalMu][a],\[CapitalMu][b],\[CapitalMu][c] and so on are zero (as well as \[CapitalMu] tilde) as well as mp[a,a],mp[b,b],mp[c,c]..."
Momenta4D::usage="This a variable which tells you which momenta have been specified to be 4 dimensional. It takes no argument of course."
NewProcess::usage="NewProcess deletes all the downvalues of any function fixed during the computation of a given process. For example masses of particles that were previously set to zero (being 4D) are restore using this function. Also conservation relations stored in AllIdentities6D are cleared."
ClearDownValues::usage="ClearDownValues[f] clears the downvalues of the function f given as an argument"


SquAngInvariant::usage="SquAngInvariant[n,m][adot,b] is the invariant [n_adot m_b > with adot and b little group indices and n,m labels of the particles. The associated shortcut is esc + sai + esc"
AngSquInvariant::usage="AngSquInvariant[n,m][a,bdot] is the invariant < n_a m_bdot ] with a and bdot littlegroup indices and n,m particle labels. The associated shortcut is esc + asi + esc"
AngAngInvariant::usage="AngAngInvariant[m,n,l,k][a,b,c,d] is the invariant < m_a n_b l_c k_d > where m,n,l,k are particle labels and a,b,c,d are little group indices. The shotcut is esc + aa + esc"
SquSquInvariant::usage="BraBrainvariant[m,n,l,k][a,b,c,d] is the invariant [ m_a n_b l_c k_d ] where m,n,l,k are particle labels and a,b,c,d are the little group indices"
Squanginvariant::usage="Auxiliary function for SquAngInvariant"
Angsquinvariant::usage="Auxiliary function for AngSquInvariant"
Anganginvariant::usage="Auxiliary function for AngAngInvariant"
Squsquinvariant::usage="Auxiliary function for SquSquInvariant"
AngSquInvariantN::usage="AngSquInvariantN is the numerical equivalent of AngSquInvariant. It can be accessed by acting on the original function with ToNum."
SquAngInvariantN::usage="SquAngInvariantN is the numerical equivalent of SquAngInvariant. It can be accessed by acting on the original function with ToNum."
AngAngInvariantN::usage="AngAngInvariantN is the numerical equivalent of AngAngInvariant. It can be accessed by acting on the original function with ToNum."
SquSquInvariantN::usage="SquSquInvariantN is the numerical equivalent of SquSquInvariant. It can be accessed by acting on the original function with ToNum."


To4D::usage="When applied to a function of the six-dimensional invariants, To4D breaks them down into 4 dimensional invariants, provided the little group indices assume values 1 or 2."
To4DAlways::usage="To4DAlways[True] will change global settings so that six-dimensional invariants are always automatically broken down into four-dimensional invariants. To4DAlways[False] undoes this global setting."


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


CompleteDenominators::usage="CompleteDenominators[exp] completes all spinor products in the denominator of exp to Mandelstam invariants."
CompleteMandelstam::usage="CompleteMandelstam[exp] returns exp where as many spinor products as possible have been replaced with the corresponding four-dimensional Mandelstam invariants. CompleteMandelstam does only gather existing spinor products without adding new terms."
ToChain::usage="ToChain[exp] returns exp where angle and square brackets are gathered together in chains of invariants."
MinimalChains::usage="MinimalChains is an option for ToChain which allows True (default) or False as values. If set to False all chains will be completely contracted. So for example the square of a chain will appear as a single longer chain."
RecursionLimit::usage="RecursionLimit is an option for ToChain. In order to make sure that ToChain recognises all possible contractions there is an internal loop repeating the same procedure multiple times untill the result does not change anymore. RecursionLimit sets a limit on this recursion in order to avoid endless loops. Default is 100."
ChainMomCon::usage="ChainMomCon[exp,Options] applies momentum conservation to the chains appearing in exp. Options include MomCon which specifies the relations to be applied. Notice that in order for the chains to act linearly on a certain sum of momenta, these need to belong to MomList."
ChainSimplify::usage="ChainSimplify[exp,Options] uses properties of the chains to simplify them, reducing them to chains where a given momentum appears at most once and scalar products. It allows for the options MomCon and ReduceComplete. Notice that in order for the simplifications to work best the momenta should be first declared through DeclareMom and massless momenta should be specified by KillMasses."
MomCon::usage="MomCon is an option for ChainSimplify and ChainMomCon which allows to use momentum conservation to simplify the chains. It must to be defined as a list of replacements."
ReduceComplete::usage="ReduceComplete is an option for ChainSimplify which assumes boolean values, default is False. If set to True the function will order the momenta inside the chains, removing in this way spurious structures which could be obtained from each other by reordering. Be aware that this might not actually reduce the number of terms in the expression because of the reordering procedure."
Chain::usage="Chain[type1,first,momList,last,type2] is the invariant obtained chaining together angle and square brackest. Type1 and type2 assume values $angle or $bracket and represent the type of bracket with which the invariants start or end. First is the first momentum in the invariant, last is the last and momList are all the intermediate ones."
chain::usage="chain is an auxiliary function for Chain. It has all the contraction properties one would expect from Chain."
S::usage="Global variable used to Label six-dimensional Mandelstam invariants."
S4::usage="Global variable used to Label four-dimensional Mandelstam invariants."
ChainToSpinor::usage="ChainToSpinor[exp] transforms chain objects into contracted spinors in exp."


$angle::usage="Global variable of SpinorHelicity6D, labels angle brackets."
$square::usage="Global variable of SpinorHelicity6D, labels square brackets."


mp::usage="mp[p_a,p_b] is the four-dimensional scalar product fo momenta p_a and p_b. mp[] has the attribute Orderless."
mpToSpinors::usage="mpToSpinors[exp] converts all scalar products in exp into \!\(\*SubscriptBox[\(s\), \(ij\)]\)->\[LeftAngleBracket]ij\[RightAngleBracket][ji] provide \!\(\*SubscriptBox[\(p\), \(i\)]\) and \!\(\*SubscriptBox[\(p\), \(j\)]\) are massless."
SetMp::usage="SetMp[list] takes as input a list of replacements of the type mp[x,y]->... and allows to fix given scalar products to a deisred value."
Mps::usage="Mps is the list of scalar products which have been fixed to a certain value."
ClearMp::usage="ClearMp[mp[p1,p2],...] clears the definitions of the scalar products given as arguments. If ClearMp is called without arguments all the scalar products are cleared."
eps::usage="eps[p_a,p_b,p_c,p_d] is the Levi-Civita tensor contracted with the four momenta pa_,p_b,p_c and p_d."
TrG::usage="TrG[mom_List] is the trace over the list of slashed four-dimensional momenta mom."
TrG5::usage="TrG5[mom_List] is the trace over the slashed four-dimensional momenta of the list mom but has also a Gamma 5 matrix inserted in the first position of the trace."
ToTrace::usage="ToTrace[exp] converts all closed chains in exp into traces and evaluates them. It admits the Option KillEpsilon whose default is False. If KillEpsilon is set to True the terms in the traces containing an epsilon tensor are set to zero."
KillEpsilon::usage="KillEpsilon is an option for ToTrace whose default value is False. If set to True the Levi-Civita tensors which might appear when computing traces will be ignored."
ScalProdToS::usage="ScalProdToS[exp] convert all the four-dimensional scalar products in exp to six dimensional mandelstam invariants S taking into account possible masses."


MomList::usage="MomList is the list containing all the labels which have been declared to be momenta. Names for mon\[IGrave]menta can be declared through DeclareMom."
DeclareMom::usage="DeclareMom[p] adds p to the list of momenta known to the package. Declaring momenta is not necessary but allows for the use of certain properties (like linearity) of functions like Chain and mp."
UndeclareMom::usage="UndeclareMom[p] removes p from the list of known momenta"


HelicityWeight::usage="HelicityWeight[exp] returns the helicity weight of exp in all the spinors appearing. The function only returns the non-vanishing spinor weights."
MassDimension::usage="MassDimension[exp,OptionsPattern[]] retuns the mass dimension of exp. It allows for the option SetDimension."
SetDimension::usage="Option for MassDimension, which allows to set the massdimension for constants as SetDimension->{constant->dimension}."


Relabel::usage="Relabel[exp_,{LabelRep->{lab1->lab2,...},ScalarProduct->funtion}] allows to perform relabelings of the momenta inside the functions S[], S4[] and mp[]. These relabelings are specified by the optional argument LabelRep. Furthermore it allows to replace the definition of the scalar product given in SpinorHelicity6D with any given function (also optional feature). This is intended to be used when interfacing the results of SpinorHelicity6D with other Mathematica package. For example by setting ScalarProduct->sp will allow direct input of the results into LiteRed for integral reductions."
LabelRep::usage="Option for the function Relabel. LabelRep specifies the label and functional replacements to be performed in a given expression. For example LabelRep->{1->p1,2->p2,S[1,2]->s12,...}."
ScalarProduct::usage="Option for the function Relabel which specifies if the scalar product has to be redefined to a new function. For example ScalarProduct->sp sets the scalar product to be the function sp (used in LiteRed for example)."


SumContracted::usage="SumContracted[index__][exp_] performs the sum over the contracted little group indices index in the expression exp."
StateSumParallel::usage="StateSumParallel is an option for SumContracted. SumContracted[a1,...,an,{StateSumParallel->True},exp] evaluates the state sum in parallel on all the avaialble parallel kernels. If no parallel kernel was generated so far, it launches the maximum number of parallel kernels and proceeds with the computation."
DisplayKernel::usage="DisplayKernel is an option for SumContracted, if set to True the kernel ID of the parallel kernels is displayed in the final result, allowing to backtrack which kernel evaulated what."
CompleteToMassive::usage="CompleteToMassive[exp_,reps_] restores the \[Mu] dependence in exp after this has been removed by choosing specific values for them. It uses the replacements reps to reinsert the \[Mu] backwards. The replacemnt argument is optional, if no replacement is given the lements of FixedSpinors are used by default."


SpinorUndotN::usage="SpinorUndotN[label][type][indexpos] is the numeric equivalent of SpinorUndot. The variable type is either $lam or $mu and the indexposition is either $up or $down. This function is used to store the numeric values of the spinors generated by GenSpinors"


SpinorDotN::usage="SpinorUndotN[label][type][indexpos] is the numeric equivalent of SpinorDot. The variable type is either $lam or $mu and the indexposition is either $up or $down. This function is used to store the numeric values of the spinors generated by GenSpinors"


SpinorUndot6DN::usage="SpinorUndot6DN is the numerical equivalent of SpinorUndot6D. It can be accessed by acting on the original function with ToNum."
SpinorDot6DN::usage="SpinorDot6DN is the numerical equivalent of SpinorDot6D. It can be accessed by acting on the original function with ToNum."


SpinorAngleBracketN::usage="SpinorAngleBracketN[x,y] returns the numeric value of the angle bracket of the two spinors x and y."


SpinorSquareBracketN::usage="SpinorSQuareBracketN[x,y] returns the numeric value of the square bracket of the two spinors x and y."


GenSpinors::usage="GenSpinors[{x1,x2,...,xn},{Options}] generates numerical values for the spinors corresponding to on-shell, conserved, complex kinematics. The spinors are labelled by x1,x2,...,xn and the function allows for the following options: {Dimension,DisplaySpinors,Parametric,ParameterName,ParameterRange,RationalKinematics,Seed,SetMomentum,Type3pt}. For further details type ?OptionName."
Dimension::usage="Dimension is an option for GenSpinors. It specifies the dimension of the generated kinematics. Default is 6, allowed are 6 and 4."
DisplaySpinors::usage="DisplaySpinors is an option for GenSpinors. If set to True the generated kinematics is displayed. Default is False."
Parametric::usage="Parametric is an option for GenSpinors. If set to True the kinematics is generated in terms of a minimal set of independent variables (3n-10 in 4 dimensions and 5n-15 is six-dimensions) instead of numbers. Default is False."
ParameterName::usage="ParameterName is an option for GenSpinors. It allows to choose a custom label for the independent variables in terms of which the kinematics is defined if Parametric->True. Default is $par."
ParameterRange::usage="ParameterRange is an option for GenSpinors. It allows to set the range in which the independent variables are chosen when numerical values of the kinmeatics are required. Default is 10000."
RationalKinematics::usage="RationalKinematics is an option for GenSpinors. It allows to to chose between rational (True) and real (False) kinematics. Default is True. "
Seed::usage="Seed is an option for GenSpinors. It allows to set a seed for SeedRandom so that the randomly generated kinematics can be rederived if necessary. Default is False, but any integer number is admitted."
RandomSpinors::usage="RandomSpinors is an option for GenSpinors. It allows to generate a random set of spinors, which lead to on-shell momenta but these do not satisfy momentum conservation"
$par::usage="Protected symbol. It is the default name of the variables in GenSpinors."
SetMomentum::usage="SetMomentum is an option for GenSpinors. It allows to set the numeric componenst of the first spinors to specific values. It allows as values spinor components as well as labels of already generated momenta."
Type3pt::usage="Type3pt is an option for GenSpinors. When generating three-particle kinematics it allows to specify which king of brackets, angle or square, are non-vanishing. Default value is $angle."


MomMat4DN::usage="MomMat4DN[label][type] is the numeric momentum written as a mtarix in spinor representation. Type is either $up or $down and represents the position of the spinor indices."
Mom4DN::usage="Mom4DN[label] is the four-dimensional numeric momentum vector associated to label."
MomMat6DN::usage="MomMat6DN[label][type] is the six-dimensional momentum matrix. The argument type represents the position of the Lorentz indices."
Mom6DN::usage="Mom6DN[label] is the six-dimensional numeric momentum vector associated to label."
PauliSix::usage="PauliSix[i] is the i'th six-dimensional pauli matrix."
MomToSpinors::usage="MomToSpinors[vector,label] generates the spinors associated to the given vector. This can be four-dimensional massless or massive or six-dimensional. The optional argument label allows to store the generated values of the spinors."


ClearKinematics::usage="ClearKinematics clears all the so far generated and stored numerical values for the kinematics."


ExtramassN::usage="ExtramassN[label] is the numerical equivalent of Extramass[label]."
ExtramasstildeN::usage="ExtramasstildeN[label] is the numerical equivalent of Extramasstilde[label]."


ToNum::usage="TuNum[exp] return numeric value of exp. It requires some kind of numerical kinematics to be generated first using GenSpinors."


Cyclic::usage="Cyclic[exp,cycle_List,OptionsPattern[]] allows to generate a cyclic permutation of the labels in a given expression. The input cycle must be of the form {x1,x2,...,xn} which means x1 is replaced with x2, x2 with x3 and so on, or {{x1,x2,...},...,{...xn}} where the replacements are done in the subcycles. The option SumAll is allowed in OptionPattern."


SumAll::usage="SumAll is an option for Cyclic, which if set to True allows to not only generate the cyclic permutations but also to sum over them. Default is false"


MDelta::usage="MDelta[dim][up1,up2][down1,down2] is the Kronecker delta in dimension dim with upper indices up1 and up2 and lower indices down1 down2. Not present indices are replaced by Null"


MDeltaBox::usage="Just the boxing function for MDelta"
$MDimension::usage="Global variable setting the dimension of MDelta. Default is 4."
Delta::usage="Delta[up,down,options] is the Kronecker delta. It admits $DeltaDim as option which allows to set the dimension of the delta to the desired value. Default is $dim."
$DeltaDim::usage="Option for Delta. Allows to set the dimension of the delta function to any desired value."


Antisymmetrize::usage="Antisymmetrize[exp,{A1,...,An},{B1,...,Bm},...] returns exp antisymmetrized on the indices in the index lists. Be careful, the indices in the list need to be in the same order as in exp for all the signs to be correct in the output."
SymmetrizeSH::usage="SymmetrizeSH[exp,{A1,...,An},{B1,...,Bm},...] returns exp symmetrized on the indices in the index lists."


Fstrength::usage="Fstrength[momlabel][A,B][C,D][lg,lgdot] is the field strength of mmentum momlabel, with A,B Lorentz indices transforming in the fundamental representation and C,D in th eantifundamental, and lg,lgdot little group indices."
FstrengthBox::usage="FstrengthBox is just the boxing function for Fstrength."


ContractReplace::usage="ContractReplace[exp] performs simplifications in exp by recognizing terms which are contracted with an \[Epsilon] tensor and differ by a simple relabelling of indices. Pay attention, it works only on <1_a,2_b] invariants and could be rather inefficient in terms of time...Will hopefully be inproved in later versions of the package."


Fst::usage="Fst[mom][mu,nu][Null,Null] is the field strength tensor with standard Lorentz Indices mu nu and momentum label mom"
Eta::usage="Eta[mu,nu][$up/$down] is the flat metric tensor with upper/lower indices mu nu"
Polar::usage="Polar[mom,refmom][mu][Null] is the polarization vector for the particle with momentum mom and reference momentum refmom, with upper index mu"
PolarPure::usage="PolarPure[mom,refmom] is the polarization vector for the particle with momentum mom and reference momentum refmom with Lorentz indices stripped off"
Mom::usage="Mom[mom][mu][Null] represents the momentum for the particle with momentum mom and upper Lorentz index mu"
MomPure::usage="MomPure[mom] represents the momentum of the particle with momentum mom with Lorentz index stripped off"
MpToSpinors::usage="MpToSpinors[exp,plus,minus] converts scalar products of momenta and polarization vectors into spinors. Momenta are converted only if they are declared to be four dimensional and massless first (done with KillMasses). plus and minus are the list of positive and negative helicity particles appearing in expression, and are needed in order to correctly convert the polarizations. Both lists must always be present, even if empty."
VecToSpinors::usage="VecToSpinors[exp,plus,minus] converts momenta and polarization vectors in vector form into spinors. Momenta are converted only if they are declared to be four dimensional and massless first (done with KillMasses). plus and minus are the list of positive and negative helicity particles appearing in expression, and are needed in order to correctly convert the polarizations. Both lists must always be present, even if empty."


FeynCToSpinor::usage="FeynCToSpinor[exp,momenta_List] converst exp from FeynClac notation to SPinorHelicity6D notation. It requires a list of the momenta in exp to work properly, also it is just a first version covering parts of the FeynCalc functions. Any suggestions for improvements are welcome."


MomReplace::usage="MomReplace[exp,replacements_List] is specifically designed to apply replacements of momenta which are contracted into chains or into Mandelstam invariants. Itadmits the option NumeratorOnly"


NumeratorOnly::usage="NumeratorOnly is an option for MomReplace. It allows to perform replacements only in the numerator if set to True (default), or on the whole expression if set to False."


ToFile::usage="ToFile[exp,filename] writes to the file filename.txt located in the documents directory all the frontend oprinted output appearing in exp, just returning the output of exp."


SpinorPalette::usage="Opens the palette associated to the package SpinorHelicity6D."


ClearSubValues::usage="ClearSubValues[f] clears all the SubValues of f leaving downvalues and upvalues intact."


AssignFunctions::usage="AssignFunctions allows to define custom functions which are permanently stored in a an external file and are loaded automatically when the package is loaded. If called without arguments a list of the already defined functions is diplayed. To clear the defined the functions use ClearFunctions."
ClearFunctions::usage="ClearFunctions allows to clear the functions defined through AssignFunctions. If called without arguments it clears all the custom functions and deletes the corresponding file."
(*LoadFunctions::usage="LoadFunctions[] loads the custom functions stored through AssignFunctions."*)


(* ::Section:: *)
(*Private: 6D spinor helicity definitions*)


Begin["`Private`"]


(*We define a private variable needed for the package to decide whether to run shortcuts and the palette or not. This is related to the availability of a frontend.*)


frontend=If[TrueQ[$FrontEnd==Null],0,1];


(* ::Subsection::Closed:: *)
(*Infycheck*)


Attributes[Infycheck]={HoldAll};
Infycheck[x_]:=TrueQ[Quiet[Check[x,$Failed,{PowerMod::ninv,Power::infy,Infinity::indet,Power::indet}],{PowerMod::ninv,Power::infy,Infinity::indet,Power::indet}]==$Failed];


(* ::Subsection::Closed:: *)
(*overbar*)


overbar[Times[-1,x_]]:=-1*OverBar[x];
overbar[x_Integer]:=If[x>=0,OverBar[x],-OverBar[-x]];
overbar[n_]:=OverBar[n];


(* ::Subsection::Closed:: *)
(*MinuSignQ*)


(*MinusSignQ[x_]:=If[StringContainsQ[ToString[x],"-"],True,False];*)
MinusSignQ[-x_]:=True;
MinusSignQ[x_?Negative]:=True;
MinusSignQ[x_]:=False


(* ::Subsection::Closed:: *)
(*MasslessQ*)


MasslessQ[x_]:=MemberQ[Join[Momenta4D,Momenta4D/.{a_/;MemberQ[MomList,a]:>MomPure[a]}],x];


(* ::Subsection::Closed:: *)
(*InverseDot*)


InverseDot[x_List,y_List]:=Table[i*j,{i,x},{j,y}];


(* ::Subsection::Closed:: *)
(*Extra mass*)


ExtramassBox[x_]:=TemplateBox[{x},"extramass",
DisplayFunction->(RowBox[{"\[CapitalMu]","[",#,"]"}]&),
InterpretationFunction->(RowBox[{"Extramass","[",#,"]"}]&)
];

extramass[x_]:=Module[{input},
input=x/.OverBar->overbar;
If[MinusSignQ[input],Return[-Extramass[-input]],Return[Extramass[input]]];
];

Extramass /: MakeBoxes[Extramass[x_],StandardForm|TraditionalForm]:=ExtramassBox[ToBoxes[x]];


(* ::Subsection::Closed:: *)
(*Extra Mass tilde*)


ExtramasstildeBox[x_]:=TemplateBox[{x},"extramasstilde",
DisplayFunction->(RowBox[{OverscriptBox["\[CapitalMu]","~"],"[",#,"]"}]&),
InterpretationFunction->(RowBox[{"Extramasstilde","[",#,"]"}]&)
];

extramasstilde[x_]:=Module[{input},
input=x/.OverBar->overbar;
If[MinusSignQ[input],Return[-Extramasstilde[-input]],Return[Extramasstilde[input]]];
];

Extramasstilde /: MakeBoxes[Extramasstilde[x_],StandardForm|TraditionalForm]:=ExtramasstildeBox[ToBoxes[x]];


(* ::Subsection::Closed:: *)
(*KillMasses*)


(*KillMasses[x_List]:=(
Unprotect[Extramass,Extramasstilde,Momenta4D];
Do[Extramass[i]=0;Extramasstilde[i]=0,{i,x}];
Momenta4D=x;
Protect[Extramass,Extramasstilde,Momenta4D];
);*)


(*KillMasses[x_List]:=(
Unprotect[Extramass,Extramasstilde,Momenta4D,mp];
Do[Extramass[i]=0;Extramasstilde[i]=0;SetMp[mp[i,i]->0],{i,x}];
Momenta4D=x;
Protect[Extramass,Extramasstilde,Momenta4D,mp];
);*)


KillMasses[moms__]:=Module[{x},
x=Flatten[{moms}];
Unprotect[Extramass,Extramasstilde,Momenta4D,mp];
Do[Extramass[i]=0;Extramasstilde[i]=0;SetMp[mp[i,i]->0],{i,x}];
Momenta4D={Momenta4D,x}//Flatten//DeleteDuplicates;
Protect[Extramass,Extramasstilde,Momenta4D,mp];
];


(* ::Subsection::Closed:: *)
(*Momenta4D*)


Momenta4D={};


(* ::Subsection::Closed:: *)
(*MomList, DeclareMom and UndeclareMom*)


MomList={};
Protect[MomList];

DeclareMom[label__]:=(Unprotect[MomList];MomList=Join[MomList,Flatten[{label}]]//DeleteDuplicates;Protect[MomList];);

UndeclareMom[label__]:=(Unprotect[MomList];MomList=DeleteCases[MomList,x_/;MemberQ[Flatten[{label}],x]]; Protect[MomList];);


(* ::Subsection::Closed:: *)
(*ClearDownValues*)


ClearDownValues[f_]:=DownValues[f]=DeleteCases[DownValues[f],_?(FreeQ[First[#],Pattern]&)];


(* ::Subsection::Closed:: *)
(*NewProcess*)


NewProcess:=(Unprotect[Extramass,Extramasstilde,Momenta4D];ClearDownValues[Extramass];ClearDownValues[Extramasstilde];ClearMp[];Momenta4D={};Protect[Extramass,Extramasstilde,Momenta4D];);


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*SpinorUndot6D*)


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


(* ::Subsection::Closed:: *)
(*SpinorUndot6DN*)


SpinorUndot6DN[a_][$down][1]:=SpinorUndot6DN[a][$down][1]=({-((ExtramassN[a]*SpinorUndotN[a][$mu][$down])/SpinorAngleBracketN[a,OverBar[a]]),SpinorDotN[a][$lam][$up]}//Flatten);
SpinorUndot6DN[a_][$down][2]:=SpinorUndot6DN[a][$down][2]=({SpinorUndotN[a][$lam][$down],-((ExtramasstildeN[a]*SpinorDotN[a][$mu][$up])/SpinorSquareBracketN[a,OverBar[a]])}//Flatten);


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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
(*SpinorUndotN and SpinorDotN*)


SpinorDotN[OverBar[p_]][type_][pos_]:=SpinorDotN[OverBar[p]][type][pos]=SpinorDotN[p][$mu][pos];
SpinorUndotN[OverBar[p_]][type_][pos_]:=SpinorUndotN[OverBar[p]][type][pos]=SpinorUndotN[p][$mu][pos];


(* ::Subsection::Closed:: *)
(*SpinorDot6D*)


(*Define boxing functions*)

SpinorDot6DBox[momlabel_,index_,lgindex_]:=TemplateBox[{momlabel,index,lgindex},"SpinorDot6D",
DisplayFunction->(RowBox[{SubscriptBox[SubscriptBox[OverscriptBox["\[CapitalLambda]","~"],#2],#3],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorDot6D","[",#1,"]","[",#2,"]","[","Null","]","[",#3,"]"}]&)
];

SpinorDot6DBox2[momlabel_,index_,lgindex_]:=TemplateBox[{momlabel,index,lgindex},"SpinorDot6D2",
DisplayFunction->(RowBox[{SuperscriptBox[SubscriptBox[OverscriptBox["\[CapitalLambda]","~"],#2],#3],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"SpinorDot6D","[",#1,"]","[",#2,"]","[",#3,"]","[","Null","]"}]&)
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


(* ::Subsection::Closed:: *)
(*SpinorDot6DN*)


SpinorDot6DN[a_][$down][1]:=SpinorDot6DN[a][$down][1]=({(ExtramasstildeN[a]*SpinorUndotN[a][$mu][$up])/SpinorAngleBracketN[a,OverBar[a]],-SpinorDotN[a][$lam][$down]}//Flatten);
SpinorDot6DN[a_][$down][2]:=SpinorDot6DN[a][$down][2]=({SpinorUndotN[a][$lam][$up],-((ExtramassN[a]*SpinorDotN[a][$mu][$down])/SpinorSquareBracketN[a,OverBar[a]])}//Flatten);


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Square-angle invariant*)


SquAngInvariant[a_, b_][x_,y_] /; (a == b) := 0;

(*SquAngInvariant[n_,m_][a_Integer,b_Integer]:=SpinorDot6D[n][$contractvariable][Null][a].SpinorUndot6D[m][$contractvariable][Null][b];
*)
SquAngInvariantBox[n_,m_,adot_,b_]:=TemplateBox[{n,m,adot,b},"SquAngInvariant",
DisplayFunction->(RowBox[{"[",SubscriptBox[#1,#3], "," ,SubscriptBox[#2,#4],"\[RightAngleBracket]"}]&),
InterpretationFunction->(RowBox[{"Squanginvariant","[",#1,",",#2,"]","[",#3,",",#4,"]"}]&)
];

(*Introduce auxiliary function with properties:*)

Squanginvariant[x_?MinusSignQ,y_][a__]:=I*SquAngInvariant[-x,y][a];
Squanginvariant[x_,y_?MinusSignQ][a__]:=I*SquAngInvariant[x,-y][a];
Squanginvariant[x_?MinusSignQ,y_?MinusSignQ][a__]:=-SquAngInvariant[-x,-y][a];

Squanginvariant[x__][y__]:=SquAngInvariant[x][y];

SquAngInvariant /: MakeBoxes[SquAngInvariant[n_,m_][adot_,b_],StandardForm|TraditionalForm]:=SquAngInvariantBox[ToBoxes[n],ToBoxes[m],ToBoxes[adot],ToBoxes[b]];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sai" -> SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection::Closed:: *)
(*Angle-square invariant*)


AngSquInvariant[a_, b_][x_,y_] /; (a == b) := 0;

(*AngSquInvariant[n_,m_][a_Integer,b_Integer]:=SpinorUndot6D[n][$contractvariable][Null][a].SpinorDot6D[m][$contractvariable][Null][b];
*)
AngSquInvariantBox[n_,m_,adot_,b_]:=TemplateBox[{n,m,adot,b},"AngSquInvariant",
DisplayFunction->(RowBox[{"\[LeftAngleBracket]",SubscriptBox[#1,#3], "," ,SubscriptBox[#2,#4],"]"}]&),
InterpretationFunction->(RowBox[{"Angsquinvariant","[",#1,",",#2,"]","[",#3,",",#4,"]"}]&)
];

(*Introduce auxiliary function with properties:*)

Angsquinvariant[x_?MinusSignQ,y_][a__]:=I*AngSquInvariant[-x,y][a];
Angsquinvariant[x_,y_?MinusSignQ][a__]:=I*AngSquInvariant[x,-y][a];
Angsquinvariant[x_?MinusSignQ,y_?MinusSignQ][a__]:=-AngSquInvariant[-x,-y][a];

Angsquinvariant[x__][y__]:=AngSquInvariant[x][y];

AngSquInvariant /: MakeBoxes[AngSquInvariant[n_,m_][adot_,b_],StandardForm|TraditionalForm]:=AngSquInvariantBox[ToBoxes[n],ToBoxes[m],ToBoxes[adot],ToBoxes[b]];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "asi" -> AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection::Closed:: *)
(*Angle-angle invariant*)


AngAngInvariantBox[m_,n_,l_,k_,a_,b_,c_,d_]:=TemplateBox[{m,n,l,k,a,b,c,d},"AngAngInvariant",
DisplayFunction->(RowBox[{"\[LeftAngleBracket]",SubscriptBox[#1,#5],",",SubscriptBox[#2,#6],",",SubscriptBox[#3,#7],",",SubscriptBox[#4,#8],"\[RightAngleBracket]"}]&),
InterpretationFunction->(RowBox[{"Anganginvariant","[",#1,",",#2,",",#3,",",#4,"]","[",#5,",",#6,",",#7,",",#8,"]"}]&)
];

AngAngInvariant /: MakeBoxes[AngAngInvariant[m_,n_,l_,k_][a_,b_,c_,d_],TraditionalForm|StandardForm]:=AngAngInvariantBox[ToBoxes[m],ToBoxes[n],ToBoxes[l],ToBoxes[k],ToBoxes[a],ToBoxes[b],ToBoxes[c],ToBoxes[d]];

(*AngAngInvariant[i_,j_,k_,l_][a_Integer,b_Integer,c_Integer,d_Integer]:=
First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[j][$contractvariable2][Null][b]]Last[SpinorUndot6D[k][$contractvariable3][Null][c]]Last[SpinorUndot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[k][$contractvariable2][Null][c]]Last[SpinorUndot6D[l][$contractvariable3][Null][d]]Last[SpinorUndot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[l][$contractvariable2][Null][d]]Last[SpinorUndot6D[j][$contractvariable3][Null][b]]Last[SpinorUndot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[k][$contractvariable1][Null][c]]First[SpinorUndot6D[l][$contractvariable2][Null][d]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[l][$contractvariable1][Null][d]]First[SpinorUndot6D[j][$contractvariable2][Null][b]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[j][$contractvariable1][Null][b]]First[SpinorUndot6D[k][$contractvariable2][Null][c]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4];
*)

(*Auxiliary function with properties*)

Anganginvariant[x___,y_?MinusSignQ,z___][a__]:=I*AngAngInvariant[x,-y,z][a];
Anganginvariant[x___,y_?MinusSignQ,z___,k_?MinusSignQ,l___][a__]:=-AngAngInvariant[x,-y,z,-k,l][a];
Anganginvariant[x___,y_?MinusSignQ,z___,k_?MinusSignQ,l___,m_?MinusSignQ,n___][a__]:=-I*AngAngInvariant[x,-y,z,-k,l,-m,n][a];
Anganginvariant[x___,y_?MinusSignQ,z___,k_?MinusSignQ,l___,m_?MinusSignQ,n___,t_?MinusSignQ][a__]:=AngAngInvariant[x,-y,z,-k,l,-m,n,-t][a];

Anganginvariant[x__][y__]:=AngAngInvariant[x][y];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "aa" -> AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection::Closed:: *)
(*Square square invariant*)


SquSquInvariantBox[m_,n_,l_,k_,a_,b_,c_,d_]:=TemplateBox[{m,n,l,k,a,b,c,d},"SquSquInvariant",
DisplayFunction->(RowBox[{"[",SubscriptBox[#1,#5],",",SubscriptBox[#2,#6],",",SubscriptBox[#3,#7],",",SubscriptBox[#4,#8],"]"}]&),
InterpretationFunction->(RowBox[{"Squsquinvariant","[",#1,",",#2,",",#3,",",#4,"]","[",#5,",",#6,",",#7,",",#8,"]"}]&)
];

SquSquInvariant /: MakeBoxes[SquSquInvariant[m_,n_,l_,k_][a_,b_,c_,d_],StandardForm|TraditionalForm]:=SquSquInvariantBox[ToBoxes[m],ToBoxes[n],ToBoxes[l],ToBoxes[k],ToBoxes[a],ToBoxes[b],ToBoxes[c],ToBoxes[d]];

(*SquSquInvariant[i_,j_,k_,l_][a_Integer,b_Integer,c_Integer,d_Integer]:=
Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[j][$contractvariable2][Null][b]]First[SpinorDot6D[k][$contractvariable3][Null][c]]First[SpinorDot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[k][$contractvariable2][Null][c]]First[SpinorDot6D[l][$contractvariable3][Null][d]]First[SpinorDot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[l][$contractvariable2][Null][d]]First[SpinorDot6D[j][$contractvariable3][Null][b]]First[SpinorDot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[k][$contractvariable1][Null][c]]Last[SpinorDot6D[l][$contractvariable2][Null][d]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[l][$contractvariable1][Null][d]]Last[SpinorDot6D[j][$contractvariable2][Null][b]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[j][$contractvariable1][Null][b]]Last[SpinorDot6D[k][$contractvariable2][Null][c]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4];
*)

(*Auxiliary function with properties*)

Squsquinvariant[x___,y_?MinusSignQ,z___][a__]:=I*SquSquInvariant[x,-y,z][a];
Squsquinvariant[x___,y_?MinusSignQ,z___,k_?MinusSignQ,l___][a__]:=-SquSquInvariant[x,-y,z,-k,l][a];
Squsquinvariant[x___,y_?MinusSignQ,z___,k_?MinusSignQ,l___,m_?MinusSignQ,n___][a__]:=-I*SquSquInvariant[x,-y,z,-k,l,-m,n][a];
Squsquinvariant[x___,y_?MinusSignQ,z___,k_?MinusSignQ,l___,m_?MinusSignQ,n___,t_?MinusSignQ][a__]:=SquSquInvariant[x,-y,z,-k,l,-m,n,-t][a];

Squsquinvariant[x__][y__]:=SquSquInvariant[x][y];

If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "bb" -> SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection::Closed:: *)
(*AngSquN*)


(*AngSquInvariantN[a_, b_][1,1]:=AngSquInvariantN[a, b][1,1]=(ExtramassN[a]*ExtramasstildeN[b]*SpinorAngleBracketN[OverBar[a],OverBar[b]])/(SpinorAngleBracketN[a,OverBar[a]]*SpinorAngleBracketN[b,OverBar[b]])+SpinorSquareBracketN[a,b];
AngSquInvariantN[a_, b_][1,2]:=AngSquInvariantN[a, b][1,2]=-((ExtramassN[a]*SpinorAngleBracketN[b,OverBar[a]])/SpinorAngleBracketN[a,OverBar[a]])+(ExtramassN[b]*SpinorSquareBracketN[a,OverBar[b]])/SpinorSquareBracketN[b,OverBar[b]];
AngSquInvariantN[a_, b_][2,1]:=AngSquInvariantN[a, b][2,1]=-((ExtramasstildeN[b]*SpinorAngleBracketN[a,OverBar[b]])/SpinorAngleBracketN[b,OverBar[b]])+(ExtramasstildeN[a]*SpinorSquareBracketN[b,OverBar[a]])/SpinorSquareBracketN[a,OverBar[a]];
AngSquInvariantN[a_, b_][2,2]:=AngSquInvariantN[a, b][2,2]=-SpinorAngleBracketN[a,b]-(ExtramassN[b]*ExtramasstildeN[a]*SpinorSquareBracketN[OverBar[a],OverBar[b]])/(SpinorSquareBracketN[a,OverBar[a]]*SpinorSquareBracketN[b,OverBar[b]]);*)


AngSquInvariantN[a_, b_][c_,d_]:=AngSquInvariantN[a, b][c,d]=SpinorUndot6DN[a][$down][c] . SpinorDot6DN[b][$down][d];


(* ::Subsection::Closed:: *)
(*SquAngN*)


(*SquAngInvariantN[a_,b_][1,1]:=SquAngInvariantN[a,b][1,1]=-((ExtramassN[b]*ExtramasstildeN[a]*SpinorAngleBracketN[OverBar[a],OverBar[b]])/(SpinorAngleBracketN[a,OverBar[a]]*SpinorAngleBracketN[b,OverBar[b]]))-SpinorSquareBracketN[a,b];
SquAngInvariantN[a_,b_][1,2]:=SquAngInvariantN[a,b][1,2]=-((ExtramasstildeN[a]*SpinorAngleBracketN[b,OverBar[a]])/SpinorAngleBracketN[a,OverBar[a]])+(ExtramasstildeN[b]*SpinorSquareBracketN[a,OverBar[b]])/SpinorSquareBracketN[b,OverBar[b]];
SquAngInvariantN[a_,b_][2,1]:=SquAngInvariantN[a,b][2,1]=-((ExtramassN[b]*SpinorAngleBracketN[a,OverBar[b]])/SpinorAngleBracketN[b,OverBar[b]])+(ExtramassN[a]*SpinorSquareBracketN[b,OverBar[a]])/SpinorSquareBracketN[a,OverBar[a]];
SquAngInvariantN[a_,b_][2,2]:=SquAngInvariantN[a,b][2,2]=SpinorAngleBracketN[a,b]+(ExtramassN[a]*ExtramasstildeN[b]*SpinorSquareBracketN[OverBar[a],OverBar[b]])/(SpinorSquareBracketN[a,OverBar[a]]*SpinorSquareBracketN[b,OverBar[b]]);*)

SquAngInvariantN[a_,b_][c_,d_]:=SquAngInvariantN[a,b][c,d]=SpinorDot6DN[a][$down][c] . SpinorUndot6DN[b][$down][d];


(* ::Subsection::Closed:: *)
(*AngAngN*)


AngAngInvariantN[x1_,x2_,x3_,x4_][a_,b_,c_,d_]:=AngAngInvariantN[x1,x2,x3,x4][a,b,c,d]=-Det[{SpinorUndot6DN[x1][$down][a],SpinorUndot6DN[x2][$down][b],SpinorUndot6DN[x3][$down][c],SpinorUndot6DN[x4][$down][d]}];


(* ::Subsection::Closed:: *)
(*SquSquN*)


SquSquInvariantN[x1_,x2_,x3_,x4_][a_,b_,c_,d_]:=SquSquInvariantN[x1,x2,x3,x4][a,b,c,d]=-Det[{SpinorDot6DN[x1][$down][a],SpinorDot6DN[x2][$down][b],SpinorDot6DN[x3][$down][c],SpinorDot6DN[x4][$down][d]}];


(* ::Subsection::Closed:: *)
(*To4D*)


To4D[exp_]:=Block[{out,SquAngInvariant,AngSquInvariant,AngAngInvariant,SquSquInvariant,SpinorUndot6D,SpinorDot6D},
SpinorUndot6D[momlabel_][index_][Null][1]:={-extramass[momlabel]/Spinoranglebracket[momlabel,overbar[momlabel]]*Spinorundot[momlabel][$mu][Null][index],Spinordot[momlabel][$lam][index][Null]};
SpinorUndot6D[momlabel_][index_][Null][2]:={Spinorundot[momlabel][$lam][Null][index],-extramasstilde[momlabel]/Spinorsquarebracket[momlabel,overbar[momlabel]]*Spinordot[momlabel][$mu][index][Null]};
SpinorDot6D[momlabel_][index_][Null][1]:={extramasstilde[momlabel]/Spinoranglebracket[momlabel,overbar[momlabel]]*Spinorundot[momlabel][$mu][index][Null],-Spinordot[momlabel][$lam][Null][index]};
SpinorDot6D[momlabel_][index_][Null][2]:={Spinorundot[momlabel][$lam][index][Null],-extramass[momlabel]/Spinorsquarebracket[momlabel,overbar[momlabel]]*Spinordot[momlabel][$mu][Null][index]};
SquAngInvariant[n_,m_][a_Integer,b_Integer]:=SpinorDot6D[n][$contractvariable][Null][a] . SpinorUndot6D[m][$contractvariable][Null][b];
AngSquInvariant[n_,m_][a_Integer,b_Integer]:=SpinorUndot6D[n][$contractvariable][Null][a] . SpinorDot6D[m][$contractvariable][Null][b];
AngAngInvariant[i_,j_,k_,l_][a_Integer,b_Integer,c_Integer,d_Integer]:=
First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[j][$contractvariable2][Null][b]]Last[SpinorUndot6D[k][$contractvariable3][Null][c]]Last[SpinorUndot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[k][$contractvariable2][Null][c]]Last[SpinorUndot6D[l][$contractvariable3][Null][d]]Last[SpinorUndot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[l][$contractvariable2][Null][d]]Last[SpinorUndot6D[j][$contractvariable3][Null][b]]Last[SpinorUndot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[k][$contractvariable1][Null][c]]First[SpinorUndot6D[l][$contractvariable2][Null][d]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[l][$contractvariable1][Null][d]]First[SpinorUndot6D[j][$contractvariable2][Null][b]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[j][$contractvariable1][Null][b]]First[SpinorUndot6D[k][$contractvariable2][Null][c]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4];
SquSquInvariant[i_,j_,k_,l_][a_Integer,b_Integer,c_Integer,d_Integer]:=
Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[j][$contractvariable2][Null][b]]First[SpinorDot6D[k][$contractvariable3][Null][c]]First[SpinorDot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[k][$contractvariable2][Null][c]]First[SpinorDot6D[l][$contractvariable3][Null][d]]First[SpinorDot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[l][$contractvariable2][Null][d]]First[SpinorDot6D[j][$contractvariable3][Null][b]]First[SpinorDot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[k][$contractvariable1][Null][c]]Last[SpinorDot6D[l][$contractvariable2][Null][d]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[l][$contractvariable1][Null][d]]Last[SpinorDot6D[j][$contractvariable2][Null][b]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[j][$contractvariable1][Null][b]]Last[SpinorDot6D[k][$contractvariable2][Null][c]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4];
out=exp;
Return[out];
];


(* ::Subsection::Closed:: *)
(*To4DAlways*)


To4DAlways[bool_?BooleanQ]:=(
Unprotect[AngSquInvariant,SquAngInvariant,AngAngInvariant,SquSquInvariant,SpinorUndot6D,SpinorDot6D];
If[bool,
(*Define the breackdown properties of the six-dimensional spinors*)
SpinorUndot6D[momlabel_][index_][Null][1]:={-extramass[momlabel]/Spinoranglebracket[momlabel,overbar[momlabel]]*Spinorundot[momlabel][$mu][Null][index],Spinordot[momlabel][$lam][index][Null]};
SpinorUndot6D[momlabel_][index_][Null][2]:={Spinorundot[momlabel][$lam][Null][index],-extramasstilde[momlabel]/Spinorsquarebracket[momlabel,overbar[momlabel]]*Spinordot[momlabel][$mu][index][Null]};
SpinorDot6D[momlabel_][index_][Null][1]:={extramasstilde[momlabel]/Spinoranglebracket[momlabel,overbar[momlabel]]*Spinorundot[momlabel][$mu][index][Null],-Spinordot[momlabel][$lam][Null][index]};
SpinorDot6D[momlabel_][index_][Null][2]:={Spinorundot[momlabel][$lam][index][Null],-extramass[momlabel]/Spinorsquarebracket[momlabel,overbar[momlabel]]*Spinordot[momlabel][$mu][Null][index]};
	SquAngInvariant[n_,m_][a_Integer,b_Integer]:=SpinorDot6D[n][$contractvariable][Null][a] . SpinorUndot6D[m][$contractvariable][Null][b];
	AngSquInvariant[n_,m_][a_Integer,b_Integer]:=SpinorUndot6D[n][$contractvariable][Null][a] . SpinorDot6D[m][$contractvariable][Null][b];
	AngAngInvariant[i_,j_,k_,l_][a_Integer,b_Integer,c_Integer,d_Integer]:=
First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[j][$contractvariable2][Null][b]]Last[SpinorUndot6D[k][$contractvariable3][Null][c]]Last[SpinorUndot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[k][$contractvariable2][Null][c]]Last[SpinorUndot6D[l][$contractvariable3][Null][d]]Last[SpinorUndot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[i][$contractvariable1][Null][a]]First[SpinorUndot6D[l][$contractvariable2][Null][d]]Last[SpinorUndot6D[j][$contractvariable3][Null][b]]Last[SpinorUndot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[k][$contractvariable1][Null][c]]First[SpinorUndot6D[l][$contractvariable2][Null][d]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[l][$contractvariable1][Null][d]]First[SpinorUndot6D[j][$contractvariable2][Null][b]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+First[SpinorUndot6D[j][$contractvariable1][Null][b]]First[SpinorUndot6D[k][$contractvariable2][Null][c]]Last[SpinorUndot6D[i][$contractvariable3][Null][a]]Last[SpinorUndot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4];
	SquSquInvariant[i_,j_,k_,l_][a_Integer,b_Integer,c_Integer,d_Integer]:=
Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[j][$contractvariable2][Null][b]]First[SpinorDot6D[k][$contractvariable3][Null][c]]First[SpinorDot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[k][$contractvariable2][Null][c]]First[SpinorDot6D[l][$contractvariable3][Null][d]]First[SpinorDot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[i][$contractvariable1][Null][a]]Last[SpinorDot6D[l][$contractvariable2][Null][d]]First[SpinorDot6D[j][$contractvariable3][Null][b]]First[SpinorDot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[k][$contractvariable1][Null][c]]Last[SpinorDot6D[l][$contractvariable2][Null][d]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[j][$contractvariable4][Null][b]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[l][$contractvariable1][Null][d]]Last[SpinorDot6D[j][$contractvariable2][Null][b]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[k][$contractvariable4][Null][c]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4]+Last[SpinorDot6D[j][$contractvariable1][Null][b]]Last[SpinorDot6D[k][$contractvariable2][Null][c]]First[SpinorDot6D[i][$contractvariable3][Null][a]]First[SpinorDot6D[l][$contractvariable4][Null][d]]*levicivita2up[$contractvariable1,$contractvariable2]levicivita2down[$contractvariable3,$contractvariable4];
,
ClearSubValues[AngSquInvariant];
ClearSubValues[SquAngInvariant];
ClearSubValues[AngAngInvariant];
ClearSubValues[SquSquInvariant];
ClearSubValues[SpinorUndot6D];
ClearSubValues[SpinorDot6D];
];
Protect[AngSquInvariant,SquAngInvariant,AngAngInvariant,SquSquInvariant,SpinorDot6D,SpinorUndot6D];);


(* ::Subsection:: *)
(*SpinorReplace*)


(*$crep=1;
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
];*)


SpinorReplace[exp_,reps_]:=Catch[Block[{locexp,locreps,agwrapper,sqwrapper,SpinorAngleBracket,SpinorSquareBracket,Chain,special,dot},

(*We replace Dot with a local defined dot symbol, because Dot has some properties which screw up our pattern matching later on*)
locreps={reps}/.Dot->dot//Flatten;

(*Apply linearity to the PureSpinors in terms of the globally defined momentum labels*)
locreps=locreps/.{a_/;MemberQ[MomList,a]:>MomPure[a]};
locreps=locreps//.{SpinorUndotPure[a_+b_][type_]/;!FreeQ[b,MomPure]:>SpinorUndotPure[a][type]+SpinorUndotPure[b][type],SpinorDotPure[a_+b_][type_]/;!FreeQ[b,MomPure]:>SpinorDotPure[a][type]+SpinorDotPure[b][type],SpinorUndotPure[a_*MomPure[x_]][type_]:>a*SpinorUndotPure[MomPure[x]][type],SpinorDotPure[a_*MomPure[x_]][type_]:>a*SpinorDotPure[MomPure[x]][type]};
locreps=locreps/.{MomPure[x_]:>x};
(*In order for linearity with respect to wrappers to be correctly applied later on expand the right hand side of the replacement whenever SpinorDotPure or SpinorUndotPure are present*)
locreps=ReplacePart[#,-1->Expand[Last[#],SpinorUndotPure|SpinorDotPure]]&/@locreps;


(*Isolate the replacements containing matrices which will get special treatment*)
special=Select[locreps,(!FreeQ[#,dot]&)];
locreps=Select[locreps,FreeQ[#,dot]&];

(*Take into account the possible presence of \[Mu] spinors*)
locreps=locreps/.{SpinorUndotPure[x_][$mu]:>SpinorUndotPure[OverBar[x]][$lam],SpinorDotPure[x_][$mu]:>SpinorDotPure[OverBar[x]][$lam]};

(*Introduces two wrappers with respect to which we will define the properties which perform the actual replacement*)
locreps=locreps/.{SpinorUndotPure[x_][$lam]:>agwrapper[x],SpinorDotPure[x_][$lam]:>sqwrapper[x]};

(*Test the correcteness of the replacements*)
If[AnyTrue[locreps,((FreeQ[#,agwrapper]&&FreeQ[#,sqwrapper])||(!FreeQ[#,agwrapper]&&!FreeQ[#,sqwrapper])&)],
Throw["Some replacements are given in an unknown form. Please check input."];
];

(*Now we trnasform replacement rules for the wrappers into equalities*)
locreps=locreps/.{Rule->Set,RuleDelayed->Set};

(*Define linearity of brackets and chains with respect to the wrappers*)
SpinorAngleBracket[a_+b_,c_]/;!FreeQ[b,agwrapper]:=SpinorAngleBracket[a,c]+SpinorAngleBracket[b,c];
SpinorAngleBracket[c_,a_+b_]/;!FreeQ[b,agwrapper]:=SpinorAngleBracket[c,a]+SpinorAngleBracket[c,b];

SpinorAngleBracket[a_*agwrapper[x_],y_]:=a*SpinorAngleBracket[agwrapper[x],y];
SpinorAngleBracket[y_,a_*agwrapper[x_]]:=a*SpinorAngleBracket[y,agwrapper[x]];

SpinorSquareBracket[a_+b_,c_]/;!FreeQ[b,sqwrapper]:=SpinorSquareBracket[a,c]+SpinorSquareBracket[b,c];
SpinorSquareBracket[c_,a_+b_]/;!FreeQ[b,sqwrapper]:=SpinorSquareBracket[c,a]+SpinorSquareBracket[c,b];

SpinorSquareBracket[a_*sqwrapper[x_],y_]:=a*SpinorSquareBracket[sqwrapper[x],y];
SpinorSquareBracket[y_,a_*sqwrapper[x_]]:=a*SpinorSquareBracket[y,sqwrapper[x]];

Chain[type1_,a_+b_,y_,z_,type2_]/;!FreeQ[b,agwrapper|sqwrapper]:=Chain[type1,a,y,z,type2]+Chain[type1,b,y,z,type2];
Chain[type1_,x_,y_,a_+b_,type2_]/;!FreeQ[b,agwrapper|sqwrapper]:=Chain[type1,x,y,a,type2]+Chain[type1,x,y,b,type2];

Chain[type1_,a_*agwrapper[x_],y_,z_,type2_]:=a*Chain[type1,agwrapper[x],y,z,type2];
Chain[type1_,a_*sqwrapper[x_],y_,z_,type2_]:=a*Chain[type1,sqwrapper[x],y,z,type2];
Chain[type1_,x_,y_,a_*agwrapper[z_],type2_]:=a*Chain[type1,x,y,agwrapper[z],type2];
Chain[type1_,x_,y_,a_*sqwrapper[z_],type2_]:=a*Chain[type1,x,y,sqwrapper[z],type2];

(*Now for the special replacements, if any*)
If[Length[special]>0,
(*Take into account \[Mu] spinors*)
special=special/.{SpinorUndotPure[x_][$mu]:>SpinorUndotPure[OverBar[x]][$lam],SpinorDotPure[x_][$mu]:>SpinorDotPure[OverBar[x]][$lam]};
(*Introduce the wrapper*)
special=special/.{SpinorUndotPure[x_][$lam]:>agwrapper[x],SpinorDotPure[x_][$lam]:>sqwrapper[x]};
(*Take into account the matrix products. When a matrix is dotted into an angle wrapper it transforms it into a square and the opposite way around*)
special=special/.{dot[A__,agwrapper[x_]]:>If[OddQ[Length[{A}]],sqwrapper[{{A},x}],agwrapper[{{A},x}]],dot[A__,sqwrapper[x_]]:>If[OddQ[Length[{A}]],agwrapper[{{A},x}],sqwrapper[{{A},x}]]};
(*Test the correcteness of the replacements*)
If[AnyTrue[special,((FreeQ[#,agwrapper]&&FreeQ[#,sqwrapper])||(!FreeQ[#,agwrapper]&&!FreeQ[#,sqwrapper])&)],
Throw["Some replacements are given in an unknown form. Please check input."];
];
(*Now we transform replacement rules for the wrappers into equalities*)
special=special/.{Rule->Set,RuleDelayed->Set};
(*And define the special properties of chains and brackets with respect to these wrappers*)
SpinorAngleBracket[x_,agwrapper[{A_List,y_}]]:=If[OddQ[Length[A]],
Chain[$angle,x,A,y,$square],
Chain[$angle,x,A,y,$angle]
];
SpinorAngleBracket[agwrapper[{A_,x_}],y_]:=If[OddQ[Length[A]],
-Chain[$square,x,Reverse[A],y,$angle],
Chain[$angle,x,Reverse[A],y,$angle]
];
SpinorSquareBracket[x_,sqwrapper[{A_,y_}]]:=If[OddQ[Length[A]],
Chain[$square,x,A,y,$angle],
Chain[$square,x,A,y,$square]
];
SpinorSquareBracket[sqwrapper[{A_,x_}],y_]:=If[OddQ[Length[A]],
-Chain[$angle,x,Reverse[A],y,$square],
Chain[$square,x,Reverse[A],y,$square]
];
Chain[type_,x_,{y__},agwrapper[{A_,z_}],$angle]:=If[OddQ[Length[A]],
Chain[type,x,{y,Sequence@@A},z,$square],
Chain[type,x,{y,Sequence@@A},z,$angle]
];
Chain[$angle,agwrapper[{A_,x_}],{y__},z_,type_]:=If[OddQ[Length[A]],
-Chain[$square,x,{Sequence@@Reverse[A],y},z,type],
Chain[$angle,x,{Sequence@@Reverse[A],y},z,type]
];
Chain[type_,x_,{y__},sqwrapper[{A_,z_}],$square]:=If[OddQ[Length[A]],
Chain[type,x,{y,Sequence@@A},z,$angle],
Chain[type,x,{y,Sequence@@A},z,$square]
];
Chain[$square,sqwrapper[{A_,x_}],{y__},z_,type_]:=If[OddQ[Length[A]],
-Chain[$angle,x,{Sequence@@Reverse[A],y},z,type],
Chain[$square,x,{Sequence@@Reverse[A],y},z,type]
];
];

(*Finally replace the arguments of SpinorAngleBracket, SpinorSquareBracket and Chain with the wrapped argumenst*)

locexp=exp/.{SpinorAngleBracket[x_,y_]:>SpinorAngleBracket[agwrapper[x],agwrapper[y]],SpinorSquareBracket[x_,y_]:>SpinorSquareBracket[sqwrapper[x],sqwrapper[y]],
Chain[$angle,x_,{y__},z_,$angle]:>Chain[$angle,agwrapper[x],{y},agwrapper[z],$angle],Chain[$square,x_,{y__},z_,$square]:>Chain[$square,sqwrapper[x],{y},sqwrapper[z],$square],Chain[$angle,x_,{y__},z_,$square]:>Chain[$angle,agwrapper[x],{y},sqwrapper[z],$square],Chain[$square,x_,{y__},z_,$angle]:>Chain[$square,sqwrapper[x],{y},agwrapper[z],$angle]};

(*Properties will be automatically applied so we can now just remove the wrappers and return the output.*)
locexp=locexp/.{agwrapper[x_]:>x,sqwrapper[x_]:>x};

Return[locexp];
];
];


(* ::Subsection:: *)
(*Momenta*)


Momenta[expr_] := Join[Cases[{expr}, HoldPattern[SpinorAngleBracket[a_, b_] | SpinorSquareBracket[a_, b_]] :> Sequence[a, b], \[Infinity]],
Cases[{expr}, HoldPattern[extramass[a_] | extramasstilde[a_]|\[CapitalMu][a_]|OverTilde[\[CapitalMu]][a_]|SpinorUndot[a_][_][_][_]|SpinorDot[a_][_][_][_]] :> a, \[Infinity]]]/.OverBar[x_]->x // DeleteDuplicates


(* ::Subsection::Closed:: *)
(*AllMomenta*)


AllMomenta[expr_] := Join[Cases[{expr}, HoldPattern[SpinorAngleBracket[a_, b_] | SpinorSquareBracket[a_, b_]] :> Sequence[a, b], \[Infinity]],
Cases[{expr}, HoldPattern[extramass[a_] | extramasstilde[a_]|\[CapitalMu][a_]|OverTilde[\[CapitalMu]][a_]|SpinorUndot[a_][_][_][_]|SpinorDot[a_][_][_][_]] :> a, \[Infinity]]] // DeleteDuplicates


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*SpinorReplaceSequential*)


SpinorReplaceSequential[exp_,subs_]:=Catch[Block[{locexp},
locexp=exp;
Do[If[Infycheck[locexp=SpinorReplace[locexp,{subs[[i]]}]],Throw["The substitution "<>ToString[subs[[i]]]<>" is not allowed"]],{i,Length[subs]}];
Return[locexp];
]];


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*ClearSpinors*)


FixedSpinors={};
ClearSpinors[]:=(Unprotect[SpinorUndot,SpinorDot,FixedSpinors];SubValues[SpinorUndot]=DeleteCases[SubValues[SpinorUndot],_?(FreeQ[First[#],HoldPattern@SpinorUndot[Pattern]]&)];
SubValues[SpinorDot]=DeleteCases[SubValues[SpinorDot],_?(FreeQ[First[#],HoldPattern@SpinorDot[Pattern]]&)];
FixedSpinors={};
Protect[SpinorUndot,SpinorDot,FixedSpinors];)


(* ::Subsection::Closed:: *)
(*ClearSpinorsPrivate*)


FixedSpinors={};
ClearSpinorsPrivate[]:=(Unprotect[SpinorUndot,SpinorDot];SubValues[SpinorUndot]=DeleteCases[SubValues[SpinorUndot],_?(FreeQ[First[#],HoldPattern@SpinorUndot[Pattern]]&)];
SubValues[SpinorDot]=DeleteCases[SubValues[SpinorDot],_?(FreeQ[First[#],HoldPattern@SpinorDot[Pattern]]&)];
Protect[SpinorUndot,SpinorDot];)


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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
    ({a,b,c,d} |->
        Sequence@@
            {SpinorAngleBracket[a,b] SpinorAngleBracket[c,d] + SpinorAngleBracket[b,c] SpinorAngleBracket[a,d] + SpinorAngleBracket[c,a] SpinorAngleBracket[b,d] == 0,
             SpinorSquareBracket[a,b] SpinorSquareBracket[c,d] + SpinorSquareBracket[b,c] SpinorSquareBracket[a,d] + SpinorSquareBracket[c,a] SpinorSquareBracket[b,d] == 0})@@@
        Subsets[momenta,{4}]

(* SchoutenSimplify*)
SchoutenSimplify[expr_] :=
    Simplify[expr,
        Assumptions -> SchoutenIdentities[Momenta[expr]],
        TransformationFunctions -> {Automatic, (e |-> e /. SchoutenRules)}]


(* ::Subsection::Closed:: *)
(*Mom4D*)


Mom4D[l1_][$up][a_][b_]:=Spinorundot[l1][$lam][a][Null]Spinordot[l1][$lam][b][Null]+extramass[l1]extramasstilde[l1]/(Spinoranglebracket[l1,OverBar[l1]]Spinorsquarebracket[OverBar[l1],l1])Spinorundot[l1][$mu][a][Null]Spinordot[l1][$mu][b][Null];
Mom4D[l1_][$down][a_][b_]:=Spinorundot[l1][$lam][Null][a]Spinordot[l1][$lam][Null][b]+extramass[l1]extramasstilde[l1]/(Spinoranglebracket[l1,OverBar[l1]]Spinorsquarebracket[OverBar[l1],l1])Spinorundot[l1][$mu][Null][a]Spinordot[l1][$mu][Null][b];


(* ::Subsection::Closed:: *)
(*S6*)


S6[x_,y_]:=-extramass[x]extramasstilde[y]-extramass[y]extramasstilde[x]+Mom4D[x][$up][$contractvariable1][$contractvariable2]Mom4D[y][$down][$contractvariable1][$contractvariable2]//Expand;


(* ::Subsection::Closed:: *)
(*S*)


SetAttributes[S,Orderless];


(* ::Subsection:: *)
(*SNum*)


SNum[x__]:=SNum[x]=Total@Join[2*mpN6@@@Subsets[{x},{2}],mpN6/@{x}];


(* ::Subsection::Closed:: *)
(*S4*)


SetAttributes[S4,{Orderless,Protected}];


(* ::Subsection::Closed:: *)
(*S6many*)


S6many[labels__]:=Block[{locvar,locexp},
locvar={labels};
locvar=DeleteDuplicates[Sort/@Tuples[locvar,2]];
locvar=DeleteCases[locvar,x_/;Length[DeleteDuplicates[x]]==1];
debugPrint["locvar= ",locvar];
locexp=Plus@@Apply[S6[#1,#2]&,locvar,{1}];
Return[locexp];
];


(* ::Subsection::Closed:: *)
(*CompleteDenominators*)


(*CompleteDenominators[exp_]:=Module[{localexp,numtot,dentot,numden},
localexp=Together[exp];
numtot=Numerator[localexp];
dentot=Denominator[localexp];
dentot=dentot/.{SpinorAngleBracket[x_,y_]:>S4[x,y]/SpinorSquareBracket[y,x]};
numden=Numerator[dentot]/.{SpinorSquareBracket[x_,y_]:>S4[x,y]/SpinorAngleBracket[y,x]};
dentot=numden/Denominator[dentot];
Return[numtot/dentot];
];*)


CompleteDenominators[exp_]:=exp/.{Power[SpinorAngleBracket[a_,b_],n_?Negative]:>Power[S4[a,b]/SpinorSquareBracket[b,a],n],Power[SpinorSquareBracket[a_,b_],n_?Negative]:>Power[S4[a,b]/SpinorAngleBracket[b,a],n]};


(* ::Subsection::Closed:: *)
(*CompleteMandelstam*)


(*Only works if both have the same power*)
(*CompleteMandelstam[exp_]:=exp/.{SpinorAngleBracket[a_,b_]*SpinorSquareBracket[a_,b_]:>-S4[a,b],Power[SpinorAngleBracket[a_,b_],n_]*Power[SpinorSquareBracket[a_,b_],n_]:>Power[-S4[a,b],n]};*)


(*The large number of possible cases is given by the fact that exp^1 is not written as Power[exp,1] but simply as exp. For the denominator, where Power is always required there are in fact only two possible cases and thus two replacement rules.*)
CompleteMandelstam[exp_]:=exp//.{SpinorAngleBracket[x_,y_]*SpinorSquareBracket[x_,y_]:>S4[x,y],
SpinorAngleBracket[x_,y_]*Power[SpinorSquareBracket[x_,y_],n2_?Positive]:>S4[x,y]Power[SpinorSquareBracket[x,y],n2-1],Power[SpinorAngleBracket[x_,y_],n1_?Positive]SpinorSquareBracket[x_,y_]:>S4[x,y]Power[SpinorAngleBracket[x,y],n1-1],
Power[SpinorAngleBracket[x_,y_],n1_?Positive]*Power[SpinorSquareBracket[x_,y_],n2_?Positive]/;n1>=n2:>Power[S4[x,y],n2]*Power[SpinorAngleBracket[x,y],n1-n2],Power[SpinorAngleBracket[x_,y_],n1_?Positive]*Power[SpinorSquareBracket[x_,y_],n2_?Positive]/;n1<n2:>Power[S4[x,y],n1]*Power[SpinorSquareBracket[x,y],n2-n1],Power[SpinorAngleBracket[x_,y_],n1_?Negative]*Power[SpinorSquareBracket[x_,y_],n2_?Negative]/;n1>=n2:> Power[S4[x,y],n1]*Power[SpinorSquareBracket[x,y],n2-n1],Power[SpinorAngleBracket[x_,y_],n1_?Negative]*Power[SpinorSquareBracket[x_,y_],n2_?Negative]/;n1<n2:> Power[S4[x,y],n2]*Power[SpinorAngleBracket[x,y],n1-n2]};


(* ::Subsection::Closed:: *)
(*ScalProdToS*)


ScalProdToS[exp_]:=exp/.{mp[i_,j_]:>S[i,j]/2+(extramass[i]extramasstilde[j]+extramass[j]extramasstilde[i])/2};


(* ::Subsection::Closed:: *)
(*chain*)


(*Contraction properties of the chains*)
(*SquareAngle to AngleSquare*)
chain[$square,x_,k_List,y_,$angle]:=(-1)^(Length[k]+1)chain[$angle,y,Reverse[k],x,$square];
(*AngleAngle with SquareSquare*)
chain /: Times[chain[$angle,x_,k_List,y_,$angle],chain[$square,y_,q_List,z_,$square]]:=chain[$angle,x,Join[k,{y},q],z,$square];
chain /: Times[chain[$angle,x_,k_List,y_,$angle],chain[$square,z_,q_List,y_,$square]]:=(-1)^(Length[q]+1)chain[$angle,x,Join[k,{y},Reverse[q]],z,$square];
chain /: Times[chain[$angle,y_,k_List,x_,$angle],chain[$square,y_,q_List,z_,$square]]:=(-1)^(Length[k]+1)chain[$angle,x,Join[Reverse[k],{y},q],z,$square];
chain /: Times[chain[$angle,y_,k_List,x_,$angle],chain[$square,z_,q_List,y_,$square]]:=chain[$square,z,Join[q,{y},k],x,$angle];
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
chain[type_,x_,{y___,z_,z_,k___},l_,type2_]:=mp[z,z]*chain[type,x,{y,k},l,type2];

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


(* ::Subsection::Closed:: *)
(*Chain*)


(*Removing minus signs from the external brackets*)
(*(*Minus signs in the external brackest*)
Chain[type1_,-x_,{y___},z_,type2_]/;MemberQ[MomList,x]:=I*Chain[type1,x,{y},z,type2];
Chain[type1_,x_,{y___},-z_,type2_]/;MemberQ[MomList,z]:=I*Chain[type1,x,{y},z,type2];
Chain[type1_,-x_,{y___},-z_,type2_]/;MemberQ[MomList,x]&&MemberQ[MomList,z]:=(-1)*Chain[type1,x,{y},z,type2];
*)
(*Minus signs in the external brackest*)
Chain[type1_,x_?MinusSignQ,{y___},z_,type2_]:=I*Chain[type1,-x,{y},z,type2];
Chain[type1_,x_,{y___},z_?MinusSignQ,type2_]:=I*Chain[type1,x,{y},-z,type2];
Chain[type1_,x_?MinusSignQ,{y___},z_?MinusSignQ,type2_]:=(-1)*Chain[type1,-x,{y},-z,type2];


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


(* ::Subsection::Closed:: *)
(*ChainN*)


ChainN[type1_,p1_,{p2__},p3_,type2_]:=ChainN[type1,p1,{p2},p3,type2]=Module[{loc,momenta,pos,count},
If[type2===$square,
loc=SpinorDotN[p3][$lam][$up];
count=0;
,
loc=SpinorUndotN[p3][$lam][$down];
count=1;
];
pos[n_?OddQ]:=$up;
pos[n_?EvenQ]:=$down;
Do[loc=MomMat4DN[i][pos[count++]] . loc,{i,Reverse[{p2}]}];
If[type1===$angle,
loc=SpinorUndotN[p1][$lam][$up] . loc;
,
loc=SpinorDotN[p1][$lam][$down] . loc;
];
Return[loc];
];


(* ::Subsection::Closed:: *)
(*ToChain*)


(*ToChainAux[exp_]:=Block[{localexp,MyPower,counter},
localexp=exp/.{Power->MyPower,Chain->chain};
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

Options[ToChain]={MinimalChains->True,RecursionLimit->10};
ToChain[exp_,OptionsPattern[]]:=Module[{expin,expout,count},
expin=Null;
expout=exp;
count=0;
While[!TrueQ[expin==expout],
expin=expout;
expout=ToChainAux[expin];
If[count>=OptionValue[RecursionLimit],
Print["Either the expression is very complicated or something has gone wrong. You can try increasing the RecursionLimit, if it still fails check your expression for errors."];
Break[];
];
];

(*Reduce the chains to minimal form if required*)

If[TrueQ[OptionValue[MinimalChains]],
expout=expout//.{Chain[$angle,x_,{y__,x_,z__},k_,type2_]/;OddQ[Length[{y}]]:>Chain[$angle,x,{y},x,$square]*Chain[$angle,x,{z},k,type2],
Chain[$square,x_,{y__,x_,z__},k_,type2_]/;OddQ[Length[{y}]]:>Chain[$square,x,{y},x,$angle]*Chain[$square,x,{z},k,type2],
Chain[type1_,k_,{y__,x_,z__},x_,$angle]/;OddQ[Length[{z}]]:>Chain[type1,k,{y},x,$angle]*Chain[$square,x,{z},x,$angle],
Chain[type1_,k_,{y__,x_,z__},x_,$square]/;OddQ[Length[{z}]]:>Chain[type1,k,{y},x,$square]*Chain[$angle,x,{z},x,$square]};
];

Return[expout];
];*)


Options[ToChain]={MinimalChains->True};
ToChain[exp_,OptionsPattern[]]:=Block[{ChainPow,locexp},
(*Chain properties:*)

(*Basics*)
ChainPow[x__][0]:=1;

(*Minus signs in the external brackest*)
ChainPow[type1_,-x_,{y___},z_,type2_][n_]/;MemberQ[MomList,x]:=(I)^n*ChainPow[type1,x,{y},z,type2][n];
ChainPow[type1_,x_,{y___},-z_,type2_][n_]/;MemberQ[MomList,z]:=(I)^n*ChainPow[type1,x,{y},z,type2][n];
ChainPow[type1_,-x_,{y___},-z_,type2_][n_]/;MemberQ[MomList,x]&&MemberQ[MomList,z]:=(-1)^n*ChainPow[type1,x,{y},z,type2][n];

(*SquareAngle as AngleSquare*)
ChainPow[$square,x_,{y__},z_,$angle][n_]:=ChainPow[$angle,z,Reverse[{y}],x,$square][n];

(*The actual contraction properties*)

(* (x...y\[RightAngleBracket][y...z) *)
ChainPow /: Times[ChainPow[type1_,x_,{k___},y_,$angle][n_?Positive],ChainPow[$square,y_,{q___},z_,type2_][m_?Positive]]:=If[n>=m,ChainPow[type1,x,{k,y,q},z,type2][m]ChainPow[type1,x,{k},y,$angle][n-m],
ChainPow[type1,x,{k,y,q},z,type2][n]ChainPow[$square,y,{q},z,type2][m-n]
];

ChainPow /: Times[ChainPow[type1_,x_,{k___},y_,$angle][n_?Negative],ChainPow[$square,y_,{q___},z_,type2_][m_?Negative]]:=If[n<=m,ChainPow[type1,x,{k,y,q},z,type2][m]ChainPow[type1,x,{k},y,$angle][n-m],
ChainPow[type1,x,{k,y,q},z,type2][n]ChainPow[$square,y,{q},z,type2][m-n]
];

(*\[LeftAngleBracket]y...x)[y...z) *)
ChainPow /: Times[ChainPow[$angle,y_,{k___},x_,type1_][n_?Positive],ChainPow[$square,y_,{q___},z_,type2_][m_?Positive]]:=If[n>=m,
(-1)^(m*(Length[{k}]+1))*ChainPow[type1,x,{Sequence@@Reverse[{k}],y,q},z,type2][m]ChainPow[$angle,y,{k},x,type1][n-m],
(-1)^(n*(Length[{k}]+1))*ChainPow[type1,x,{Sequence@@Reverse[{k}],y,q},z,type2][n]ChainPow[$square,y,{q},z,type2][m-n]
];

ChainPow /: Times[ChainPow[$angle,y_,{k___},x_,type1_][n_?Negative],ChainPow[$square,y_,{q___},z_,type2_][m_?Negative]]:=If[n<=m,
(-1)^(m*(Length[{k}]+1))*ChainPow[type1,x,{Sequence@@Reverse[{k}],y,q},z,type2][m]ChainPow[$angle,y,{k},x,type1][n-m],
(-1)^(n*(Length[{k}]+1))*ChainPow[type1,x,{Sequence@@Reverse[{k}],y,q},z,type2][n]ChainPow[$square,y,{q},z,type2][m-n]
];

(*(x...y\[RightAngleBracket][y...z)*)
ChainPow /: Times[ChainPow[type1_,x_,{k___},y_,$angle][n_?Positive],ChainPow[type2_,z_,{q___},y_,$square][m_?Positive]]:=If[n>=m,
(-1)^(m*(Length[{q}]+1))*ChainPow[type1,x,{k,y,Sequence@@Reverse[{q}]},z,type2][m]ChainPow[type1,x,{k},y,$angle][n-m],
(-1)^(n*(Length[{q}]+1))*ChainPow[type1,x,{k,y,Sequence@@Reverse[{q}]},z,type2][n]ChainPow[type2,z,{q},y,$square][m-n]
];

ChainPow /: Times[ChainPow[type1_,x_,{k___},y_,$angle][n_?Negative],ChainPow[type2_,z_,{q___},y_,$square][m_?Negative]]:=If[n<=m,
(-1)^(m*(Length[{q}]+1))*ChainPow[type1,x,{k,y,Sequence@@Reverse[{q}]},z,type2][m]ChainPow[type1,x,{k},y,$angle][n-m],
(-1)^(n*(Length[{q}]+1))*ChainPow[type1,x,{k,y,Sequence@@Reverse[{q}]},z,type2][n]ChainPow[type2,z,{q},y,$square][m-n]
];

(* \[LeftAngleBracket]y...x)(z...y] *)
ChainPow /: Times[ChainPow[$angle,y_,{k___},x_,type1_][n_?Positive],ChainPow[type2_,z_,{q___},y_,$square][m_?Positive]]:=If[n>=m,
ChainPow[type2,z,{q,y,k},x,type1][m]ChainPow[$angle,y,{k},x,type1][n-m],
ChainPow[type2,z,{q,y,k},x,type1][n]ChainPow[type2,z,{q},y,$square][m-n]
];

ChainPow /: Times[ChainPow[$angle,y_,{k___},x_,type1_][n_?Negative],ChainPow[type2_,z_,{q___},y_,$square][m_?Negative]]:=If[n<=m,
ChainPow[type2,z,{q,y,k},x,type1][m]ChainPow[$angle,y,{k},x,type1][n-m],
ChainPow[type2,z,{q,y,k},x,type1][n]ChainPow[type2,z,{q},y,$square][m-n]
];

(*Applying the properties to the expression*)
(*Convert angle and square brackets to Chains*)
locexp=exp//.{SpinorAngleBracket[x_,y_]:>Chain[$angle,x,{},y,$angle],SpinorSquareBracket[x_,y_]:>Chain[$square,x,{},y,$square],Chain[x__]:>ChainPow[x][1],Power[Chain[x__],n_]:>ChainPow[x][n]};

(*Expand part of the expression containing ChainPow in order for the contractions to happen*)
locexp=Expand[locexp,ChainPow];

(*Convert back to angle and square brackets*)
locexp=locexp//.{Chain[$angle,x_,{},y_,$angle]:>SpinorAngleBracket[x,y],Chain[$square,x_,{},y_,$square]:>SpinorSquareBracket[x,y],ChainPow[x__][n_]:>Power[Chain[x],n]};

(*Reduce the chains to minimal form if required*)

If[TrueQ[OptionValue[MinimalChains]],
locexp=locexp//.{Chain[$angle,x_,{y__,x_,z__},k_,type2_]/;OddQ[Length[{y}]]:>Chain[$angle,x,{y},x,$square]*Chain[$angle,x,{z},k,type2],
Chain[$square,x_,{y__,x_,z__},k_,type2_]/;OddQ[Length[{y}]]:>Chain[$square,x,{y},x,$angle]*Chain[$square,x,{z},k,type2],
Chain[type1_,k_,{y__,x_,z__},x_,$angle]/;OddQ[Length[{z}]]:>Chain[type1,k,{y},x,$angle]*Chain[$square,x,{z},x,$angle],
Chain[type1_,k_,{y__,x_,z__},x_,$square]/;OddQ[Length[{z}]]:>Chain[type1,k,{y},x,$square]*Chain[$angle,x,{z},x,$square]};
];

Return[locexp];
];


(* ::Subsection::Closed:: *)
(*ChainMomCon*)


(*Options[ChainMomCon]={MomCon->{}};

ChainMomCon[exp_,OptionsPattern[]]:=Catch[Block[{locexp,momcon,Chain,mom,mom2},
momcon=OptionValue[MomCon];
If[Head[momcon]===Rule||Head[momcon]==RuleDelayed,momcon={momcon}];
(*Before applying ChainMomCon the user needs to declare which labels are momenta, which will be used to impose linearity for the Chain*)
momcon=momcon/.{x_/;MemberQ[MomList,x]:>MomPure[x]};

(*If no momentum conservation is defined return the original expression, or if it is ill defined returne error message and original expression*)
Which[Length[momcon]==0,
Throw[exp],
!AllTrue[momcon,(Head[#]===Rule||Head[#]===RuleDelayed)&],
Print["Momentum conservation entered in the wrong format, will proceed ignoring it."];
Throw[exp],
True,
Null;
];

(*Else prepare the chains for the replacements*)
locexp=exp/.{x_/;MemberQ[MomList,x]:>MomPure[x]};

(*Define the linearity property of chains with respect to pure momenta and the rule to take constants out of the chains*)
Chain[type1_,x1_,{x2___,A_*MomPure[y_]+B_,x3___},x4_,type2_]/;FreeQ[A,MomPure]:=A*Chain[type1,x1,{x2,MomPure[y],x3},x4,type2]+Chain[type1,x1,{x2,B,x3},x4,type2];
Chain[type1_,x1_,{x2___,MomPure[y_]+B_,x3___},x4_,type2_]:=Chain[type1,x1,{x2,MomPure[y],x3},x4,type2]+Chain[type1,x1,{x2,B,x3},x4,type2];
Chain[type1_,x1_,{x2___,Times[A_,MomPure[y_]],x3___},x4_,type2_]/;FreeQ[A,MomPure]:=A*Chain[type1,x1,{x2,MomPure[y],x3},x4,type2];

(*Apply the replacement through rules*)
Do[
mom=First[i];
mom2=Last[i];
			(*If the momentum to be replaced appears at the extrema of a chain which is a Dirac trace we can move it. Notice that it must be replaced by a momentum which can be written in terms of spinors, i.e. a massless momentum*)
		locexp=locexp/.{Chain[$angle,mom,{x___,y_?MasslessQ,z___},mom,$square]:>Chain[$angle,y,{z,mom,x},y,$square],Chain[$square,mom,{x___,y_?MasslessQ,z___},mom,$angle]:>Chain[$square,y,{z,mom,x},y,$angle]};
(*Perform the actual momentum conservation replacement*)
locexp=locexp/.Chain[t1_,x1_,{x2___,mom,x3___},x4_,t2_]:>Chain[t1,x1,{x2,mom2,x3},x4,t2];
			,{i,momcon}];

(*Return output*)
locexp=locexp/.{MomPure[x_]:>x};
Return[locexp];
];
];*)


Options[ChainMomCon]={MomCon->{}};

ChainMomCon[exp_,OptionsPattern[]]:=Catch[Block[{locexp,momcon,Chain,mom,mom2},
momcon=OptionValue[MomCon];
If[Head[momcon]===Rule||Head[momcon]==RuleDelayed,momcon={momcon}];
(*Before applying ChainMomCon the user needs to declare which labels are momenta, which will be used to impose linearity for the Chain*)
momcon=momcon/.{x_/;MemberQ[MomList,x]:>MomPure[x]};

(*If no momentum conservation is defined return the original expression, or if it is ill defined returne error message and original expression*)
Which[Length[momcon]==0,
Throw[exp],
!AllTrue[momcon,(Head[#]===Rule||Head[#]===RuleDelayed)&],
Print["Momentum conservation entered in the wrong format, will proceed ignoring it."];
Throw[exp],
True,
Null;
];

(*Else prepare the chains for the replacements*)
locexp=exp/.{x_/;MemberQ[MomList,x]:>MomPure[x]};

(*Define the linearity property of chains with respect to pure momenta and the rule to take constants out of the chains*)
Chain[type1_,x1_,{x2___,A_*MomPure[y_]+B_,x3___},x4_,type2_]/;FreeQ[A,MomPure]:=A*Chain[type1,x1,{x2,MomPure[y],x3},x4,type2]+Chain[type1,x1,{x2,B,x3},x4,type2];
Chain[type1_,x1_,{x2___,MomPure[y_]+B_,x3___},x4_,type2_]:=Chain[type1,x1,{x2,MomPure[y],x3},x4,type2]+Chain[type1,x1,{x2,B,x3},x4,type2];
Chain[type1_,x1_,{x2___,Times[A_,MomPure[y_]],x3___},x4_,type2_]/;FreeQ[A,MomPure]:=A*Chain[type1,x1,{x2,MomPure[y],x3},x4,type2];

(*Apply the replacement through rules*)
Do[
mom=First[i];
mom2=Last[i];
			(*If the momentum to be replaced appears at the extrema of a chain which is a Dirac trace we can move it. Notice that it must be replaced by a momentum which can be written in terms of spinors, i.e. a massless momentum*)
		locexp=locexp/.{Chain[$angle,mom,{x___,y_?MasslessQ,z___},mom,$square]:>Chain[$angle,y,{z,mom,x},y,$square],Chain[$square,mom,{x___,y_?MasslessQ,z___},mom,$angle]:>Chain[$square,y,{z,mom,x},y,$angle]};
(*Perform the actual momentum conservation replacement*)
locexp=locexp/.{Chain[t1_,x1_,{x2___,mom,x3___},x4_,t2_]:>Chain[t1,x1,{x2,mom2,x3},x4,t2],mp[mom,x_]:>mp[mom2,x]};
			,{i,momcon}];

(*Return output*)
locexp=locexp/.{MomPure[x_]:>x};
Return[locexp];
];
];


(* ::Subsection::Closed:: *)
(*ChainOrder*)


(*Auxiliary function whivh simply reorders the momenta in chains*)

ChainOrder[exp_]:=Block[{Chain,locexp},
(*Define properties which reorder the chains with momenta in canonical order*)
Chain[type_,x1_,{y1___,z1_,z2_,y2___},x2_,type2_]/;OrderedQ[{z2,z1}]:=2*mp[z1,z2]*Chain[type,x1,{y1,y2},x2,type2]-Chain[type,x1,{y1,z2,z1,y2},x2,type2];
(*Chains which reduce to Dirac traces can be further simplified including also the extrema in the ordering. Here however you need to be carefull because the traces are chiral so there is a hidden gamma5! In other words if y3 and y1 are of even length the angle and square brackets swap roles*)
Chain[$angle,x_,{y1___,y2_,y3___},x_,$square]/;AnyTrue[{y1,y2,y3},MasslessQ]&&First[Sort[Select[{y1,y2,y3},MasslessQ]]]===y2&&OrderedQ[{y2,x}]:=
If[OddQ[Length[{y3}]],
Chain[$angle,y2,{y3,x,y1},y2,$square],
Chain[$square,y2,{y3,x,y1},y2,$angle]];
Chain[$square,x_,{y1___,y2_,y3___},x_,$angle]/;AnyTrue[{y1,y2,y3},MasslessQ]&&First[Sort[Select[{y1,y2,y3},MasslessQ]]]===y2&&OrderedQ[{y2,x}]:=
If[OddQ[Length[{y3}]],
Chain[$square,y2,{y3,x,y1},y2,$angle],
Chain[$angle,y2,{y3,x,y1},y2,$square]];
locexp=exp;
Return[locexp];
];


(* ::Subsection::Closed:: *)
(*ChainSimplify*)


(*ChainSimplify[exp_]:=Block[{Chain,locexp},
Chain[type_,x_,{x_,y___},z_,type2_]:=0;
	Chain[type_,x_,{y___,z_},z_,type2_]:=0;
	Chain[type_,x_,{y___,z_,z_,k___},l_,type2_]:=mp[z,z]*Chain[type,x,{y,k},l,type2];
Chain[type1_,p1_,{x___,y_,l___,z_,y_,k___},p2_,type2_]:=2*mp[MomPure[y],MomPure[z]]*Chain[type1,p1,{x,y,l,k},p2,type2]-Chain[type1,p1,{x,y,l,y,z,k},p2,type2];
Chain[t1_,x_,{y___,z_,x_,k___},p2_,t2_]:=2*mp[MomPure[z],MomPure[x]]Chain[t1,x,{y,k},p2,t2]-Chain[t1,x,{y,x,z,k},p2,t2];
Chain[$angle,x_,{y_},x_,$square]:=2*mp[MomPure[x],MomPure[y]];
Chain[$square,x_,{y_},x_,$angle]:=2*mp[MomPure[x],MomPure[y]];
locexp=exp/.{Chain[$angle,x_,{},y_,$angle]:>SpinorAngleBracket[x,y],Chain[$square,x_,{},y_,$square]:>SpinorSquareBracket[x,y]};
Return[locexp];
];*)


Options[ChainSimplify]={MomCon->{},ReduceComplete->False};

ChainSimplify[exp_,OptionsPattern[]]:=Block[{Chain,locexp},
Chain[type_,x_,{x_,y___},z_,type2_]:=0;
	Chain[type_,x_,{y___,z_},z_,type2_]:=0;
	Chain[type_,x_,{y___,z_,z_,k___},l_,type2_]:=mp[z,z]*Chain[type,x,{y,k},l,type2];
Chain[type1_,p1_,{x___,y_,l___,z_,y_,k___},p2_,type2_]:=2*mp[MomPure[y],MomPure[z]]*Chain[type1,p1,{x,y,l,k},p2,type2]-Chain[type1,p1,{x,y,l,y,z,k},p2,type2];
Chain[t1_,x_,{y___,z_,x_,k___},p2_,t2_]:=2*mp[MomPure[z],MomPure[x]]Chain[t1,x,{y,k},p2,t2]-Chain[t1,x,{y,x,z,k},p2,t2];
Chain[$angle,x_,{y_},x_,$square]:=2*mp[MomPure[x],MomPure[y]];
Chain[$square,x_,{y_},x_,$angle]:=2*mp[MomPure[x],MomPure[y]];

(*Apply momentum conservation*)
locexp=ChainMomCon[exp,{MomCon->OptionValue[MomCon]}];

(*If ReduceComplete is et to True then reduce the structures as much as possible by also ordering things:*)

If[OptionValue[ReduceComplete],
locexp=ChainOrder[locexp];
];

locexp=locexp/.{Chain[$angle,x_,{},y_,$angle]:>SpinorAngleBracket[x,y],Chain[$square,x_,{},y_,$square]:>SpinorSquareBracket[x,y]};
Return[locexp];
];


(* ::Subsection::Closed:: *)
(*ChainToSpinor*)


(*ChainToSpinor[exp_]:=Module[{localexp},
localexp=exp;
localexp=localexp/.{Chain[$angle,a_,b_,c_,$square]:>Spinoranglebracket[a,b[[1]]]Product[Spinoranglebracket[b[[i]],b[[i+1]]],{i,2,Length[b],2}]Spinorsquarebracket[Last[b],c]Product[Spinorsquarebracket[b[[i]],b[[i+1]]],{i,1,Length[b]-1,2}],Chain[$square,a_,b_,c_,$angle]:>Spinorsquarebracket[a,b[[1]]]Product[Spinoranglebracket[b[[i]],b[[i+1]]],{i,1,Length[b]-1,2}]Spinoranglebracket[Last[b],c]Product[Spinorsquarebracket[b[[i]],b[[i+1]]],{i,2,Length[b],2}],Chain[$angle,a_,b_,c_,$angle]:>Spinoranglebracket[a,b[[1]]]Product[Spinoranglebracket[b[[i]],b[[i+1]]],{i,2,Length[b]-1,2}]Spinoranglebracket[Last[b],c]Product[Spinorsquarebracket[b[[i]],b[[i+1]]],{i,1,Length[b],2}],Chain[$square,a_,b_,c_,$square]:>Spinorsquarebracket[a,b[[1]]]Product[Spinoranglebracket[b[[i]],b[[i+1]]],{i,1,Length[b],2}]Spinorsquarebracket[Last[b],c]Product[Spinorsquarebracket[b[[i]],b[[i+1]]],{i,2,Length[b]-1,2}],
chain[$angle,a_,b_,c_,$square]:>Spinoranglebracket[a,b[[1]]]Product[Spinoranglebracket[b[[i]],b[[i+1]]],{i,2,Length[b],2}]Spinorsquarebracket[Last[b],c]Product[Spinorsquarebracket[b[[i]],b[[i+1]]],{i,1,Length[b]-1,2}],chain[$angle,a_,b_,c_,$angle]:>Spinoranglebracket[a,b[[1]]]Product[Spinoranglebracket[b[[i]],b[[i+1]]],{i,2,Length[b]-1,2}]Spinoranglebracket[Last[b],c]Product[Spinorsquarebracket[b[[i]],b[[i+1]]],{i,1,Length[b],2}],chain[$square,a_,b_,c_,$square]:>Spinorsquarebracket[a,b[[1]]]Product[Spinoranglebracket[b[[i]],b[[i+1]]],{i,1,Length[b],2}]Spinorsquarebracket[Last[b],c]Product[Spinorsquarebracket[b[[i]],b[[i+1]]],{i,2,Length[b]-1,2}]};
Return[localexp];
];*)


ChainToSpinor[exp_]:=Block[{locexp,Chain},
(*Define the local properties of Chain which will allow for the splitting*)
Chain[$angle,a1_,{x___,y_,z___},a2_,type_]/;MasslessQ[y]:=If[OddQ[Length[{x,Null}]],
Chain[$angle,a1,{x},y,$angle]*Chain[$square,y,{z},a2,type],
Chain[$angle,a1,{x},y,$square]*Chain[$angle,y,{z},a2,type]];
Chain[$square,a1_,{x___,y_,z___},a2_,type_]/;MasslessQ[y]:=If[OddQ[Length[{x,Null}]],
Chain[$square,a1,{x},y,$square]*Chain[$angle,y,{z},a2,type],
Chain[$square,a1,{x},y,$angle]*Chain[$square,y,{z},a2,type]
];

(*Define locexp and let the definitions act*)
locexp=exp;

(*Replace empty chains with angle and square brackets*)
locexp=locexp/.{Chain[$angle,x_,{},y_,$angle]:>SpinorAngleBracket[x,y],Chain[$square,x_,{},y_,$square]:>SpinorSquareBracket[x,y]};

(*Return output*)
Return[locexp];
];


(* ::Subsection::Closed:: *)
(*mp*)


(*mpBox[x_,y_]:=TemplateBox[{x,y},"ScalarProduct",
DisplayFunction->(RowBox[{"(",#1,"\[CenterDot]",#2,")"}]&),
InterpretationFunction->(RowBox[{"mp","[",#1,",",#2,"]"}]&)];

(*In order to make it more user friendly we define the function in such a way that it automatically adds the MomPure to its argument if it is not already in that format.*)
mp[x_]:=mp[x,x];
mp[x_,y_]/;FreeQ[x,MomPure]&&FreeQ[x,PolarPure]:=mp[MomPure[x],y];


mp /: MakeBoxes[mp[x_,y_],StandardForm|TraditionalForm]:=mpBox[ToBoxes[x],ToBoxes[y]];
SetAttributes[mp,{Orderless,Protected}];*)


mpBox[x_,y_]:=TemplateBox[{x,y},"ScalarProduct",
DisplayFunction->(RowBox[{"(",#1,"\[CenterDot]",#2,")"}]&),
InterpretationFunction->(RowBox[{"mp","[",#1,",",#2,"]"}]&)];

(*Different display when the two momenta are equal*)

mpBox[x_,x_]:=TemplateBox[{x},"ScalarProduct2",
DisplayFunction->(SuperscriptBox[RowBox[{"(",#1,")"}],"2"]&),
InterpretationFunction->(RowBox[{"mp","[",#1,",",#1,"]"}]&)];

(*In order to make it more user friendly we define the function in such a way that it automatically adds the MomPure to its argument if it is not already in that format.*)
mp[x_]:=mp[x,x];
mp[x_,y_]/;FreeQ[x,MomPure]&&FreeQ[x,PolarPure]:=mp[MomPure[x]/.{a_/;MemberQ[MomList,a]:>MomPure[a]},y];

(*Linearity in declared momenta*)
mp[A_*MomPure[x_]+y_,z_]:=A*mp[MomPure[x],z]+mp[y,z];
mp[MomPure[x_]+y_,z_]:=mp[MomPure[x],z]+mp[y,z];
mp[Times[A_,x_],y_]:=A*mp[x,y];
mp[z_,A_*MomPure[x_]+y_]:=A*mp[MomPure[x],z]+mp[y,z];
mp[z_,MomPure[x_]+y_]:=mp[MomPure[x],z]+mp[y,z];
mp[y_,Times[A_,x_]]:=A*mp[x,y];


mp /: MakeBoxes[mp[x_,y_],StandardForm|TraditionalForm]:=mpBox[ToBoxes[x],ToBoxes[y]];
SetAttributes[mp,{Orderless,Protected}];


(* ::Subsection:: *)
(*mpToSpinors*)


mpToSpinors[exp_]:=exp//.{mp[i_,j_]/;MemberQ[Momenta4D,i]&&MemberQ[Momenta4D,j]:>1/2*SpinorAngleBracket[i,j]SpinorSquareBracket[j,i],mp[MomPure[i_],MomPure[j_]]/;MemberQ[Momenta4D,i]&&MemberQ[Momenta4D,j]:>1/2*SpinorAngleBracket[MomPure[i],MomPure[j]]SpinorSquareBracket[MomPure[j],MomPure[i]]};


(* ::Subsection:: *)
(*mpN6*)


mpN6[x_]:=mpN6[x]=mpN6[x,x];
mpN6[MomPure[x_],y_]:=mpN6[MomPure[x],y]=mpN6[x,y];
mpN6[x_,MomPure[y_]]:=mpN6[x,MomPure[y]]=mpN6[x,y];
mpN6[x_,y_]:=mpN6[x,y]=scalarprod[Mom6DN[x],Mom6DN[y]];


(* ::Subsection::Closed:: *)
(*SetMp*)


(*(*List of fixed scalar products*)
Mps={};
Protect[Mps];

(*Assigns certain values to given scalar products:*)
SetMp[x_]:=(Unprotect[mp,Mps];
AppendTo[Mps,Flatten[{x}]];
DeleteCases[Flatten[{x}],y_/;FreeQ[y,mp]]/.{Rule->SetDelayed,RuleDelayed->SetDelayed};
Protect[mp,Mps];)*)


(*List of fixed scalar products*)
Mps={};
Protect[Mps];
SetAttributes[SetMp,HoldAll];

SetMp[x__]:=Module[{loclist,new},
(*First inactivate mp in order to avoid undesired evaluations of already defined scalar products*)
loclist=Flatten[Inactivate[{x},mp]];
(*Delete the instances where mp does not appear, for any reason*)
loclist=DeleteCases[loclist,y_/;FreeQ[y,mp]];
new=First/@loclist;
new=StringReplace[ToString[#,InputForm]&/@new,{"Inactive[mp]"->"mp"}];
Unprotect[mp,Mps];
Mps=DeleteCases[Mps,y_/;AnyTrue[new,StringContainsQ[y,#]&]];
Mps=Join[Mps,StringReplace[ToString[#,InputForm]&/@loclist,{"Inactive[mp]"->"mp"}]];
ClearDownValues[mp];
new=ToExpression/@Mps/.{Rule->SetDelayed,RuleDelayed->SetDelayed, Equal->SetDelayed};
Protect[mp,Mps];
];


(* ::Subsection::Closed:: *)
(*ClearMP*)


(*(*If ClearMp is called without any argument then all the scalar products are cleared, else only the specified values are reset.*)
ClearMp[x___]:=Module[{loc},
Unprotect[mp,Mps];
If[Length[Flatten[{x}]]==0,
ClearDownValues[mp];
Mps={},
Mps=DeleteCases[Mps,y_/;MemberQ[Flatten[{x}],First[y]]];
ClearDownValues[mp];
DeleteCases[Mps,y_/;FreeQ[y,mp]]/.{Rule->SetDelayed,RuleDelayed->SetDelayed};
];
Protect[mp,Mps];
];*)


(*If ClearMp is called without any argument then all the scalar products are cleared, else only the specified values are reset.*)

SetAttributes[ClearMp,HoldAll];

ClearMp[x___]:=Module[{loc,loclist,new},
loclist=DeleteCases[Flatten[Inactivate[{x},mp]],y_/;FreeQ[y,mp]];
new=StringReplace[ToString[#,InputForm]&/@loclist,{"Inactive[mp]"->"mp"}];
Unprotect[mp,Mps];
If[Length[loclist]==0,
ClearDownValues[mp];
Mps={},
Mps=DeleteCases[Mps,y_/;AnyTrue[new,StringContainsQ[y,#]&]];
new=Mps;
ClearDownValues[mp];
new=ToExpression/@new;
new=new/.{Rule->SetDelayed,RuleDelayed->SetDelayed};
];
Protect[mp,Mps];
];


(* ::Subsection::Closed:: *)
(*eps*)


(*epsBox[a_,b_,c_,d_]:=TemplateBox[{a,b,c,d},"eps",
DisplayFunction->(RowBox[{"\[Epsilon]","[",#1,",",#2,",",#3,",",#4,"]"}]&),
InterpretationFunction->(RowBox[{"eps","[",#1,",",#2,",",#3,",",#4,"]"}]&)
];
eps /: MakeBoxes[eps[a_,b_,c_,d_],StandardForm|TraditionalForm]:=epsBox[ToBoxes[a],ToBoxes[b],ToBoxes[c],ToBoxes[d]];

(*Contraction with twice the same vector vanishes*)
eps[x___,y_,z___,y_,k___]:=0;
(*Antisymmetry*)
eps[x___,y_,z_,k___]/;OrderedQ[{z,y}]:=-eps[x,z,y,k];*)


epsBox[a_,b_,c_,d_]:=TemplateBox[{a,b,c,d},"eps",
DisplayFunction->(RowBox[{"\[Epsilon]","[",#1,",",#2,",",#3,",",#4,"]"}]&),
InterpretationFunction->(RowBox[{"eps","[",#1,",",#2,",",#3,",",#4,"]"}]&)
];
eps /: MakeBoxes[eps[a_,b_,c_,d_],StandardForm|TraditionalForm]:=epsBox[ToBoxes[a],ToBoxes[b],ToBoxes[c],ToBoxes[d]];

(*Contraction with twice the same vector vanishes*)
eps[x___,y_,z___,y_,k___]:=0;
(*Antisymmetry*)
eps[x___,y_,z_,k___]/;OrderedQ[{z,y}]:=-eps[x,z,y,k];
(*Linearity with respect to declared momenta*)

eps[x___,A_*MomPure[y_]+z_,k___]:=A*eps[x,MomPure[y],k]+eps[x,z,k];
eps[x___,MomPure[y_]+z_,k___]:=eps[x,MomPure[y],k]+eps[x,z,k];
eps[x___,Times[A_,MomPure[y_]],z___]:=A*eps[x,MomPure[y],z];


SetAttributes[eps,Protected];


(* ::Subsection::Closed:: *)
(*TrG*)


TrG[x_List]/;OddQ[Length[x]]:=0;
TrG[{}]:=4;
TrG[x_List]:=Sum[(-1)^i*mp[x[[1]],x[[i]]]TrG[Delete[x,{{1},{i}}]],{i,2,Length[x]}];


(* ::Subsection::Closed:: *)
(*TrG5*)


TrG5[x_List]/;Length[x]<4:=0;
TrG5[x_List]/;OddQ[Length[x]]:=0;
TrG5[x_List]:=mp[x[[-3]],x[[-2]]]*TrG5[Delete[x,{{-3},{-2}}]]+mp[x[[-2]],x[[-1]]]*TrG5[x[[;;-3]]]-mp[x[[-3]],x[[-1]]]*TrG5[Delete[x,{{-3},{-1}}]]-I*Sum[(-1)^i*eps[x[[-i]],x[[-3]],x[[-2]],x[[-1]]]*TrG[Delete[x[[;;-4]],{-(i-3)}]],{i,4,Length[x]}];


(* ::Subsection::Closed:: *)
(*ToTrace*)


Options[ToTrace]={KillEpsilon->False};

ToTrace[exp_,OptionsPattern[]]:=Block[{eps,localexp},

(*Convert the chains to traces*)
localexp=exp/.{Chain[$angle,a_,b_List,a_,$square]:>(TrG[Join[{a},b]]-TrG5[Join[{a},b]])/2, Chain[$square,a_,b_List,a_,$angle]:>(TrG[Join[{a},b]]+TrG5[Join[{a},b]])/2};

(*Remove epsilon if requested:*)
Which[OptionValue[KillEpsilon],
eps /: Times[eps[a1_,b1_,c1_,d1_],eps[a2_,b2_,c2_,d2_]] := -(mp[MomPure[a1],MomPure[d2]]*mp[MomPure[a2],MomPure[d1]]*mp[MomPure[b1],MomPure[c2]]*mp[MomPure[b2],MomPure[c1]]-mp[MomPure[a1],MomPure[c2]]*mp[MomPure[a2],MomPure[d1]]*mp[MomPure[b1],MomPure[d2]]*mp[MomPure[b2],MomPure[c1]]-mp[MomPure[a1],MomPure[d2]]*mp[MomPure[a2],MomPure[c1]]*mp[MomPure[b1],MomPure[c2]]*mp[MomPure[b2],MomPure[d1]]+mp[MomPure[a1],MomPure[c2]]*mp[MomPure[a2],MomPure[c1]]*mp[MomPure[b1],MomPure[d2]]*mp[MomPure[b2],MomPure[d1]]-mp[MomPure[a1],MomPure[d2]]*mp[MomPure[a2],MomPure[d1]]*mp[MomPure[b1],MomPure[b2]]*mp[MomPure[c1],MomPure[c2]]+mp[MomPure[a1],MomPure[b2]]*mp[MomPure[a2],MomPure[d1]]*mp[MomPure[b1],MomPure[d2]]*mp[MomPure[c1],MomPure[c2]]+mp[MomPure[a1],MomPure[d2]]*mp[MomPure[a2],MomPure[b1]]*mp[MomPure[b2],MomPure[d1]]*mp[MomPure[c1],MomPure[c2]]-mp[MomPure[a1],MomPure[a2]]*mp[MomPure[b1],MomPure[d2]]*mp[MomPure[b2],MomPure[d1]]*mp[MomPure[c1],MomPure[c2]]+mp[MomPure[a1],MomPure[c2]]*mp[MomPure[a2],MomPure[d1]]*mp[MomPure[b1],MomPure[b2]]*mp[MomPure[c1],MomPure[d2]]-mp[MomPure[a1],MomPure[b2]]*mp[MomPure[a2],MomPure[d1]]*mp[MomPure[b1],MomPure[c2]]*mp[MomPure[c1],MomPure[d2]]-mp[MomPure[a1],MomPure[c2]]*mp[MomPure[a2],MomPure[b1]]*mp[MomPure[b2],MomPure[d1]]*mp[MomPure[c1],MomPure[d2]]+mp[MomPure[a1],MomPure[a2]]*mp[MomPure[b1],MomPure[c2]]*mp[MomPure[b2],MomPure[d1]]*mp[MomPure[c1],MomPure[d2]]+mp[MomPure[a1],MomPure[d2]]*mp[MomPure[a2],MomPure[c1]]*mp[MomPure[b1],MomPure[b2]]*mp[MomPure[c2],MomPure[d1]]-mp[MomPure[a1],MomPure[b2]]*mp[MomPure[a2],MomPure[c1]]*mp[MomPure[b1],MomPure[d2]]*mp[MomPure[c2],MomPure[d1]]-mp[MomPure[a1],MomPure[d2]]*mp[MomPure[a2],MomPure[b1]]*mp[MomPure[b2],MomPure[c1]]*mp[MomPure[c2],MomPure[d1]]+mp[MomPure[a1],MomPure[a2]]*mp[MomPure[b1],MomPure[d2]]*mp[MomPure[b2],MomPure[c1]]*mp[MomPure[c2],MomPure[d1]]+mp[MomPure[a1],MomPure[b2]]*mp[MomPure[a2],MomPure[b1]]*mp[MomPure[c1],MomPure[d2]]*mp[MomPure[c2],MomPure[d1]]-mp[MomPure[a1],MomPure[a2]]*mp[MomPure[b1],MomPure[b2]]*mp[MomPure[c1],MomPure[d2]]*mp[MomPure[c2],MomPure[d1]]-mp[MomPure[a1],MomPure[c2]]*mp[MomPure[a2],MomPure[c1]]*mp[MomPure[b1],MomPure[b2]]*mp[MomPure[d1],MomPure[d2]]+mp[MomPure[a1],MomPure[b2]]*mp[MomPure[a2],MomPure[c1]]*mp[MomPure[b1],MomPure[c2]]*mp[MomPure[d1],MomPure[d2]]+mp[MomPure[a1],MomPure[c2]]*mp[MomPure[a2],MomPure[b1]]*mp[MomPure[b2],MomPure[c1]]*mp[MomPure[d1],MomPure[d2]]-mp[MomPure[a1],MomPure[a2]]*mp[MomPure[b1],MomPure[c2]]*mp[MomPure[b2],MomPure[c1]]*mp[MomPure[d1],MomPure[d2]]-mp[MomPure[a1],MomPure[b2]]*mp[MomPure[a2],MomPure[b1]]*mp[MomPure[c1],MomPure[c2]]*mp[MomPure[d1],MomPure[d2]]+mp[MomPure[a1],MomPure[a2]]*mp[MomPure[b1],MomPure[b2]]*mp[MomPure[c1],MomPure[c2]]*mp[MomPure[d1],MomPure[d2]]);
eps /: Power[eps[a1_,b1_,c1_,d1_],n_?EvenQ]:=Power[-(mp[MomPure[a1], MomPure[d1]]^2*mp[MomPure[b1], MomPure[c1]]^2 - 2*mp[MomPure[a1], MomPure[c1]]*mp[MomPure[a1], MomPure[d1]]*
  mp[MomPure[b1], MomPure[c1]]*mp[MomPure[b1], MomPure[d1]] + mp[MomPure[a1], MomPure[c1]]^2*mp[MomPure[b1], MomPure[d1]]^2 - 
 mp[MomPure[a1], MomPure[d1]]^2*mp[MomPure[b1], MomPure[b1]]*mp[MomPure[c1], MomPure[c1]] + 
 2*mp[MomPure[a1], MomPure[b1]]*mp[MomPure[a1], MomPure[d1]]*mp[MomPure[b1], MomPure[d1]]*mp[MomPure[c1], MomPure[c1]] - 
 mp[MomPure[a1], MomPure[a1]]*mp[MomPure[b1], MomPure[d1]]^2*mp[MomPure[c1], MomPure[c1]] + 
 2*mp[MomPure[a1], MomPure[c1]]*mp[MomPure[a1], MomPure[d1]]*mp[MomPure[b1], MomPure[b1]]*mp[MomPure[c1], MomPure[d1]] - 
 2*mp[MomPure[a1], MomPure[b1]]*mp[MomPure[a1], MomPure[d1]]*mp[MomPure[b1], MomPure[c1]]*mp[MomPure[c1], MomPure[d1]] - 
 2*mp[MomPure[a1], MomPure[b1]]*mp[MomPure[a1], MomPure[c1]]*mp[MomPure[b1], MomPure[d1]]*mp[MomPure[c1], MomPure[d1]] + 
 2*mp[MomPure[a1], MomPure[a1]]*mp[MomPure[b1], MomPure[c1]]*mp[MomPure[b1], MomPure[d1]]*mp[MomPure[c1], MomPure[d1]] + 
 mp[MomPure[a1], MomPure[b1]]^2*mp[MomPure[c1], MomPure[d1]]^2 - mp[MomPure[a1], MomPure[a1]]*mp[MomPure[b1], MomPure[b1]]*
  mp[MomPure[c1], MomPure[d1]]^2 - mp[MomPure[a1], MomPure[c1]]^2*mp[MomPure[b1], MomPure[b1]]*mp[MomPure[d1], MomPure[d1]] + 
 2*mp[MomPure[a1], MomPure[b1]]*mp[MomPure[a1], MomPure[c1]]*mp[MomPure[b1], MomPure[c1]]*mp[MomPure[d1], MomPure[d1]] - 
 mp[MomPure[a1], MomPure[a1]]*mp[MomPure[b1], MomPure[c1]]^2*mp[MomPure[d1], MomPure[d1]] - 
 mp[MomPure[a1], MomPure[b1]]^2*mp[MomPure[c1], MomPure[c1]]*mp[MomPure[d1], MomPure[d1]] + 
 mp[MomPure[a1], MomPure[a1]]*mp[MomPure[b1], MomPure[b1]]*mp[MomPure[c1], MomPure[c1]]*mp[MomPure[d1], MomPure[d1]]),n/2];
localexp=Expand[localexp,eps];
localexp=localexp//.eps[x__]:>0;
,
OptionValue[KillEpsilon]==False,
Null,
True,
Print["Undefined value for the option KillEpsilon. Only True or False are allowed. Proceed assuming default value False."];
];

(*Return output*)
Return[localexp];
];

SetAttributes[ToTrace,Protected];


(* ::Subsection::Closed:: *)
(*SumContracted*)


Options[SumContracted]={StateSumParallel->False,DisplayKernel->False};

SumContracted[x__,OptionsPattern[]][exp_]:=Block[{out,vars,local},
vars=Sequence@@Table[{i,2},{i,{x}}];
If[TrueQ[OptionValue[StateSumParallel]],
(*Parallel sum over states*)
If[$KernelCount==0,
LaunchKernels[];
];
(*Prepare objects to be evaluated in parallel*)
If[TrueQ[OptionValue[DisplayKernel]],
(*Display which kernel computed what in the final result*)
local=Table[ParallelSubmit[{x},Labeled[Framed[exp],$KernelID]],Evaluate[vars]]//Flatten,
(*Without displaying the Kernel*)
local=Table[ParallelSubmit[{x},exp],Evaluate[vars]]//Flatten;
];

(*Evaluate*)
out=WaitAll[local];
out=Plus@@out,
out=Sum[exp,Evaluate[vars]];
];
Return[out];
];


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*CompleteToMassive*)


(*MomMassive is an auxiliary function for CompleteToMassive*)

(*Now we define the contraction properties of MomMassive with itself*)
MomMassive /: Times[MomMassive[l1_List,type1_,$angle][a1_,A_][a2_,Null],MomMassive[l2_List,type2_,$angle][b1_,Null][b2_,A_]]:=(-1)^(Length[l2]+1)MomMassive[Join[l1,Reverse[l2]],type1,type2][a1,b1][a2,b2];
MomMassive /: Times[MomMassive[l1_List,$angle,type1_][A_,a1_][Null,a2_],MomMassive[l2_List,$angle,type2_][Null,b1_][A_,b2_]]:=(-1)^(Length[l1]+1)MomMassive[Join[Reverse[l1],l2],type1,type2][a1,b1][a2,b2];
MomMassive /:  Times[MomMassive[l1_List,type1_,$angle][a1_,A_][a2_,Null],MomMassive[l2_List,$angle,type2_][Null,b1_][A_,b2_]]:=MomMassive[Join[l1,l2],type1,type2][a1,b1][a2,b2];
MomMassive /: Times[MomMassive[l1_List,$square,type1_][Null,a1_][A_,a2_],MomMassive[l2_List,$square,type2_][A_,b1_][Null,b2_]]:=(-1)^(Length[l1]+1)MomMassive[Join[Reverse[l1],l2],type1,type2][a1,b1][a2,b2];
MomMassive /: Times[MomMassive[l1_List,type1_,$square][a1_,Null][a2_,A_],MomMassive[l2_List,type2_,$square][b1_,A_][b2_,Null]]:=(-1)^(Length[l2]+1)MomMassive[Join[l1,Reverse[l2]],type1,type2][a1,b1][a2,b2];
MomMassive /: Times[MomMassive[l1_List,type1_,$square][a1_,Null][a2_,A_],MomMassive[l2_List,$square,type2_][A_,b1_][Null,b2_]]:=MomMassive[Join[l1,l2],type1,type2][a1,b1][a2,b2];
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

localexp=Together[exp];
numtot={Numerator[localexp]//Expand//CompleteMandelstam}/.{Plus->List}//Flatten;
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
numtot=SpinorReplaceSequential[numtot,reps];

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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*SpinorAngleBracketN*)


SpinorAngleBracketN[x_,y_]:=SpinorAngleBracketN[x,y]=SpinorUndotN[x][$lam][$up] . SpinorUndotN[y][$lam][$down];
SpinorAngleBracketN[x_,OverBar[y_]]:=SpinorAngleBracketN[x,OverBar[y]]=SpinorUndotN[x][$lam][$up] . SpinorUndotN[y][$mu][$down];
SpinorAngleBracketN[OverBar[x_],y_]:=SpinorAngleBracketN[OverBar[x],y]=SpinorUndotN[x][$mu][$up] . SpinorUndotN[y][$lam][$down];
SpinorAngleBracketN[OverBar[x_],OverBar[y_]]:=SpinorAngleBracketN[OverBar[x],OverBar[y]]=SpinorUndotN[x][$mu][$up] . SpinorUndotN[y][$mu][$down];


(* ::Subsection::Closed:: *)
(*SpinorSquareBracketN*)


SpinorSquareBracketN[x_,y_]:=SpinorSquareBracketN[x,y]=SpinorDotN[x][$lam][$down] . SpinorDotN[y][$lam][$up];
SpinorSquareBracketN[x_,OverBar[y_]]:=SpinorSquareBracketN[x,OverBar[y]]=SpinorDotN[x][$lam][$down] . SpinorDotN[y][$mu][$up];
SpinorSquareBracketN[OverBar[x_],y_]:=SpinorSquareBracketN[OverBar[x],y]=SpinorDotN[x][$mu][$down] . SpinorDotN[y][$lam][$up];
SpinorSquareBracketN[OverBar[x_],OverBar[y_]]:=SpinorSquareBracketN[OverBar[x],OverBar[y]]=SpinorDotN[x][$mu][$down] . SpinorDotN[y][$mu][$up];


(* ::Subsection:: *)
(*Auxiliary functions for GenSpinors*)


(* ::Subsubsection::Closed:: *)
(*GenerateKinematics*)


Options[GenerateKinematics]={RationalKinematics->True,ParameterRange->1000,Parametric->False,ParameterName->$par}
{RationalKinematics->True,ParameterRange->1000,Parametric->False,ParameterName->$par}
GenerateKinematics[total_Integer,fourD_Integer,OptionsPattern[]]:=Catch[Module[{\[Xi],\[Eta],\[Xi]t,\[Eta]t,n,random,system,sol,par,count,out},

(*First we check that total \[GreaterEqual] fourD+2 *)
If[total<fourD+2,Throw["Please check input, impossible kinematics has been requested."];
];

n=total;

(*Next fix the components of the four-dimensional spinors*)
Do[
\[Xi][i+n]=0;
\[Eta][i+n]=0;
\[Eta]t[i+n]=\[Eta]t[i]*\[Xi]t[i+n]/\[Xi]t[i];
,{i,n-fourD+1,n}];

(*Based on the options given assign rational rather than real kinematics and the range of the interval over which to generate it*)
If[TrueQ[OptionValue[RationalKinematics]],
random:=RandomInteger[OptionValue[ParameterRange]],
random:=RandomReal[OptionValue[ParameterRange]];
];

(*Next generate the random spinor components. First the 3n*)

Do[
\[Xi][i+n]=random;
\[Eta][i+n]=random;
\[Eta]t[i+n]=random;
,{i,n-fourD}];

(*Then the 9:*)
\[Eta]t[1]=random;
\[Eta]t[2]=random;
\[Xi]t[1]=random;
\[Xi]t[2]=random;
\[Xi][3]=random;
\[Eta][3]=random;
\[Xi][4]=random;
\[Eta][1]=random;
\[Eta][4]=random;

(*Depending on whether a parametric expression is required or not, we set the other variables to either a parameter or a number*)

If[TrueQ[OptionValue[Parametric]],
(*Parametric components*)
par=OptionValue[ParameterName];
\[Eta]t[3]=par[1];
\[Eta]t[4]=par[2];
\[Xi]t[4]=par[3];
\[Xi]t[3+n]=par[4];
\[Xi]t[4+n]=par[5];
count=6;
Do[
\[Xi][i]=par[count++];
\[Eta][i]=par[count++];
\[Xi]t[i]=par[count++];
\[Eta]t[i]=par[count++];
\[Xi]t[i+n]=par[count++];
,{i,5,n}];
,
(*Numeric components*)
\[Eta]t[3]=random;
\[Eta]t[4]=random;
\[Xi]t[4]=random;
\[Xi]t[3+n]=random;
\[Xi]t[4+n]=random;
Do[
\[Xi][i]=random;
\[Eta][i]=random;
\[Xi]t[i]=random;
\[Eta]t[i]=random;
\[Xi]t[i+n]=random;
,{i,5,n}];
];

(*Generate momentum conservation:*)
system={Sum[\[Xi][i]\[Xi]t[i],{i,2*n}]==0,Sum[\[Xi][i]\[Eta]t[i],{i,2*n}]==0,Sum[\[Eta][i]\[Xi]t[i],{i,2*n}]==0,Sum[\[Eta][i]\[Eta]t[i],{i,2*n}]==0,Sum[\[Xi][i]\[Eta][i+n]-\[Xi][i+n]\[Eta][i],{i,n}]==0,Sum[\[Eta]t[i+n]\[Xi]t[i]-\[Xi]t[i+n]\[Eta]t[i],{i,n}]==0};

(*Solve momentum conservation*)
sol=Solve[system,{\[Xi][1],\[Xi][2],\[Eta][2],\[Xi]t[3],\[Xi]t[1+n],\[Xi]t[2+n]}];

(*Safety check*)
If[sol==={},
Throw["Anomalous kinematic point has been randomly generated, momentum conservation could not be solved."],
sol=sol//First;
];

(*Now that all the spinor components have been generated we just need to return them in a suitably packaged output. The oupt will be divided into 6D and 4D and then further into {\[Lambda],\[Lambda]t,\[Lambda]',\[Lambda]t'}. Notice that these spinors will be considered all having upper indices so the spinors will be like \[Lambda]=\[LeftAngleBracket]\[Lambda]| and \[Lambda]t=|\[Lambda]t].*)

(*List of the 6D spinors*)
out=Table[{{\[Xi][i],\[Eta][i]},{\[Xi]t[i],\[Eta]t[i]},{\[Xi][i+n],\[Eta][i+n]},{\[Xi]t[i+n],\[Eta]t[i+n]}},{i,n-fourD}];

(*Then append the table of the 4D components and replece the solutions to momentum conservation*)
out={out,Table[{{\[Xi][i],\[Eta][i]},{\[Xi]t[i],\[Eta]t[i]}},{i,n-fourD+1,n}]}/.sol;

Return[out];
];
];


(* ::Subsubsection::Closed:: *)
(*GenerateKinematics4D*)


Options[GenerateKinematics4D]={RationalKinematics->True,ParameterRange->1000,Parametric->False,ParameterName->$par};


GenerateKinematics4D[total_Integer,OptionsPattern[]]:=Catch[Module[{\[Xi],\[Eta],\[Xi]t,\[Eta]t,n,random,system,sol,par,count,out},

n=total;

(*Based on the options given assign rational rather than real kinematics and the range of the interval over which to generate it*)
If[TrueQ[OptionValue[RationalKinematics]],
random:=RandomInteger[OptionValue[ParameterRange]],
random:=RandomReal[OptionValue[ParameterRange]];
];

(*Next generate the random spinor components. First the n*)

Do[
\[Eta]t[i]=random;
,{i,n}];

(*Then the 6:*)
\[Xi][3]=random;
\[Xi][4]=random;
\[Eta][3]=random;
\[Eta][4]=random;
\[Xi]t[1]=random;
\[Xi]t[2]=random;

(*Depending on whether a parametric expression is required or not, we set the other variables to either a parameter or a number*)

If[TrueQ[OptionValue[Parametric]],
(*Parametric components*)
par=OptionValue[ParameterName];
\[Xi]t[3]=par[1];
\[Xi]t[4]=par[2];
count=3;
Do[
\[Xi]t[i]=par[count++];
\[Xi][i]=par[count++];
\[Eta][i]=par[count++];
,{i,5,n}];
,
(*Numeric components*)
\[Xi]t[3]=random;
\[Xi]t[4]=random;
Do[
\[Xi]t[i]=random;
\[Xi][i]=random;
\[Eta][i]=random;
,{i,5,n}];
];

(*Generate momentum conservation:*)
system={Sum[\[Xi][i]\[Xi]t[i],{i,n}]==0,Sum[\[Xi][i]\[Eta]t[i],{i,n}]==0,Sum[\[Eta][i]\[Xi]t[i],{i,n}]==0,Sum[\[Eta][i]\[Eta]t[i],{i,n}]==0};

(*Solve momentum conservation*)
sol=Solve[system,{\[Xi][1],\[Xi][2],\[Eta][1],\[Eta][2]}];

(*Safety check*)
If[sol==={},
Throw["Anomalous kinematic point has been randomly generated, momentum conservation could not be solved."],
sol=sol//First;
];

(*Now that all the spinor components have been generated we just need to return them in a suitably packaged output. The oupt will be divided into {\[Lambda],\[Lambda]t}. Notice that these spinors will be considered all having upper indices so the spinors will be like \[Lambda]=\[LeftAngleBracket]\[Lambda]| and \[Lambda]t=|\[Lambda]t].*)

(*Table of the 4D components and replece the solutions to momentum conservation*)
out=Table[{{\[Xi][i],\[Eta][i]},{\[Xi]t[i],\[Eta]t[i]}},{i,n}]/.sol;

Return[out];
];
];


(* ::Subsubsection::Closed:: *)
(*GenerateKinematicsFixed4D*)


Options[GenerateKinematicsFixed4D]={ParameterRange->1000,RationalKinematics->True};

GenerateKinematicsFixed4D::overconstrained="Maximum number of momenta which can be fixed a priori is number of particles minus two. Overconstrained kinematics has been requested.";

GenerateKinematicsFixed4D::undeclaredmom="One of the fixing conditions is not well defined. Did you declare the necessary momenta? Proceed randomizing the undefined momentum.";

GenerateKinematicsFixed4D::unsolvablekinematics="Anomalous kinematic point has been randomly generated, momentum conservation could not be solved.";


GenerateKinematicsFixed4D[nlegs_Integer,fixedmom_List,OptionsPattern[]]:=Catch[Module[{\[Xi],\[Eta],\[Xi]t,\[Eta]t,random,system,sol,out,input},

(*First a safety check: the maximum number of momenta which can be fixed a priori is n-2*)
If[nlegs-Length[fixedmom]<2,
Message[GenerateKinematicsFixed4D::overconstrained];
Throw[$Failed];
];

(*Rational vs Real kinematics*)
If[TrueQ[OptionValue[RationalKinematics]],
random:=RandomInteger[OptionValue[ParameterRange]],
random:=RandomReal[OptionValue[ParameterRange]];
];

(*3 possible options: either the list of spinor components, or the label of the corresponding momentum, or constant times the label. It can also be mixed*)
input={};
Do[
Which[
MatchQ[i,{{_,_},{_,_}}],
(*Spinor components*)
input={input,i};
,
Head[i]===Symbol(*&&MemberQ[MomList,i]*),
(*Symbol, supposedely a momentum label*)
input={input,{SpinorUndotN[i][$lam][$up],SpinorDotN[i][$lam][$up]}};
,
MatchQ[i,Times[_?NumberQ,_Symbol]],
(*Number times symbol*)
input={input,{i[[1]]*SpinorUndotN[i[[2]]][$lam][$up],SpinorDotN[i[[2]]][$lam][$up]}};
,
MatchQ[i,Times[x_,y_]/;MemberQ[MomList,y]&&!MemberQ[MomList,x]],
(*symbol times symbol with one of them a declared momentum label*)
input={input,{Select[i,!MemberQ[MomList,#]&]*SpinorUndotN[Select[i,MemberQ[MomList,#]&]][$lam][$up],SpinorDotN[Select[i,MemberQ[MomList,#]&]][$lam][$up]}};
,
True,
Message[GenerateKinematicsFixed4D::undeclaredmom];
input={input,{{random,random},{random,random}}};
];
,{i,fixedmom}];

(*Fix spinor components to be the required ones, and generate the missing ones randomly*)
out=Table[{\[Xi][i],\[Eta][i],\[Xi]t[i],\[Eta]t[i]},{i,Length[fixedmom]}];
input=Flatten[input];
Evaluate[out//Flatten]=input;
Do[{\[Xi][i],\[Eta][i],\[Xi]t[i],\[Eta]t[i]}={random,random,random,random},{i,Length[fixedmom]+1,nlegs-2}];

(*Finally solve momentum conservation for the last two momenta, in particular solve for the angle components and set the squares to random numbers*)
{\[Xi]t[nlegs-1],\[Eta]t[nlegs-1],\[Xi]t[nlegs],\[Eta]t[nlegs]}={random,random,random,random};

(*Generate momentum conservation:*)
system={Sum[\[Xi][i]\[Xi]t[i],{i,nlegs}]==0,Sum[\[Xi][i]\[Eta]t[i],{i,nlegs}]==0,Sum[\[Eta][i]\[Xi]t[i],{i,nlegs}]==0,Sum[\[Eta][i]\[Eta]t[i],{i,nlegs}]==0};

(*Solve momentum conservation*)
sol=Solve[system,{\[Xi][nlegs-1],\[Xi][nlegs],\[Eta][nlegs-1],\[Eta][nlegs]}];

(*Safety check*)
If[sol==={},
Message[GenerateKinematicsFixed4D::unsolvablekinematics];
Throw[$Failed],
sol=sol//First;
];

(*Now that all the spinor components have been generated we just need to return them in a suitably packaged output. The oupt will be divided into {\[Lambda],\[Lambda]t}. Notice that these spinors will be considered all having upper indices so the spinors will be like \[Lambda]=\[LeftAngleBracket]\[Lambda]| and \[Lambda]t=|\[Lambda]t].*)

(*Table of the 4D components and replece the solutions to momentum conservation*)
out=Table[{{\[Xi][i],\[Eta][i]},{\[Xi]t[i],\[Eta]t[i]}},{i,nlegs}]/.sol;

Return[out];

];
];


(* ::Subsubsection::Closed:: *)
(*GenKinematics3pt*)


Options[GenKinematics3pt]={RationalKinematics->True,ParameterRange->1000,SetMomentum->{}};

GenKinematics3pt::invalidset="Invalid option for SetMomentum. Proceed ignoring the option.";


GenKinematics3pt[type_,OptionsPattern[]]:=Module[{random,lam,lamtil,a2,a3},

(*Based on the options given assign rational rather than real kinematics and the range of the interval over which to generate it*)
	If[TrueQ[OptionValue[RationalKinematics]],
	random:=RandomInteger[OptionValue[ParameterRange]],
	random:=RandomReal[OptionValue[ParameterRange]];
	];

(*Generate the \[Lambda] and \[Lambda] tilde for the first spinor. If SetMomentum is not empty set the spinors to the given spinor components or to the components of the label if any is given.*)

Which[OptionValue[SetMomentum]==={},
lam[1]={random,random};
lamtil[1]={random,random};
,
(*Here we could add the condition for the symbol to be a declared momentum, but not compulsory...*)
Head[OptionValue[SetMomentum]]===Symbol,
{lam[1],lamtil[1]}={SpinorUndotN[OptionValue[SetMomentum]][$lam][$up],SpinorDotN[OptionValue[SetMomentum]][$lam][$up]};
,
MatchQ[OptionValue[SetMomentum],{{_,_},{_,_}}],
{lam[1],lamtil[1]}=OptionValue[SetMomentum];
,
MatchQ[OptionValue[SetMomentum],Times[_?NumberQ,_Symbol]],
(*Number times symbol*)
{lam[1],lamtil[1]}={OptionValue[SetMomentum][[1]]*SpinorUndotN[OptionValue[SetMomentum][[2]]][$lam][$up],SpinorDotN[OptionValue[SetMomentum][[2]]][$lam][$up]};
,
MatchQ[OptionValue[SetMomentum],Times[x_,y_]/;MemberQ[MomList,y]&&!MemberQ[MomList,x]],
(*symbol times symbol with one of them a declared momentum label*)
{lam[1],lamtil[1]}={Select[OptionValue[SetMomentum],!MemberQ[MomList,#]&]*SpinorUndotN[Select[OptionValue[SetMomentum],MemberQ[MomList,#]&]][$lam][$up],SpinorDotN[Select[OptionValue[SetMomentum],MemberQ[MomList,#]&]][$lam][$up]};
,
True,
Message[GenKinematics3pt::invalidset];
lam[1]={random,random};
lamtil[1]={random,random};
];

(*Dependin on the type we set either all the \[Lambda] or the \[Lambda] tildes proportional to the one of p1, and generate the remaining spinors satisfying momentum conservation. No internal security condition on the type input, TO BE PUT IN THE EXTERNAL ENVIRONMENT!!!*)
a2=random;
a3=random;

Which[type===$angle,
lamtil[2]=a2*lamtil[1];
lamtil[3]=a3*lamtil[1];
lam[2]={random,random};
lam[3]=-lam[1]/a3-a2/a3*lam[2];
,
type===$square,
lam[2]=a2*lam[1];
lam[3]=a3*lam[1];
lamtil[2]={random,random};
lamtil[3]=-lamtil[1]/a3-a2/a3*lamtil[2];
];


Return[Table[{lam[i],lamtil[i]},{i,3}]];

];


(* ::Subsubsection::Closed:: *)
(*GenSpinorsAux*)


Options[GenSpinorsAux]={RationalKinematics->True,ParameterRange->1000,Parametric->False,ParameterName->$par,Seed->False,Dimension->6,DisplaySpinors->False,SetMomentum->{}};

GenSpinors::parametric="Sorry, the option Parametric is not yet supported in the requested kinematics.";

GenSpinors::notsupported="Sorry, the requested kinematics is not supported in the current version.";


GenSpinorsAux[labels_List,OptionsPattern[]]:=Catch[Module[{lab4,lab6,type,ra,rs,la,ls,\[Xi],\[Xi]t,\[Eta],\[Eta]t,kinem,n,kinem2},

(*First of all, clear all the stored values of non-fundamental building blocks, like angle and square brackets and so on. This is achieved with ClearDownValues and ClearSubValues*)
ClearDependentKinematics[];

(*If labels is a list of two lists, then the first one is to be treated as the list of 6D momenta and the second one as the list of 4D momenta. If the option Dimension is set to 4 then all the momenta are considered 4 dimensional. The variable type is flag for the 3 different cases*)
Which[
OptionValue[Dimension]===4,
lab4=labels;
lab6={};
(*Pure 4D*)
type=0;
,
MatchQ[labels,{_List,_List}],
lab6=labels[[1]];
lab4=labels[[2]];
(*Mixed kinematics*)
type=1;
(*If the list of 6D momenta is epty, this is equivalent to case 1*)
If[lab6==={},type=0;];
,
True,
lab6=labels;
lab4={};
(*Pure 6D*)
type=2;
];

(*If Seed has been defined then SeedRandom*)
If[MatchQ[Head[OptionValue[Seed]],Integer|String],
SeedRandom[OptionValue[Seed]];
];

(*Definition of the spinors in terms of the components. In the six-dimensional case there will be 2n of these spinors, where the first n refer to \[Lambda] and the second n to \[Lambda]' which is redefinition of the \[Mu] encoding also the masses, see package documentation.*)
	ra[i_]:={-\[Xi][i],\[Eta][i]}(*|i>*);
		   rs[i_]:={\[Xi]t[i],\[Eta]t[i]}(*|i]*);
		   la[i_]:={\[Eta][i],\[Xi][i]}(*<i|*);
		   ls[i_]:={-\[Eta]t[i],\[Xi]t[i]}(*[i|*);

(*Now actually generate the kinematics, depending on whether there are fixed momenta or not and the dimension we call different functions.*)

If[OptionValue[SetMomentum]==={},
(*Generate all momenta from scratch*)
Which[
type===0,
(*Pure 4D*)
n=Length[lab4];
kinem=Table[{la[i],rs[i]},{i,n}];
kinem2=GenerateKinematics4D[n,{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange],Parametric->OptionValue[Parametric],ParameterName->OptionValue[ParameterName]}];
If[Head[kinem2]===String,Throw[kinem2]];
Evaluate[kinem]=kinem2;
,
type===1,
(*Mixed*)
n=Length[lab4]+Length[lab6];
kinem={Table[{la[i],rs[i],la[i+n],rs[i+n]},{i,Length[lab6]}],Table[{la[i],rs[i]},{i,Length[lab6]+1,n}]};
kinem2=GenerateKinematics[n,Length[lab4],{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange],Parametric->OptionValue[Parametric],ParameterName->OptionValue[ParameterName]}];
If[Head[kinem2]===String,Throw[kinem2]];
Evaluate[kinem]=kinem2;
,
type===2,
(*Pure 6D*)
n=Length[lab6];
kinem={Table[{la[i],rs[i],la[i+n],rs[i+n]},{i,n}],{}};
kinem2=GenerateKinematics[n,0,{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange],Parametric->OptionValue[Parametric],ParameterName->OptionValue[ParameterName]}];
If[Head[kinem2]===String,Throw[kinem2]];
Evaluate[kinem]=kinem2;
];
,
(*Some of the spinors are fixed a priori*)
Which[
type===0,
(*Pure 4D*)
n=Length[lab4];
kinem=Table[{la[i],rs[i]},{i,n}];
kinem2=GenerateKinematicsFixed4D[n,If[(!Head[OptionValue[SetMomentum]]===List)||MatchQ[OptionValue[SetMomentum],{{_,_},{_,_}}],{OptionValue[SetMomentum]},OptionValue[SetMomentum]],{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange]}];
If[TrueQ[OptionValue[Parametric]],
Message[GenSpinors::parametric];
];
If[kinem2===$Failed,Throw[kinem2]];
Evaluate[kinem]=kinem2;
,
type===1,
Message[GenSpinors::notsupported],
type===2,
Message[GenSpinors::notsupported]
];
];

(*Finally relate the generated kinematics to the spinor labels:*)
(*6D part*)
Do[
		(*\[Lambda] spinors*)
		SpinorUndotN[lab6[[i]]][$lam][$up]=la[i];
		SpinorUndotN[lab6[[i]]][$lam][$down]=ra[i];
		SpinorDotN[lab6[[i]]][$lam][$down]=ls[i];
		SpinorDotN[lab6[[i]]][$lam][$up]=rs[i];
		(*\[Mu] spinors*)
		SpinorUndotN[lab6[[i]]][$mu][$up]=la[i+n];
		SpinorUndotN[lab6[[i]]][$mu][$down]=ra[i+n];
		SpinorDotN[lab6[[i]]][$mu][$down]=ls[i+n];
		SpinorDotN[lab6[[i]]][$mu][$up]=rs[i+n];
		(*Masses:*)
		ExtramassN[lab6[[i]]]=la[i] . ra[i+n];
		ExtramasstildeN[lab6[[i]]]=ls[i+n] . rs[i];
		,{i,Length[lab6]}];
(*4D part*)
Do[
		(*\[Lambda] spinors*)
		SpinorUndotN[lab4[[i]]][$lam][$up]=la[i+Length[lab6]];
		SpinorUndotN[lab4[[i]]][$lam][$down]=ra[i+Length[lab6]];
		SpinorDotN[lab4[[i]]][$lam][$down]=ls[i+Length[lab6]];
		SpinorDotN[lab4[[i]]][$lam][$up]=rs[i+Length[lab6]];
(*Initialise the \[Mu] spinors to {Null,Null} for consistency reasons*)SpinorUndotN[lab4[[i]]][$mu][$up]={Null,Null};SpinorUndotN[lab4[[i]]][$mu][$down]={Null,Null};SpinorDotN[lab4[[i]]][$mu][$down]={Null,Null};SpinorDotN[lab4[[i]]][$mu][$up]={Null,Null};(*Masses to zero:*)
ExtramassN[lab4[[i]]]=0;
ExtramasstildeN[lab4[[i]]]=0;
		,{i,Length[lab4]}];

(*If DisplaySpinors is set to True display the generated kinematics*)
If[OptionValue[DisplaySpinors],
Print["Output reads {|\[Lambda]\[RightAngleBracket],|\[Lambda]],|\[Mu]\[RightAngleBracket],|\[Mu]]} and {|\[Lambda]\[RightAngleBracket],|\[Lambda]]} for 6D and 4D spinors respectively."];
Return[DeleteCases[{Table[{ra[i],rs[i],ra[i+n],rs[i+n]},{i,Length[lab6]}],Table[{ra[i],rs[i]},{i,Length[lab6]+1,n}]},{}]];
,
Return["Numerical kinematics has been generated."];
];


];
];


(* ::Subsubsection:: *)
(*ClearDependentKinematics*)


ClearDependentKinematics[]:=Block[{},

(*Clearing functions with DownValues*)
ClearDownValues[#]&/@{SpinorAngleBracketN,SpinorSquareBracketN,Mom4DN,Mom6DN,ChainN,mpN6,SNum};

(*Clearing functions with SubValues*)
ClearSubValues[#]&/@{SpinorUndot6DN,SpinorDot6DN,AngSquInvariantN,SquAngInvariantN,AngAngInvariantN,SquSquInvariantN,MomMat4DN,MomMat6DN};

];


(* ::Subsection::Closed:: *)
(*GenSpinors*)


(*
Options[GenSpinors]={RationalKinematics->True,ParameterRange->1000,Parametric->False,ParameterName->$par,Seed->False,Dimension->6,DisplaySpinors->False}
{RationalKinematics->True,ParameterRange->1000,Parametric->False,ParameterName->$par,Seed->False,Dimension->6,DisplaySpinors->False}
GenSpinors[labels_List,OptionsPattern[]]:=Catch[Module[{lab4,lab6,type,ra,rs,la,ls,\[Xi],\[Xi]t,\[Eta],\[Eta]t,kinem,n,kinem2},

(*First of all, clear all the stored values of non-fundamental building blocks, like angle and square brackets and so on. This is achieved with ClearDownValues and ClearSubValues*)
ClearDependentKinematics[];

(*If labels is a list of two lists, then the first one is to be treated as the list of 6D momenta and the second one as the list of 4D momenta. If the option Dimension is set to 4 then all the momenta are considered 4 dimensional. The variable type is flag for the 3 different cases*)
Which[
OptionValue[Dimension]===4,
lab4=labels;
lab6={};
(*Pure 4D*)
type=0;
,
MatchQ[labels,{_List,_List}],
lab6=labels[[1]];
lab4=labels[[2]];
(*Mixed kinematics*)
type=1;
(*If the list of 6D momenta is epty, this is equivalent to case 1*)
If[lab6==={},type=0;];
,
True,
lab6=labels;
lab4={};
(*Pure 6D*)
type=2;
];

(*Check if all the 4D momenta have been declared as 4D, else print a warning message but proceed*)
If[AnyTrue[lab4,(!MemberQ[Momenta4D,#]&)],
Print["Warning: some of the four-dimensional momenta have not been declared as four-dimensional through KillMasses. Use of the generated kinematics in combination with analytic calculations might result in errors."];
];

(*If Seed has been defined then SeedRandom*)
If[MatchQ[Head[OptionValue[Seed]],Integer|String],
SeedRandom[OptionValue[Seed]];
];

(*Definition of the spinors in terms of the components. In the six-dimensional case there will be 2n of these spinors, where the first n refer to \[Lambda] and the second n to \[Lambda]' which is redefinition of the \[Mu] encoding also the masses, see package documentation.*)
	ra[i_]:={-\[Xi][i],\[Eta][i]}(*|i>*);
		   rs[i_]:={\[Xi]t[i],\[Eta]t[i]}(*|i]*);
		   la[i_]:={\[Eta][i],\[Xi][i]}(*<i|*);
		   ls[i_]:={-\[Eta]t[i],\[Xi]t[i]}(*[i|*);

(*Now actually generate the kinematics*)
Which[
type===0,
(*Pure 4D*)
n=Length[lab4];
kinem=Table[{la[i],rs[i]},{i,n}];
kinem2=GenerateKinematics4D[n,{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange],Parametric->OptionValue[Parametric],ParameterName->OptionValue[ParameterName]}];
If[Head[kinem2]===String,Throw[kinem2]];
Evaluate[kinem]=kinem2;
,
type===1,
(*Mixed*)
n=Length[lab4]+Length[lab6];
kinem={Table[{la[i],rs[i],la[i+n],rs[i+n]},{i,Length[lab6]}],Table[{la[i],rs[i]},{i,Length[lab6]+1,n}]};
kinem2=GenerateKinematics[n,Length[lab4],{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange],Parametric->OptionValue[Parametric],ParameterName->OptionValue[ParameterName]}];
If[Head[kinem2]===String,Throw[kinem2]];
Evaluate[kinem]=kinem2;
,
type===2,
(*Pure 6D*)
n=Length[lab6];
kinem={Table[{la[i],rs[i],la[i+n],rs[i+n]},{i,n}],{}};
kinem2=GenerateKinematics[n,0,{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange],Parametric->OptionValue[Parametric],ParameterName->OptionValue[ParameterName]}];
If[Head[kinem2]===String,Throw[kinem2]];
Evaluate[kinem]=kinem2;
];

(*Finally relate the generated kinematics to the spinor labels:*)
(*6D part*)
Do[
		(*\[Lambda] spinors*)
		SpinorUndotN[lab6[[i]]][$lam][$up]=la[i];
		SpinorUndotN[lab6[[i]]][$lam][$down]=ra[i];
		SpinorDotN[lab6[[i]]][$lam][$down]=ls[i];
		SpinorDotN[lab6[[i]]][$lam][$up]=rs[i];
		(*\[Mu] spinors*)
		SpinorUndotN[lab6[[i]]][$mu][$up]=la[i+n];
		SpinorUndotN[lab6[[i]]][$mu][$down]=ra[i+n];
		SpinorDotN[lab6[[i]]][$mu][$down]=ls[i+n];
		SpinorDotN[lab6[[i]]][$mu][$up]=rs[i+n];
		(*Masses:*)
		ExtramassN[lab6[[i]]]=la[i].ra[i+n];
		ExtramasstildeN[lab6[[i]]]=ls[i+n].rs[i];
		,{i,Length[lab6]}];
(*4D part*)
Do[
		(*\[Lambda] spinors*)
		SpinorUndotN[lab4[[i]]][$lam][$up]=la[i+Length[lab6]];
		SpinorUndotN[lab4[[i]]][$lam][$down]=ra[i+Length[lab6]];
		SpinorDotN[lab4[[i]]][$lam][$down]=ls[i+Length[lab6]];
		SpinorDotN[lab4[[i]]][$lam][$up]=rs[i+Length[lab6]];
(*Initialise the \[Mu] spinors to {Null,Null} for consistency reasons*)SpinorUndotN[lab4[[i]]][$mu][$up]={Null,Null};SpinorUndotN[lab4[[i]]][$mu][$down]={Null,Null};SpinorDotN[lab4[[i]]][$mu][$down]={Null,Null};SpinorDotN[lab4[[i]]][$mu][$up]={Null,Null};(*Masses to zero:*)
ExtramassN[lab4[[i]]]=0;
ExtramasstildeN[lab4[[i]]]=0;
		,{i,Length[lab4]}];

(*If DisplaySpinors is set to True display the generated kinematics*)
If[OptionValue[DisplaySpinors],
Print["Output reads {|\[Lambda]\[RightAngleBracket],|\[Lambda]],|\[Mu]\[RightAngleBracket],|\[Mu]]} and {|\[Lambda]\[RightAngleBracket],|\[Lambda]]} for 6D and 4D spinors respectively."];
Return[DeleteCases[{Table[{ra[i],rs[i],ra[i+n],rs[i+n]},{i,Length[lab6]}],Table[{ra[i],rs[i]},{i,Length[lab6]+1,n}]},{}]];
,
Return["Numerical kinematics has been generated."];
];


];
];
*)


Options[GenSpinors]={RationalKinematics->True,ParameterRange->1000,Parametric->False,ParameterName->$par,Seed->False,Dimension->6,DisplaySpinors->False,SetMomentum->{},Type3pt->$angle};


GenSpinors[labels_List,OptionsPattern[]]:=Module[{test,out,labels3pt},

(*test is the test outcome on divergent kinematics. As long as test stays true we generate singular kinematics so we repeat the process.*)
test=True;
While[test,

(*Check if the required kinematics is singular (3pt) or not, also requires 4d kinematics*)
Which[Length[labels3pt=labels]===3&&OptionValue[Dimension]===4,
ClearDependentKinematics[];
(*Generate kinematics*)
test=Infycheck[out=GenKinematics3pt[OptionValue[Type3pt],{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange],SetMomentum->OptionValue[SetMomentum]}]];
(*Assign labels if non-singular kinematics has been generated*)
If[!test,
{{SpinorUndotN[labels3pt[[1]]][$lam][$up],SpinorDotN[labels3pt[[1]]][$lam][$up]},{SpinorUndotN[labels3pt[[2]]][$lam][$up],SpinorDotN[labels3pt[[2]]][$lam][$up]},{SpinorUndotN[labels3pt[[3]]][$lam][$up],SpinorDotN[labels3pt[[3]]][$lam][$up]}}=out;
Do[
SpinorUndotN[i][$lam][$down]={-SpinorUndotN[i][$lam][$up][[2]],SpinorUndotN[i][$lam][$up][[1]]};
SpinorDotN[i][$lam][$down]={-SpinorDotN[i][$lam][$up][[2]],SpinorDotN[i][$lam][$up][[1]]};
SpinorUndotN[i][$mu][$up]={Null,Null};
SpinorUndotN[i][$mu][$down]={Null,Null};
SpinorDotN[i][$mu][$up]={Null,Null};
SpinorDotN[i][$mu][$down]={Null,Null};
ExtramassN[i]=0;
ExtramasstildeN[i]=0;
,{i,labels3pt}];
];
(*Take into account the option DisplaySpinors*)
If[TrueQ[OptionValue[DisplaySpinors]],
Print["Output is a list of {\[LeftAngleBracket]\[Lambda]|,|\[Lambda]]} for each spinor:"];
,
out="Numerical kinematics has been generated.";
];
(*If the option parametric is set to true print error message*)
If[TrueQ[OptionValue[Parametric]],
Message[GenSpinors::parametric];
];
,
MatchQ[labels,{{},{_,_,_}}],
ClearDependentKinematics[];
labels3pt=labels[[2]];
(*Generate kinematics*)
test=Infycheck[out=GenKinematics3pt[OptionValue[Type3pt],{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange],SetMomentum->OptionValue[SetMomentum]}]];
(*Assign labels if non-singular kinematics has been generated*)
If[!test,
{{SpinorUndotN[labels3pt[[1]]][$lam][$up],SpinorDotN[labels3pt[[1]]][$lam][$up]},{SpinorUndotN[labels3pt[[2]]][$lam][$up],SpinorDotN[labels3pt[[2]]][$lam][$up]},{SpinorUndotN[labels3pt[[3]]][$lam][$up],SpinorDotN[labels3pt[[3]]][$lam][$up]}}=out;
Do[
SpinorUndotN[i][$lam][$down]={-SpinorUndotN[i][$lam][$up][[2]],SpinorUndotN[i][$lam][$up][[1]]};
SpinorDotN[i][$lam][$down]={-SpinorDotN[i][$lam][$up][[2]],SpinorDotN[i][$lam][$up][[1]]};
SpinorUndotN[i][$mu][$up]={Null,Null};
SpinorUndotN[i][$mu][$down]={Null,Null};
SpinorDotN[i][$mu][$up]={Null,Null};
SpinorDotN[i][$mu][$down]={Null,Null};
ExtramassN[i]=0;
ExtramasstildeN[i]=0;
,{i,labels3pt}];
];
(*Take into account the option DisplaySpinors*)
If[TrueQ[OptionValue[DisplaySpinors]],
Print["Output is a list of {\[LeftAngleBracket]\[Lambda]|,|\[Lambda]]} for each spinor:"];
,
out="Numerical kinematics has been generated.";
];
(*If the option parametric is set to true print error message*)
If[TrueQ[OptionValue[Parametric]],
Message[GenSpinors::parametric];
];
,
Length[Flatten[labels]]===3,
(*This would be 3pt kinematics either in pure 6D or mixed 6D and 4D, not yet supported*)
Message[GenSpinors::notsupported];
out=$Failed;
Break[];
,
True,
(*Proceed with the higher point kinematics generation*)
test=Infycheck[out=GenSpinorsAux[labels,{RationalKinematics->OptionValue[RationalKinematics],ParameterRange->OptionValue[ParameterRange],Parametric->OptionValue[Parametric],ParameterName->OptionValue[ParameterName],Seed->OptionValue[Seed],Dimension->OptionValue[Dimension],DisplaySpinors->OptionValue[DisplaySpinors],SetMomentum->OptionValue[SetMomentum]}]];
];
];
Return[out];
];


(* ::Subsection:: *)
(*ClearKinematics*)


ClearKinematics:=(ClearSubValues[SpinorUndotN];
ClearSubValues[SpinorDotN];
ClearSubValues[ExtramassN];
ClearSubValues[ExtramasstildeN];
ClearSubValues[SpinorUndot6DN];
ClearSubValues[SpinorDot6DN];
ClearSubValues[AngSquInvariantN];
ClearSubValues[AngAngInvariantN];
ClearSubValues[SquAngInvariantN];
ClearSubValues[SquSquInvariantN];
ClearDownValues[SpinorAngleBracketN];
ClearDownValues[SpinorSquareBracketN];

ClearDependentKinematics[];

(*We now have to redefine the six-dimensional invariants*)
RedefineNumerics6D[];
);


(* ::Subsection::Closed:: *)
(*RedefineNumerics6D*)


(*This is an auxiliary function which gives the definitions for the *D numeric objects. This is needed because it is not possible to clear the stored values without clearing the definition itself.
Or better it is possible but it is risky because if anything goes wrong at some point the error might not be cleared if we cleare only the numerical definitions. So to be on the safe side we clear completely the
definition of the sidevalues and then define the functions a new.*)

RedefineNumerics6D[]:=
((*First the spinors*)
SpinorUndot6DN[a_][$down][1]:=SpinorUndot6DN[a][$down][1]=({-((ExtramassN[a]*SpinorUndotN[a][$mu][$down])/SpinorAngleBracketN[a,OverBar[a]]),SpinorDotN[a][$lam][$up]}//Flatten);
SpinorUndot6DN[a_][$down][2]:=SpinorUndot6DN[a][$down][2]=({SpinorUndotN[a][$lam][$down],-((ExtramasstildeN[a]*SpinorDotN[a][$mu][$up])/SpinorSquareBracketN[a,OverBar[a]])}//Flatten);
SpinorDot6DN[a_][$down][1]:=SpinorDot6DN[a][$down][1]=({(ExtramasstildeN[a]*SpinorUndotN[a][$mu][$up])/SpinorAngleBracketN[a,OverBar[a]],-SpinorDotN[a][$lam][$down]}//Flatten);
SpinorDot6DN[a_][$down][2]:=SpinorDot6DN[a][$down][2]=({SpinorUndotN[a][$lam][$up],-((ExtramassN[a]*SpinorDotN[a][$mu][$down])/SpinorSquareBracketN[a,OverBar[a]])}//Flatten);
(*And now the invariants*)
AngSquInvariantN[a_, b_][c_,d_]:=AngSquInvariantN[a, b][c,d]=SpinorUndot6DN[a][$down][c] . SpinorDot6DN[b][$down][d];
SquAngInvariantN[a_,b_][c_,d_]:=SquAngInvariantN[a,b][c,d]=SpinorDot6DN[a][$down][c] . SpinorUndot6DN[b][$down][d];
AngAngInvariantN[x1_,x2_,x3_,x4_][a_,b_,c_,d_]:=AngAngInvariantN[x1,x2,x3,x4][a,b,c,d]=-Det[{SpinorUndot6DN[x1][$down][a],SpinorUndot6DN[x2][$down][b],SpinorUndot6DN[x3][$down][c],SpinorUndot6DN[x4][$down][d]}];
SquSquInvariantN[x1_,x2_,x3_,x4_][a_,b_,c_,d_]:=SquSquInvariantN[x1,x2,x3,x4][a,b,c,d]=-Det[{SpinorDot6DN[x1][$down][a],SpinorDot6DN[x2][$down][b],SpinorDot6DN[x3][$down][c],SpinorDot6DN[x4][$down][d]}];
);


(* ::Subsection:: *)
(*ToNum*)


(*ToNum[exp_]:=exp/.S->S6/.{SpinorAngleBracket->SpinorAngleBracketN,SpinorSquareBracket->SpinorSquareBracketN,Extramass->ExtramassN,Extramasstilde->ExtramasstildeN,AngSquInvariant->AngSquInvariantN,SquAngInvariant->SquAngInvariantN,AngAngInvariant->AngAngInvariantN,SquSquInvariant->SquSquInvariantN,Chain->ChainN,mp->mpN6,SpinorUndot[mom_][$lam][a_][Null]:>SpinorUndotN[mom][$lam][$up],SpinorUndot[mom_][$lam][Null][a_]:>SpinorUndotN[mom][$lam][$down],SpinorUndot[mom_][$mu][a_][Null]:>SpinorUndotN[mom][$mu][$up],SpinorUndot[mom_][$mu][Null][a_]:>SpinorUndotN[mom][$mu][$down],
SpinorDot[mom_][$lam][a_][Null]:>SpinorDotN[mom][$lam][$up],SpinorDot[mom_][$lam][Null][a_]:>SpinorDotN[mom][$lam][$down],SpinorDot[mom_][$mu][a_][Null]:>SpinorDotN[mom][$mu][$up],SpinorDot[mom_][$mu][Null][a_]:>SpinorDotN[mom][$mu][$down],SpinorUndot6D[mom_][A_][Null][a_]:>SpinorUndot6DN[mom][$down][a],SpinorDot6D[mom_][A_][Null][a_]:>SpinorDot6DN[mom][$down][a]};*)


ToNum[exp_]:=Block[{S,SpinorAngleBracket,SpinorSquareBracket,Extramass,Extramasstilde,AngSquInvariant,SquAngInvariant,AngAngInvariant,SquSquInvariant,Chain,mp,SpinorUndot,SpinorDot,SpinorUndot6D,SpinorDot6D},
S=SNum;SpinorAngleBracket=SpinorAngleBracketN;SpinorSquareBracket=SpinorSquareBracketN;Extramass=ExtramassN;Extramasstilde=ExtramasstildeN;AngSquInvariant=AngSquInvariantN;SquAngInvariant=SquAngInvariantN;AngAngInvariant=AngAngInvariantN;SquSquInvariant=SquSquInvariantN;Chain=ChainN;mp=mpN6;SpinorUndot[mom_][$lam][a_][Null]:=SpinorUndotN[mom][$lam][$up];SpinorUndot[mom_][$lam][Null][a_]:=SpinorUndotN[mom][$lam][$down];SpinorUndot[mom_][$mu][a_][Null]:=SpinorUndotN[mom][$mu][$up];SpinorUndot[mom_][$mu][Null][a_]:=SpinorUndotN[mom][$mu][$down];
SpinorDot[mom_][$lam][a_][Null]:=SpinorDotN[mom][$lam][$up];SpinorDot[mom_][$lam][Null][a_]:=SpinorDotN[mom][$lam][$down];SpinorDot[mom_][$mu][a_][Null]:=SpinorDotN[mom][$mu][$up];SpinorDot[mom_][$mu][Null][a_]:=SpinorDotN[mom][$mu][$down];SpinorUndot6D[mom_][A_][Null][a_]:=SpinorUndot6DN[mom][$down][a];SpinorDot6D[mom_][A_][Null][a_]:=SpinorDot6DN[mom][$down][a];
exp
];


(* ::Subsection:: *)
(*MomMat4D*)


MomMat4DN[label_][$up]:=MomMat4DN[label][$up]=InverseDot[SpinorDotN[label][$lam][$up],SpinorUndotN[label][$lam][$up]]-(ExtramassN[label]ExtramasstildeN[label])/(SpinorDotN[label][$lam][$down] . SpinorDotN[label][$mu][$up] SpinorUndotN[label][$lam][$up] . SpinorUndotN[label][$mu][$down])*InverseDot[SpinorDotN[label][$mu][$up],SpinorUndotN[label][$mu][$up]];
MomMat4DN[label_][$down]:=MomMat4DN[label][$down]=InverseDot[SpinorUndotN[label][$lam][$down],SpinorDotN[label][$lam][$down]]-(ExtramassN[label]ExtramasstildeN[label])/(SpinorDotN[label][$lam][$down] . SpinorDotN[label][$mu][$up] SpinorUndotN[label][$lam][$up] . SpinorUndotN[label][$mu][$down])*InverseDot[SpinorUndotN[label][$mu][$down],SpinorDotN[label][$mu][$down]];


(* ::Subsection:: *)
(*Mom4DN*)


Mom4DN[label_]:=Mom4DN[label]=1/2*{Tr[MomMat4DN[label][$up] . PauliMatrix[0]],Tr[MomMat4DN[label][$up] . PauliMatrix[1]],Tr[MomMat4DN[label][$up] . PauliMatrix[2]],Tr[MomMat4DN[label][$up] . PauliMatrix[3]]};


(* ::Subsection:: *)
(*MomMat6D*)


MomMat6DN[label_][$up]:=MomMat6DN[label][$up]=-InverseDot[SpinorUndot6DN[label][$down][1],SpinorUndot6DN[label][$down][2]]+InverseDot[SpinorUndot6DN[label][$down][2],SpinorUndot6DN[label][$down][1]];
MomMat6DN[label_][$down]:=MomMat6DN[label][$down]=InverseDot[SpinorDot6DN[label][$down][1],SpinorDot6DN[label][$down][2]]-InverseDot[SpinorDot6DN[label][$down][2],SpinorDot6DN[label][$down][1]];


(* ::Subsection:: *)
(*Mom6DN*)


Mom6DN[label_]:=Mom6DN[label]=1/4{Tr[MomMat6DN[label][$up] . PauliSix[0]],Tr[MomMat6DN[label][$up] . PauliSix[1]],Tr[MomMat6DN[label][$up] . PauliSix[2]],Tr[MomMat6DN[label][$up] . PauliSix[3]],Tr[MomMat6DN[label][$up] . PauliSix[4]],Tr[MomMat6DN[label][$up] . PauliSix[5]]};


(* ::Subsection:: *)
(*PauliSix*)


PauliSix[0]={{0,0,0,1},{0,0,-1,0},{0,1,0,0},{-1,0,0,0}}
{{0,0,0,1},{0,0,-1,0},{0,1,0,0},{-1,0,0,0}};
PauliSix[1]={{0,0,1,0},{0,0,0,-1},{-1,0,0,0},{0,1,0,0}}
{{0,0,1,0},{0,0,0,-1},{-1,0,0,0},{0,1,0,0}};
PauliSix[2]={{0,0,I,0},{0,0,0,I},{-I,0,0,0},{0,-I,0,0}}
{{0,0,I,0},{0,0,0,I},{-I,0,0,0},{0,-I,0,0}};
PauliSix[3]={{0,0,0,-1},{0,0,-1,0},{0,1,0,0},{1,0,0,0}}
{{0,0,0,-1},{0,0,-1,0},{0,1,0,0},{1,0,0,0}};
PauliSix[4]={{0,I,0,0},{-I,0,0,0},{0,0,0,-I},{0,0,I,0}}
{{0,I,0,0},{-I,0,0,0},{0,0,0,-I},{0,0,I,0}};
PauliSix[5]={{0,1,0,0},{-1,0,0,0},{0,0,0,1},{0,0,-1,0}}
{{0,1,0,0},{-1,0,0,0},{0,0,0,1},{0,0,-1,0}};


(* ::Subsection::Closed:: *)
(*MomToSpinors auxiliary functions*)


scalarprod[x_List,y_List]:=x[[1]]*y[[1]]-Sum[x[[i]]*y[[i]],{i,2,Length[x]}];
scalarprod[x_]:=scalarprod[x,x];


MomToSpinors4DMasslessN[mom_List,label_:True,type_:$lam,precise_:True]:=Module[{lamup,lamdown,lamdotup,lamdotdown,p0,p1,p2,p3,fun},
{p0,p1,p2,p3}=mom;
lamdown={-(p1-I*p2)/Sqrt[p0+p3],Sqrt[p0+p3]};(*|\[Lambda]>*)
lamup={Sqrt[p0+p3],(p1-I*p2)/Sqrt[p0+p3]};(*<\[Lambda]|*)
lamdotdown={-(p1+I*p2)/Sqrt[p0+p3],Sqrt[p0+p3]};(*[\[Lambda]|*)
lamdotup={Sqrt[p0+p3],(p1+I*p2)/Sqrt[p0+p3]};(*|\[Lambda]]*)

(*Apply //N if required*)
If[TrueQ[precise],
fun[x_]:=x,
fun=N;
];


(*If a label is given we assign these spinor values to the corresponding spinors.*)
If[TrueQ[label],
(*If the optional argument label is not given then we just return the generated spinors*)
Return[{lamdown,lamdotup,lamup,lamdotdown}];(*{|\[Lambda]>,|\[Lambda]],<\[Lambda]|,[\[Lambda]|}*),
SpinorUndotN[label][type][$down]=lamdown//fun;
SpinorUndotN[label][type][$up]=lamup//fun;
SpinorDotN[label][type][$down]=lamdotdown//fun;
SpinorDotN[label][type][$up]=lamdotup//fun;

(*If we are setting the $lam spinors then initialise the \[Mu] spinors to {Null,Null} for consistency and set masses to zero*)
If[type===$lam,
SpinorUndotN[label][$mu][$down]={Null,Null};
SpinorUndotN[label][$mu][$up]={Null,Null};
SpinorDotN[label][$mu][$down]={Null,Null};
SpinorDotN[label][$mu][$up]={Null,Null};
ExtramassN[label]=0;
ExtramasstildeN[label]=0;
];

(*Return the generated 4D spinors*)
Return[{lamdown,lamdotup,lamup,lamdotdown}];(*{|\[Lambda]>,|\[Lambda]],<\[Lambda]|,[\[Lambda]|}*)
];

];


MomToSpinors4DMassiveN[pr_List,label_:True,precise_:True]:=Module[{sys,q0,q1,q2,q3,k0,k1,k2,k3,sol,qvec,kvec,mass,masstil,fun,solve},
k1=RandomInteger[100];
k2=RandomInteger[100];
k3=RandomInteger[100];
k0=Sqrt[k1^2+k2^2+k3^2];

(*Apply //N if required*)
If[TrueQ[precise],
fun[x_]:=x;
solve=Solve,
fun=N;
solve=NSolve;
];

sys=({q0,q1,q2,q3}==pr-scalarprod[pr]/(2*scalarprod[{q0,q1,q2,q3},{k0,k1,k2,k3}])*{k0,k1,k2,k3});
sol=solve[sys,{q0,q1,q2,q3}]//First;

(*Now generate the spinors associated to the massless momenta and store them if required*)
qvec=MomToSpinors4DMasslessN[{q0,q1,q2,q3}/.sol,label,$lam,precise];
kvec=MomToSpinors4DMasslessN[{k0,k1,k2,k3},label,$mu,precise];
mass=Sqrt[scalarprod[pr]];
masstil=Sqrt[scalarprod[pr]];
(*And now assign the values to the masses if label is given*)
If[!TrueQ[label],
ExtramassN[label]=mass//fun;
ExtramasstildeN[label]=masstil//fun;
];

Return[{qvec,kvec,mass,masstil}];

];


MomToSpinors6DN::massive="Warning: the given momentum is not massless."
"Warning: the given momentum is not massless."
MomToSpinors6DN[{q0_,q1_,q2_,q3_,q4_,q5_},label_:True,precise_:True]:=Catch[Module[{x,y,z,k,xt,yt,zt,kt,P1p,P1m,P2p,P2m,P3p,P3m,lamup,lamdown,lamdotdown,lamdotup,muup,mudown,mudotup,mudotdown,mass,masstil,reps,fun,p0,p1,p2,p3,p4,p5},
(*Firts we convert the upper index momentum given as input to lower index, because the parametrization is constructed in terms of the latter*)
{p0,p1,p2,p3,p4,p5}={q0,-q1,-q2,-q3,-q4,-q5};

(*(*First check if the given momentum is massles, if not print error message*)
If[!((p0^2-p1^2-p2^2-p3^2-p4^2-p5^2)//N//Chop)===0,
(*Throw["The given momentum is not massless, spinor generation aborted."];*)
Message[MomToSpinors6DN::massive];
];*)
If[!(scalarprod[{p0,p1,p2,p3,p4,p5}]//N//Chop)===0,
(*Throw["The given momentum is not massless, spinor generation aborted."];*)
Message[MomToSpinors6DN::massive];
];

(*Define the objects:*)
P1p=p0+p3;
P1m=p0-p3;
P2p=p1+I*p2;
P2m=p1-I*p2;
P3p=p5+I*p4;
P3m=p5-I*p4;

lamdown={x,y};
lamdotup={xt,yt};
mudown={z,k};
mudotup={zt,kt};
lamup={y,-x};
muup={k,-z};
lamdotdown={-yt,xt};
mudotdown={-kt,zt};

mass=lamup . mudown;
masstil=mudotdown . lamdotup;

(*Apply //N if required*)
If[TrueQ[precise],
fun[x_]:=x,
fun=N;
];

(*Assign values of the parameters previously computed.*)
reps={x->-P3m,y->0,xt->-(P2m/P3m),yt->P1p/P3m,zt->P1m,z->0,kt->-P2p,k->1};
(*Recall that in this parametrization we already used little-group invariance to fix the three arbitrary parameters to nice values.*)

{x,y,z,k,xt,yt,zt,kt}={x,y,z,k,xt,yt,zt,kt}/.reps;

If[TrueQ[label],
Return[{{lamdown,lamdotup},{mudown,mudotup},mass,masstil}],
(*If a label is given set the spinors to the generated values*)
SpinorUndotN[label][$lam][$up]=lamup//fun;
SpinorUndotN[label][$lam][$down]=lamdown//fun;
SpinorUndotN[label][$mu][$up]=muup//fun;
SpinorUndotN[label][$mu][$down]=mudown//fun;
SpinorDotN[label][$lam][$up]=lamdotup//fun;
SpinorDotN[label][$lam][$down]=lamdotdown//fun;
SpinorDotN[label][$mu][$up]=mudotup//fun;
SpinorDotN[label][$mu][$down]=mudotdown//fun;
ExtramassN[label]=mass//fun;
ExtramasstildeN[label]=masstil//fun;
Return[{{lamdown,lamdotup},{mudown,mudotup},mass,masstil}];
];

];
];


(* ::Subsection::Closed:: *)
(*MomToSpinors*)


MomToSpinors[Momentum_List,label_:True,precise_:True]:=Catch[Module[{p0,p1,p2,p3,p4,p5,len,out,mass,momentum},
(*If the momentum is of the form (p0,p1,p2,p3,0,0) then it is actually 4d and needs to be traeted as such*)
If[MatchQ[Momentum//Chop,{_,_,_,_,0,0}],
momentum=Momentum[[;;4]],
momentum=Momentum;
];

ClearDependentKinematics[];

len=Length[momentum];
Which[len===4,
{p0,p1,p2,p3}=momentum;
mass=(p0^2-p1^2-p2^2-p3^2)//Chop;
Which[mass===0,
(*4D massless*)
out=MomToSpinors4DMasslessN[momentum,label,$lam,precise],
mass>0,
(*4D massive*)
out=MomToSpinors4DMassiveN[momentum,label,precise],
True,
Throw["The given four-dimensional momentum has negative mass. Generation of spinors has been aborted."];
];
,
len===6,
(*6D massless*)
out=MomToSpinors6DN[momentum,label,precise];
,
True,
Throw["Unknown form of the momentum, acceptable expressions are: four-dimensional massless, four-dimensional massive and six-dimensional massless. Please check input."];
];
Return[out];
];
];


(* ::Subsection::Closed:: *)
(*Cyclic*)


(*Auxiliary function cyclic for Cyclic*)

cyclic[exp_,cycle_List]:=Module[{localexp,reps,localcycle},
(*Consider the possibility of many cycles*)
If[TrueQ[Head[cycle[[1]]]==List],
localcycle=cycle,
localcycle={cycle};
];
(*Transform cycle_List into a list of replacements*)
reps={};
Do[
AppendTo[reps,Join[Table[{subcycle[[i]]->subcycle[[i+1]],ToExpression["r"<>ToString[subcycle[[i]]]]->ToExpression["r"<>ToString[subcycle[[i+1]]]]},{i,Length[subcycle]-1}],{subcycle[[-1]]->subcycle[[1]],ToExpression["r"<>ToString[subcycle[[-1]]]]->ToExpression["r"<>ToString[subcycle[[1]]]]}]//Flatten];
,{subcycle,localcycle}];
reps=reps//Flatten;
(*Apply replacements to the functions in the package.*)
localexp=exp/.{MomPure[x_]:>(MomPure[x]/.reps),PolarPure[x__]:>(PolarPure[x]/.reps),S[x__]:>(S[x]/.reps),S4[x__]:>(S4[x]/.reps),Mom[x_][y_][z_]:>(Mom[x][y][z]/.reps),Polar[x__][y_][z_]:>(Polar[x][y][z]/.reps),Eta[x__]:>(Eta[x]/.reps),SpinorAngleBracket[x__]:>(SpinorAngleBracket[x]/.reps),SpinorSquareBracket[x__]:>(SpinorSquareBracket[x]/.reps),AngSquInvariant[x_]:>(AngSquInvariant[x]/.reps),SquAngInvariant[x__]:>(SquAngInvariant[x]/.reps),AngAngInvariant[x__]:>(AngAngInvariant[x]/.reps),SquSquInvariant[x__]:>(SquSquInvariant[x]/.reps),SpinorUndot[x__]:>(SpinorUndot[x]/.reps),SpinorDot[x__]:>(SpinorDot[x]/.reps),SpinorUndot6D[x__]:>(SpinorUndot6D[x]/.reps),SpinorDot6D[x__]:>(SpinorDot6D[x]/.reps),SpinorDotPure[x__]:>(SpinorDotPure[x]/.reps),SpinorUndotPure[x__]:>(SpinorUndotPure[x]/.reps)};
Return[localexp];
];

(*Options*)
Options[Cyclic]={SumAll->False};

(*The actual function*)
Cyclic[exp_,cycle_List,OptionsPattern[]]:=Module[{len,addenda,out},
If[TrueQ[OptionValue[SumAll]]&&!TrueQ[Head[cycle[[1]]]==List],
len=Length[cycle];
addenda[1]=exp;
Do[addenda[i]=cyclic[addenda[i-1],cycle],{i,2,len}];
out=Sum[addenda[i],{i,len}];
,
out=cyclic[exp,cycle];
];
Return[out];
];


(* ::Subsection::Closed:: *)
(*FeynCToSpinor*)


FeynCToSpinor[exp_,momenta_List]:=Block[{Eta,locexp,Sreps,momrep,proprep1,proprep2,Pair},
Sreps=Table[ToExpression["s"<>ToString[i]<>ToString[j]]->S[i,j],{i,momenta},{j,momenta}]//Flatten;
locexp=exp/.Sreps;
momrep={Pair[LorentzIndex[m_,___],Momentum[p1_,___]]:>Mom[p1][m][Null],Pair[LorentzIndex[\[Mu]_,___],LorentzIndex[\[Nu]_,___]]:>Eta[\[Mu],\[Nu]][$up]};
locexp=locexp/.momrep;
proprep1={FeynAmpDenominator[x__]:>1/(Times[x])};
proprep2={PropagatorDenominator[x_,m_]:>mp[x,x]-mp[m,m]};
locexp=locexp/.proprep1;
locexp=locexp/.proprep2;
proprep1={mp[0,0]->0,Momentum[x_,_]:>MomPure[x]};
locexp=locexp/.proprep1;
Return[locexp];
];


(* ::Subsection::Closed:: *)
(*MomReplace*)


MomReplace[exp_,reps_List,OptionsPattern[]]:=Block[{locexp,localden,count1,count2,\[Mu],\[Nu],locreps,subcounter,Eta2,loc,loopmom,len,backrep},

(*Set up: start by creating a list of th emomenta to be replaced*)
loopmom=Table[i[[1]],{i,reps}];
loopmom=loopmom/.{Mom[x_][_][_]:>x};
(*Definition of counters needed later on in the replacemnets*)
count1=1;
count2=2;
subcounter:=IntegerPart[count2++/2];
(*Definition of the replecements*)
locreps={S[i_,j_]:>2*Mom[i][\[Mu][subcounter]][Null]Eta2[\[Mu][subcounter],\[Mu][subcounter]][$down]Mom[j][\[Mu][subcounter]][Null],chain[type1_,a_,{b___,x_,c___},d_,type2_]/;MemberQ[loopmom,x]:>chain[type1,a,{b,\[Nu][loc=count1++],c},d,type2]Mom[x][\[Nu][loc]][Null],Chain[type1_,a_,{b___,x_,c___},d_,type2_]/;MemberQ[loopmom,x]:>Chain[type1,a,locexp=Plus@@Table[locexp[[i]]/localden[[i]],{i,len}];{b,\[Nu][loc=count1++],c},d,type2]Mom[x][\[Nu][loc]][Null]};
backrep={chain[type1_,a_,{b___,\[Mu]_,c___},d_,type2_]Mom[x_][\[Mu]_][Null]:>chain[type1,a,{b,x,c},d,type2],Chain[type1_,a_,{b___,\[Mu]_,c___},d_,type2_]Mom[x_][\[Mu]_][Null]:>Chain[type1,a,{b,x,c},d,type2]};

(*Create a list of numerators and denominators where the replacemnt has to be performed. this separation is needed in order to handle the replacements in the denominator safely*)
locexp=exp//Expand;
If[TrueQ[Head[locexp]==Plus],
locexp=List@@locexp,
locexp={locexp}//Flatten[#,1]&;
];
debugPrint[locexp];
len=Length[locexp];
localden=Table[Denominator[i],{i,locexp}];
debugPrint[localden];
locexp=Table[Numerator[i],{i,locexp}];
debugPrint[locexp];

(*Apply replecemnts in the numerator*)
locexp=locexp/.locreps;
locexp=locexp/.reps;
locexp=locexp/.{Eta2->Eta}//Expand;
debugPrint[locexp];
locexp=locexp//.backrep;
debugPrint[locexp];
(*Differentiate among the possibilities of performing the replacement in the numerator only or in the denominator as well*)
If[TrueQ[OptionValue[NumeratorOnly]],
locexp=Plus@@Table[locexp[[i]]/localden[[i]],{i,len}],
localden=localden/.locreps;
localden=localden/.reps;
localden=localden/.{Eta2->Eta}//Expand;
debugPrint[localden];
localden=localden//.backrep;
debugPrint[localden];
locexp=Plus@@Table[locexp[[i]]/localden[[i]],{i,len}];
];
Return[locexp];
];


(* ::Subsection::Closed:: *)
(*HelicityWeight*)


(*HelicityWeight::unequalweight="Some terms have unequal weights, please check input.";

HelicityWeight[exp_]:=Module[{moms,locexp,weight,hel},
(*Extract all the momenta in the expression*)
moms=Join[Cases[{exp},HoldPattern[SpinorAngleBracket[x_,y_]|SpinorSquareBracket[x_,y_]|mp[x_,y_]|S[x_,y_]|S4[x_,y_]]:>Sequence[x,y],\[Infinity]],Cases[{exp},HoldPattern[Chain[type1_,x_,{y__},z_,type2_]]:>Sequence[x,y,z],\[Infinity]]]//DeleteDuplicates;
(*Remove form the expression everything which is not needed, i.e. all multiplicative factors are turned into 1s, upon expanding*)
locexp=exp//Expand;
locexp=locexp//.Times[x_,y_]/;FreeQ[x,SpinorAngleBracket|SpinorSquareBracket|Chain]:>y;

(*Do the power counting using the weight function*)
locexp=locexp/.{SpinorAngleBracket[x_,y_]:>weight[x,-1]weight[y,-1],SpinorSquareBracket[x_,y_]:>weight[x,1]weight[y,1],Chain[$angle,x_,{y__},z_,$angle]:>weight[x,-1]weight[z,-1],Chain[$angle,x_,{y__},z_,$square]:>weight[x,-1]weight[z,1],Chain[$square,x_,{y__},z_,$angle]:>weight[x,1]weight[z,-1],
Chain[$square,x_,{y__},z_,$square]:>weight[x,1]weight[z,1]};

(*Take powers into account and recollect weights*)
locexp=locexp//.{Power[weight[x_,n_],m_]:>weight[x,n*m],weight[x_,n_]*weight[x_,m_]:>weight[x,n+m]};

(*If locexp now still contains a sum of any kind it means that the terms are not uniform in the spinor weight. The ToString replacement is needed so that spinor arguments like n+1 are still allowed*)
If[!FreeQ[locexp/.weight[x_,n_]:>weight[ToString[x],n],Plus],
Message[HelicityWeight::unequalweight];
Return[$Failed];
];

(*Putting things together*)
hel={};
locexp/.weight[x_,n_]:>(hel=Join[hel,{{x,n}}];1);

(*Append the missing momenta with weight zero*)
Do[If[FreeQ[hel,i],
hel=Join[hel,{{i,0}}]
];
,{i,moms}];

(*Sort and return output*)
Return[hel//Sort];
];*)


HelicityWeight::unequalweight="Some terms have unequal weights, please check input.";

HelicityWeight[exp_]:=Module[{moms,locexp,weight,hel},
(*Extract all the momenta in the expression*)
moms=Join[Cases[{exp},HoldPattern[SpinorAngleBracket[x_,y_]|SpinorSquareBracket[x_,y_]|mp[x_,y_]|S[x_,y_]|S4[x_,y_]]:>Sequence[x,y],\[Infinity]],Cases[{exp},HoldPattern[Chain[type1_,x_,{y__},z_,type2_]]:>Sequence[x,y,z],\[Infinity]]]//DeleteDuplicates;
(*Remove form the expression everything which is not needed, i.e. all multiplicative factors are turned into 1s, upon expanding*)
locexp=exp//Expand;
locexp=locexp//.Times[x_,y_]/;FreeQ[x,SpinorAngleBracket|SpinorSquareBracket|Chain]:>y;

(*Do the power counting using the weight function*)
locexp=locexp/.{SpinorAngleBracket[x_,y_]:>weight[x,-1]weight[y,-1],SpinorSquareBracket[x_,y_]:>weight[x,1]weight[y,1],Chain[$angle,x_,{y__},z_,$angle]:>weight[x,-1]weight[z,-1],Chain[$angle,x_,{y__},z_,$square]:>weight[x,-1]weight[z,1],Chain[$square,x_,{y__},z_,$angle]:>weight[x,1]weight[z,-1],
Chain[$square,x_,{y__},z_,$square]:>weight[x,1]weight[z,1]};

(*Take powers into account and recollect weights*)
locexp=locexp//.{Power[weight[x_,n_],m_]:>weight[x,n*m],weight[x_,n_]*weight[x_,m_]:>weight[x,n+m]};

(*Now remove the scalars*)
hel={};
locexp=locexp/.weight[x_,0]:>(hel=Join[hel,{{x,0}}];1);
(*scalars might appear in multiple terms, so we delete the duplicates*)
hel=hel//DeleteDuplicates;

(*If locexp now still contains a sum of any kind it means that the terms are not uniform in the spinor weight. The ToString replacement is needed so that spinor arguments like n+1 are still allowed*)
If[!FreeQ[locexp/.weight[x_,n_]:>weight[ToString[x],n],Plus],
Message[HelicityWeight::unequalweight];
Return[$Failed];
];

(*Putting things together*)
locexp/.weight[x_,n_]:>(hel=Join[hel,{{x,n}}];1);

(*Append the missing momenta with weight zero*)
Do[If[FreeQ[hel,i],
hel=Join[hel,{{i,0}}]
];
,{i,moms}];

(*Sort and return output*)
Return[hel//Sort];
]


(* ::Subsection::Closed:: *)
(*MassDimension*)


Options[MassDimension]={SetDimension->{}};

MassDimension[exp_,OptionsPattern[]]:=Catch[Module[{locexp,locrep,pow,invpow,weight},

(*Now replace with pow in exp*)
locexp=exp/.{SpinorAngleBracket[x__]:>pow*SpinorAngleBracket[x],SpinorSquareBracket[x__]:>pow*SpinorSquareBracket[x],Chain[type1_,x_,{y__},z_,type2_]:>Chain[type1,x,{y},z,type2]*pow^(Length[{y}]+1),S4[x__]:>pow^2*S4[x],S[x__]:>pow^2*S[x],mp[x_,y_]:>pow^2*mp[x,y]};

If[!(locrep=OptionValue[SetDimension])==={},
(*Check that the dimension has been assigned consistently*)
If[AllTrue[locrep,(Head[#]===Rule||Head[#]===RuleDelayed)&],
(*Put SetDimension in a form which we like*)
locrep=Table[i[[1]]->i[[1]]*pow^i[[2]],{i,locrep}];
locexp=locexp/.locrep;
,
Throw["Dimensions in SetDimension must be given as Rule or RuleDelayed, please check the value assigned to this option."];
];
];

(*Collect the weights*)
locexp=locexp/.{Power[pow,n_?Negative]:>invpow^(-n)};
locexp=CoefficientRules[locexp,{pow,invpow}];
locexp=Table[i[[1,1]]-i[[1,2]]->i[[2]],{i,locexp}]//Sort;
locexp=Gather[locexp,First[#1]==First[#2]&];
locexp=Table[i[[1,1]]->Sum[i[[j,2]],{j,Length[i]}],{i,locexp}];

If[Length[locexp]==1,
Return[locexp[[1,1]]],
Print["Terms with different mass dimension appear in expression. Check your input"];
locexp=Table[{i[[1]],i[[2]]},{i,locexp}];
Return[locexp];
];


];
];


(* ::Section:: *)
(*Symbolic calculus*)


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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

(*define dm4 as a shorthand notation for the four-dimensional delta*)

(*Shortcuts*)
If[frontend==1,
SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "md" -> MDeltaBox[$MDimension,"\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]];
    ];


(* ::Subsection::Closed:: *)
(*Delta*)


(*Displaying it nicely:*)
DeltaBox[x_,y_]:=TemplateBox[{x,y},"Delta",
DisplayFunction->(RowBox[{SubsuperscriptBox["\[Delta]",#2,#1]}]&),
InterpretationFunction->(RowBox[{"Delta","[",#1,",",#2,"]"}]&)
];

Delta /: MakeBoxes[Delta[x_,y_],StandardForm|TraditionalForm]:=DeltaBox[ToBoxes[x],ToBoxes[y]];

(*Set default dimension:*)
Options[Delta]={$DeltaDim->$dim};
Protect[$dim];

(*Some properties.*)
(*Contracted with itself:*)
Delta[up_,down_,OptionsPattern[]]/;TrueQ[up==down]:=OptionValue[$DeltaDim];
Delta /: Times[Delta[up1_,down1_],Delta[down1_,down2_]]:=Delta[up1,down2];
Delta /: Times[Delta[up1_,down1_,opt1_],Delta[down1_,down2_,opt1_]]:=Delta[up1,down2,opt1];

(*Contracted with Eta, Mom and Polar:*)
Delta /: Times[Delta[a_,b_],Eta[b_,c_][$up]]:=Eta[a,c][$up];
Delta /: Times[Delta[a_,b_],Eta[c_,b_][$up]]:=Eta[a,c][$up];
Delta /: Times[Delta[a_,b_],Eta[a_,c_][$down]]:=Eta[b,c][$down];
Delta /: Times[Delta[a_,b_],Eta[c_,a_][$down]]:=Eta[b,c][$down];
Delta /: Times[Delta[up_,do_],Mom[lab_][do_][Null]]:=Mom[lab][up][Null];
Delta /: Times[Delta[up_,do_],Mom[lab_][Null][up_]]:=Mom[lab][Null][do];
Delta /: Times[Delta[up_,do_],Polar[lab_,ref_][do_][Null]]:=Polar[lab,ref][up][Null];
Delta /: Times[Delta[up_,do_],Polar[lab_,ref_][Null][up_]]:=Polar[lab,ref][Null][do];


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*SymmetrizeSH*)


SetAttributes[SymmetrizeSH,HoldFirst];

SymmetrizeSH[exp_,indices__List]:=Module[{len,combos,localexp,output,localindices,nsubs},
(*Start by counting the number of different lists given as input*)
localindices={indices};
nsubs=Length[localindices];

output=HoldForm[exp];

(*Now we loop over every single list of antisymmetrized indices*)
Do[
(*Generate all the possible reshuffelings of the indices in list*)
len=Length[localindices[[k]]];
combos=Permutations[localindices[[k]]];
debugPrint[combos];

(*Now do the replacements in the expression*)
localexp=output;
output={};
Do[AppendTo[output,localexp/.Table[localindices[[k,j]]->combos[[i,j]],{j,len}]],{i,Length[combos]}];
debugPrint[output];
output=(1/(len!))*Plus@@output//Expand;
,{k,nsubs}];

(*Before returning the result release the hold on the expression*)
output=ReleaseHold[output];
Return[output];
];


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Computing F with Lorentz indices*)


FstBoxup[mom_,mu_,nu_]:=TemplateBox[{mom,mu,nu},"Fstup",
DisplayFunction->(RowBox[{SuperscriptBox["F",RowBox[{#2,#3}]],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Fst","[",#1,"]","[",#2,",",#3,"]","[","Null",",","Null","]"}]&)
];
FstBoxdown[mom_,mu_,nu_]:=TemplateBox[{mom,mu,nu},"Fstdown",
DisplayFunction->(RowBox[{SubscriptBox["F",RowBox[{#2,#3}]],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Fst","[",#1,"]","[","Null",",","Null","]","[",#2,",",#3,"]"}]&)
];
Fst /: MakeBoxes[Fst[mom_][mu_,nu_][Null,Null],StandardForm|TraditionalForm]:=FstBoxup[ToBoxes[mom],ToBoxes[mu],ToBoxes[nu]];
Fst /: MakeBoxes[Fst[mom_][Null,Null][mu_,nu_],StandardForm|TraditionalForm]:=FstBoxdown[ToBoxes[mom],ToBoxes[mu],ToBoxes[nu]];
Fst[mom_][mu_,nu_][Null,Null]:=Mom[mom][mu][Null]Polar[mom,ToExpression["ref"<>ToString[mom]]][nu][Null]-Mom[mom][nu][Null]Polar[mom,ToExpression["ref"<>ToString[mom]]][mu][Null];

EtaBoxup[mu_,nu_]:=TemplateBox[{mu,nu},"etaup",
DisplayFunction->(SuperscriptBox["\[Eta]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"Eta","[",#1,",",#2,"]","[","$up","]"}]&)
];
EtaBoxdown[mu_,nu_]:=TemplateBox[{mu,nu},"etadown",
DisplayFunction->(SubscriptBox["\[Eta]",RowBox[{#1,#2}]]&),
InterpretationFunction->(RowBox[{"Eta","[",#1,",",#2,"]","[","$down","]"}]&)
];
Eta /: MakeBoxes[Eta[mu_,nu_][$up],StandardForm|TraditionalForm]:=EtaBoxup[ToBoxes[mu],ToBoxes[nu]];
Eta /: MakeBoxes[Eta[mu_,nu_][$down],StandardForm|TraditionalForm]:=EtaBoxdown[ToBoxes[mu],ToBoxes[nu]];
SetAttributes[Eta,Orderless];

PolarBoxup[mom_,refmom_,mu_]:=TemplateBox[{mom,refmom,mu},"Polarup",
DisplayFunction->(RowBox[{SuperscriptBox["\[CurlyEpsilon]",#3],"[",#1,",",#2,"]"}]&),
InterpretationFunction->(RowBox[{"Polar","[",#1,",",#2,"]","[",#3,"]","[","Null","]"}]&)
];
PolarBoxdown[mom_,refmom_,mu_]:=TemplateBox[{mom,refmom,mu},"Polardown",
DisplayFunction->(RowBox[{SubscriptBox["\[CurlyEpsilon]",#3],"[",#1,",",#2,"]"}]&),
InterpretationFunction->(RowBox[{"Polar","[",#1,",",#2,"]","[","Null","]","[",#3,"]"}]&)
];
PolarPureBox[mom_,refmom_]:=TemplateBox[{mom,refmom},"PolarPure",
DisplayFunction->(RowBox[{"\[CurlyEpsilon]","[",#1,",",#2,"]"}]&),
InterpretationFunction->(RowBox[{"PolarPure","[",#1,",",#2,"]"}]&)
];
Polar /: MakeBoxes[Polar[mom_,refmom_][mu_][Null],TraditionalForm|StandardForm]:=PolarBoxup[ToBoxes[mom],ToBoxes[refmom],ToBoxes[mu]];
Polar /: MakeBoxes[Polar[mom_,refmom_][Null][mu_],TraditionalForm|StandardForm]:=PolarBoxdown[ToBoxes[mom],ToBoxes[refmom],ToBoxes[mu]];
PolarPure /: MakeBoxes[PolarPure[mom_,refmom_],StandardForm|TraditionalForm]:=PolarPureBox[ToBoxes[mom],ToBoxes[refmom]];

(*MomBoxup[mom_,mu_]:=TemplateBox[{mom,mu},"Momup",
DisplayFunction->(RowBox[{SuperscriptBox["p",#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Mom","[",#1,"][",#2,"]","[","Null","]"}]&)
];
MomBoxdown[mom_,mu_]:=TemplateBox[{mom,mu},"Momdown",
DisplayFunction->(RowBox[{SubscriptBox["p",#2],"[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"Mom","[",#1,"]","[","Null","]","[",#2,"]"}]&)
];
MomPureBox[mom_]:=TemplateBox[{mom},"MomPure",
DisplayFunction->(RowBox[{"p","[",#1,"]"}]&),
InterpretationFunction->(RowBox[{"MomPure","[",#1,"]"}]&)
];*)

MomBoxup[mom_,mu_]:=TemplateBox[{mom,mu},"Momup",
DisplayFunction->(RowBox[{SuperscriptBox[#1,#2]}]&),
InterpretationFunction->(RowBox[{"Mom","[",#1,"]","[",#2,"]","[","Null","]"}]&)
];
MomBoxdown[mom_,mu_]:=TemplateBox[{mom,mu},"Momdown",
DisplayFunction->(RowBox[{SubscriptBox[#1,#2]}]&),
InterpretationFunction->(RowBox[{"Mom","[",#1,"]","[","Null","]","[",#2,"]"}]&)
];
MomPureBox[mom_]:=TemplateBox[{mom},"MomPure",
DisplayFunction->(RowBox[{#1}]&),
InterpretationFunction->(RowBox[{"MomPure","[",#1,"]"}]&)
];


Mom /: MakeBoxes[Mom[mom_][mu_][Null],StandardForm|TraditionalForm]:=MomBoxup[ToBoxes[mom],ToBoxes[mu]];
Mom /: MakeBoxes[Mom[mom_][Null][mu_],StandardForm|TraditionalForm]:=MomBoxdown[ToBoxes[mom],ToBoxes[mu]];
MomPure /: MakeBoxes[MomPure[mom_],StandardForm|TraditionalForm]:=MomPureBox[ToBoxes[mom]];
MomPure[MomPure[x_]]:=MomPure[x];
(*MomPure[x_]/;!FreeQ[x,MomPure]:=x;*)
MomPure[A_*MomPure[x_]+B_]:=A*MomPure[x]+MomPure[B];
MomPure[MomPure[x_]+B_]:=MomPure[x]+MomPure[B];
MomPure[Times[A_,MomPure[x_]]]:=A*MomPure[x];
Mom[MomPure[x_]]:=Mom[x];

Eta /: Times[Eta[mu_,nu_][$down],f_[lab__][nu_][Null]]:=f[lab][Null][mu];
Eta /: Times[Eta[mu_,nu_][$up],f_[lab__][Null][nu_]]:=f[lab][mu][Null];
Eta /: Times[Eta[mu_,nu_][$up],Eta[nu_,ro_][$down]]:=Delta[mu,ro];
Mom /: Times[Mom[lab1_][mu_][Null],f_[lab2__][Null][mu_]]:=mp[MomPure[lab1],ToExpression[ToString[f]<>"Pure"][lab2]];
Polar /: Times[Polar[lab1_,lab2_][mu_][Null],f_[lab3__][Null][mu_]]:=mp[PolarPure[lab1,lab2],ToExpression[ToString[f]<>"Pure"][lab3]];
PolarPure /: mp[PolarPure[x_,ref_],MomPure[x_]]:=0;
PolarPure /: mp[PolarPure[x_,ref_],MomPure[ref_]]:=0;
PolarPure /: mp[PolarPure[x_,ref_],PolarPure[x_,ref_]]:=0;


(* ::Subsection::Closed:: *)
(*MpToSpinors*)


MpToSpinors[exp_,plus_List,minus_List]:=Module[{locexp,rep1,rep2,rep3},
locexp=exp;
(*Momenta*)
rep1={mp[MomPure[x_],MomPure[y_]]/;MemberQ[Momenta4D,x]&&MemberQ[Momenta4D,y]:>1/2*Spinoranglebracket[x,y]Spinorsquarebracket[y,x]};
(*Momentum with polarization*)
rep2={mp[MomPure[p_],PolarPure[k_,r_]]/;MemberQ[Momenta4D,p]&&MemberQ[Momenta4D,k]&&MemberQ[plus,k]:>1/Sqrt[2]*(Spinoranglebracket[p,r]Spinorsquarebracket[p,k])/Spinoranglebracket[k,r],mp[MomPure[p_],PolarPure[k_,r_]]/;MemberQ[minus,k]:>-1/Sqrt[2]*(Spinoranglebracket[p,k]Spinorsquarebracket[p,r])/Spinorsquarebracket[k,r]};
(*Polarizations*)
rep3={mp[PolarPure[k1_,r1_],PolarPure[k2_,r2_]]/;MemberQ[Momenta4D,k1]&&MemberQ[Momenta4D,k2]&&MemberQ[plus,k1]&&MemberQ[plus,k2]:>(Spinoranglebracket[r1,r2]Spinorsquarebracket[k2,k1])/(Spinoranglebracket[k1,r1]Spinoranglebracket[k2,r2]),mp[PolarPure[k1_,r1_],PolarPure[k2_,r2_]]/;MemberQ[plus,k1]&&MemberQ[minus,k2]:>-(Spinoranglebracket[r1,k2]Spinorsquarebracket[r2,k1])/(Spinoranglebracket[k1,r1]Spinorsquarebracket[k2,r2]),mp[PolarPure[k1_,r1_],PolarPure[k2_,r2_]]/;MemberQ[minus,k1]&&MemberQ[minus,k2]:>(Spinoranglebracket[k1,k2]Spinorsquarebracket[r2,r1])/(Spinorsquarebracket[k1,r1]Spinorsquarebracket[k2,r2])};
locexp=locexp/.Flatten[{rep1,rep2,rep3}];
Return[locexp];
];


(* ::Subsection::Closed:: *)
(*VecToSpinors*)


VecToSpinors[exp_,plus_List,minus_List]:=exp/.{Mom[p_][a_][Null]:>1/2*Chain[$angle,p,{a},p,$square],Mom[p_][Null][a_]:>1/2*Chain[$angle,p,{a},p,$square],Polar[k_,r_][a_][Null]/;MemberQ[plus,k]:>1/Sqrt[2]*Chain[$angle,r,{a},k,$square]/Spinoranglebracket[r,k],Polar[k_,r_][a_][Null]/;MemberQ[minus,k]:>1/Sqrt[2]*Chain[$angle,k,{a},r,$square]/Spinorsquarebracket[k,r]};


(* ::Subsection::Closed:: *)
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


(* ::Subsection:: *)
(*ClearSubvalues*)


(*ClearSubValues[f_]:=(SubValues[f]=DeleteCases[SubValues[f],_?(FreeQ[First[#],HoldPattern[f[Pattern]]]&)]);*)

ClearSubValues[f_]:=(SubValues[f]=DeleteCases[SubValues[f],_?(FreeQ[First[#],Pattern]&)]);


(* ::Section:: *)
(*Shortcuts*)


(* ::Subsection::Closed:: *)
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
(*Custom functions*)


(* ::Subsection::Closed:: *)
(*AssignNames*)


SetAttributes[AssignFunctions,HoldAllComplete];

AssignFunctions[names__]:=Module[{path,filename,rd,defs,wr,tofile,redundant,myset,mysetdelayed,defsclear},

(*First of all we clear all definitions. To do so we need to apply special treatment for Set and SetDelayed since these do not allow fo a straight pattern matching as Rule and RuleDelayed do*)

tofile=Hold[{names}]/.{Set->myset,SetDelayed->mysetdelayed};
tofile=tofile/.{Rule[g_[__],_]:>ClearAll[g],RuleDelayed[g_[__],_]:>ClearAll[g],mysetdelayed[g_[__],_]:>ClearAll[g],myset[x_,y_]:>ClearAll[x]};
tofile=tofile/.{mysetdelayed[x_,y_]:>ClearAll[x],RuleDelayed[x_,y_]:>ClearAll[x],Rule[x_,y_]:>Clear[x]};
ReleaseHold[tofile];

(*Now we convert everything into a list of replacements which is safe to handle*)
tofile=Hold[{names}]/.{Set->Rule,SetDelayed->RuleDelayed};
tofile=tofile//ReleaseHold;

(*Find the package*)
path=FindFile["SpinorHelicity6D`"];
(*Extract the position without the package name*)
path=FileNameTake[path,{1,FileNameDepth[path]-1}];
(*Define the name of the file containing the definitions*)
filename=FileNameJoin[{path,"SpinorHelicityCustomFunctions.wl"}];

(*Check if a file that contains definitions already exists. If it does, open it and load its content, which will be a list of replacements*)
If[FileExistsQ[filename],
(*Load the content of the file*)
rd=OpenRead[filename];
defs=Read[rd];
Close[filename];
(*Check if there are any redundant definitions to be replaced. To do so we need also here to clear all the definitions*)
defsclear=defs/.{Rule[g_[__],_]:>ClearAll[g],RuleDelayed[g_[__],_]:>ClearAll[g]};
defsclear=defsclear/.{RuleDelayed[x_,y_]:>Clear[x],Rule[x_,y_]:>Clear[x]};
ReleaseHold[defsclear];
(*Extract all the function names from tofile*)
redundant=tofile/.{Rule[g_[__],_]:>g,RuleDelayed[g_[__],_]:>g};
redundant=redundant/.{RuleDelayed[x_,y_]:>x,Rule[x_,y_]:>x};
redundant=Flatten[redundant];
defs=DeleteCases[defs//ReleaseHold,x_/;AnyTrue[redundant,(!FreeQ[x[[1]],#]&)]];
tofile=Join[defs,tofile//Flatten];
];

(*Open a stream to write to the file*)
wr=OpenWrite[filename];
(*Write to the file*)
Write[wr,Hold/@tofile];
(*Close the stream*)
Close[filename];

(*Convert rules to Set and SetDelayed so that the functions are evaluated and usable*)
tofile/.{RuleDelayed[x_,y_]/;FreeQ[x,Pattern]:>Set[x,y],RuleDelayed[x_,y_]/;!FreeQ[x,Pattern]:>SetDelayed[x,y],Rule[x_,y_]/;FreeQ[x,Pattern]:>Set[x,y],Rule[x_,y_]/;!FreeQ[x,Pattern]:>SetDelayed[x,y]};

];


(*Display all the custom functions*)

AssignFunctions[]:=Module[{path,filename,rd,defs},

(*Find the package*)
path=FindFile["SpinorHelicity6D`"];
(*Extract the position without the package name*)
path=FileNameTake[path,{1,FileNameDepth[path]-1}];
(*Define the name of the file containing the definitions*)
filename=FileNameJoin[{path,"SpinorHelicityCustomFunctions.wl"}];

(*Check if a file that contains definitions already exists. If it does, open it and save its content, which will be a list of holded replacements*)
If[FileExistsQ[filename],
(*Load the content of the file*)
rd=OpenRead[filename];
defs=Read[rd];
Close[filename];
,
defs="No available definitions.";
];

Return[defs];

];


(* ::Subsection::Closed:: *)
(*ClearNames*)


SetAttributes[ClearFunctions,HoldAllComplete];

ClearFunctions[names__]:=Module[{path,filename,rd,defs,wr,tofile,redundant,myset,mysetdelayed,defsclear},

(*Find the package*)
path=FindFile["SpinorHelicity6D`"];
(*Extract the position without the package name*)
path=FileNameTake[path,{1,FileNameDepth[path]-1}];
(*Define the name of the file containing the definitions*)
filename=FileNameJoin[{path,"SpinorHelicityCustomFunctions.wl"}];

(*Check if a file that contains definitions already exists. If it does, open it and load its content, which will be a list of replacements*)
If[FileExistsQ[filename],
(*Clear definitions of the functions*)
tofile=Hold[{names}]/.List->ClearAll;
ReleaseHold[tofile];
tofile={names};

(*Now proceed to removing them from the file. Load the content of the file*)
rd=OpenRead[filename];
defs=Read[rd];
Close[filename];

(*Clear definitions of all the stored functions*)
defsclear=defs/.{Rule[g_[__],_]:>ClearAll[g],RuleDelayed[g_[__],_]:>ClearAll[g]};
defsclear=defsclear/.{RuleDelayed[x_,y_]:>Clear[x],Rule[x_,y_]:>Clear[x]};
ReleaseHold[defsclear];
defs=DeleteCases[defs//ReleaseHold,x_/;AnyTrue[tofile,(!FreeQ[x[[1]],#]&)]];
tofile=defs;
,
Return["No file from which to remove the definitions has been found."];
];

(*Open a stream to write to the file*)
wr=OpenWrite[filename];
(*Write to the file*)
Write[wr,Hold/@tofile];
(*Close the stream*)
Close[filename];

(*Convert rules to Set and SetDelayed so that the functions are evaluated and usable*)
tofile/.{RuleDelayed[x_,y_]/;FreeQ[x,Pattern]:>Set[x,y],RuleDelayed[x_,y_]/;!FreeQ[x,Pattern]:>SetDelayed[x,y],Rule[x_,y_]/;FreeQ[x,Pattern]:>Set[x,y],Rule[x_,y_]/;!FreeQ[x,Pattern]:>SetDelayed[x,y]};

];


(*Clear all the definitions in the file and delete the file itself.*)

ClearFunctions[]:=Module[{path,filename,rd,defs,defsclear},

(*Find the package*)
path=FindFile["SpinorHelicity6D`"];
(*Extract the position without the package name*)
path=FileNameTake[path,{1,FileNameDepth[path]-1}];
(*Define the name of the file containing the definitions*)
filename=FileNameJoin[{path,"SpinorHelicityCustomFunctions.wl"}];

If[FileExistsQ[filename],
(*Load the content of the file*)
rd=OpenRead[filename];
defs=Read[rd];
Close[filename];

(*Clear definitions of all the stored functions*)
defsclear=defs/.{Rule[g_[__],_]:>ClearAll[g],RuleDelayed[g_[__],_]:>ClearAll[g]};
defsclear=defsclear/.{RuleDelayed[x_,y_]:>Clear[x],Rule[x_,y_]:>Clear[x]};
ReleaseHold[defsclear];
(*Then delete the file*)
DeleteFile[filename],
Return["No file has been found."];
];
];


(* ::Subsection::Closed:: *)
(*File loader*)


(*(*Define the function to load the existing file with the custom-functions' definitions, if any.*)

(*In order not to define unwanted global parameters we keep this inside the local context but we have to call outside of it in order to properly load the functions.*)

LoadFunctions[]:=Module[{path,filename,rd,defs},

(*Find the package*)
path=FindFile["SpinorHelicity6D`"];
(*Extract the position without the package name*)
path=FileNameTake[path,{1,FileNameDepth[path]-1}];
(*Define the name of the file containing the definitions*)
filename=FileNameJoin[{path,"SpinorHelicityCustomFunctions.wl"}];

(*Check if a file that contains definitions already exists. If it does, open it and save its content, which will be a list of holded replacements*)
If[FileExistsQ[filename],
(*Load the content of the file*)
rd=OpenRead[filename];
defs=Read[rd];
Close[filename];
defs=defs/.{Rule\[Rule]Set,RuleDelayed\[Rule]SetDelayed};
ReleaseHold[defs];
];

];*)


(* ::Section:: *)
(*Attributes*)


SetAttributes[{overbar,MinusSignQ,SpinorDot,SpinorUndot,SpinorLaDownBox,SpinorLaUpBox,SpinorLatDownBox,
SpinorLatUpBox,SpinorMuDownBox,SpinorMuUpBox,SpinorMutDownBox,SpinorMutUpBox,Spinordot,Spinorundot,$lam,$mu,
extramass,extramasstilde,extramasstildeBox,extramassBox,ExtramassBox,ExtramasstildeBox,Extramasstilde,Extramass,
KillMasses,Momenta4D,SpinorAngleBracket,SpinorAngleBracketBox,SpinorSquareBracket,SpinorSquareBracketBox,
NewProcess,ClearDownValues,levicivita2up,levicivita2down,levicivita2Up,levicivita2Down,levicivita2up,levicivita2down,
SpinorDot6D,SpinorDot6DBox,SpinorUndot6D,SpinorUndot6DBox,AngAngInvariant,AngAngInvariantBox,SquSquInvariant,SquSquInvariantBox,
SquAngInvariant,SquAngInvariantBox,AngSquInvariant,AngSquInvariantBox,Momenta,AllMomenta,
SpinorReplace,SubCounter,ConvenientMu,SchoutenSimplify,Mom4D,S6,S6many,MDelta,MDeltaBox,Fstrength,FstrengthBox,\[Lambda],\[Mu],\[CapitalLambda],\[CapitalMu],\[Epsilon],\[Delta],FixedSpinors,ClearSpinors,FixSpinors,CompleteDenominators,CompleteMandelstam,ToChain,Chain,chain,$angle,$square,S,
MasslessQ,DeclareMom,UndeclareMom,ChainMomCon},Protected]


(* ::Section:: *)
(*Create a palette*)


If[frontend==1,
SpinorPalette:=CreatePalette[Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],SpinorUndotPureMBox["\[SelectionPlaceholder]"],SpinorDotPureMBox["\[SelectionPlaceholder]"],SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[RowBox[{"\[CapitalMu][","\[SelectionPlaceholder]","]"}]],Defer[extramass[\[SelectionPlaceholder]]]],PasteButton[RawBoxes[RowBox[{OverscriptBox["\[CapitalMu]","~"],"[","\[SelectionPlaceholder]","]"}]],Defer[extramasstilde[\[SelectionPlaceholder]]]]}}],Spacings->Automatic],WindowTitle->"SpinorHelicity6D"];
];


(*If[frontend==1,
CreatePalette[Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],SpinorUndotPureMBox["\[SelectionPlaceholder]"],SpinorDotPureMBox["\[SelectionPlaceholder]"],SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[RowBox[{"\[CapitalMu][","\[SelectionPlaceholder]","]"}]],Defer[extramass[\[SelectionPlaceholder]]]],PasteButton[RawBoxes[RowBox[{OverscriptBox["\[CapitalMu]","~"],"[","\[SelectionPlaceholder]","]"}]],Defer[extramasstilde[\[SelectionPlaceholder]]]]}}],Spacings->Automatic],WindowTitle->"SpinorHelicity6D"];
];*)


(*If[frontend==1,
CreatePalette[Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],SpinorUndotPureMBox["\[SelectionPlaceholder]"],SpinorDotPureMBox["\[SelectionPlaceholder]"],SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[RowBox[{"\[CapitalMu][","\[SelectionPlaceholder]","]"}]],Defer[extramass[\[SelectionPlaceholder]]]],PasteButton[RawBoxes[RowBox[{OverscriptBox["\[CapitalMu]","~"],"[","\[SelectionPlaceholder]","]"}]],Defer[extramasstilde[\[SelectionPlaceholder]]]]}},Partition[PasteButton[RawBoxes[#]]&/@{AngleSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],AngleAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"]},4]],Spacings->Automatic],WindowTitle->"SpinorHelicity6D"];
];*)


(*If[frontend==1,
CreatePalette[Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],AngleSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],AngleAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureMBox["\[SelectionPlaceholder]"],SpinorDotPureMBox["\[SelectionPlaceholder]"],SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[RowBox[{"\[CapitalMu][","\[SelectionPlaceholder]","]"}]],Defer[extramass[\[SelectionPlaceholder]]]],PasteButton[RawBoxes[RowBox[{OverscriptBox["\[CapitalMu]","~"],"[","\[SelectionPlaceholder]","]"}]],Defer[extramasstilde[\[SelectionPlaceholder]]]]}}],Spacings->Automatic],WindowTitle->"SpinorHelicity6D"];
];*)


(*If[frontend==1,
CreatePalette[Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],AngleSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],AngleAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureMBox["\[SelectionPlaceholder]"],SpinorDotPureMBox["\[SelectionPlaceholder]"],SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[RowBox[{"\[CapitalMu][","\[SelectionPlaceholder]","]"}]],Defer[extramass[\[SelectionPlaceholder]]]],PasteButton[RawBoxes[RowBox[{OverscriptBox["\[CapitalMu]","~"],"[","\[SelectionPlaceholder]","]"}]],Defer[extramasstilde[\[SelectionPlaceholder]]]]}},{OpenerView[{"test","test2"}]}],Spacings->Automatic],WindowTitle->"SpinorHelicity6D"];
];*)


(*If[frontend==1,
CreatePalette[DynamicModule[{opener1=True,opener2=False},Column[{OpenerView[{"4 dimensions",Grid[{{Grid[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngleSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],AngleAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"]},4],Spacings->Automatic]},{Grid[{PasteButton[RawBoxes[#]]&/@{SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"]}}]}}]},Dynamic[opener1,(opener1=#;opener2=!opener2)&]],OpenerView[{"6 dimensions",Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],SpinorUndotPureMBox["\[SelectionPlaceholder]"],SpinorDotPureMBox["\[SelectionPlaceholder]"],SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngleSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],AngleAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[RowBox[{"\[CapitalMu][","\[SelectionPlaceholder]","]"}]],Defer[extramass[\[SelectionPlaceholder]]]],PasteButton[RawBoxes[RowBox[{OverscriptBox["\[CapitalMu]","~"],"[","\[SelectionPlaceholder]","]"}]],Defer[extramasstilde[\[SelectionPlaceholder]]]]}},Partition[PasteButton[RawBoxes[#]]&/@{},4]],Spacings->Automatic]},Dynamic[opener2,(opener2=#;opener1=!opener1)&]]}]],WindowTitle\[Rule]"Palette"];
];*)


If[frontend==1&&!TrueQ[$MachineID=="5113-13572-95048"],
CreatePalette[DynamicModule[{opener1=True,opener2=False},Column[{OpenerView[{"4 dimensions",Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngleSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],AngleAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"]]],SpanFromLeft,PasteButton[RawBoxes[SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"]]],SpanFromLeft}}],Spacings->{2,0.6}]},Dynamic[opener1,(opener1=#;opener2=!opener2)&]],OpenerView[{"6 dimensions",Grid[Join[Partition[PasteButton[RawBoxes[#]]&/@{SpinorLaUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLaDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorLatDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMuDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutUpBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorMutDownBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorUndotPureLBox["\[SelectionPlaceholder]"],SpinorDotPureLBox["\[SelectionPlaceholder]"],SpinorUndotPureMBox["\[SelectionPlaceholder]"],SpinorDotPureMBox["\[SelectionPlaceholder]"],SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"],levicivita2upBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],levicivita2downBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngleSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],AngleAngleChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SquareSquareChainBox["\[SelectionPlaceholder]",ToBoxes[{\[SelectionPlaceholder]}],"\[SelectionPlaceholder]"],SpinorUndot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SpinorDot6DBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],SquAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"],AngSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]},4],{{PasteButton[RawBoxes[AngAngInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[SquSquInvariantBox["\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]","\[SelectionPlaceholder]"]]],PasteButton[RawBoxes[RowBox[{"\[CapitalMu][","\[SelectionPlaceholder]","]"}]],Defer[extramass[\[SelectionPlaceholder]]]],PasteButton[RawBoxes[RowBox[{OverscriptBox["\[CapitalMu]","~"],"[","\[SelectionPlaceholder]","]"}]],Defer[extramasstilde[\[SelectionPlaceholder]]]]}},Partition[PasteButton[RawBoxes[#]]&/@{},4]],Spacings->Automatic]},Dynamic[opener2,(opener2=#;opener1=!opener1)&]]}]],WindowTitle->"SpinorHelicity6D"];
];


Print["===============SpinorHelicity6D================"];
Print["Authors: Manuel Accettulli Huber (QMUL)"];
Print["Please report any bug to:"];
Print["m.accettullihuber@qmul.ac.uk"];
Print["Version 1.2 , last update 05/06/2020"];
Print["============================================="];


If[TrueQ[$MachineID=="6239-87290-05914"],Speak["Ubi maior minor cessat"]];
(*If[TrueQ[$MachineID=="5113-13572-95048"],Speak["Tricche tracche bombe a mano"]];*)


(*End the private context*)
End[]

(*End the package*)
EndPackage[]


(*Now that we are in the global context, load the file with user-defined definitions*)

Module[{path,filename,rd,defs},

(*Find the package*)
path=FindFile["SpinorHelicity6D`"];
(*Extract the position without the package name*)
path=FileNameTake[path,{1,FileNameDepth[path]-1}];
(*Define the name of the file containing the definitions*)
filename=FileNameJoin[{path,"SpinorHelicityCustomFunctions.wl"}];

(*Check if a file that contains definitions already exists. If it does, open it and save its content, which will be a list of holded replacements*)
If[FileExistsQ[filename],
(*Load the content of the file*)
(*rd=OpenRead[filename];
defs=Read[rd];
Close[filename];
defs=defs/.{Rule->Set,RuleDelayed->SetDelayed};
ReleaseHold[defs];*)

(<<SpinorHelicityCustomFunctions`)/.{Rule->Set,RuleDelayed->SetDelayed}//ReleaseHold;

];

];
