(* ::Package:: *)

BeginPackage["AntonAntonov`SalvoCombatModeling`"];

(*{\[Beta], \[CapitalPsi], B, \[Gamma], \[CapitalTheta], \[Zeta], \[Sigma], \[Tau], \[Rho], \[Delta], \[CapitalDelta]};*)
\[Beta]::usage = "Offensive potential.";
\[CapitalPsi]::usage = "Fraction of B units that engage A units.";
\[Gamma]::usage = "Defensive combat power.";
\[CapitalTheta]::usage = "Fraction of A units that engage B units.";
\[Zeta]::usage = "Staying power.";
\[Sigma]::usage = "Scouting effectiveness.";
\[Tau]::usage = "Training effectiveness.";
\[Rho]::usage = "Distraction factor.";
\[CurlyEpsilon]::usage = "Offensive effectiveness factor.";
\[Delta]::usage = "Defender alertness.";
\[CapitalDelta]::usage = "Number of unit out of action.";

SalvoForceNameQ::usage = "Check is the argument suitable to be salvo combat model force name.";
SalvoVariable::usage = "Gives rules for salvo combat modeling variables.";
SalvoVariableRules::usage = "Salvo variable rules.";

(*GenerateVariables::usage = "Generate variables for give fighting sides.";*)
SalvoTerms::usage = "Salvo terms.";
SalvoDamage::usage = "Salvo damage.";

HeterogeneousSalvoModel::usage = "Creates heterogeneous salvo combat model matrices.";

SalvoNotionDefinitions::usage = "Model notions definitions dataset.";

Begin["`Private`"];

(*=================================================================*)
(* Generic variable definitions                                    *)
(*=================================================================*)

Clear[SalvoForceNameQ];
SalvoForceNameQ[x_?AtomQ] := True;
SalvoForceNameQ[x_RGBColor] := True;
SalvoForceNameQ[___] := False;


Clear[SalvoVariable];

SalvoVariable[s_Symbol, B_, A_, j_, i_] /; SymbolName[s] == "\[Beta]" :=
    s[B, A, j, i] ->
        StringTemplate["\[Beta]: Offensive combat potential of `B`[`j`] units against `A`[`i`]. (hits /shooting unit)"][<|"A" -> A, "B" -> B, "j" -> j, "i" -> i|>];

SalvoVariable[s_, B_, A_, j_, i_] /; SymbolName[s] == "\[CapitalPsi]" :=
    s[B, A, j, i] ->
        StringTemplate["\[CapitalPsi]: Fraction of `B`[`j`] units that engage `A`[`i`] units. [0,1]"][<|"A" -> A, "B" -> B, "j" -> j, "i" -> i|>];

SalvoVariable[B_Symbol, j_] :=
    B[j] -> StringTemplate["Number of `B` units of type `j`. {`B`[`j`] units}"][<|"B" -> B, "j" -> j|>];

SalvoVariable[s_Symbol, A_, B_, i_, j_] /; SymbolName[s] == "\[Gamma]" :=
    s[A, B, i, j] ->
        StringTemplate["\[Gamma]: Defensive combat power of side `A`[`i`] against `B`[`j`] units, {shots /defending units}"][<|"A" -> A, "B" -> B, "j" -> j, "i" -> i|>];

SalvoVariable[s_Symbol, A_, B_, i_, j_] /; SymbolName[s] == "\[CapitalTheta]" :=
    s[A, B, i, j] ->
        StringTemplate["\[CapitalTheta]: Fraction of `A`[`i`] units that engage `B`[`j`] units. [0,1]"][<|"A" -> A, "B" -> B, "j" -> j, "i" -> i|>];

SalvoVariable[s_Symbol, A_, i_] /; SymbolName[s] == "\[Zeta]" :=
    s[A, i] ->
        StringTemplate["\[Zeta]: Staying power of `A`[`i`] unit, {hits}"][<|"A" -> A, "i" -> i|>];

SalvoVariable[s_Symbol, B_, A_, j_, i_] /; SymbolName[s] == "\[Sigma]" :=
    s[B, A, j, i] ->
        StringTemplate["\[Sigma]: Scouting effectiveness of unit `B`[`j`] against `A`[`i`]. [0,1]"][<|"A" -> A, "B" -> B, "j" -> j, "i" -> i|>];

SalvoVariable[s_Symbol, B_, A_, j_, i_] /; SymbolName[s] == "\[Tau]" :=
    s[B, A, j, i] ->
        StringTemplate["\[Tau]: Training effectiveness of unit `B`[`j`] against `A`[`i`]. [0,1]"][<|"A" -> A, "B" -> B, "j" -> j, "i" -> i|>];

SalvoVariable[s_Symbol, A_, B_, i_, j_] /; SymbolName[s] == "\[Rho]" :=
    s[A, B, i, j] ->
        StringTemplate["\[Rho]: Distraction factor of unit `A`[`i`] against `B`[`j`]. [0,1]"][<|"A" -> A, "B" -> B, "j" -> j, "i" -> i|>];

SalvoVariable[s_Symbol, B_, A_, j_, i_] /; SymbolName[s] == "\[CurlyEpsilon]" :=
    s[B, A, j, i] ->
        StringTemplate["\[CurlyEpsilon]: Offensive effectiveness of `B`[`j`] against `A`[`i`]. [0,1]"][<|"A" -> A, "B" -> B, "j" -> j, "i" -> i|>];

SalvoVariable[s_Symbol, A_, B_, i_, j_] /; SymbolName[s] == "\[Delta]" :=
    s[A, B, i, j] ->
        StringTemplate["\[Delta]: Defender alertness or readiness of unit `A`[`i`] against `B`[`j`]. {[0,1]}"][<|"A" -> A, "B" -> B, "j" -> j, "i" -> i|>];

SalvoVariable[s_Symbol, A_] /; SymbolName[s] == "\[CapitalDelta]" :=
    ToExpression[Context[s] <> "\[CapitalDelta]" <> SymbolName[A]] ->
        StringTemplate["The number of `A` units put out of action"][<|"A" -> A|>];

(*=================================================================*)
(* Generic variable definitions                                    *)
(*=================================================================*)

(*
Clear[GenerateVariables];
GenerateVariables[A_Symbol, B_Symbol, contextName_String : "`AntonAntonov`SalvoCombatModeling`"] :=
    With[{
      CBeta = ToExpression[contextName <> "\[Beta]"],
      CCapitalPsi = ToExpression[contextName <> "\[CapitalPsi]"],
      CGamma = ToExpression[contextName <> "\[Gamma]"],
      CCapitalTheta = ToExpression[contextName <> "\[CapitalTheta]"],
      CZeta = ToExpression[contextName <> "\[Zeta]"],
      CSigma = ToExpression[contextName <> "\[Sigma]"],
      CTau = ToExpression[contextName <> "\[Tau]"],
      CRho = ToExpression[contextName <> "\[Rho]"],
      CDelta = ToExpression[contextName <> "\[Delta]"],
      CCapitalDelta = ToExpression[contextName <> "\[CapitalDelta]"]
    },

      CBeta::usage = SalvoVariable[\[Beta], A, B, "i", "j"][[2]];
      CCapitalPsi::usage = SalvoVariable[\[CapitalPsi], A, B, "i", "j"][[2]];
      CGamma::usage = SalvoVariable[\[Gamma], A, B, "i", "j"][[2]];
      CCapitalTheta::usage = SalvoVariable[\[CapitalTheta], A, B, "i", "j"][[2]];
      CZeta::usage = SalvoVariable[\[Zeta], A, B, "i", "j"][[2]];
      CSigma::usage = SalvoVariable[\[Sigma], A, B, "i", "j"][[2]];
      CTau::usage = SalvoVariable[\[Tau], A, B, "i", "j"][[2]];
      CRho::usage = SalvoVariable[\[Rho], A, B, "i", "j"][[2]];
      CDelta::usage = SalvoVariable[\[Delta], A, B, "i", "j"][[2]];
      CCapitalDelta::usage = SalvoVariable[\[CapitalDelta], A, B, "i", "j"][[2]];
    ];
*)

(*=================================================================*)
(* Generic formulas                                                *)
(*=================================================================*)

Clear[MakeVarRules, MakeVarRulesRec];
MakeVarRulesRec[{A_, m_Integer}, {B_, n_Integer}] :=
    Block[{lsGenVars = {\[Beta], \[CapitalPsi], \[Gamma], \[CapitalTheta], \[Zeta], \[Sigma], \[Tau], \[Rho], \[CurlyEpsilon], \[Delta]}},
      Table[
        Switch[v,
          \[Zeta], SalvoVariable[v, A, i],
          _, SalvoVariable[v, A, B, i, j]
        ],
        {v, lsGenVars}, {i, m}, {j, n}]
    ];

SalvoVariableRules[{A_, m_Integer}, {B_, n_Integer}] :=
    Join[MakeVarRulesRec[{A, m}, {B, n}], MakeVarRulesRec[{B, n}, {A, m}]] // Flatten // Union;


Clear[SalvoTerms];

SyntaxInformation[SalvoTerms] = {"ArgumentsPattern" -> {{_Symbol, _Integer}, {{_Symbol, _Integer}}}};

SalvoTerms[{A_?SalvoForceNameQ, m_Integer}, {B_?SalvoForceNameQ, n_Integer}] :=
    Table[(\[Sigma][B, A, j, i] * \[Tau][B, A, j, i] * \[Rho][A, B, i, j] * \[Beta][ B, A, j, i] * \[CapitalPsi][B, A, j, i] * B[j] - \[Delta][A, B, i, j] * \[Tau][A, B, i, j] * \[Gamma][A, B, i, j] * \[CapitalTheta][A, B, i, j] * A[i]) * 1 / \[Zeta][A, i], {i, m}, {j, n}];

Clear[SalvoDamage];

SyntaxInformation[SalvoDamage] = {"ArgumentsPattern" -> {{_Symbol, _Integer}, {{_Symbol, _Integer}}}};

SalvoDamage[{A_?SalvoForceNameQ, m_Integer}, {B_?SalvoForceNameQ, n_Integer}] := Total[Flatten@ SalvoTerms[{A, m}, {B, n}]];

(*=================================================================*)
(* Matrix notation (definition)                                    *)
(*=================================================================*)

Clear[HeterogeneousSalvoModel];

SyntaxInformation[HeterogeneousSalvoModel] = {"ArgumentsPattern" -> {{_?SalvoForceNameQ, _Integer}, {{_?SalvoForceNameQ, _Integer}}, OptionsPattern[]}};

Options[HeterogeneousSalvoModel] = {"OffensiveEffectivenessTerms" -> False};

HeterogeneousSalvoModel[{A_?SalvoForceNameQ, m_Integer}, {B_?SalvoForceNameQ, n_Integer}, opts:OptionsPattern[]] :=
    Block[{epsQ, vecA, vecB, matOffenseA, matDefenseA, matOffenseB, matDefenseB, nameA, nameB, res},

      epsQ = TrueQ[OptionValue[HeterogeneousSalvoModel, "OffensiveEffectivenessTerms"]];

      vecA = Array[A, m];
      vecB = Array[B, n];

      matOffenseA =
          Table[\[Beta][B, A, j, i] * \[Rho][A, B, i, j] * \[Sigma][B, A, j, i] * \[Tau][B, A, j, i] * \[CapitalPsi][B, A, j, i] * 1 / \[Zeta][A, i], {i, m}, {j, n}];
      matDefenseA =
          DiagonalMatrix[
            Total /@ Table[\[Gamma][A, B, i, j] * \[Delta][A, B, i, j] * \[CapitalTheta][A, B, i, j] * \[Tau][A, B, i, j] * 1 / \[Zeta][A, i], {i, m}, {j, n}]
          ];

      matOffenseB =
          Table[\[Beta][A, B, j, i] * \[Rho][B, A, i, j] * \[Sigma][A, B, j, i] * \[Tau][A, B, 1 j, i] * \[CapitalPsi][A, B, j, i] * 1 / \[Zeta][B, i], {i, n}, {j, m}];
      matDefenseB =
          DiagonalMatrix[
            Total /@ Table[\[Gamma][B, A, i, j] * \[Delta][B, A, i, j] * \[CapitalTheta][B, A, i, j] * \[Tau][B, A, i, j] * 1 / \[Zeta][B, i], {i, n}, {j, m}]
          ];

      nameA = If[Head[A] === Symbol, SymbolName[A], A];
      nameB = If[Head[B] === Symbol, SymbolName[B], B];

      res = <|
        nameA -> <|
          "Units" -> vecA,
          "OffenseMatrix" -> matOffenseA,
          "DefenseMatrix" -> matDefenseA
        |>,
        nameB -> <|
          "Units" -> vecB,
          "OffenseMatrix" -> matOffenseB,
          "DefenseMatrix" -> matDefenseB
        |>
      |>;

      If[epsQ,
        (*
          Assuming this is enough. Otherwise:
          (1) Simplify with assumptions has to be used;
          (2) direct replacement in the formulas above has to be used.
        *)
        res //. {(\[Sigma][b_, a_, j_, i_] * \[Tau][b_, a_, j_, i_] * \[Rho][a_, b_, i_, j_] ):> \[CurlyEpsilon][b, a, j, i]},
        (*ELSE*)
        res
      ]
    ];

(*=================================================================*)
(* Notion definitions                                              *)
(*=================================================================*)
Clear[SalvoNotionDefinitions];

SyntaxInformation[SalvoNotionDefinitions] = {"ArgumentsPattern" -> {_.}};

SalvoNotionDefinitions::noargs = "The first argument is expected to be a string, one of \"Bulgarian\", \"English\", \"Russian\"";

SalvoNotionDefinitions[lang_String : "English"] :=
    Module[{pObj, fileName, dsDefinitions},
      If[!MemberQ[ToLowerCase /@ {"Bulgarian", "English", "Russian"}, ToLowerCase@lang],
        Message[SalvoNotionDefinitions::noargs];
        Return[$Failed];
      ];
      pObj = PacletObject["AntonAntonov/SalvoCombatModeling"];
      fileName = FileNameJoin[{pObj["Location"], "Resources", "Definitions" <> lang <> ".json"}];
      dsDefinitions = Import[fileName, "Dataset"];
      dsDefinitions
    ];

SalvoNotionDefinitions[___] := (
  Message[SalvoNotionDefinitions::noargs];
  $Failed
);

End[];
EndPackage[];
