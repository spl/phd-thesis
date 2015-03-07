\chapter{Lambda Calculus}

\section{Types}

Type variables are elements of the countably infinite set |Syn.tvar|, ranged
over by |Meta.tvara|.

A type is an element of the set |Syn.typ|, inductively defined by the grammar:

\noindent
\begin{align*}
|Meta.typa, Meta.typb| &|synspace::=synspace| |Meta.tvara || Meta.typa -> Meta.typb || (poly(Meta.tvara)(Meta.typa))| 
\end{align*}

\noindent
%
A type is either a type variable, a function or arrow type, or a polymorphic
type.

Type variables are given meaning by type substitution.
%
A type substitution is a function |Meta.tsuba `elem` Syn.tvar -> Syn.typ|.
%
We write a singleton type substitution as
|(singletsub(Meta.tvara)(Meta.typa))|.
%
The identity substitution, |id = (singletsub(Meta.tvara)(Meta.tvara))| for all
|Meta.tvara `elem` Syn.tvar|, injects type variables into types.
%
To apply a type substitution to a type, we use the function |applytsub `elem`
Syn.typ -> (Syn.tvar -> Syn.typ) -> Syn.typ|, defined as:

\noindent
\begin{align*}
|(applytsub(Meta.tvara)(Meta.tsuba))| &|=|
  \begin{cases}
  |Meta.tvara|            &\text{if } |Meta.tvara `notElem` dom Meta.tsuba| \\
  |Meta.tsuba Meta.tvara| &\text{if } |Meta.tvara `elem`    dom Meta.tsuba|
  \end{cases} \\
|(applytsub(Meta.typa -> Meta.typb)(Meta.tsuba))| &|=|
|((applytsub(Meta.typa)(Meta.tsuba))) -> ((applytsub(Meta.typb)(Meta.tsuba)))| \\
|(applytsub(poly(Meta.tvara)(Meta.typa))(Meta.tsuba))| &|=|
|(poly(Meta.tvara)((applytsub(Meta.typa)(composetsub(singletsub(Meta.tvara)(Meta.tvara'))(composetsub(Meta.tsuba)(singletsub(Meta.tvara)(Meta.tvara')))))))|
\end{align*}

\noindent
%
TODO: fix prime of |Meta.tvara'|
%
(footnote: Note that |applytsub| has a lower precedence than |poly|, which
itself has a lower precedence than |arrow|.)
%
Type substitutions are sequenced by composition:

\noindent
\begin{align*}
|(composetsub(Meta.tsuba_2)(Meta.tsuba_1))| &|=| |\ Meta.tvara `elem` dom Meta.tsuba_1 . ((applytsub(Meta.tsuba_1 Meta.tvara)(Meta.tsuba_2)))| \\
\end{align*}

\subsection{Type Environment}

A type environment (or type assignment) is a function from

\begin{figure}[h]
\begin{tabular}{l @@{|quad|} l @@{|synspace ::= synspace|} l}
|Syn.exp|
&
|e,f|
&
|n || s || x || f e || \ x :: ty . e|
\\
|Syn.typ|
&
|ty,tz|
&
|T.num || T.str || ty -> tz|
\\
|Syn.env|
&
|env|
&
|empty || env, x : ty|
\end{tabular}
\caption{Syntax of the simply typed lambda-calculus}
\label{fig:obj-lang-syn}
\end{figure}

\begin{figure}[h]
\[\boxed{|env +- e : ty|}\]
\begin{mathpar}
\inferrule*[right=|R.num|]
           { }
           {|env +- n : T.num|}
\and
\inferrule*[right=|R.str|]
           { }
           {|env +- s : T.str|}
\and
\inferrule*[right=|R.var|]
           {|ty `inst` (p_app(env)(x))|}
           {|env +- x : ty|}
\and
\inferrule*[right=|R.app|]
           {|env +- f : ty -> tz| \and
            |env +- e : ty |}
           {|env +- f e : tz|}
\and
\inferrule*[right=|R.lam|]
           {|env, x : ty +- e : tz|}
           {|env +- \ x :: ty . e : ty -> tz|}
\end{mathpar}
\caption{Typing inference rules for the simply typed lambda-calculus}
\label{fig:typing}
\end{figure}

\begin{figure}[h]
\begin{tabular}{l @@{|quad|} l @@{|synspace ::= synspace|} l}
|Syn.tyf|
&
|tyf,tzf|
&
|T.num || T.str || tyf -> tzf || dx|
\\
|Syn.envf|
&
|envf|
&
|empty || envf, x : tyf|
\end{tabular}
\caption{Syntax of type functors}
\label{fig:obj-lang-syn}
\end{figure}

|forall ^^ dx . dx|

|exists ^^ dx . dx|

|Lambda ^^ dx . dx|

|"qz"|

\begin{spec}
(subst_one(dx)(T.str)(T.R T.A))
\end{spec}

x\(\mathrm{x}\)
