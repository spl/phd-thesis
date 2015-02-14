\chapter{STLC}

\section{Types}

For the convenience of our representation of the polymorphic lambda calculus, we
use variant of Narayanan's view\footnote{I don't like this word. Better term?}
on type variables\footnote{mention chapter, cite Ma and Reynolds}.

We use natural numbers as type variables.
%
We also regard a natural number as the set of its predecessors.
%
In other words, each |tvM, tvN `elem` NN| is a both finite set of natural
numbers, i.e.\ |{0, dotsc, tvN-1}|, as well as the first natural number not
found in that set.
%
To notationally distinguish members from sets, we use |tva| for a type variable
that is a member of |tvN| and |(sub(tva)(tvN))| for the first type variable not
found in |tvN|.
%
Note that the set |tvN `union` {(sub(tva)(tvN))}| is equivalent to |tvN+1|.

A type expression with |tvN| free type variables is an element of the set
|(sub(Syn.typ)(tvN))|, which is the least set satisfying the rules in
Figure~\ref{fig:types}. These rules describe the grammar of type variables,
function types, and polymorphic types. Note that, in |STPoly|, we require that
the type variable used in the |Delta| quantifier be the greatest free type
variable in the body.

TODO: Properly format rule labels.

TODO: Look up ATTAPL for more discussion to be had on PolyLambda.

TODO: Finish talking about why we use Narayanan's view.

\begin{figure}[t]
\[\boxed{|ty, tz `elem` (sub(Syn.typ)(tvN))|}\]
\begin{mathpar}
\inferrule*[right=|STVar|]
           {|tva `elem` tvN|}
           {|tva `elem` (sub(Syn.typ)(tvN))|}
\and
\inferrule*[right=|STFun|]
           {|ty `elem` (sub(Syn.typ)(tvN))| \and
            |tz `elem` (sub(Syn.typ)(tvN))|}
           {|ty -> tz `elem` (sub(Syn.typ)(tvN))|}
\and
\inferrule*[right=|STPoly|]
           {|ty `elem` (sub(Syn.typ)(tvN+1))|}
           {|Delta (sub(tva)(tvN)) . ty `elem` (sub(Syn.typ)(tvN))|}
\end{mathpar}
\label{fig:types}
\caption{Type expressions}
\end{figure}

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

|forall dx . dx -> tyf|

|"qz"|

\begin{spec}
(subst_one(dx)(T.str)(T.R T.A))
\end{spec}

x\(\mathrm{x}\)
