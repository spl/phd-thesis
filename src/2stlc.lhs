\chapter{STLC}

\section{Types}

\subsection{Type Variables}

For the convenience of our representation of the polymorphic lambda calculus, we
use a variant of Narayanan's view\footnote{I don't like this word. Better term?}
on type variables\footnote{mention chapter, cite Ma and Reynolds}.

We use natural numbers (i.e.\ elements of the set |NN|) as type variables.
%
We also regard a natural number as the set of its predecessors.
%
In other words, each |tvM, tvN `elem` NN| is a both a finite set of natural
numbers, i.e.\ |{0, dotsc, tvN-1}|, as well as the first natural number not
found in that set.
%
To distinguish members from sets by notation, we use |tva| for a type variable
that is a member of |tvN| and |(sub(tva)(tvN))| for the first type variable not
found in |tvN|.
%
Note that the set |tvN `union` {(sub(tva)(tvN))}| is equivalent to |tvN+1|.

\subsection{Type Expressions}

A type expression with |tvN| free type variables is an element of the set
|(sub(Syn.typ)(tvN))|, which is the least set satisfying the following rules:

% Set the vertical spacing between lines in multi-line formulas (e.g. for
% gather).
% TODO: put this somewhere better
\setlength{\jot}{6pt}

\noindent
\begin{gather}
\inferrule
  {|tva `elem` tvN|}
  {|tva `elem` (sub(Syn.typ)(tvN))|}
\tag{|STVar|}\label{rule:STVar} \\
\inferrule
  {|ty `elem` (sub(Syn.typ)(tvN))| \and
   |tz `elem` (sub(Syn.typ)(tvN))|}
  {|ty -> tz `elem` (sub(Syn.typ)(tvN))|}
\tag{|STFun|}\label{rule:STFun} \\
\inferrule
  {|ty `elem` (sub(Syn.typ)(tvN+1))|}
  {|Delta ^^ (sub(tva)(tvN)) . ty `elem` (sub(Syn.typ)(tvN))|}
\tag{|STPoly|}\label{rule:STPoly}
\end{gather}

\noindent
These rules describe the grammar of type variables, function types, and
polymorphic types.
%
Note that, in \eqref{rule:STPoly}, we require that the type variable used in the |Delta|
quantifier be the greatest free type variable in the body.

The outermost bound type variable of any type expression |ty `elem`
(sub(Syn.typ)(tvN))| is |(sub(tva)(tvN))|.
%
This means that type expressions are unique ... TODO

TODO: Properly format rule labels.

TODO: Look up ATTAPL for more discussion to be had on PolyLambda.

TODO: Finish talking about why we use Narayanan's view.

\subsection{Type Substitution}

\noindent
\begin{align*}
|tva / sb| &|=| |sb^^tva| \\
|(ty -> tz) / sb| &|=| |(ty / sb) -> (tz / sb)| \\
|(Delta ^^ (sub(tva)(tvM)) . ty) / sb| &|=| |Delta ^^ (sub(tva)(tvN)) . (ty / [sb || m : n])|
\end{align*}

TODO: Figure out proper precedence of |/|.

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
