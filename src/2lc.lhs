\chapter{Lambda Calculus}

\section{Types}

Type variables are elements of the countably infinite set |Syn.tvar|, ranged
over by |tvar_a|.

A type is an element of the set |Syn.typ|, inductively defined by the grammar:

\noindent
\begin{align*}
|typ_a, typ_b| &|synspace::=synspace| |tvar_a || typ_a -> typ_b || (poly(tvar_a)(typ_a))|
\end{align*}

\noindent
%
A type is either a type variable, a function or arrow type, or a polymorphic
type.
%
The free variables of a type |typ_a| are given by the function |fv typ_a|.

Type variables are given meaning by type substitution.
%
A type substitution is a partial function |tsub_a `elem` Syn.tvar -> Syn.typ|.
%
We write a singleton type substitution as |(singletsub(tvar_a)(typ_a))|.
%
The identity substitution, |id = (singletsub(tvar_a)(tvar_a))| for all |tvar_a
`elem` Syn.tvar|, injects type variables into types.
%
To apply a type substitution to a type, we use the (total) function |applytsub
`elem` Syn.typ -> (Syn.tvar -> Syn.typ) -> Syn.typ|, defined as:\footnote{Note
the precedence of these operators: |applytsub| |<| |composetsub| |<| |poly| |<|
|arrow|.}

\noindent
\begin{align*}
|(applytsub(tvar_a)(tsub_a))| &|=|
  \begin{cases}
  |tvar_a|        &\text{if } |tvar_a `notElem` dom tsub_a| \\
  |tsub_a tvar_a| &\text{if } |tvar_a `elem`    dom tsub_a|
  \end{cases}
\\
|(applytsub(typ_a -> typ_b)(tsub_a))| &|=|
  |((applytsub(typ_a)(tsub_a))) -> ((applytsub(typ_b)(tsub_a)))|
\\
|(applytsub(poly(tvar_a)(typ_a))(tsub_a))| &|=|
  |(poly(tvar_a')((applytsub(typ_a)(applytsub(funsub(tsub_a)(tvar_a)(tvar_a'))(singletsub(tvar_a)(tvar_a'))))))|
  \quad \text{if } |tvar_a' `notElem` fv typ_a `union` fv (ran tsub_a)|
\end{align*}

\noindent
%
The last case uses type substitution composition, defined as:

\noindent
\begin{align*}
|(applytsub(tsub_a_1)(tsub_a_2))| &|=| |\ tvar_a `elem` dom tsub_a_1 . ((applytsub(tsub_a_1 tvar_a)(tsub_a_2)))|
\end{align*}

\noindent
%
Substitution application (|applytsub|) could be considered as a many-valued
function because it maps a lhs to a rhs with any number of variables |tvar_a'|.
%
However, we assume that we can always choose a fresh variable for |tvar_a'| to
regain the operation's property of being a single-valued function.

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
