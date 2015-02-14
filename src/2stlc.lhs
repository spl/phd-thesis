\chapter{STLC}

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
