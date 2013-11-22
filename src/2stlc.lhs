%include spl-thesis.fmt

\chapter{STLC}

\begin{figure}[t]
\begin{tabular}{l @@{|quad|} l @@{|synspace ::= synspace|} l}
Terms
&
|e,f|
&
|x || f e || \ x . e|
\\
Types
&
|ty,tz|
&
|bty || ty -> tz|
\\
Environments
&
|env|
&
|empty || env, x : ty|
\end{tabular}
\caption{Syntax of the simply typed lambda-calculus}
\label{fig:obj-lang-syn}
\end{figure}

\begin{figure}[t]
\[\boxed{|env +- e : ty|}\]
\begin{mathpar}
\inferrule*[right=|Con|]
           { }
           {|env +- b : bty|}
\and
\inferrule*[right=|Var|]
           {|ty `inst` (p_app(env)(x))|}
           {|env +- x : ty|}
\and
\inferrule*[right=|App|]
           {|env +- f : ty -> tz| \and
            |env +- e : ty |}
           {|env +- f e : tz|}
\and
\inferrule*[right=|Lam|]
           {|env, x : ty +- e : tz|}
           {|env +- \ x . e : ty -> tz|}
\end{mathpar}
\caption{Typing inference rules for the simply typed lambda-calculus}
\label{fig:typing}
\end{figure}

