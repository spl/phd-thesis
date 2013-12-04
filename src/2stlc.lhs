\chapter{STLC}

\begin{figure}[t]
\begin{tabular}{l @@{|quad|} l @@{|synspace ::= synspace|} l}
|expsyn|
&
|e,f|
&
|n || s || x || f e || \ x . e|
\\
|typsyn|
&
|ty,tz|
&
|numty || strty || ty -> tz|
\\
|envsyn|
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
\inferrule*[right=|Num|]
           { }
           {|env +- n : numty|}
\and
\inferrule*[right=|Str|]
           { }
           {|env +- s : strty|}
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
           {|env +- \ x : ty . e : ty -> tz|}
\end{mathpar}
\caption{Typing inference rules for the simply typed lambda-calculus}
\label{fig:typing}
\end{figure}

