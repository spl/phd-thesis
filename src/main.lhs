\documentclass[paper=A4]{scrbook}

% --- FORMATS AND STYLE ---

%include spl-thesis.fmt
\usepackage{spl-thesis}

% --- BEGIN ---

\title{Type-and-Transform Systems}
\author{Sean Leather}

\begin{document}

\frontmatter

\tableofcontents

\mainmatter

%include 1intro.lhs
%include 2stlc.lhs
%include 2lc.lhs
%include 3fix.lhs
%include 4pattern.lhs
%include 5systemf.lhs
%include 6poly.lhs
%include 7types.lhs
%include 8conclusion.lhs

\backmatter

\bibliographystyle{plainnat}
\bibliography{bibliography}

\end{document}

% --- END ---
