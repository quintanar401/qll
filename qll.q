.ll.aA:.Q.a,.Q.A;
.ll.depth:1; / max recursion depth
.ll.static:1b; / dfa generation or live parsing
.ll.lex:0b; / lex mode
.ll.autoraze:1b; / autoflat X* and X+
.ll.lr:1b; / autofix left recursion
.ll.statelim:50; / limit max static state num
.ll.savePos:1b; / save in P the current position: before the first expr if it is a terminal, before the second expr if it exists  - usefull for error reporting in cases "`term expr" and "expr `term expr"
.ll.e:{-1 "WAR: ",x;x}; / error
.ll.init:{
  .ll.n:`; / name
  .ll.T:(); / TOKENS
  .ll.RP:.ll.R:(0#`)!(); / rules & rule predicates
  .ll.t:((),`:EOF;(),`:EOR); / terminals
  .ll.lkey:`$(); / lexer key rules ()
  .ll.A:{:enlist[v1],v2},{:enlist[(v1;v2)],v3},{;0b},{1b},{0b}; / actions
 };
.ll.init[];

/ value+enum for the grammar lang. "s" rule, "i" - term, "h" - action, "J" - simple action; later "j" - rule, "C" - term, "h" action
.ll.lval:{$[not`LEXER in .ll.T;x;any i:x~/:.ll.L 4;enlist .ll.L[3] first where i;x]};
.ll.val:{$[((x0:x 0)in"!?+*():|;-^")&1=count x;x0;"`"=x0;.ll.lval 1_x;"\""=x0;.ll.lval(),value x;x0 in .ll.aA;$[(x:.ll.mn`$x)in .ll.T;enlist x;x];x0 in .Q.n;$[-7=type v:value x;"J"$(),/:x;type[v]in -6 -5h;v;'x];'x]};
/ extract functions
.ll.fext:{if[count w:where differ[0;]1&sums(not{$[x=2;1;x;$["\""=y;0;"\\"=y;2;1];1*"\""=y]}\[0;x])*(neg"}"=x)+"{"=x;a:.ll.addA each value each x b:a+til each d:1+w[;1]-a:(w:(0N 2)#w)[;0];x:@[;;:;]/[x;b;d$(string a),\:"h"]];x};
.ll.en:{.ll.t:distinct .ll.t,x i:where(type each x)in 10 11h; @[x;i;{"i"$first where x~/:.ll.t}]}; / enumerate terminals
.ll.mn:{$[(x in`TOKENS`GNAME`PARSER`LEXER`Q`KEYRULES`LEXER)|(.ll.n=`)|"." in n:string x;x;`$string[.ll.n],".",n]}; / add a prefix to a rule name
.ll.addA:{if[any i:x~/:.ll.A;:first where i]; .ll.A,:x;-1+count .ll.A};
.ll.Anot:{$[x=2;2;x=3;4;x=4;3;.ll.addA (')[not;.ll.A x]]};
.ll.Aor:{if[any x in 2 3;:max 3&x]; if[all 4=x;:4]; $[1=count x:distinct x except 4;x 0;.ll.addA{any x@\:y}.ll.A x]};
.ll.Aand:{$[all x in 2 3;max x;any 4=x;4;1=count x:distinct x except 2 3;x 0;.ll.addA{all x@\:y}[.ll.A x]]};

/ top level rule
.ll.rl:{.[{y:{$["("~f:first z;enlist[1_z],x;")"~f;[.ll.R[y]:.ll.ro[y;x 0];enlist[x[1],y,1_z],2_x];enlist z]}/[();`$(string[x],"-"),/::string[til count c];c:(0,where("("~/:y)|")"~/:y)cut y];
  .ll.R[x]:.ll.ro[x;y 0]};(x;y);{'"rule ",string[x],": ",y}x]};
.ll.ro:{$[any p:"|"~/:y;y:.ll.ra[x]each{enlist[$[0=count x 0;();x 0]],1_'1_x}(0,where p)cut y;enlist .ll.ra[x;y]]}; / | clause
.ll.ra:{ .ll.RP[x],:$[p:(-5=type y 0)&any(y 1)~/:"!?";y[0]*-1 1 "?"=y 1;2]; if[p;y:2_y]; if[{c[0]|(7=x 0)|any(c&p|prev c:x=-10)|(p:prev f)&f:x in -5 7h}type each y;'"bad specsym/predicate/action apply"]; / & clause, extract predicates
  x:`$string[x],"[o]",string count .ll.RP[x]; y:{if[null i:first where "-"~/:y;:y]; if[not 6=type v:y 1 -1+i;'"-: wrong format"]; if[not all(10=type each v)&1=count each v:.ll.t v; '"-: wrong format"]; x:`$string[x],"[-]",string count y;
  .ll.rl[x] .ll.en 1_raze{("|";(),x)}each"c"$v[1]+til 1+(-). v:"j"$v[;0];#[i-1;y],x,(i+2)_y}[x]/[y];  {@[y;-1+w;.ll.rs x;flip(y w;til count w:where z)]where not z}[x;y;-10=type each y]};
.ll.rs:{if[(-11=type y)&"?"=z 0; if["-"in string y;.ll.R[y],:enlist`;.ll.RP[y],:2;:y]]; x:`$string[x],"[",z[0],"]",string z 1; if["^"=z:z 0; if[not 6=type y:$[-6=type y;(),y;raze .ll.R y];'"^: non term"]; .ll.R[x]:enlist(^;0i),y; .ll.RP[x]:1#2;:x];
  a:(();0h).ll.autoraze; p:$[-11=type y;$[(2<>p 0)&1=count p:.ll.RP y;("h"$p 0;"?");()];()]; .ll.rl[x;$["*"=z;p,(y;x),a,("|";`);z="+";(y;"("),p,(x;"|";`;")"),a;z="?";p,(y;"|";`);'"unexpected: ",z]];x}; / spec char
.ll.chr:{if[count i:where not(t:t where -11=type each t:raze raze value .ll.R)in `,key .ll.R;'"undefined rules: ",","sv string t distinct i]; .ll.R[`$":EOR"]:enlist 1 1i;
  {if[(^)~(r:.ll.R x)[0;0];.ll.R[x]:r:enlist each"i"$0,til[count .ll.t] except 1_r 0; .ll.RP[x]:count[r]#.ll.RP x]}each key .ll.R}; / undefined rules
.ll.rflt:{{x where(type each x)in -6 -11h}each x};
.ll.rpAdj:{{if[not(2<p:first .ll.RP x)&(enlist[`]~last r)&2=count r:.ll.R0 x;:()]; .ll.RP[x]:p,.ll.Anot p}each key .ll.R}; / preds for xxx*|+|? for empty alt
.ll.lrElim:{if[not(s<count[r]-sum enlist[`]~/:r)&s:sum i:(x~/:last each r)|x~/:first each r:.ll.rflt rl:.ll.R x;:()];
  n:{`$x[0],"+",string 1+0^"J"$x 1}"+"vs string x; isL:x~first r f:last i:where i; isRA:{assoc:right}~{$[-5=type x;.ll.A x;::]}first rl f;
  rl:@[rl;$[isL;::;-1_]i;{@[z;where x~/:z;:;y]}[x;n]]; .ll.R[n]:rl _ f; .ll.RP[x,n]:(enlist p:p f;(p:.ll.RP x)_f); if[not isL; .ll.R[x]:(isRA _ rl f;enlist n); .ll.RP[x],:.ll.Anot p; :.z.s n]; / prefix rule
  .ll.R[x,xn]:{v:(f:1+first i:where(type each z)in -6 -11h)#z;(enlist v,x,l _ z;((f _(l:1+last i)#z),$[y;();(x;"h"$2<count i)];enlist`))}[xn:`$string[x],"*";isRA]$[isRA;{1_@[z;1_where y~/:z;:;x]}[x;n];::] rl f; .ll.RP[xn]:p,2;.z.s n}; / infix & postfix

.ll.raiseRP:{if[x in .ll.RPpr; :.ll.RP x]; .ll.RPpr,:x; :.ll.RP[x]:{$[x<0;x;`~f:first y;x;-11=type f;.ll.Aand x,.ll.Aor abs .ll.raiseRP f;x]}'[.ll.RP x;.ll.R0 x]}; / raise predicates
/ state consts, State: (i;state;stack), where i is one of | alternatives, state: rulename|i|j
.ll.st:{
  .ll.R0:.ll.rflt each .ll.R;
  / .ll.RPpr:`$(); .ll.raiseRP each key .ll.RP;
  .ll.rpAdj[]; / deal with the predicates
  .ll.st2p:(`$x)!.[;(::;1 2);"J"$].[;(::;0);`$]"|"vs/:x:raze(string[key .ll.R0],\:"|"),/:'{raze(string[til count x],\:"|"),/:'string(til count@)each x}each value .ll.R0; / state 2 path
  .ll.r2s:group(where 0=.ll.st2p[;2])#.ll.st2p[;0]; / rule to init states
  .ll.st2n:k!next?[not differ value .ll.st2p[;0 1];k:key .ll.st2p;`]; / next state
  .ll.stl:where null .ll.st2n; / last states - for tail recursion
  .ll.st2v:.ll.R0 . .ll.st2p@; / state value
 };
.ll.mrg:{y[;1]:(y[;1]except\:.ll.stl),\:$[count[x 1]&x[0]in .ll.stl;();x 0],x 1; y}; / merge stacks
/ next states
.ll.nstup:{$[count x 1;.ll.nst(x[1;0];1_x 1);.ll.C2 n:.ll.st2p[x 0;0]]}; / drop is to avoid immediate rec in xxx* and etc
.ll.nst0:{$[-11=t:type v:.ll.st2v x 0;$[`=v;.ll.nstup x;$[` in key v;.ll.uns .ll.nst x;::].ll.mrg[x]each`_v:.ll.C1 v];t=-6;((),x 0)!enlist enlist x;.ll.nst x]};
.ll.nst:{$[`=n:.ll.st2n x 0;.ll.nstup x;.ll.nst0(n;x 1)]};
.ll.nob:{x where not `=x[;0]}; / no ` state
.ll.stkeq:{$[0=m:(count y:y 1)&count x:x 1;1b;m=count x;x~m#y;y~m#x]} / stack equiv
.ll.uset:{{{$[any y .ll.stkeq/:x;x;x,enlist y]}/[();x]}each x group x[;0]}; / unique state set (st;stk) -> st![(st;stk)]
.ll.unc:{x,y where not{any x .ll.stkeq\:y}[x]each y}; / union cfgs
.ll.uns:{$[0=count x;y;0=count y;x;x,y,k!.ll.unc'[x k;y k:distinct key[x] inter key y]]}; / union set
/ DFA state equiv : (`st1!..;...) - for each alt I a map of states into stacks
.ll.stsyg:{`$" "sv string[til count x],'":",/:{$[0=count x;"";" "sv string[k i],'"-",/:string[count each value x]i:iasc k:key[.ll.st2p]?key x]}each x};
.ll.steq:{{$[x;{$[x;count[y]=count .ll.unc[y;z];0b]}/[1b;value y;value z];0b]}/[1b;x w;y w:where 0<count each y]};

/ preclosure maps. C1 - walk down a nonterm, C2 - all states after a nonterm. C3 - all non rec states
.ll.pcls:{.ll.C1:(0#`)!(); .ll.pcls1 each key .ll.R0; .ll.pcls2[]; .ll.C1:.ll.uset each .ll.C1; .ll.C2[`$":EOR"]:()!()};
.ll.pcls1:{if[1<sum x=s:last x;'"left recursion detected: ","->"sv string x]; if[s in key .ll.C1;:.ll.C1 s];
  :.ll.C1[s]:distinct raze{$[-11=t:type v:.ll.st2v z;$[`=v;enlist(`;());any`=(v:{y[;1]:(y[;1]except\:.ll.stl),\:x;y}[z].ll.pcls1 x,v)[;0];(.ll.nob v),.z.s[x;y].ll.st2n z;v];-6=t;enlist(z;());'"unexp"]}[x;s]each .ll.r2s s};
.ll.pcls2:{.ll.C2:.ll.C3:(k:key .ll.R)!(); .ll.pcls3 each k; .ll.C2:{{distinct y,raze x y}[x]each x}/[.ll.C2]; .ll.C2:k!{.ll.uset .ll.C3[x],raze .ll.C3 .ll.C2 x}each k};
.ll.pcls3:{{$[null n:.ll.st2n y;.ll.C2[x],:.ll.st2p[y]0;-11=t:type v:.ll.st2v n;$[`=v;.z.s[x;n];[if[` in(v:.ll.C1 v)[;0];.z.s[x;n]];.ll.C3[x],:.ll.nob v]];-6=t;.ll.C3[x],:enlist(n;());'"unexp"]}[x]each w:where x~/:(.ll.R0 .)each .ll.st2p;
  if[0=count w except x;.ll.C3[x],:enlist(r:`$":EOR|0|0";())]};

/ DFA state: (list of all possible states for i alt;state idx;unresolved states on dfa err;rule name;trans)
/ transitions: Ni -> J, where J - -1=error,  -1> another state, 0<= - production x, 0N unknown, -0W - dfa failed
.ll.dfac:{
  .ll.dfa:([] states:(::); idx:1#0; unres:(::); rname:1#x; trans:(::); syg:`);
  .ll.dfaa @[;where 0>.ll.RP x;{()}]{.ll.nst0(x;())}each .ll.r2s x; / add the first state
  {if[not[.ll.lex]&.ll.statelim<x;.ll.e"state limit: ",string .ll.dfa[0;`rname];:x];{.ll.dfaNxt[x]each where null .ll.dfa[x;`trans]}each x+til(c:count .ll.dfa)-x;c}/[1];
  if[f:not` in .ll.dfa`rname; .ll.dfa[`states]:count[.ll.dfa]#(::)]; / clear closed dfas
  if[not[.ll.lex]&f&count s:(til c:count .ll.R x)except(where 0>.ll.RP x),s where -1<s:distinct raze 1_.ll.dfa`trans; .ll.e $[c=count s;"rule will always fail: ";"unreachable alt ",(" "sv string s)," in rule: "],string x];
 };
.ll.dfaa:{ `.ll.dfa upsert enlist (x;c:count .ll.dfa;(0#0i)!();``ok .ll.static;@[count[.ll.t]#-1;raze{.ll.st2v each(v:raze value x)[;0]}each x;:;0N];.ll.stsyg x); c}; / add a state
.ll.dfau:{.[`.ll.dfa;(x;`trans`unres `u=y);{x[y 0]:y 1;x};z]; z 1}; / update a transition
.ll.dfam:{.[`.ll.dfa;(x;`rname);:;`]}; / mark incomplete
.ll.dfacs:{{raze each x[;0 1] group x[;2]}raze{if[(0=count s1:x y)|0=count s2:x z;:()]; (y;z),/:k where {(count[x]+count y)<>count .ll.unc[x;y]}'[s1 k;s2 k:key[s1]inter key s2]}[x]./:{x where(<)./:x:cross[x;x]}til count x}; / conflict set
/ 1 step
.ll.dfaNxt:{[s;k] .ll.dfan1[s;k]{.ll.uns/[();.ll.nst each v:v where x=k:.ll.st2v each(v:raze value y)[;0]]}[k]each .ll.dfa[s;`states]}; / new state
.ll.dfaerr:{if[x&.ll.lex|not ` in .ll.dfa`rname;.ll.e z]; :.ll.dfam y};
.ll.dfan1:{[s;k;st]
  if[not[.ll.lex]&1=sum i:0<c:count each st; if[k=1;.ll.dfa[s;`trans]:@[t;where -1=t:.ll.dfa[s;`trans];:;first where i]]; :.ll.dfau[s;`t;(k;first where i)]]; / final state, 1 - rule end token
  if[0=max c; :.ll.dfau[s;`t;(k;-1)]]; / error
  if[not null j:{$[null y;(y;z).ll.steq[x;.ll.dfa[z;`states]];y]}[st]/[0N;where .ll.stsyg[st]=.ll.dfa`syg]; :.ll.dfau[s;`t;(k;neg 1+j)]]; / duplicate
  if[.ll.static;
    if[1<sum j:1<count each v:{raze{raze{$[1=m:max count each $[count x;group x;1];();enlist (x;m)]}each x[;1]}each value x}each st;  / rec depth + representatives
      .ll.dfaerr[1;s;"at least two recursive paths on ",.Q.s1[.ll.t k]," in ",string[.ll.dfa[0;`rname]]," state ",string[s]," : 1) ",("<-"sv string v[0;0;0]),"; 2) ","<-"sv string(v:v where j)[1;0;0]]; .ll.est:st; :k];
    if[.ll.depth<max 0,(v:raze v)[;1]; .ll.dfaerr[1;s;"recursion overflow on ",.Q.s1[.ll.t k]," in rule ",string[.ll.dfa[0;`rname]]," state ",string[s]," : ","<-"sv string first v[;0]where .ll.depth<v[;1]]; .ll.est:st; :k]; / overflow, stop on this state
  ];
  if[any 0<count each value v:.ll.dfacs st; / conflict set
    if[.ll.static;
      if[.ll.lex&1=count v; if[not null fv:first where {@[x;where not -11=type each x;:;`]in .ll.lkey,.ll.mn[`KEYWORDS]}(.ll.R0 .ll.dfa[0;`rname])[;0]vv:value[v]0; if[any like[;"*:start|0|1"]key st vv fv; n:.ll.dfaa st; .ll.lx,:enlist(n;vv fv); :.ll.dfau[s;`t;(k;neg 1+n)]]]]; / prefer KEYWORDS rule
      .ll.dfaerr[all 2=.ll.RP n;s;"grammar ambiguity in rule ",string[n:.ll.dfa[0;`rname]]," state ",string[s],", on ",.Q.s1[.ll.t k],", conflicting set: ","; "sv string[key v],'": ",/:","sv/:string value v]; .ll.est:st; :k]; / in stat mode ignore this state
    if[0=count (til count st)except (where not i),raze value v; .ll.dfau[s;`u;(k;where i)]; :.ll.dfau[s;`t;(k;-0W)]]; / dfa failed - only rec states
  ];
  n:.ll.dfaa st;
  if[.ll.lex; if[count v:raze{$[any key[x y]like"*:start|0|1";y;()]}[st]each where i; .ll.lx,:enlist(n;first v)]];
  :.ll.dfau[s;`t;(k;neg 1+n)]; / move to the next state
 };
.ll.dfaas:{[n;s;t] / add state
  .ll.dfa:.ll.P[1;n]; .ll.dfa[`trans]:.ll.P[4;n]; .ll.lex:.ll.static:0b; / setup env
  .ll.dfaNxt[s;t]; / next state
  .ll.P[1;n]:.ll.dfa; .ll.P[4;n]:.ll.dfa`trans; / update parser
 };
.ll.gen:{[s]
  .ll.static:1b; .ll.lex:0b; if[not s in key .ll.R; '"bad rule: ",.Q.s1 s]; .ll.R[s:`$":",string[.ll.n],":start"]:enlist(s;0i;0i); / ensure the initial rule is not referenced in other rules + add EOF
  .ll.chr[]; if[.ll.lr; .ll.lrElim each key .ll.R]; .ll.st[]; .ll.pcls[]; / precalculate
  r:{{{$[-11=t:type y;x y;y]}[x]each y}[x]each y}[k!til count k]each .ll.R k:distinct s,key .ll.R; / rules
  d:{if[1>=count .ll.R x;:()]; .ll.dfac x; .ll.dfa} each k; / dfas
  .ll.t:{$[11=type x;x 0;x]}each .ll.t;
  .ll.P:(r;d;k;{$[all x=2;();x]}each value k#.ll.RP);
  `.ll.F`.ll.F1 set' flip .ll.fgen[];
  .ll.tm:({$[10=type x;`$x;x]}each .ll.t)!til count .ll.t;
  .ll.P,:{$[0=count a:.ll.P[1;x];();.ll.fasubst x]}each til count k;
 };
.ll.lgen:{[s]
  if[not 10=type t:raze 2_pt:.ll.t;'"lexer can use only string/sym terminals: "," "sv .Q.s1 each pt 2_where not 10=type each pt]; .ll.t:distinct t;
  .ll.R:{{raze{$[(not 0i~y)&-6=type y;"i"$2+.ll.t?x y;y]}[x]each y}[x]each y}[pt]each .ll.R; .ll.t:(2#pt),.ll.t; / reenumerate to chars
  .ll.static:.ll.lex:1b; .ll.lx:(); if[not s in key .ll.R; '"bad rule: ",.Q.s1 s]; .ll.R[r0:`$":",string[.ll.n],":start"]:enlist(s;0i); if[count k:.ll.lkey; .ll.R[s],:(),/:k; .ll.RP[s],:(count k)#2];
  .ll.chr[]; .ll.st[]; .ll.pcls[]; .ll.C2[r0]:()!();
  .ll.dfac s; if[not(::)~.ll.dfa[1;`states];'"lexer can't be created for this grammar"];
  is: distinct 0,first st:0|neg 2+1_.ll.dfa`trans; tn:(fs:-1+.ll.lx[;0])!@[tn;where (.ll.mn[`KEYWORDS]~/:tn)|not -11=type each tn:first each .ll.R0 s;:;`] .ll.lx[;1]; / initial/final states, token names
  st:(1#st),(sm:k!@[mx+k:til count st;0;:;0])(1_(mx:1+max is)#st),st; st[mx]:st 0; / ensure there are clear defined initial states
  tm:@[256#0;2_.ll.t;:;2_v:{distinct[x]?x:flip x}st]; st:st[;value first each group v];
  .ll.L:({w:where 0=y;@[y;w;:;x w]}[@[st 0;0;:;mx]]each st;tm;mx+1;{@[x#`ERROR;(key y),0;:;(value y),`]}[count st]#[is inter fs;tn],(mx+fs)!value tn);  / matrix, char remap, max start, token map
  .ll.L,:enlist{$[y in``ERROR;();1<>count t:.ll.R y;();6=type t 0;.ll.t t 0;()]}[.ll.R]each .ll.L 3; / tokens with only one string repr
  n:.ll.n; .ll.init[]; .ll.n:n; / reset state
 };
.ll.lexmap:{x}; / calc all pos: sums[s]j;(c-|\[(c:til count t)*s:prev"\n"=x])j
.ll.lexer0:{if[0=count x;:(();`$();0#0)]; j:where(t:.ll.L[0]\[0;.ll.L[1]$[count .ll.U;last[.ll.U]^.ll.U .ll.u8 x;x]])<.ll.L 2; v:(j cut x;.ll.L[3]t@[next j-1;-1+count j;:;-1+count t];j);
  if[any e:`ERROR=v 1; '"Unrecognized sequence ",(v[0]e)," at ",string[p 0],":",string[last p:.ll.getPos[v,enlist x;e:first where e]]]; v}; / lexer0 can be reused
.ll.lexer:{v:.ll.lexer0 x; .ll.lexmap (v@\:where not v[1]in .ll.mn each`COMMENT`WS),enlist x};
.ll.getPos:{[t;i]dx:-1^w last l:where(i:count[t 3]^t[2;i])>w:where ("\r\n" "\n"in t 3)=t 3;(1+count l;"j"$sum{(x>191)|x<128}t[3]dx+til i-dx)}; / utf8 friendly
.ll.lsave:{v:{".ll.L:enlist (",x,");"}";\n  "sv " "sv/:string .ll.L 0; v,:"\n.ll.L,:"," "sv string .ll.L 1;v,:";\n.ll.L,:",string[.ll.L 2],";";v,:"\n.ll.L,:",raze[.Q.s1 each .ll.L 3],";";
  v,:{"\n.ll.L,:enlist(",x,");"}";"sv {$[0=c:count x;"()";1=c;"enlist \"",x,"\"";"\"",x,"\""]} each .ll.L 4; x 1: v};

/ pretty print
.ll.pp:{("NAME: ",string `none^.ll.n;"TOKENS: ",","sv string .ll.T;"RULES:"),raze .ll.ppr[v]each til count first v:.ll.P};
.ll.ppr:{v:("  rule[",string[y],"] ",string x[2;y];"    productions:"),"      ",/:{" "sv{$[-6=t:type y;.Q.s1 .ll.t y;-7=t;string `skip^x y;-5=t;string .ll.A y;.Q.s1 y]}[x]each y}[x 2]each x[0;y]; v,$[count x[1;y];"  ",/:.ll.pps x[1;y];()]};
.ll.pps:{enlist["  states(",$[` in x`rname;"open";"closed"],"):"],"    ",/:raze{enlist["state ",string[x`idx],":"],"  ",/:{$[10=type x;.Q.s1 x;string first x]," -> ",
  $[0<=y;"production ",string y;y=-0W;"<unresolved>";"state ",string neg 1+y]}'[.ll.t w;d w:where not -1=d:x`trans]}each 1_ x};
.ll.ppt:{$[-11=type x;$[`LEXER in .ll.T;$[count v:.ll.L[4].ll.L[3]?x;v;string x];string x];10=type x;x;.Q.s1 x]};
.ll.pppath:{{" "sv .ll.ppt each x} each .ll.t neg {(-1_(0N 2)#x)[;1]}each{x where 1=first each x}{raze{$[count s:s i:where any each w:(neg 1+first y)=x s:til[count x]except 0,y;
  (s,'neg first each where each w i),\:y;enlist y]}[x]each y}[.ll.P[4].ll.P[2]?x]/[(),y]};

.ll.posErrIn:{"[",string[p 0],":",(string last p:.ll.getPos[x;y]),"]"}; / Error function for lexer
.ll.posErr:{.ll.posErrIn[.ll.p_t;x]};
.ll.ferr:{i:.ll.p_i^x; '"unexpected term at ",.ll.posErr[i],": ",.ll.ppt[$[i<count .ll.p_t0;.ll.p_t[0]i;.ll.t 0]],$[.ll.p_i<>i;", starting at ",.ll.posErr[.ll.p_i];()]," expected: "," "sv .ll.ppt each .ll.t (),y};
.ll.fgenp:{v:$[0=count p:.ll.P[3;x];".ll.fpred0 ",string x;".ll.fpred[",string[x],";x]"]; $[0=count p;:v;all 0<=p;:v;"$[",(";"sv".ll.A[",/:(string abs p w),'"][x];",/:string w:where p<0),";",v,"]"]};
.ll.fgen0:{if[null first y;:{()}]; v:{s:string y; $[-7=t:type y;z,$[x&z~"v1";":f[x]";":(.ll.F[",s,"]",$[1=count .ll.P[0;y];"";.ll.fgenp y],")[x]"];-6=t;z,":$[",s,"i=0i^.ll.p_t0 .ll.p_i;.ll.p_t[0;-1+.ll.p_i+:1];.ll.ferr[0N;",s,"]]";
  7=t;":(",(";"sv"v",/:s),")";-5=t;-1_1_string .ll.A y;'"unexp"]}[x]'[y;a:"v",/:string sums t:(type each y)in -7 -6h]; if[(11=type y)&(1=count y)&x;:(::)];
  if[.ll.savePos&count t:where t; s:"P:.ll.p_i;"; $[-6=type y t 0; v[t 0]:s,v t 0;1<count t;v[t 1]:s,v t 1;()]]; value "{",$[x;"[f;x]";""],(";"sv v,enlist":(",(";"sv a t),")"),"}"};
.ll.fgen:{{$[1=c:count x;(.ll.fgen0[0] x 0;(),.ll.fgen0[1] x 0);(.ll.fgen0[0] each x;.ll.fgen0[1] each x)]}each .ll.P 0};
.ll.fpdfa:{[n;tk;i] / runtime predictor
  s:1; t:.ll.P[4;n];
  do[0W;$[-2<s1:t[s]0i^tk i;:s1;-0W<s1;[s:neg 1+s1;i+:1];null s1;[.ll.dfaas[n;s;0i^tk i];t:.ll.P[4;n]];[.ll.p_w:i;:neg 1+s]]];
 };
.ll.fpdfa2:{[a;tk;i] s:1; do[0W;$[null s:a[s;`trans]tk i;:-1;-2<s;:s;-0W=s;:-1;[s:neg 1+s;i+:1]]]}; / generation predictor
.ll.fpdfa3:{[a;tk;i] ps:s:1; do[0W;$[null s:a[ps:s;`trans]tk i;:(i;ps);-2<s;:(i;ps);-0W=s;:(i;ps);[s:neg 1+s;i+:1]]]}; / path
.ll.fpred:{if[-1<r:.ll.fpdfa[x;.ll.p_t0;.ll.p_i];:r]; a:(); if[r<-1; if[(count p:.ll.P[3;x])&count a:.ll.P[1;x][neg 1+r;`unres].ll.p_t0 .ll.p_w; a:{$[count y;x y;x]}[a] where(.ll.A p a)@\:y]]; / 0N!(`pred;x);
  if[1=count a;:a 0]; v:.ll.fpdfa3[.ll.P[1;x];.ll.p_t0;.ll.p_i]; if[0=count a;.ll.ferr[v 0;where not -1=.ll.P[4;x;v 1]]];
  .ll.e"Multiple alternatives in ",string[.ll.P[2;x]],": ",(" "sv string a),", selecting the first, path: "," "sv .ll.ppt each .ll.p_t[0].ll.p_i+til 1+v[0]-.ll.p_i; :a 0};
.ll.fpred0:{if[-1<r:.ll.fpdfa[x;.ll.p_t0;.ll.p_i];:r]; v:.ll.fpdfa3[.ll.P[1;x];.ll.p_t0;.ll.p_i]; .ll.ferr[v 0;where not -1=.ll.P[4;x;v 1]]}; / no pred
/ .ll.fpred:{.ll.P[2;x];if[-1<r:.ll.fpdfa[x;.ll.p_t0;.ll.p_i];:r];
.ll.parse:{.ll.p_t:x:$[10=type x;.ll.lexer x;x]; .ll.p_t0:"i"${if[any w:null y;'"unknown terminal ",(.ll.ppt x[0;i])," at ",.ll.posErr i:first where w];y}[x]@[v;w;:;.ll.tm{$[11=type x;x;count x;`$x;`$()]}x[0]w:where null v:.ll.tm x 1]; .ll.p_i:0; first .ll.F[0][]};
.ll.getPF:{n:.ll.P[2]?x; $[0=count .ll.P[1]n;.ll.F n;value"{(.ll.F[",string[n],"]",.ll.fgenp[n],")x}"]};

.ll.flw:{$[0=count a:.ll.P[1;x];();0>p:.ll.fpdfa2[a;y;0];();enlist[(x;p)],$[(not null r)&-7=type r:first .ll.P[0;x]p;.z.s[r;y];()]]};
.ll.fsubst:{if[1=count v:.ll.flw[x;(),y];:v[0]1]; v2:-1+count .ll.F[x],:f:{y x}/[(enlist{((),.ll.F x 0)x 1}last v),reverse .ll.F1 ./:-1_v]; if[100=type f;aaaaa];v2};
.ll.fasubst:{if[count s:{$[count[x]=count distinct x;x;()]}neg 1+t[1]k:where{(-0W<x)&-1>x}(t:.ll.P[1;x;`trans])1; t[s]:{[s;t;k;v] t[k2]:.ll.fsubst[s]each k,/:k2:where -1<t:t v; t}[x;t]'[k;s]]; t[1;k]:.ll.fsubst[x]each k:where -1<t 1; t};

/ rule eval
.ll.ty:{$[-11=type x;x;'"bad name: ",.Q.s1 x]};
.ll.eval:{if[0=count x:x where not(()," ")~/:x:-4!.ll.fext @[a:x;where x in"\n\t\r";:;" "];:()]; if[enlist["Q"]~x 0;:value 1_a]; x:.ll.val each x; n:.ll.ty x 0; if[n=`GNAME; :.ll.n:.ll.ty x 1]; if[n in`PARSER`LEXER; :.ll[`gen`lgen n=`LEXER].ll.ty x 1];
  if[not":"~x 1;'"bad rule: ",.Q.s1 x 0]; x:.ll.en 2_x;if[n=`TOKENS;if[11=abs type x;:.ll.T:{x,$[`LEXER in x;.ll.L[3]except``ERROR;()]}x];'"bad TOKENS"]; if[`KEYRULES=n;if[11=abs type x;:.ll.lkey:x];'"bad KEYRULES"]; if[";"~last x; x:-1_x]; .ll.rl[.ll.mn n;x];};
.ll.cmt:{(0 1 1 0;0 1 1 2;2 2 1 2)\[1;0^(" \t\r\n/"!1 1 1 2 3)x]}; / mark comments
.g.e:{.ll.eval x where not 2=(0 1 1 0;0 1 1 2;2 2 1 2)\[1;0^(" \t\r\n/"!1 1 1 2 3)x];};

.ll.match:{$[-11=type x;x~.ll.p_t[1].ll.p_i;((),x)~.ll.p_t[0].ll.p_i]}; / match term

/ utf8 map. utf8 chars into unicode: each 2-4 bytes seq into seq - 128 .... unicode value. All values above 127 must be then mapped into groups before passing them to the lexer
.ll.u8map:(til 128),(til 64),(til 32),(til 16),til 16;
.ll.u8:{@[;i+3;+;64*u 2+i:where x>239]u:@[;i+2;+;64*u 1+i:where x>223]u:@[@[u;i;:;128];i+1;+;64*(u:.ll.u8map x)i:where(x:"j"$x)>191]};
.ll.U:();
.ll.mkU:{if[0=count .ll.U;.ll.U:@[(prd 4#16)#0;til 256;:;til 256]]; v:"j"$"X"$(y 0 1;y 2 3;y 5 6;y 7 8); @[`.ll.U;s+til 1+(v[3]+16*v 2)-s:v[1]+16*v 0;:;x]}; / .ll.mkU[128;"ABCD-ACDE"]
