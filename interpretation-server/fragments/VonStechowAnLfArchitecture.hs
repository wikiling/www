
Runtime:<v,i>;

[id] = \x:<A> . x;

[V] = \y:<e> . \x:<e> . \e:<v> . V:<v,<e,<e,t>>>(e)(y)(x);
[NP] = NP:<e>;
[PF] = \t:<i> . \p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e);

[z] = z;
[bindt] = \z:<i>