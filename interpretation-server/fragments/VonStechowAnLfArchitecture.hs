
V = \y:<e> . \x:<e> . \e:<v> . V(e,y,x)
NP = NP:e

Runtime: <e,i>

PF = \t:<i> . \p:<v,t> . exists e:v . Runtime(e) & (p e)

t = T:i

-- [tClosure] = \t:i 