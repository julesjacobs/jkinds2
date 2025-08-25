# Notes

## Rigid variables vs solver variables

Only solve solver variables! Much clearer!
- Rigid variables are rigid from the beginning and stay rigid throughout
- No leq assertions on rigid variables. For assert_leq, solve x := x_rigid /\ bound
- Solver variables are always solve exactly once
- Remaining polys can be fully normalized
- LFP's are always solved first, then GFP's
- Simplified algorithm
    * To solve an equation, normalize rhs, take lfp or gfp and store result
    * No need to track dependencies, just normalize (force) before use
    * When end goal is leq or round_up, we can omit certain rigid variables and use \top or \bot instead
    * Still need to do lfp and gfp correctly

Types:
    two : one
    one = two

Equations:
    two := two_rigid /\ one   (gfp)
    one := two                (lfp)


Why LFP's are solved first:

Solve gfp first
~~~~~~~~~~~~~~~
two gfp:
    two = two_rigid /\ one
    one := two_rigid /\ one

one lfp:
    one = \bot
    two = \bot

=> incorrect

Solve lfp first
~~~~~~~~~~~~~~~
one lfp:
    one = two
    two := two_rigid /\ two

two gfp:
    two = two_rigid
    one = two_rigid

=> correct