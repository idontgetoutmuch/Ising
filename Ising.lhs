% Haskell, Ising, Markov & Metropolis
% Dominic Steinitz
% 18th November 2013

---
bibliography: Ising.bib
---

Introduction
============

About a year ago there was a reddit post on the [Ising Model in
Haskell](http://www.reddit.com/r/haskell/comments/16uc2x/ising_model_in_haskell). The
discussion seems to have fizzled out but Ising models looked like a
perfect fit for Haskell using
[repa](http://hackage.haskell.org/package/repa). In the end it turns
out that they are *not* a good fit for repa. As we can get some
parallelism at a gross level but this is in a way that does not really
show off Haskell's strengths in this area. In any event, it makes a
good example for the
[vector](http://hackage.haskell.org/package/vector) package and random
number generation using the
[random-fu](http://hackage.haskell.org/package/random-fu) package.

The [Ising model](http://en.wikipedia.org/wiki/Ising_model) was (by
[Stigler's
law](http://en.wikipedia.org/wiki/Stigler%27s_law_of_eponymy))
proposed by Lenz in 1920 as a model for ferromagnetism, that is, the
magnetism exhibited by bar magnets. The phenomenon ferromagnetism is
so named because it was first observed in iron (Latin ferrum and
chemical symbol Fe). It is also exhibited, for example, by rare earths
such as gadolinium. Ferromagnetic materials lose their magnetism at a
critical temperature: the [Curie
temperature](http://en.wikipedia.org/wiki/Curie_temperature) (named
after Pierre not his more famous wife). This is an example of a phase
transition (ice melting into water is a more familiar example).

The Ising model (at least in 2 dimensions) predicts this phase
transition and can also be used to describe phase transitions in
alloys.

Abstracting the Ising model from its physical origins, one can think
of it rather like [Conway's Game of
Life](http://idontgetoutmuch.wordpress.com/2013/02/23/comonads-life-and-klein-bottles):
there is a grid and each cell on the grid is updated depending on the
state of its neighbours. The difference with the Game of Life is that
the updates are not deterministic but are random with the randomness
selecting which cell gets updated as well as whether it gets
updated. Thus we cannot update all the cells in parallel as would
happen if we used repa. The reader only interested in this abstraction
can go straight to the implementation.

The diagram below shows a 2 dimensional grid of cells. Each cell can
either be in an (spin) up state or (spin) down state as indicated by
the arrows and corresponding colours. The Ising model then applies a
parameterized set of rules by which the grid is updated. For certain
parameters the cells remain in a random configuration, that is the net
spin (taking up = 1 and down = -1) remains near zero; for other
parameters, the spins in the cells line up (not entirely as there is
always some randomness). It is this lining up that gives rise to
ferromagnetism.

```{.dia width='500'}
import Diagrams
import Ising
dia = chessBoard' 10 trialGrid
```

On the other hand, the physics and the Monte Carlo method used to
simulate the model are of considerable interest in their own
right. Readers interested in the Monte Carlo method can skip the
physics and go to Monte Carlo Estimation. Readers interested in the
physics can start with the section on Magnetism.

Definitions, theorems etc. are in **bold** and terminated by $\blacksquare$.

Magnetism
=========

Following Ziman [@Ziman:Principles] and Reif [@reif2009fundamentals],
we assume that each atom in the ferromagnetic material behaves like a
small magnet. According to [Hund's
rules](http://hyperphysics.phy-astr.gsu.edu/hbase/atomic/hund.html),
we would expect unpaired electrons in the $d$ and $f$ shells for
example in the transition elements and rare earths and these would
supply the magnetic moment. However, the magnetic interaction between
atoms is far too small to account for ferromagnetism. For Iron, for
example, the Curie temperature is 1043K and the magnetic interaction
between atoms cannot account for this by some margin. There is a
quantum mechanical effect called the exchange interaction which is a
consequence of the Pauli exclusion principle. Two electrons with the
same spin on neighbouring atoms cannot get too close to each other. If
the electrons have anti-parallel spins then the exclusion principle
does not apply and there is no restriction on how close they can get
to each other. Thus the electrostatic interaction between electrons on
neighbouring atoms depends on whether spins are parallel or
anti-parallel. We can thus write the Hamiltonian in the form:

$$
E = -J\sum_{\langle i, j\rangle} \sigma_i \sigma_j - H\sum_k \sigma_k
$$

Where

 * The notation $\langle i, j\rangle$ means we sum over all the
nearest neighbours in the lattice;

 * $H$ is the applied magnetic field (note we use E for the Hamiltonian);

 * The range of each index is $1 \ldots M$ where $N = M \times M$ is
the total number of atoms;

 * And $J$ the **coupling constant** expressing the strength of the
interaction between neighboring spins and depending on the balance
between the Pauli exclusion principle and the electrostatic
interaction energy of the electrons, this may be positive
corresponding to parallel spins (ferromagnetism which is the case we
consider in this article) or negative corresponding to anti parallel
spins (antiferromagnetism or ferrimagnetism which we consider no
further).

Acknowledgments
================

This post uses the
[random-fu](http://hackage.haskell.org/package/random-fu) package for
random number generation and has also benefitted from comments by the
author of that package (James Cook).

All diagrams were drawn using the Haskell
[diagrams](http://projects.haskell.org/diagrams) domain specific
language; the inhabitants of #diagrams were extremely helpful in
helping create these.

Internet sources too numerous to mention were used for the physics and
Monte Carlo. Some are listed in the bibliography. Apologies if you
recognize something which does not get its just acknowledgement. The
advantage of blog posts is that this can easily be remedied by leaving
a comment.

Haskell Preamble
================

Pragmas and imports to which only the over-enthusiastic reader need pay attention.

> {-# OPTIONS_GHC -Wall                      #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults    #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods  #-}
> {-# OPTIONS_GHC -fno-warn-orphans          #-}

> {-# LANGUAGE TypeFamilies                  #-}
> {-# LANGUAGE NoMonomorphismRestriction     #-}

FIXME: End of interlude

> module Ising (
>        --   example
>        {- , -} energy
>        , McState(..)
>        , measure
>        , nitt
>        , expDv
>        , tCrit
>        , singleUpdate
>        , magnetization
>        , testData
>        , trialInitState
>        , trial
>        , trialGrid
>        , getTemps
>        , xs
>        , newGrids
>        , main
>        , testData'
>        ) where
>
> import Diagrams ( example
>                 -- , errChart
>                 , chessBoard'
>                 )

> import qualified Data.Vector.Unboxed as V
> import qualified Data.Vector.Unboxed.Mutable as M

> import Data.Random.Source.PureMT
> import Data.Random
> import Control.Monad.State

> import Diagrams.Prelude hiding ( sample, render )
> import Diagrams.Backend.CmdLine
> import Diagrams.Backend.Cairo.CmdLine ( B )
> -- import Graphics.Rendering.Chart.Backend.Cairo hiding ( runBackend, defaultEnv )

The Boltzmann Distribution
==========================

Statistical physics is an extremely large subject but there needs to
be some justification for the use of the Boltzmann distribution. For
an excellent introduction to the subject of Statistical Physics (in
which the Boltzmann distribution plays a pivotal role) see David
Tong's lecture notes (@Tong:Statphys:Online).

Suppose we have we 3 boxes (we use the more modern nomenclature rather
than the somewhat antiquated word urn) and 7 balls and we randomly
assign the balls to boxes. Then it is far more likely that we will get
an assignment of 2,2,3 rather than 0,0,7. When the numbers of boxes
and balls become large (which is the case in statistical physics where
we consider e.g. $10^23$ numbers of atoms) then it becomes very, very,
very likely that the balls (or atoms) will spread themselves out
uniformly as in the example.

Now suppose we have $N$ balls and $M$ boxes and we associate an energy
$k$ with the $k$-th box but restrict the total energy to be
constant. Thus we have two constraints:

* The total number of balls $N = \sum_1^M n_k$ and

* The total energy $E = \sum_1^M k\, n_k$.

Let us assume the balls are allocated to boxes in the most likely way
and let us denote the values in each box for this distribution as
$\tilde{n}_k$.

Let us now move 2 balls from box $k$, one to box $k-1$ and one to box
$k+1$. Note that both constraints are satisified by this move. We must
therefore have

$$
\begin{aligned}
\frac{1}{(\tilde{n}_{k-1} + 1)!}\frac{1}{(\tilde{n}_{k} - 2)!}\frac{1}{(\tilde{n}_{k+1} + 1)!} &\le \frac{1}{\tilde{n}_{k-1}!}\frac{1}{\tilde{n}_{k}!}\frac{1}{\tilde{n}_{k+1}!} \\
\frac{\tilde{n}_k}{\tilde{n}_{k-1} + 1}\frac{\tilde{n}_k - 1}{\tilde{n}_{k+1} + 1} &\le 1
\end{aligned}
$$

And let us start from the most likely distribution and move 1 ball
from box $k-1$ and 1 ball from box $k+1$ into box $k$. Again both
constraints are satisified by this move. Doing a similar calculation
to the one above we get

$$
\begin{aligned}
\frac{1}{(\tilde{n}_{k-1} - 1)!}\frac{1}{(\tilde{n}_{k} + 2)!}\frac{1}{(\tilde{n}_{k+1} - 1)!} &\le \frac{1}{\tilde{n}_{k-1}!}\frac{1}{\tilde{n}_{k}!}\frac{1}{\tilde{n}_{k+1}!} \\
1 &\le \frac{\tilde{n}_k + 1}{\tilde{n}_{k+1}}\frac{\tilde{n}_{k} + 2}{\tilde{n}_{k-1}}
\end{aligned}
$$

From this we deduce that as $n_k \rightarrow \infty$ then $n_k^2
\approx n_{k-1}n_{k+1}$ or that $n_{k+1} / n_k = B$ for some constant
$B$.

Telecsoping we can write $n_k = n_1 B^{k-1} \propto B^k$. Thus the probability of a ball being box $k$ is

$$
\mathbb{P}(k) = \frac{B^k}{\sum_{i=1}^M B^i}
$$

If we now set $B = e^{-\beta}$ and $Z = \sum_{i=1}^M B^i$ then we can
re-write our distribution as

$$
\mathbb{P}(k) = \frac{e^{-\beta k}}{Z}
$$

It should therefore be plausible that the probability of finding a
system with a given energy $E(\sigma)$ in a given state $\sigma$ is given by
the Boltzmann distribution

$$
\mathbb{P}(\sigma) = \frac{\exp(-E(\sigma) / k_B T)}{Z(T)}
$$

where we have defined the temperature to be $T = 1 / k_B\beta$ with
$k_B$ being Boltzmann's constant and $Z(T)$ is another normalizing
constant.

Monte Carlo Estimation
======================

Although Ising himself developed an analytic solution in 1 dimension
and Onsager later developed an analytic solution in 2 dimensions,
no-one has (yet) found an analytic solution for 3 dimensions.

One way to determine an approximate answer is to use a Monte Carlo
method.

Uniform Sampling
----------------

We could pick sample configurations at random according to the
Boltzmann distribution

$$
\mathbb{P}(\sigma) = \frac{\exp(-E(\sigma) / k_B T)}{Z(T)}
$$

where the sum $T$ is the temperature, $k_B$ is Boltzmann's constant,
$E$ is the energy of a given state

$$
E(\sigma) = -J\sum_{i, j} \sigma_i \sigma_j - B \sum_k \sigma_k
$$

and $Z(T)$ is a normalizing constant (Z for the German word
Zustandssumme, "sum over states")

$$
Z(T) = \sum_\sigma \exp(-E(\sigma) / k_B T)
$$

We can evaluate the energy for one state easily enough as the Haskell
below demonstrates.

[As an aside, we represent each state by a *Vector* of *Int*. No doubt
more efficient representations can be implemented. We also have to
calculate offsets into the vector given a point's grid co-ordinates.]

> gridSize :: Int
> gridSize = 10
>
> energy :: (Fractional a, Integral a1, M.Unbox a1) => V.Vector a1 -> a
> energy v = 0.5 * (fromIntegral $ V.sum energyAux)
>   where
>
>     energyAux = V.generate l f
>
>     l = V.length v
>
>     f m = c * d
>       where
>         i = m `mod` gridSize
>         j = (m `mod` (gridSize * gridSize)) `div` gridSize
>
>         c = v V.! jc
>         jc = gridSize * i + j
>
>         d = n + e + s + w
>
>         n = v V.! jn
>         e = v V.! je
>         s = v V.! js
>         w = v V.! jw
>
>         jn = gridSize * ((i + 1) `mod` gridSize) + j
>         js = gridSize * ((i - 1) `mod` gridSize) + j
>         je = gridSize * i + ((j + 1) `mod` gridSize)
>         jw = gridSize * i + ((j - 1) `mod` gridSize)

But what about the normalizing constant $Z$? Even for a modest grid
size say $10 \times 10$, the number of states that needs to be summed
over is extremely large $2^{10 \times 10}$.

Instead of summing the entire state space, we could draw R random
samples $(\sigma^{(i)})_{0 \le i \lt R}$ uniformly from the state
space. We could then use

$$
Z_R = \sum_0^{R-1} P^*(\sigma^{(i)})
$$

to estimate e.g. the magnetization

$$
\langle M \rangle = \sum_\sigma M(\sigma) \frac{\exp(-\beta E(\sigma))}{Z(T)} \mathrm{d} \sigma
$$

by

$$
\widehat{\langle M \rangle} = \sum_{i=0}^{R-1} M(\sigma) \frac{exp(-\beta E(\sigma(i)))}{Z(T)}
$$


However we know from statistical physics that systems with large
numbers of particles will occupy a small portion of the state space
with any significant probability.  And according to [@MacKay:itp], a
high dimensional distribution is often concentrated on small region of
the state space known as its typical set $T$ whose volume is given by
$|T| \approx 2^H$ where $H$ is the entropy of the (Boltzmann)
distribution which for ease of exposition we temporarily denote by
$P$.

$$
H = -\sum_\sigma P(\sigma)\log_2(P(\sigma))
$$

If almost all the probability mass is located in $T$ then the actual
value of the (mean) magnetization will determined by the values that
$M$ takes on that set. So uniform sampling will only give a good
estimate if we make $R$ large enough that we hit $T$ at least a small
number of times. The total size of the state space is $2^N$ and the
$|T| \approx 2^H$, so there is a probability of $2^H / 2^N$ of hitting
$T$. Thus we need roughly $2^{N - H}$ samples to hit $T$.

At high temperatures, the Boltzmann distribution flattens out so
roughly all of the states have an equal likelihood of being
occupied. We can calculate the (Shannon) entropy for this.

$$
H \approx \sum_\sigma \frac{1}{2^N}\log_2 2^N = N
$$

We can do a bit better than this. At high temperatures, $\beta J \ll
1$ and taking $B = 0$

$$
\begin{aligned}
Z &= \sum_\sigma \exp\big(-\beta E(\sigma)\big) \\
  &= \sum_\sigma \exp\bigg(\beta J \sum_{i, j} \sigma_i \sigma_j - B \sum_k \sigma_k\bigg) \\
  &= \sum_\sigma \prod_{i, j} \exp\beta J \sigma_i \sigma_j
\end{aligned}
$$

By definition

$$
\begin{aligned}
\cosh x &= \frac{e^x + e^{-x}}{2} \\
\sinh x &= \frac{e^x - e^{-x}}{2}
\end{aligned}
$$

Thus

$$
\begin{aligned}
\exp \beta J \sigma_i \sigma_j &= \cosh\beta J + \sigma_i\sigma_j\sinh \beta J \\
                               &= \cosh\beta J (1 + \sigma_i\sigma_j\tanh \beta J)
\end{aligned}
$$

We can therefore re-write the partition function as

$$
\begin{aligned}
Z &= \sum_\sigma \prod_{i, j} \cosh\beta J (1 + \sigma_i\sigma_j\tanh \beta J) \\
  &= (\cosh \beta J)^{4N/2} \sum_\sigma \prod_{i, j} (1 + \sigma_i\sigma_j\tanh \beta J) \\
\end{aligned}
$$

where the factor 2 is because we count the exchange interaction twice
for each pair of atoms and the factor 4 is because each atom is
assumed to only interact with 4 neighbours.

Since at high temperatures, by assumption, we have $\beta J \ll 1$
then $\tanh \beta J \ll 1$ also.

Thus

$$
Z \approx (\cosh \beta J)^{2N}\sum_\sigma 1 = 2^N(\cosh \beta J)^{2N}
$$

Calculating the free energy

$$
\begin{aligned}
F &= -k_B T \ln Z \\
  &= -k_B T N \ln 2 -k_B T 2 N \ln (\cosh \beta J) \\
  &\approx -k_B T N \ln 2 -k_B T N (\beta J)^2 \\
  &= -k_B T N \ln 2 - N\frac{J^2}{k_B T} \\
\end{aligned}
$$

From this we can determine the (Boltzmann) entropy

$$
S = k_B N (\ln 2 - (\beta J)^2) \approx k_B N \ln 2
$$

which agrees with our rather hand-wavy derivation of the (Shannon)
entropy at high temperatures.

At low temperatures the story is quite different.
We can calculate the ground state energy where all
the spins are in the same direction.

$$
E_0 = -J\sum_{i,j}\sigma_i\sigma_j = -4JN / 2 = -2JN
$$

And we can assume that at low temperatures that flipped spins are
isolated from other flipped spins. The energy for an atom in the
ground state is -4J and thus the energy change if it flips is 8J. Thus
the energy at low temperatures is

$$
E_1 \approx E_0 + 8J\sum_i (\sigma_i + 1) / 2
$$

The partition function is

$$
\begin{aligned}
Z &= 2\sum_\sigma \exp{-\beta \bigg(E_0 + 8J\sum_i (\sigma_i + 1) /2\bigg)} \\
  &= 2\exp{-\beta E_0} \prod_i \sum_{\sigma_i = \pm 1} \exp{-8\beta J(\sigma_i + 1) / 2} \\
  &= 2\exp{-\beta E_0} (1 + \exp{-8\beta J})^N
\end{aligned}
$$

Again we can calculate the free energy and since we are doing this
calculation to get a rough estimate of the entropy we can approximate
further.

$$
\begin{aligned}
F &= -k_B T \ln Z \\
  &= -k_B (T \ln 2 - T E_0 + T N \ln (1 + \exp{-8\beta J})) \\
  &\approx -k_B (T \ln 2 - T E_0 + T N \exp{-8\beta J})
\end{aligned}
$$

From this we can determine the (Boltzmann) entropy. Using the fact that $\beta = 1 / k_B T$ and thus that

$$
\frac{\partial \beta}{\partial T} = - \frac{1}{k_B T^2} = -k_B\beta^2
$$

we have

$$
\begin{aligned}
S &= - \frac{\partial F}{\partial T} \\
  &= k_B\bigg(\ln 2 + N\frac{\partial}{\partial T} \exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 + N\frac{\partial}{\partial T} \frac{1}{\beta}\exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 + N\frac{\partial \beta}{\partial T} \frac{\partial}{\partial \beta}\frac{1}{\beta}\exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 + N\frac{\partial \beta}{\partial T} \frac{\partial}{\partial \beta}\frac{1}{\beta}\exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 + N\frac{\partial \beta}{\partial T}
     \bigg(-\frac{1}{\beta^2}\exp{-8\beta J} -
     \frac{8J}{\beta}\exp{-8\beta J}\bigg) \\
  &= k_B\bigg(\ln 2 +
     N\exp{-8\beta J} +
     8JN\beta\exp{-8\beta J}\bigg)
\end{aligned}
$$

The critical temperature (as we shall obtain by simulation) given by
[Onsager's exact
result](http://en.wikipedia.org/wiki/Ising_model#Onsager.27s_exact_solution)
in 2d is

$$
\frac{1}{\beta} = k_BT_{\text Crit} = \frac{2J}{\ln(\sqrt{2}+1)} \approx 2.269J
$$

Plugging this in we get

    [ghci]
    exp (-8.0/2.269) * (1.0 + 8.0 / 2.269)

Thus the (Shannon) entropy is about 0.13N at the interesting
temperature and is about N at high temperatures. So uniform sampling
would require $\sim 2^{(N - N)}$ samples at high temperatures but $\sim
2^{(N - 0.13N)} \approx 2^{N /2}$ at temperatures of interest. Even for
our modest $10 \times 10$ grid this is $2^{50} \approx 10^{17}$ samples!

Fortunately, Metropolis and his team [@Metropolis53] discovered a way
of constructing a Markov chain with a limiting distribution of the
distribution required which does not require the evaluation of the
partition function and which converges in a reasonable time (although
theoretical results substantiating this latter point seem to be hard
to come by).

Markov Chains
=============

Markov first studied the stochastic processes that came to be named after him in 1906.

We follow [@DBLP:books/daglib/0095301], [@Beichl615768],
[@Diaconis:1995:WKM:225058.225095], [@Greenbert95] [@citeulike:7907579] and
[@Gravner:mat135a:Online].

As usual we work on a probability measure space $(\Omega, {\mathbb F},
\mu)$ (that is $\mu(\Omega) = 1$). Although this may not be much in
evidence, it is there lurking behind the scenes.

Let $S$ be a finite set. In the case of an Ising model with $N$ cells,
this set will contain $2^N$ elements (all possible configurations).

A **Markov chain** is a discrete time stochastic process $X_0, X_1,
\ldots$ such that

$$
\mu (X_{n+1} = j \,|\, X_0 = i_0, X_1 = i_1, \dots X_n = i) = \mu (X_{n+1} = j \,|\, X_n = i)
$$

$\blacksquare$

That is, where a Markov chain goes next only depends on where it is
not on its history.

A **stochastic transition matrix** is a matrix $P = \{ p_{ij} : i, j \in S
\}$ such that

$$
\sum_{j \in S} p_{ij} = 1 \, \forall i \in S
$$

$\blacksquare$

We can describe a Markov chain by its transition matrix and initial
distribution.

Stationarity
------------

A Markov chain has a **stationary distribution** $\pi_i$ if

$$
\sum_{i \in S} \pi_i p_{ji} = \pi_j
$$

One question one might ask is whether a given Markov chain has such a
distribution. For example, for the following chain, any distribution
is a stationary distribution. That is $\pi P = \pi$ for any $\pi$.

$$
\begin{bmatrix}
  1 & 0 \\
  0 & 1
 \end{bmatrix}
$$

Another key question is, if there is a unique stationary distribution,
will the $n$-th transition probabilities converge to that distribution
(FIXME: really badly expressed but will do as a reminder).

In the case of chains with a countably infinite state space, a

Irreducibility
--------------

Write ${\mathbb P}_i(A) = {\mathbb P}(A \, | \, X_0 = i)$

We say that $i$ **leads to** $j$ and write $i \rightarrow j$ if

$$
{\mathbb P}_i(X_n = j \, \text{for some n}) \gt 0
$$

**Theorem** For distinct states $i$ and $j$, the following are equivalent:

* $i \rightarrow j$
* $p_{i_0i_1} p_{i_1i_2} \ldots p_{i_{n-1}i_n} \gt 0$
* $p_{ij}^{(n)} \gt 0$

$\blacksquare$

This makes it clear that $\rightarrow$ is transitive and reflexive
hence an equivalence relation. We can therefore partition the states
into classes. If there is only one class then the chain is called
**irreducible**.

For example,

$$
\begin{bmatrix}
  \frac{1}{2} & \frac{1}{2} &           0 &           0 \\
  \frac{1}{2} & \frac{1}{2} &           0 &           0 \\
            0 &           0 & \frac{1}{4} & \frac{3}{4} \\
            0 &           0 &           0 &           1
 \end{bmatrix}
$$

has classes $\{1, 2\}$, $\{3\}$ and $\{4\}$ so is *not* irreducible.

On the other hand

$$
\begin{bmatrix}
  \frac{1}{2} & \frac{1}{2} &           0 \\
  \frac{1}{2} & \frac{1}{4} & \frac{1}{4} \\
            0 & \frac{1}{3} & \frac{2}{3}
\end{bmatrix}
$$

is irreducible.

Recurrence
----------

Let $X_n$ be a Markov chain. A state $i$ is **recurrent** if

$$
{\mathbb P} (X_n = i \, \text{i.o.}) = 1
$$

The **first passage time** is defined as

$$
T_j = \inf_{n \ge 1} \{X_n = j\}
$$

Note that the $\inf$ is taken over $n$ *strictly* greater than
1. Incidentally the first passage time is a stopping time but that any
discussion of that would take this already long article even longer.

The expectation of the $i$-th first passage time starting from $i$ is
denoted ${\mathbb E}_i(T_i)$.

Define $m_i = {\mathbb E}_i(T_i)$

**Theorem** Let $P$ be irreducible then the following are equivalent:

* Every state is positive recurrent.

* Some state is positive recurrent.

* P has an invariant distribution $\pi$ and in this case $\pi_i = 1 / m_i$.

$\blacksquare$

A state $i$ is **aperiodic** if $p_{nn} \gt 0$ for *all* sufficiently large $n$.

Example:

$$
\begin{bmatrix}
  0 & 1 \\
  1 & 0
 \end{bmatrix}
$$

FIXME: Put Haskell example here with ghci for the first few terms

**Theorem** Let $P$ be irreducible and aperiodic and suppose that $P$ has an
invariant distribution $\pi$. Let $\pi_0$ be any distribution (on the state space???). Suppose that $(X_n)_{n \ge 0}$ is Markov $(\pi_0, P)$ then

* $\mu(X_n = j) \rightarrow \pi_j$ as $n \rightarrow \infty$ for all $j$

$\blacksquare$

A proof of this theorem uses *coupling* developed by Doeblin; see
[@Gravner:mat135a:Online] for more details.

**Corollary** With the conditions of the preceding Theorem

* $p_{ij}^{(n)} \rightarrow \pi_j$ as $n \rightarrow \infty$ for all $i, j$

$\blacksquare$

If the state space is infinite, the existence of a stationary
distribution is not guaranteed even if the Markov chain is
irreducible, see [@Srikant:ece534:Online] for more details.

The Ergodic Theorem
-------------------

An irreducible, aperiodic, positive recurrent Markov chain has a
unique stationary distribution, which is also the limiting
distribution

Such Markov chains are called ergodic

Define the number of visits to $i$ strictly before time $n$ as

$$
V_i(n) \triangleq \sum_{k = 0}^{n - 1}{\mathcal I}_{\{X_k = i\}}
$$

$V_i(n) / n$ is the proportion of time before $n$ spent in state $i$.

**Theorem** Let $(X_n)_{(n \ge 0)}$ be an irreducible Markov chain then

$$
{\mathbb P} \Bigg(\frac{V_i(n)}{n} \rightarrow \frac{1}{m_i} \, {\text as} \, n \rightarrow \infty \Bigg) = 1
$$

If further the chain is positive recurrent then for any bounded
function $f : {\mathbb I} \rightarrow {\mathbb R}$ then

$$
{\mathbb P} \Bigg(\frac{1}{n} \sum_{k = 0}^{n - 1}f(X_i)\rightarrow \hat{f} \, {\text as} \, n \rightarrow \infty \Bigg) = 1
$$

$\blacksquare$

The Metropolis Algorithm
------------------------

Thus if we can find a Markov chain with the required stationary
distribution and we sample a function of this chain we will get an
estimate for the average value of the function. What Metropolis and
his colleagues did was to provide a method of producing such a chain.

**Algorithm**

Specify an ergodic Markov chain on $\Omega$ with transition
probabilities $q_{ij}$ and create a new (ergodic) Markov chain with
transition probabilities

$$
p_{ij} =
\begin{cases}
q_{ij}\bigg[\frac{\pi_j q_{ji}}{\pi_i q_{ij}} \land 1 \bigg] & \text{if } y \ne x \\
1 - \sum_{k : k \ne i} q_{ik} \bigg[\frac{\pi_j q_{ji}}{\pi_i q_{ij}} \land 1 \bigg] & \text{if } y = x
\end{cases}
$$

Other Other
-----------

A warm start but we could try a cold start with all spins up.


Calculate magnetization:

Calculate energy:

> data McState = McState { mcMagnetization :: !Double
>                        , mcMAvg          :: !Double
>                        , mcCount         :: !Int
>                        , mcNumSamples    :: !Int
>                        , mcGrid          :: !(V.Vector Int)
>                        }
>   deriving Show
>
> measure :: Int
> measure = 100
>
> nitt :: Int
> nitt = 1000000
>
> tCrit :: Double
> tCrit = 2.0 / log (1.0 + sqrt 2.0) - 0.1
>
> magnetization :: (Num a, M.Unbox a) => V.Vector a -> a
> -- magnetization :: (M.Unbox a, Num a) => V.Vector a => a
> magnetization = V.sum
>
>
> expDv :: Double -> V.Vector Double
> expDv t = V.generate 9 f
>   where
>     f n | odd n = 0.0
>     f n         = exp (((fromIntegral (8 - n)) - 4.0) * 2.0 / t)
>
> singleUpdate :: Int -> V.Vector Double -> McState -> (Int, Int, Double) -> McState
> singleUpdate measure expDvT u (i, j, r) = -- D.trace (show $ mcMAvg u) $
>   McState { mcMagnetization = newMag
>           , mcMAvg =
>             if (mcCount u) `mod` measure == 0
>             then mcMAvg u + newMag
>             else mcMAvg u
>           , mcCount = mcCount u + 1
>           , mcNumSamples =
>             if (mcCount u) `mod` measure == 0
>             then mcNumSamples u + 1
>             else mcNumSamples u
>           , mcGrid = newGrid
>           }
>   where
>     newGrid = if p > r
>               then V.modify (\v -> M.write v jc (-c)) v
>               else v
>
>     oldMag = mcMagnetization u
>
>     newMag = if p > r
>               then oldMag - 2 * (fromIntegral c)
>               else oldMag
>
>     v = mcGrid u
>
>     p = expDvT V.! (4 + c * d)
>
>     c = v V.! jc
>     jc = gridSize * i + j
>
>     d = n + e + s + w
>
>     n = v V.! jn
>     e = v V.! je
>     s = v V.! js
>     w = v V.! jw
>
>     jn = gridSize * ((i + 1) `mod` gridSize) + j
>     js = gridSize * ((i - 1) `mod` gridSize) + j
>     je = gridSize * i + ((j + 1) `mod` gridSize)
>     jw = gridSize * i + ((j - 1) `mod` gridSize)
>
> testData :: Int -> V.Vector (Int, Int, Double)
> testData m =
>   V.fromList $
>   evalState (replicateM m x)
>   (pureMT 2)
>   where
>     x = do r <- sample (uniform (0 :: Int)    (gridSize - 1))
>            c <- sample (uniform (0 :: Int)    (gridSize - 1))
>            v <- sample (uniform (0 :: Double)            1.0)
>            return (r, c, v)
>
> trial :: McState -> Double -> V.Vector (Int, Int, Double) -> McState
> trial s t = V.foldl (singleUpdate 1 (expDv t)) s
>
> trialInitState :: McState
> trialInitState = McState { mcMagnetization = fromIntegral $
>                                              magnetization trialGrid
>                          , mcMAvg = 0.0
>                          , mcCount = 0
>                          , mcNumSamples = 0
>                          , mcGrid = trialGrid
>                         }
>
> trialGrid :: V.Vector Int
> trialGrid = V.fromList $ concat $ initGridL
>   where
>     initGridL = [ [-1, -1,  1, -1, -1, -1, -1,  1, -1, -1]
>                 , [ 1,  1, -1,  1, -1,  1,  1, -1,  1, -1]
>                 , [ 1, -1, -1, -1, -1,  1, -1, -1, -1, -1]
>                 , [-1, -1,  1, -1,  1, -1,  1,  1, -1,  1]
>                 , [ 1,  1,  1, -1,  1, -1, -1,  1,  1,  1]
>                 , [ 1, -1, -1,  1, -1, -1, -1, -1,  1,  1]
>                 , [ 1,  1,  1, -1, -1,  1, -1, -1,  1, -1]
>                 , [ 1, -1, -1, -1,  1,  1, -1, -1,  1, -1]
>                 , [ 1, -1,  1, -1, -1, -1, -1,  1,  1, -1]
>                 , [-1, -1,  1,  1, -1, -1,  1,  1, -1,  1]
>                 ]
>
> getTemps :: Double -> Double -> Int -> [Double]
> getTemps h l n = [ m * x + c |
>                    w <- [1..n],
>                    let x = fromIntegral w ]
>   where
>     m = (h - l) / (fromIntegral n - 1)
>     c = l - m
>
> xs :: [Double]
> xs = getTemps 4.0 0.5 100
> newGrids :: [McState]
> newGrids = map (\t -> trial trialInitState t (testData nitt)) xs
>
> main :: IO ()
> main = do print "Magnetization"
>
>           -- renderableToPNGFile (errChart xs mcMAvg trial trialInitState testData nitt)
>           --                     500 500 "diagrams/Magnetism.png"
>           mainRender (DiagramOpts (Just 500) (Just 500) "diagrams/exampleGrid.png"
>                      , DiagramLoopOpts False Nothing 0)
>                      (example :: Diagram B R2)
>           mainRender (DiagramOpts (Just 500) (Just 500) "diagrams/vectorGrid.png"
>                      , DiagramLoopOpts False Nothing 0)
>                      ((chessBoard' 10 trialGrid) :: Diagram B R2)

> testData' :: Int -> V.Vector (Int, Int, Double)
> testData' m =
>   V.fromList $
>   evalState (replicateM m x)
>   (pureMT 1)
>   where
>     x = do r <- sample (uniform (0 :: Int)    (gridSize - 1))
>            c <- sample (uniform (0 :: Int)    (gridSize - 1))
>            v <- sample (uniform (0 :: Double)           1.0)
>            return (r, c, v)
>

```{.dia width='500'}
dia = image "diagrams/Magnetism.png" 1.0 1.0
```

Bibliography and Resources
--------------------------
