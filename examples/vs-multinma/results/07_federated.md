## Example 7 — Federated ML-NMR: no pooled data, no coordinator

The psoriasis ML-NMR (Examples 5–6) with each study as a separate site that never shares patient data. Sites agree on the model and a random seed and exchange numbers only through a ring **secure sum** (random masks), so nobody — not even another site — sees a single site's contribution. Every site evaluates only its own data springs (`gc-rust/src/bin/mlnmr_site.rs`).

|site      |data                 | springs| patients| numbers sent| patient records sent|
|:---------|:--------------------|-------:|--------:|------------:|--------------------:|
|UNCOVER-1 |IPD                  |    1296|     1296|        20213|                    0|
|UNCOVER-2 |IPD                  |    1219|     1219|        20213|                    0|
|UNCOVER-3 |IPD                  |    1339|     1339|        20213|                    0|
|FIXTURE   |AgD (published arms) |       4|     1297|        20213|                    0|

### Protocol

0. **Network mean of the covariates** (for centring): one secure sum of Σw·x and Σw. Equal to the pooled value to 2e-14.
1. **Force rounds — relax the joint network.** At the current configuration θ each site sends its log-likelihood, force (24 numbers) and stiffness (24 × 24); the sums are the joint network's force and stiffness, and a Newton step moves θ. **7 rounds** (0.2 s) reach the equilibrium; it equals the pooled springs fit to **5.4e-14**. The summed stiffness is each site's *equivalent spring*: exact for linear-Gaussian models, a quadratic (Laplace) approximation for non-linear ones.
2. **One energy round — exact posterior.** Every site regenerates the same 16000 random configurations from the shared seed (multivariate t around the equilibrium) and returns one number per configuration: its log-likelihood there. The secure sum is the exact joint energy; reweighting gives the exact posterior (ESS 9275; 3.3 s).
3. **Priors last.** The prior multiplies the weights after the sum, so each site applies its own.

### Federated = pooled

* Against **multinma on the pooled data** (Example 5; d and β, 20 quantities): equivalent quadratic springs only (after step 1): max |z| = **0.24**; after the energy round: max |z| = **0.029**, SD ratio 0.98–1.02.
* Against the **pooled springs fit** (all data in one place): max |z| = 0.019.
* The step from 1 to 2 is the non-linearity of the binary springs (rare placebo responses, probit tail): a site's patients cannot be replaced *exactly* by a quadratic spring, but one round of energy evaluations restores exactness.

### Every site its own posterior (no further communication)

Each site reweights the same shared energies with its own prior. Checked against a pooled springs fit under the same prior. `target`: effect vs placebo in the transport population of Example 6 (25 y disease, 60% BSA, 120 kg, PsA, prior systemic).

|site      |prior                                      |  ESS|d IXE Q2W   |d SEC 300   |d ETN       |target: IXE Q2W |target: ETN |max &#124;z&#124; vs pooled fit, same prior |local s |
|:---------|:------------------------------------------|----:|:-----------|:-----------|:-----------|:---------------|:-----------|:-------------------------------------------|:-------|
|UNCOVER-1 |vignette: all N(0, 10²)                    | 9275|2.95 (0.09) |2.45 (0.12) |1.55 (0.08) |2.76 (0.24)     |1.06 (0.27) |0.039                                       |0.057   |
|UNCOVER-2 |sceptical effects: d ~ N(0, 1)             | 7581|2.90 (0.08) |2.38 (0.12) |1.50 (0.08) |2.72 (0.24)     |1.02 (0.27) |0.012                                       |0.043   |
|UNCOVER-3 |sceptical modifiers: β ~ N(0, 0.5²)        | 6090|2.95 (0.09) |2.44 (0.12) |1.55 (0.08) |2.71 (0.20)     |1.02 (0.22) |0.040                                       |0.044   |
|FIXTURE   |heavy-tailed modifiers: β ~ Cauchy(0, 0.5) | 5252|2.95 (0.09) |2.44 (0.12) |1.54 (0.08) |2.71 (0.19)     |1.01 (0.22) |0.045                                       |0.053   |

### What leaves a site

Force rounds: (1 + P + P²) = 601 numbers per round; energy round: 16000 numbers (log-likelihoods at shared random configurations). No patient record, covariate value or per-patient quantity. What site 2 saw of the first message passed to it (masked partial sums): -1033.0,  1082.0,   351.5.

Scope: fixed effects. Random effects add study-specific δ springs owned by each site and the τ² grid (one force/energy round per grid point, all in parallel); not run here.
