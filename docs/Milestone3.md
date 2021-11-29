# Milestone 3
2021-11-29

## Deliverables

These were our deliverables from the proposal:

* Bridge for example domain
  * We will define a Bridge for our example domain (an implementation in Haskell of the interface we defined) that can perform quantification and evaluate user-defined atomic propositions.
* Working verifier driver
  * We will implement a small driver program in Haskell to feed logs (in memory) through the LTL verifier and report results.
* Results on running driver on example logs
  * We will evaluate the results of our verifier on the representative scenarios for which we generated logs. Again, this will be arranged as a suite of unit tests.

We have completed the per-domain deliverables for the simplest of our three domains, and are in the process of finishing the others for our final submission. The reason for this is that the scope of this milestone increased significantly in order to prioritize work that will lead to a more cohesive presentation (as explained below). The presenatation in progress can be found [here](https://github.com/ejconlon/ltlspec/blob/master/docs/Presentation.md).

## Bridges for example domains

The Ping bridge can be found [here](https://github.com/ejconlon/ltlspec/blob/075a596c6d9cc41dacbdef0d800cc1b0ec314f45/src/Ltlspec/Models/Ping/Verification.hs#L113).
The Chat bridge can be found [here](https://github.com/ejconlon/ltlspec/blob/master/src/Ltlspec/Models/Chat/Chat.hs#L262).
The DinningHakker bridge can be found [here](https://github.com/ejconlon/ltlspec/blob/master/src/Ltlspec/Models/DinningHakker/Verification.hs#L160).

## Verifier driver

The driver can be found [here](https://github.com/ejconlon/ltlspec/blob/075a596c6d9cc41dacbdef0d800cc1b0ec314f45/src/Ltlspec/Driver.hs#L31).

## Evaluation

Evaluation of correctness of analysis on the Ping system can be found [here](https://github.com/ejconlon/ltlspec/blob/master/src/Ltlspec/Test/Main.hs#L226-L273)
The testing results can be found [here](https://app.circleci.com/pipelines/github/ejconlon/ltlspec/68/workflows/8de426c8-92b1-410a-98cc-dce0da88eeb9/jobs/69?invite=true#step-104-108)

## Additional work (truncation, actor framework)

To better demonstrate the applicability of our analysis framework to real systems, we wrote an actor framework with which we can execute models of our domains (found [here](https://github.com/ejconlon/ltlspec/blob/master/src/Ltlspec/System/Actors.hs)). We also implemented "truncation" of proposition evaluation [here](https://github.com/ejconlon/ltlspec/blob/075a596c6d9cc41dacbdef0d800cc1b0ec314f45/src/Ltlspec.hs#L354), and we will explain more about this in the presentation.
