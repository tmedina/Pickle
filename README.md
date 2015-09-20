Pickle
======

Pickle uses the Scala Simple Build Tool (SBT) for building and running.

To build:

sbt compile

To run:

start sbt
$sbt

run an application

Pickle Simulation
>pickle apps/BoidFlockingDemo.xml

Pickle Evolution
>evolve conf/evolve.xml evolutionResults

Replay an evolved controller result
>replay evolutionResults/generation.1/result_0.xml
