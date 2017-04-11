# Subject-Drop

Paper title: Nothing but the truth, but not the whole truth: Adults choose to mention agents and patients in proportion to informativity, even if it doesn’t fully disambiguate the message

### Abstract

How do we decide what to say to ensure our meanings will be understood? The Rational Speech Act model (RSA, Frank & Goodman, 2012) asserts that speakers plan what to say by comparing the informativity of words in a particular context. We present the first example of an RSA model of sentence level (who-did-what-to-whom) meanings. In these contexts, the set of possible messages must be abstracted from entities in common ground (people and objects) to possible events (*Jane eats the apple, Marco peels the banana*), with each word contributing unique semantic content. How do speakers accomplish the transformation from context to compositional messages? In a communication game, participants described transitive events (e.g. Jane pets the dog), with only two words, in contexts where two words either were or were not enough to uniquely identify an event. Adults chose utterances matching the predictions of the RSA even when there was no possible fully successful choice. Thus we show that adults’ communicative behavior can be described by a model that accommodates informativity in context, beyond the set of possible entities in common ground.  This study suggests that full-blown natural speech may result from speakers who model and adapt to the listener’s needs.

### Repository Contents

####Adult - Multidistractor/

#####Folders

* batch/
* log/

The raw data: outputs from AMT and the willow script

* MD11-20-12/

The willow script (randomizes order/condition assignments, displays trials to participants, records responses to our server)

* Response coding/

Files used during initial coding by RAs of raw responses -> category (e.g. 'dog' -> PATIENT)

* Stims for Multi-distractor/

All stimuli used for the experiment reported in this paper. We include the original clip art used while generating the stimuli (objects), all context scenes (e.g. 1 person and 6 animals for the 'FEED' event, FEED_1_6.jpg), and all actions (FEED, DRINK, etc.)


#####Files

* MD_turk.R, lab-misc.R

The main analysis pipeline, which starts from the raw data and produces all analyses & (human) graphs reported in Experiment 1. This file generates the following:

** humandata.csv - includes just the 'normal' ({AVP} choose 2) responses, used to calculate parameters for the models

** humanPerformance.jpg

* snazzy potato 11-20.txt

IDs of AMT workers who participated in a pilot version of the study and thus shouldn't be included in analysis (we told them not to sign up if they had been in that experiment but some did anyway)

####MODELS/