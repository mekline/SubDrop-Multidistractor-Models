function p = modelInformativeWord(contextS, contextO, contextV)
            
    %The real model!
    
    nS = length(contextS);
    nO = length(contextO);
    nV = length(contextV); %Currently a free parameter of how many verbs are assumed is set here! We try this with v = 5 and v = 50

    %Need to calculate |w| (% of possible event referents) for each word in the triple of options. For now,
    %assume we always have 3 options (SVO) to describe the target event; in the future we could provide 
    %a lexicon and calculate big W manually. (big W = set of words that
    %could be spoken)
    %.  For now we are cheating by defining r and W together with the 'sentence' input).

    %In words, |w'| is the number of scenarios r in nEvents that word w' could
    %be referring to.  Again we calculate this for the simple case described, but in the
    %general one you could check whether each possible scenario can be 
    %described by w' and sum all the successes.

    wS = nO*nV; 
    wO = nS*nV;
    wV = nS*nO;

    %single words are produced inverse to the # of scenarios they
    %describe.  Normalize it!

    p = [0 0 0];

    myNormalize = 1/(1/wS + 1/wO + 1/wV);

    p(1) = (1/wS)*myNormalize;
    p(2) = (1/wO)*myNormalize;
    p(3) = (1/wV)*myNormalize;