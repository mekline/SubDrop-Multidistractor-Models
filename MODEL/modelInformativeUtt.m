function p = modelInformativeUtt(contextS, contextO, contextV)
            
    %The rsa model!
    
    nS = length(contextS);
    nO = length(contextO);
    nV = length(contextV); %Currently a free parameter of how many verbs are assumed is set here! We try this with v = 5 and v = 50, using v=5 for demonstration

    nEvents = nS*nO*nV;
    
    %Need to calculate |d| (% of possible event referents) for each
    %possible 2 word utterance (SO, SV, OV (unordered). Eg. f we produce the
    %word S, we limit the possible events in D to ones with that S value.
    %In words, |d'| is the number of scenarios e in nEvents that utterance d' could
    %be referring to: 

    dSV = nO; 
    dVO = nS;
    dSO = nV;

    %Under RSA, utterances are produced inverse to the # of scenarios they
    %describe.  Normalize those probabilities and send them back

    p = [0 0 0];

    myNormalize = 1/(1/dSV + 1/dVO + 1/dSO);

    p(1) = (1/dSV)*myNormalize;
    p(2) = (1/dVO)*myNormalize;
    p(3) = (1/dSO)*myNormalize;