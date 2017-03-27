function probs = mainFun(whichModel, myEmpirical)
%
%This generates model predictions in a few different ways!
%
%There are two free parameters in this model: the base rates of mentioning
%S/V/O, and the expected value for the 'number of possible verbs' the 
%model is presumed to be assuming. 
%
%We set both of these parameters empirically, taking the human data split/
%half and setting/testing on opposite halves, twice. 
%
%Four models are compared here:
%
%A model that does not consider context, but just generates words at the
%baserate that that element is mentioned overall. The dummy model. 
%
%A "deterministic" model that uses success/failure to decide what to say:
%it generates 2-word utterances, checks whether any uniquely 
%identify the intended meaning in context, and emits those, or a random one if none 
%qualifies, using the baserates for each word. This model always includes the
%verb. 
%
%The 'informativity' model: From the array of subject, verbs, objects it generates
%the full hypothesis set of possible events.  It then generates two words
%without replacement, based on the number of possible scenarios that each
%word limits us to (fewer scenarios are better)
%
%The 'informativity + base' model does the same, but p(produce) =
%p(baserate) * p(informative)
%
%To compare to how the humans did it, given a context + model, I will get back 
%the probability that a speaker will choose a particular 2-word combo, then
%marginalize to get p(utterance includes word) for each of S,V,O


%PRELIMINARIES
% Get empirical baserates of each 2 word solution

humans = readtable('humandata.csv');
empiricalA = [mean(humans.p_include(1:6)), mean(humans.p_include(7:12)), mean(humans.p_include(13:18))];
empiricalB = [mean(humans.p_include(19:24)), mean(humans.p_include(25:30)), mean(humans.p_include(31:36))];

if myEmpirical == 'A'
    empirical = empiricalA;
else
    empirical = empiricalB;
end

verbParam = 5; %For now, setting this to a random value. It could also be
%empirically fit to the data

%The possible context members, with dummy values
agents = {'Amy','George','Alvin','Luke','Leia','Vader'};
patients = {'apple','pear','mango','durian','peach','plum'};
verbs = {};
for j=1:verbParam
    verbs(j) = {'dummyVerb'};
end

%The values to store
condition = {};
probs = [];

for i=1:6 
    %Choose a random 'context' with 7 total participants of the right
    %distribution
    contextS = agents(1:i);
    contextO = patients(1:(7-i));
    contextV = verbs;
    
    switch whichModel
        case 'dummycost' %Just emits 2 words at p(baseline)
            probs = [probs; empirical];
            
        case 'succeedorfail' %Always include V, include S or O if it's uniquely informative, otherwise at baserate
            includeSV = empirical(1);
            includeVO = empirical(2);
            includeSO = 0;
            
            %SV works?
            succeedS = length(contextO) == 1;
            %VO words?
            succeedO = length(contextS) == 1;
            
            if (not(succeedS + succeedO == 0)) %There is actually a successful solution! Renormalize
                includeSV = includeSV*succeedS;
                includeVO = includeVO*succeedO;
                
            end
            
            normedval = [includeSV, includeVO, includeSO]/sum([includeSV, includeVO, includeSO])
            probs = [probs; normedval];
            
        case 'succeedorfail_nobase'
            %The same, but without even a base rate!
            includeSV = 0.5;
            includeVO = 0.5;
            includeSO = 0;
            
            %SV works?
            succeedS = length(contextO) == 1;
            %VO words?
            succeedO = length(contextS) == 1;
            
            if (not(succeedS + succeedO == 0)) %There is actually a successful solution! Renormalize
                includeSV = includeSV*succeedS;
                includeVO = includeVO*succeedO;
                
            end
            
            normedval = [includeSV, includeVO, includeSO]/sum([includeSV, includeVO, includeSO])
            probs = [probs; normedval];
            
        case 'informative_nobaserate'
            
            %This yields the probability of producing [SV, VO, SO].  For
            %this evaulation, recalc to reflect whether utt includes each
            %word. 
            
            p = modelInformativeUtt(contextS, contextO, contextV);
            
            probs = [probs; p];

        case 'informative_baserate'
            %Same as above! But we scale the probability of mentioning
            %subject/object by the general tendency to include that
            %word in an utterance.
            
            p = modelInformativeUtt(contextS, contextO, contextV);
            
            rsac = p .* empirical; 
            scaledInclude = normr(rsac);
            probs = [probs; scaledInclude];
            
            
            
    end          
end


csvwrite([whichModel myEmpirical '.csv'], probs);

end



