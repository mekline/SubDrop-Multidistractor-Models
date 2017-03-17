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
% Get empirical baserates!

humans = readtable('Human_split_Data.csv');
empiricalA = [mean(humans.pInclude(1:6)), mean(humans.pInclude(7:12)), mean(humans.pInclude(13:18))];
empiricalB = [mean(humans.pInclude(19:24)), mean(humans.pInclude(25:30)), mean(humans.pInclude(31:36))];

if (strmatch(myEmpirical, 'A'))
    empirical = empiricalA 
else
        empirical = empiricalB
end
%Scale those so they add to 2 (normalizing out the small % of non-SOV words
%that humans produce); this gives p(word is included in 2 word phrase). 
empirical = 2*empirical/sum(empirical);

verbParam = 5; %For now, setting this to a random value. Later it will be
%empirically fit to the data (?)


%EmpiricalA/B represent 2 draws without replacement from a weighted system.
%Let's try and get approximate numerical solutions for the weights!


syms pS pO pV
S = vpasolve([empirical(1) == pS + pV*(pS/(1-pV)) + pO*(pS/(1-pO)), ... 
    empirical(2) == pO + pS*(pO/(1-pS)) + pV*(pO/(1-pV)), ...
    empirical(3) == pV + pS*(pV/(1-pS)) + pO*(pV/(1-pO))], ...
    [pS, pO, pV]);

%Happily in both cases the above has a single soln with all reasonable 
%probability distributions :)

if (strmatch(myEmpirical, 'A'))
    pS = S.pS(4);
    pO = S.pO(4);
    pV = S.pV(4);
    
else
    pS = S.pS(5);
    pO = S.pO(5);
    pV = S.pV(5);
end

% A full context (from which individual contexts can be selected) with
%dummy values; we'll cycle through these to evaluate individual predictions

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
        case 'dummy' %Just emits 2 words at p(baseline)
            [includeS, includeO, includeV] = get2([pS,pO,pV]);
            probs = [probs; includeS, includeO, includeV]; %Sanity check! This should give back the empirical observations, and they do.
            
        case 'succeedorfail' %Try both combinations! (unordered, always includes V)
            includeV = 1;
            
            %SV works?
            succeedS = length(contextO) == 1;
            %VO words?
            succeedO = length(contextS) == 1;
            
            %Normalize baserates to S and O
            includeS = empirical(1)/(empirical(1) + empirical(2));
            includeO = empirical(2)/(empirical(1) + empirical(2));
            
            if (not(succeedS + succeedO == 0)) %There is a successful solution! Renormalize
                pSV = includeS*succeedS;
                pVO = includeO*succeedO;
                
                includeS = pSV/(pSV+pVO);
                includeO = pVO/(pSV+pVO);
            end
            
            probs = [probs; includeS, includeO, includeV];
            
        case 'informative_nobaserate'
            
            p = modelInformativeWord(contextS, contextO, contextV);
       
            %But actually, we produce two words! They are sampled without
            %replacement according to those probabilities. 
            [includeS, includeO, includeV] = get2(p);
            probs = [probs; includeS, includeO, includeV];

        case 'informative_baserate'
            %Same as above! But we scale the probability of mentioning
            %subject/object by the general tendency to include that
            %word in an utterance.
            
            p = modelInformativeWord(contextS, contextO, contextV);
            [includeS, includeO, includeV] = get2(p);
            
             
            scaledInclude = [includeS, includeO, includeV] + empirical; 
            scaledInclude = 2*scaledInclude/sum(scaledInclude);
            probs = [probs; scaledInclude];
            
            
            
    end          
end


csvwrite(['Model_' whichModel myEmpirical '.csv'], probs);

end



