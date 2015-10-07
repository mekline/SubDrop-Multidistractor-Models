function probMatrix = mainFun(verbParam)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

%Actually for now this is just some notes on where we're going from here.
%
%From how the humans did it, given a context, I want to get back the probability that the speaker will
%choose a particular 2-word combo out of the 3.  Then
%outside of this fn. sum those up to get p(utterance includes a word.) 
%
%Someday if we were feeling fancy, we could include costs here; if there's
%a fixed cost for every additional word included, we should be able to see
%that some scenarios (many of all 3 elements) encourage full sentences,
%while others encourage short sentences (e.g. there is only one Agent and
%only one Verb possible). 

agents = {'Amy','George','Alvin','Luke','Leia','Vader'};
patients = {'apple','pear','mango','durian','peach','plum'};
verbs = {};
for j=1:verbParam
    verbs(j) = {'dummyVerb'};
end


probMatrix = zeros(3,6);
for i=1:6 %Choose a context with 7 total participants
    contextA = agents(1:i);
    contextP = patients(1:(7-i));
    probs = modelSpeaker(contextA, contextP, 'verbArray', verbs);



    pA = probs.Produce(1);
    pP = probs.Produce(2);
    pV = probs.Produce(3);

    %Model a choice of 2 words,without considering order and WITHOUT
    %REPLACEMENT! This is specialized for the case of an utterance of the form
    %SVO; there's some nice abstracted form which I won't figure right now. 

    %SV (AV)
    pSV = pA*(pV/(pP+pV)) + pV*(pA/(pP+pA));
    %VO (VP)
    pVO = pV*(pP/(pP+pA)) + pP*(pV/(pV+pA));
    %SO (AP)
    pSO = pA*(pP/(pP+pV)) + pP*(pA/(pV+pA));

    %Get probability word was mentioned, and return that!

    includeA = pSV + pSO;
    includeP = pVO + pSO;
    includeV = pSV + pVO;
    
    probMatrix(:,i) = [includeA includeP includeV];
    
end

csvwrite(['Model_' num2str(verbParam) '.csv'], probMatrix);

end

