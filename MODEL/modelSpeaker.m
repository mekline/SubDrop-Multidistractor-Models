function p = modelSpeaker(agentArray, patientArray, varargin)
%modelSpeaker takes in a representation of context (array of agents,
%patients, and optionally verbs) and optionally a specific sentence, and
%returns the probability that the speaker would choose to generate each
%individual word in the sentence (no function words allowed!)

p = inputParser;
p.addRequired('agentArray', @iscell);
p.addRequired('patientArray', @iscell);
p.addParamValue('verbArray', {'eat', 'throw', 'send'}, @(x) true); 
p.addParamValue('sentence', {'Amy', 'eat', 'apple'},@iscell);

p.parse(agentArray, patientArray, varargin{:});
inputs = p.Results;
p = inputs;

%Get the relevant numbers for calculation!
nA = length(p.agentArray);
nP = length(p.patientArray);
nV = length(p.verbArray); %Arbitrary if not supplied!! %Free parameter of how many verbs are assumed is set here! We try this with v = 5 and v = 50

%How many possible scenarios are there in our shared context? We
%assume that both speaker and listener can do this combination. 
%Assumes all agent-verb-patient combinations are possible too!
nEvents = nA*nP*nV;

%Need to calculate |w| for each word in the triple of options. For now,
%assume we always have 3 options (SVO) to describe event r; in the future we could provide 
%a lexicon and calculate big W manually. (big W = set of words that can 
%apply to event r.  For now we are cheating by defining r and W together with the 'sentence' input).

%In words, |w'| is the number of scenarios r in nEvents that word w' could
%be referring to.  Again we calculate this for the simple case described, but in the
%general one you could check whether each possible scenario can be 
%described by w' and sum all the successes.

wA = nP*nV;
wP = nA*nV;
wV = nA*nP;

%Now we can spit out the probability that a single word would be produced!
%Agan, here we just manually assume that there are 3 words, one S one V and
%one O, but we could extend this to the case where we were fed an arbitrary
%set of words and searched the lexicon for them.  

p.Produce = [0 0 0];

myNormalize = 1/(1/wA + 1/wP + 1/wV);

p.Produce(1) = (1/wA)*myNormalize;
p.Produce(2) = (1/wP)*myNormalize;
p.Produce(3) = (1/wV)*myNormalize;

