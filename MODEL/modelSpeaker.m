function p = modelSpeaker(agentArray, patientArray, varargin)

p = inputParser;
p.addRequired('agentArray', @iscell);
p.addRequired('patientArray', @iscell);
p.addParamValue('verbArray', 0, @(x) true);
p.addParamValue('sentence', {'Amy', 'eat', 'apple'},@iscell);

p.parse(agentArray, patientArray, varargin{:});
inputs = p.Results;

p = inputs;

nA = length(p.agentArray);
nP = length(p.patientArray);
nV = 5; %Arbitrary! Setting to a random high-ish value for now...

%How many possible scenarios are there in our shared context? We
%assume that both speaker and listener can do this combination. 
%Assumes all combinations are possible too!

nEvents = nA*nP*nV;

%Need to calculate |w| for each word in the triple of options. For now,
%assume we always have 3 options, the 2 nouns and verb that could describe
%the event; in the future we could provide a lexicon and calculate big W
%manually. (big W = set of words that can apply to event r.  For now we are
%cheating by defining r and W together in the 'sentence' input).

%In words, |w'| is the number of scenarios r in nEvents that word w' could
%be referring to.  Again we calculate this for the simple case, but in the
%general one you could check each scenario and sum all the successes.

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
p.Produce(2) = (1/wV)*myNormalize;
p.Produce(3) = (1/wP)*myNormalize;

