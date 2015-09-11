function [ output_args ] = mainFun( input_args )
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

%Actually for now this is just some notes on where we're going from here.
%What I want to do is to define a 2 word speaker who gets a sentence and a
%context (in the simple case the sentence is superfluous: it's always an
%SVO triple, assuming each value is uniformly sampled from the correct part
%of the context.)
%
%Given a context, I want to get back the probability that the speaker will
%choose a particular 2-word combo out of the 3.  This is pretty easy to
%define: choose each word independently!  Then p(SV) = p(S)*p(V); and then
%normalize over all 3 choices of words to include/drop. Then I would
%outside of this fn. sum those up to get p(utterance includes a word.) That
%would most clearly mirror the question we asked participants and the
%presentation of data we subsequently give, but I think I just need to
%derive that that process is just equivalent to direct reporting of p(S).
%
%Someday if we were feeling fancy, we could include costs here; if there's
%a fixed cost for every additional word included, we should be able to see
%that some scenarios (many of all 3 elements) encourage full sentences,
%while others encourage short sentences (e.g. there is only one Agent and
%only one Verb possible). 

end

