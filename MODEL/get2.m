function [includeS, includeO, includeV] = get2(probs)
    %Model a choice of 2 words,without considering order and WITHOUT
    %REPLACEMENT! This is specialized for the case of an utterance of the form
    %SVO; there's some nice abstracted form which I won't figure right now. 

    pS = probs(1);
    pO = probs(2);
    pV = probs(3);
    
    %SV (unordered)
    pSV = pS*(pV/(pO+pV)) + pV*(pS/(pO+pS));
    %VO
    pVO = pV*(pO/(pO+pS)) + pO*(pV/(pV+pS));
    %SO
    pSO = pS*(pO/(pO+pV)) + pO*(pS/(pV+pS));

    %Get probability word was mentioned, and return that!

    includeS = pSV + pSO;
    includeO = pVO + pSO;
    includeV = pSV + pVO;