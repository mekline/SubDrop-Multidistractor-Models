import random
from willow.willow import *

def session(me):
	
	def ping():
		put({"tag":"PING", "client": me})
	
	#Make a unique paycode for this user!  Note that all paycodes end with the session number as a reality check!
	x = random.randint(0, 16777215000)
	myrand = "%x" % x
	payCode = random.choice('abcdefghij')  + myrand + random.choice('abcdefghij')
	payCode = payCode + str(me) 
	
	#Randomize order of presentation!
	items = open('MD_Stimlist_full.csv', 'r').readlines()[1:]
	random.shuffle(items)
	
	print(payCode)

	#Get all files/parameters we'll need from the stimlist
	stimNo = []
	verb = []
	sentence = []
	d1 = []
	d2 = []
	sub = []
	obj =[]
	d3 = []

	for i in xrange(len(items)):
		fields = items[i].strip().split(',')
		stimNo.append(fields[0])
		verb.append(fields[1])
		sentence.append(fields[2])
		d1.append(fields[3])
		d2.append(fields[4])
		sub.append(fields[5])
		obj.append(fields[6])
		d3.append(fields[7])
		

	#Set conditions (within subjects contrast)
	trialVersion = ["6_1", "6_1", "5_2","5_2", "4_3", "4_3","3_4","3_4","2_5","2_5","1_6","1_6"]
	random.shuffle(trialVersion)
	
	#Set materials for each trial (do this now so we don't waste any time during presentation)
	
	prepSentence = []
	exposePicture = []
	actionPicture = []
	
	for i in xrange(len(items)):
		exposePicture.append(“REDACTED/MDPictures/" + verb[i] + "_" + trialVersion[i] + ".jpg")
		actionPicture.append("REDACTED/MDPictures/" + verb[i] +".jpg")
		prepSentence.append("Get ready for the next presentation scene!")

	#And intialize the lists for the response info we collect:
	
	word1 = []
	word2 = []
	mistakeFlag = []
	responseTime = []
	
	#Okay, now we start the experiment!
	log('paycode','itemNo', 'stimNo', 'verb', 'sentence', 'trialVersion', 'prepSentence', 'exposePicture', 'actionPicture','word1', 'word2', 'mistakeFlag', 'responseTime', 'surpriseRating','giveCat')
	
	#Consent screen with button - enter the Turk code here!
	consentScreen = open("consent.html").read()
	consentScreen = consentScreen.replace("QQQ", "SNAZZY POTATO")
	add(consentScreen)
	take({"tag": "click", "id": "ConsentButton", "client": me})
	let("")
	
	#Show instruction screen with button
	instructionScreen = open("instructions.html").read()
	instructionScreen = instructionScreen.replace("QQQ", "SNAZZY POTATO")
	add(instructionScreen)
	take({"tag": "click", "id": "InstructButton", "client": me})
	let("")
	

	#Wait 1 second before displaying first trial!
	
	background(ping, 1)
	take({"tag":"PING", "client": me})
	
	#Cycle through trial presentation/exposure

	for i in xrange(len(items)):
		let("") #Make sure screen is clear!
	
		########
		#PREP
		########
		
		# Show prep sentence
		prepScreen = open("prepare_sentence.html").read()
		prepScreen = prepScreen.replace("ZZZ", prepSentence[i])
		add(prepScreen)
		
		#Wait 3 seconds
		background(ping,3)
		take({"tag":"PING", "client": me})
		
		#And move on
		let("")
		
		########
		#EXPOSE
		########
		
		#Show exposure screen
		exposeScreen = open("expose.html").read()
		exposeScreen = exposeScreen.replace("ZZZ", exposePicture[i])
		add(exposeScreen)
		
		#Wait 10 seconds
		background(ping,10)
		take({"tag":"PING", "client": me})
		
		#And move on
		let("")
		
		########
		#PREP-Action
		########
		
		# Show prep sentence
		prepScreen = open("prepare_action.html").read()
		prepScreen = prepScreen.replace("ZZZ", sentence[i])
		add(prepScreen)
		
		#Wait 3 seconds
		background(ping,3)
		take({"tag":"PING", "client": me})
		
		#And move on
		let("")
		
		########
		#ACTION
		########
		
		#Show action screen
		actionScreen = open("action.html").read()
		actionScreen = actionScreen.replace("ZZZ", actionPicture[i])
		actionScreen = actionScreen.replace("XXX", sentence[i])
		add(actionScreen)
		
		#Wait 8 seconds
		background(ping,10)
		take({"tag":"PING", "client": me})
		
		
		########
		#TEST
		########
		
		#Record the start time (how long will it take the user to pass out of the test loop?)
		let("", timestamp=True)
		clock0 = take({"tag":"timestamp", "client": me})["time"]
		
		gotAnswers = 0
		mistakeFlag.append('none')
		
		#Make the little vector of options
		#mywords = [verb[i], d1[i], d2[i], sub[i], obj[i]]
		#random.shuffle(mywords)
		
		while not gotAnswers:
			#Show test screen
			let("")
			testScreen = open("test.html").read()
				
			testScreen = testScreen.replace("AAA", sentence[i])
			testScreen = testScreen.replace("ZZZ", exposePicture[i])
			add(testScreen)
			
			#Wait for user to click the done button
			take({"tag": "click", "id": "TrialButton", "client": me})
		
			
			#Check whether we got legal answers!            
			word1.append(peek("#Word1"))
			word2.append(peek("#Word2"))
			
			word1[i] = word1[i].strip()
			word2[i] = word2[i].strip()
			
			if (len(word1[i].split()) == 1) and (len(word2[i].split()) == 1): #Single word
				if (len(word1[i]) > 1) and  (len(word2[i]) > 1): #Put something reasonable length 
					gotAnswers = 1
			
			if gotAnswers:
				continue
			else:
				#We made a mistake! Mark it
				mistakeFlag[i] = 'bad input'
				#Tell the user!
				let("")
				add(open("bad_response.html"))
				background(ping,2)
				take({"tag":"PING", "client": me})
				
				#And clear those previous word responses!
				word1.pop()
				word2.pop()
				
				
			
		 #Calculate how long it took to get here
		let("", timestamp=True)
		clock1 = take({"tag":"timestamp", "client": me})["time"]

		responseTime.append((float(clock1)-float(clock0))/1000)
		
		 ########
		 #FEEDBACK
		 ########
		
		 #Show feedback screen
		feedbackScreen = open("good_response.html").read()
		feedbackScreen = feedbackScreen.replace("ZZZ", str(responseTime[i]))
		feedbackScreen = feedbackScreen.replace("YYY", str(float(sum(responseTime)) / len(responseTime)))
		
		feedbackScreen = feedbackScreen.replace("XXX", str(i+1))
		feedbackScreen = feedbackScreen.replace("PPP", str(100*float(i+1)/len(items)))
		add(feedbackScreen)
		
		#Wait 5 seconds
		background(ping,5)
		take({"tag":"PING", "client": me})
		
		########
		#RECORD
		########
		
		log(payCode, i, stimNo[i], verb[i], sentence[i], trialVersion[i], prepSentence[i], exposePicture[i], actionPicture[i],word1[i], word2[i], mistakeFlag[i], responseTime[i])

	#End of trial loop! 
	
	########
	#Verb Ratings Task!
	########

	#Re-randomize trial orders!!
	random.shuffle(items)
	
	stimNo = []
	verb = []
	set = []
	prefix = []

	for i in xrange(len(items)):
		fields = items[i].strip().split(',')
		stimNo.append(fields[0])
		verb.append(fields[1])
		set.append(fields[8])
		prefix.append(fields[9])
		
	
	#Instructions
	
	instructionScreen = open("verb_instructions.html").read()
	add(instructionScreen)
	take({"tag": "click", "id": "VerbButton", "client": me})
	let("")
	
	#Run the task
	for i in xrange(len(items)):
		
		let("")
	
		#Get the contrast set objects!
		myset = set[i].split('.')
		random.shuffle(myset)
	
		verbScreen = open("verb_question.html").read()
		verbScreen = verbScreen.replace("XXX", verb[i])
		replacement = prefix[i]+"_"+ myset[0]
		verbScreen.replace("111", replacement)
	
		for j in xrange(len(myset)):
			toreplace = str(j+1) + str(j+1) + str(j+1)
			verbScreen = verbScreen.replace(toreplace, prefix[i]+'_'+myset[j])
		
		add(verbScreen)
		
		response = None
		complete = 0
		while (not(complete)): #Need to escape the training check
			
			#If we don't have  a response, get one
			if (response == None):
				response = take({"tag":"click", "client":me,"id":"1"},
						{"tag":"click", "client":me,"id":"2"},
						{"tag":"click", "client":me,"id":"3"},
						{"tag":"click", "client":me,"id":"4"},
						{"tag":"click", "client":me,"id":"5"},
						{"tag":"click", "client":me,"id":"6"},
						{"tag":"click", "client":me,"id":"7"})["id"] 
			else:#If we have a response, wait for a new radio button OR a go-on signal
				sig = take({"tag":"click", "client":me,"id":"GoOnButton"},
						{"tag":"click", "client":me,"id":"1"},
						{"tag":"click", "client":me,"id":"2"},
						{"tag":"click", "client":me,"id":"3"},
						{"tag":"click", "client":me,"id":"4"},
						{"tag":"click", "client":me,"id":"5"},
						{"tag":"click", "client":me,"id":"6"},
						{"tag":"click", "client":me,"id":"7"})["id"] 
				if (not(sig == "GoOnButton")):
					response = sig
				else: #we got a Go click!
					checkCat = peek("#checkCatMember")
					complete = 1
					
		########
		#RECORD
		########
		
		log(payCode, i, stimNo[i], verb[i], 0, 0,0, 0, 0,0, 0, 0,0,response, checkCat)
				
		let("")
	
	#End of verb surprisal loop!
		

	########
	#THANK YOU
	########
	let("")
	thanks = open("thankyou.html", "r").read()
	thanks = thanks.replace("YYY", payCode)
	add(thanks)
		
		
	

run(session, 4999)
