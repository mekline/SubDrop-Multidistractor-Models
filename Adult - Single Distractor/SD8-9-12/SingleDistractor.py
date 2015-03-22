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
	items = open('SD_Stimlist_full.csv', 'r').readlines()[1:]
	random.shuffle(items)
	
	print(payCode)

	#Get all files/parameters we'll need from the stimlist
	stimNo = []
	verb = []
	sentence = []
	presentAgent = []
	presentPatient = []
	presentItems = []

	for i in xrange(len(items)):
		fields = items[i].strip().split(',')
		stimNo.append(fields[0])
		verb.append(fields[1])
		sentence.append(fields[2])
		presentAgent.append(fields[3])
		presentPatient.append(fields[4])
		presentItems.append(fields[5])

	#Set conditions (within subjects contrast)
	trialVersion = ["agent", "agent", "agent","patient", "patient", "patient"]
	random.shuffle(trialVersion)
	
	#Set materials for each trial (do this now so we don't waste any time during presentation)
	
	prepSentence = []
	exposePicture = []
	actionPicture = []
	
	for i in xrange(len(items)):
		exposePicture.append("http://tedlab.mit.edu/~mekline/Stimuli/SubDrop_SDPictures/" + verb[i] + "-" + trialVersion[i] + "-expose.jpg")
		print(exposePicture)
		actionPicture.append("http://tedlab.mit.edu/~mekline/Stimuli/SubDrop_SDPictures/" + verb[i] + "-" + trialVersion[i] + "-action.jpg")
		if (trialVersion[i] == "agent"):
			prepSentence.append("In the next scene, " + presentAgent[i] + ", " +  presentItems[i])
		elif (trialVersion[i] == "patient"):
			prepSentence.append("In the next scene, " + presentPatient[i] +", " + presentItems[i])
		else:
			print 'shuffle error!'

	#And intialize the lists for the response info we collect:
	
	word1 = []
	word2 = []
	mistakeFlag = []
	responseTime = []
	
	#Okay, now we start the experiment!
	log('paycode','itemNo', 'stimNo', 'verb', 'sentence', 'trialVersion', 'prepSentence', 'exposePicture', 'actionPicture','word1', 'word2', 'mistakeFlag', 'responseTime')
	
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
		
		#Wait 5 seconds
		background(ping,5)
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
		
		#Wait 5 seconds
		background(ping,5)
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
		
		#Wait 5 seconds
		background(ping,5)
		take({"tag":"PING", "client": me})
		
		
		########
		#TEST
		########
		
		#Record the start time (how long will it take the user to pass out of the test loop?)
		let("", timestamp=True)
		clock0 = take({"tag":"timestamp", "client": me})["time"]
		
		gotAnswers = 0
		mistakeFlag.append('none')
		
		while not gotAnswers:
			#Show test screen
			let("")
			testScreen = open("test.html").read()
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
		feedbackScreen = feedbackScreen.replace("PPP", str(100*float(i+1)/6))
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
	#THANK YOU
	########
	let("")
	thanks = open("thankyou.html", "r").read()
	thanks = thanks.replace("YYY", payCode)
	add(thanks)
		
		
	

run(session, 4999)
