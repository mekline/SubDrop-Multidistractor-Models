import csv
f = open("C:/Users/mekline/Documents/My Dropbox/_Projects/Message Compressions - Adult Experiments/Two-five distractors/MD - with verb ratings/2012-11-21-10-32-35.csv","r")
w = open("C:/Users/mekline/Documents/My Dropbox/_Projects/Message Compressions - Adult Experiments/Two-five distractors/MD - with verb ratings/2012-11-21-10-32-35CODED.csv","wb")
r = csv.DictReader(f)

header = [['word1_CODED','word2_CODED']]

#Checks for obvious choices, AGENT VERB OBJECT, otherwise marks as OTHER
for row in r:
    verb = row["verb"]
    word1 = row["word1"]
    word2 = row["word2"]
    word1 = word1.lower() #Lowercases input to avoid 'Geroge' or 'george' etc.
    word2 = word2.lower()
    if verb=='DRINK': #Uses the column labeled 'verb' to check for answers
        if (word1=='george') or (word1=='man'): #(word1=='some word') or ...etc.
            type1='AGENT'
        elif (word1=='drink') or (word1=='drinks') or (word1=='drinking'):
            type1='VERB'
        elif (word1=='juice'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='george') or (word2=='man'): #Duplicated to check 2nd word
            type2='AGENT'
        elif (word2=='drink') or (word2=='drinks') or (word2=='drinking'):
            type2='VERB'
        elif (word2=='juice'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='DROP': #Next verb...
        if (word1=='maria') or (word1=='marie') or (word1=='woman'):
            type1='AGENT'
        elif (word1=='drop') or (word1=='drops') or (word1=='dropped'):
            type1='VERB'
        elif (word1=='newspaper') or (word1=='newspapers') or (word1=='papers'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='maria') or (word2=='marie') or (word2=='woman'):
            type2='AGENT'
        elif (word2=='drop') or (word2=='drops') or (word2=='dropped'):
            type2='VERB'
        elif (word2=='newspaper') or (word2=='newspapers') or (word2=='papers'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='EAT':
        if (word1=='alexa') or (word1=='girl'):
            type1='AGENT'
        elif (word1=='eat') or (word1=='eats'):
            type1='VERB'
        elif (word1=='orange') or (word1=='fruit'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='alexa') or (word2=='girl'):
            type2='AGENT'
        elif (word2=='eat') or (word2=='eats'):
            type2='VERB'
        elif (word2=='orange') or (word2=='fruit'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='FEED':
        if (word1=='john'):
            type1='AGENT'
        elif (word1=='feed') or (word1=='feeds'):
            type1='VERB'
        elif (word1=='pet') or (word1=='dog'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='john'):
            type2='AGENT'
        elif (word2=='feed') or (word2=='feeds'):
            type2='VERB'
        elif (word2=='pet') or (word2=='dog'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='HOLD':
        if (word1=='amy') or (word1=='girl') or (word1=='lady'):
            type1='AGENT'
        elif (word1=='hold') or (word1=='holds') or (word1=='holding'):
            type1='VERB'
        elif (word1=='spoon') or (word1=='spoons'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='amy') or (word2=='girl') or (word2=='lady'):
            type2='AGENT'
        elif (word2=='hold') or (word2=='holds') or (word2=='holding'):
            type2='VERB'
        elif (word2=='spoon') or (word2=='spoons'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='KICK':
        if (word1=='emily') or (word1=='lady'):
            type1='AGENT'
        elif (word1=='kick') or (word1=='kicks') or (word1=='kicking'):
            type1='VERB'
        elif (word1=='tire') or (word1=='tires'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='emily') or (word2=='lady'):
            type2='AGENT'
        elif (word2=='kick') or (word2=='kicks') or (word2=='kicking'):
            type2='VERB'
        elif (word2=='tire') or (word2=='tires'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='POUR':
        if (word1=='ron'):
            type1='AGENT'
        elif (word1=='pour') or (word1=='pours') or (word1=='pouring') or (word1=='poured'):
            type1='VERB'
        elif (word1=='milk'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='ron'):
            type2='AGENT'
        elif (word2=='pour') or (word2=='pours') or (word2=='pouring') or (word2=='poured'):
            type2='VERB'
        elif (word2=='milk'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='READ':
        if (word1=='sam') or (word1=='man'):
            type1='AGENT'
        elif (word1=='read') or (word1=='reads') or (word1=='reading'):
            type1='VERB'
        elif (word1=='book') or (word1=='books'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='sam') or (word2=='man'):
            type2='AGENT'
        elif (word2=='read') or (word2=='reads') or (word2=='reading'):
            type2='VERB'
        elif (word2=='book') or (word2=='books'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='ROLL':
        if (word1=='zack') or (word1=='zach'):
            type1='AGENT'
        elif (word1=='roll') or (word1=='rolls') or (word1=='rolling') or (word1=='rolled'):
            type1='VERB'
        elif (word1=='ball') or (word1=='balls'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='zack') or (word2=='zach'):
            type2='AGENT'
        elif (word2=='roll') or (word2=='rolls') or (word2=='rolling') or (word2=='rolled'):
            type2='VERB'
        elif (word2=='ball') or (word2=='balls'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='THROW':
        if (word1=='sue') or (word1=='lady'):
            type1='AGENT'
        elif (word1=='throw') or (word1=='throws') or (word1=='throwing'):
            type1='VERB'
        elif (word1=='banana') or (word1=='bananas') or (word1=='fruit') or (word1=='fruits'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='sue') or (word2=='lady'):
            type2='AGENT'
        elif (word2=='throw') or (word2=='throws') or (word2=='throwing'):
            type2='VERB'
        elif (word2=='banana') or (word2=='bananas') or (word2=='fruit') or (word2=='fruits'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='TOUCH':
        if (word1=='mike') or (word1=='man'):
            type1='AGENT'
        elif (word1=='touch') or (word1=='touches') or (word1=='pets') or (word1=='petting'):
            type1='VERB'
        elif (word1=='kitten') or (word1=='cat'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='mike') or (word2=='man'):
            type2='AGENT'
        elif (word2=='touch') or (word2=='touches') or (word2=='pets') or (word2=='petting'):
            type2='VERB'
        elif (word2=='kitten') or (word2=='cat'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    elif verb=='WASH':
        if (word1=='jenn') or (word1=='girl'):
            type1='AGENT'
        elif (word1=='wash') or (word1=='washes') or (word1=='washing') or (word1=='washed'):
            type1='VERB'
        elif (word1=='bowl') or (word1=='bowls') or (word1=='dishes'):
            type1='OBJECT'
        else:
            type1='OTHER'

        if (word2=='jenn') or (word2=='girl'):
            type2='AGENT'
        elif (word2=='wash') or (word2=='washes') or (word2=='washing') or (word2=='washed'):
            type2='VERB'
        elif (word2=='bowl') or (word2=='bowls') or (word2=='dishes'):
            type2='OBJECT'
        else:
            type2='OTHER'
        header.append([type1,type2])
    else:
        header.append(['!!!','!!!'])


writer = csv.writer(w) #Writes just the corrected rows to a new document
writer.writerows(header)
f.close()
w.close()
         
