# # !pip install SpeechRecognition moviepy
# import speech_recognition as sr
# import moviepy.editor as mp

# # clip = mp.VideoFileClip(r"GOV_AL_ALDP_RILEY_FACT.wmv")
# # clip.audio.write_audiofile("dang.wav")

# r = sr.Recognizer()
# audio = sr.AudioFile("GOV_CA_WESTLY_CONSERVATION.wav")

# with audio as source: audio_file = r.record(source)

# result = r.recognize_google(audio_file)

# result

# #exporting the result
# with open('recognized.txt', mode = 'w') as file:
#     file.write("Recognized Speech:")
#         file.write("\n")
#         file.write(result)
#         print("ready!")

!pip install ibm_watson
!pip install ffmpeg moviepy


import subprocess
import os
from ibm_watson import SpeechToTextV1
from ibm_watson.websocket import RecognizeCallback, AudioSource
from ibm_cloud_sdk_core.authenticators import IAMAuthenticator   
import shutil


#convert video to audio 
video_files = os.listdir(os.curdir)

for file in video_files:
    file_name=file.rsplit('.',1)[0]
    print(file_name)
    command = 'ffmpeg -i {file_name}.wmv -ab 106k -ar 44100 -vn {file_name}.wav'.format(file_name=file_name)
    subprocess.call(command, shell=True)

# move video that has not been converted to audio to a different folder 
audio_files = os.listdir(os.curdir)

file_name=[]
for file in audio_files:
    file_name.append(file.rsplit('.',1)[0])

def countnonduplicates(file_name):
    ''' Get frequency count of duplicate elements in the given list '''
    dictOfElems = dict()
    # Iterate over each element in list
    for elem in file_name:
        # If element exists in dict then increment its value else add it in dict
        if elem in dictOfElems:
            dictOfElems[elem] += 1
        else:
            dictOfElems[elem] = 1    
    # Filter key-value pairs in dictionary. Keep pairs whose value is greater than 1 i.e. only duplicate elements from list.
    dictOfElems = { key:value for key, value in dictOfElems.items() if value == 1}
    # Returns a dict of duplicate elements and thier frequency count
    return dictOfElems

dictOfElems = countnonduplicates(file_name)     

for key in dictOfElems.items():
    key = key[0]
    shutil.move('{file}.wmv'.format(file=key),'C:/Users/grant/Desktop/Desktop/Video/2014SenateVideo_her98d/2016GovVideo_Not_Converted/{file}.wmv'.format(file=key))


# delete wav files 
all_files = os.listdir(os.curdir)
for wmv in all_files:
    try:
        wmv = wmv.rsplit('.',1)[0]
        os.remove('{file}.wmv'.format(file=wmv))
    except: 
        pass 



# apikey = 'ZpsmSHwLiew6eYX4nts9_cCS7sNOtdt1e1RVpBlMT6OQ'
# url = 'https://api.us-south.speech-to-text.watson.cloud.ibm.com/instances/d8bfa693-0584-4a5d-ba18-34fea19143af'
apikey = 'f3nTigFyaQ2OWs7JZa977mIlWMdP4UODucn1T77-QLAY'
url = 'https://api.us-south.speech-to-text.watson.cloud.ibm.com/instances/1ecfc693-15ed-454e-acc6-eb4c80550cd0'

apikey = 'zs6qXEDdbeTTsfq8sI9b9IVDARRB10D9Dj-Jm8G7byvW'
url = 'https://api.us-south.speech-to-text.watson.cloud.ibm.com/instances/52c75067-33e7-47ca-9933-ef34ceabf51b'

apikey = 'NCFwUvdzJqwR6H9je7l2jJ96ccEjwbveGN6hJaxzENUW'
url = 'https://api.us-south.speech-to-text.watson.cloud.ibm.com/instances/fd4a83fb-2e1d-4922-81be-21ba001ecd75'

apikey = 'BVAgK1BOq2ZSHP2vY9g8XIrS5uzadIvg6wJgKaIUoP4W'
url = 'https://api.us-south.speech-to-text.watson.cloud.ibm.com/instances/6321049f-431e-416a-bb91-61b42949594e'

# kakWuR_Etj_Say968LY3QRsYwmfA9zd8y0rNEcs8BHx9
# https://api.us-south.speech-to-text.watson.cloud.ibm.com/instances/990aeb6b-6cae-4252-ac26-758e1149cb18
#setup service 
authenticator = IAMAuthenticator(apikey)
stt = SpeechToTextV1(authenticator=authenticator)
stt.set_service_url(url)


#Open audio source and convert 

audio_files = os.listdir(os.curdir)

for file in audio_files:
    try:
        file_name=file.rsplit('.',1)[0]
        print(file_name)
        with open('{file}.wav'.format(file=file_name), 'rb') as audio_file:
            print(audio_file)
            res = stt.recognize(audio=audio_file, content_type='audio/wav', model='en-US_BroadbandModel', continuous=True).get_result() 
            text = [result['alternatives'][0]['transcript'].rstrip() + '.\n' for result in res['results']]
            with open('{file_name}.txt'.format(file_name=file_name), mode ='w') as out:
                out.writelines(text)
    except: 
        pass
            




#text = [result['alternatives'][0]]['transcript'].rstrip() + '.\n' for result in res['results']]
# audio_files = os.listdir(os.curdir)
# for file in audio_files:
#     file_name=file.rsplit('.',1)[0]
#     print(file_name)
#     with open('{file}.wav'.format(file=file_name), 'rb') as audio_file:
#         print(audio_file)
#         res = stt.recognize(audio=audio_file, content_type='audio/wav', model='en-US_NarrowbandModel', continuous=True).get_result() 
#         transcripts = res['results'][0]['alternatives']
#         print(transcripts)
#         combined_transcript=''
#         for transcript in transcripts:
#             print(transcript['transcript'])
#             combined_transcript += transcript['transcript']
#             # text = res['results'][0]['alternatives'][0]['transcript']
#         with open('{file_name}.txt'.format(file_name=file_name), mode ='w') as file: file.write(combined_transcript)	

# Loop through Not converted files to transcribe         
directory = ['2006/2006Federal_NotConverted', '2006/2006Gov_NotConverted', '2010/2010Gov_NotConverted', '2010/2010House_NotConverted',
             '2010/2010Senate_NotConverted', '2012/2012Gov_NotConverted', '2012/2012House_NotConverted', '2012/2012Pres_NotConverted',
             '2012/2012Senate_NotConverted', '2014/2014Gov_NotConverted', '2014/2014House_NotConverted', '2014/2014Senate_NotConverted',
             '2016/2016Gov_NotConverted', '2016/2016House_NotConverted', '2016/2016Pres_NotConverted', '2016/2016Senate_NotConverted',
             '2018/2018Gov_NotConverted', '2018/2018House_NotConverted', '2018/2018Senate_NotConverted']
for direc in directory:
    audio_files = os.listdir(direc)
    os.chdir(direc)    
    for file in audio_files:
        try:
            file_name=file.rsplit('.',1)[0]
            print(file_name)
            with open('{file}.wav'.format(file=file_name), 'rb') as audio_file:
                print(audio_file)
                res = stt.recognize(audio=audio_file, content_type='audio/wav', model='en-US_BroadbandModel', continuous=True).get_result() 
                text = [result['alternatives'][0]['transcript'].rstrip() + '.\n' for result in res['results']]
                with open('{file_name}.txt'.format(file_name=file_name), mode ='w') as out:
                    out.writelines(text)
        except: 
            pass
    os.chdir('../..')
            










