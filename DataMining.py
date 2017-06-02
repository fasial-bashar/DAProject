#OnlineResorce: https://www.youtube.com/watch?v=pUUxmvvl2FE&t=679s
#https://pythonprogramming.net/use-twitter-api-v1-1-python-stream-tweets/

#importing packages:
from tweepy import Stream
from tweepy import OAuthHandler
from tweepy.streaming import StreamListener, json
from pprint import pprint
from tweepy import Stream
from tweepy import OAuthHandler
from tweepy.streaming import StreamListener

# Twitter Authentication Info:
ckey = 'msjng3EcYPGdL19gt2npzMnZn'
csecret = 'Kx4DyXylBXnGkdHOJ2Szmm2KDyWYSA6ohVm7MgY2qw49wH7sFt'
atoken = '313278116-BOQBiDPukATJAjxbWP4BHGGgXAaf3huy1icaxgCP'
asecret = 'eoBSIy91NUVtDLQu0djYZvaPZt9ZiKoKNlfDBFIxMu758'

#Creating a Class:
class listener(StreamListener):

    def on_data(self, data):
        line_object = json.dumps(data)  # Dumps takes an object and produces a string:
        file = open('MCILIV.csv', 'a')  # Creating a .csv file to save data
        file.write(line_object+"\n")    # creating new line for each data
        file.close()                    # close after process complete
        print(data)                     # print data once connected to TwitterDB
        return(True)

    def on_error(self, status):         # Show error if not connected
        print (status)


auth = OAuthHandler(ckey, csecret)      #Twitter Authentication
auth.set_access_token(atoken, asecret)

twitterStream = Stream(auth, listener())
stream = Stream(auth, listener())
twitterStream.filter(track=["MCILIV", "ManCity", "LFC"])    #gathering data that have the given twitter hashatag in it...


