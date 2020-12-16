module DBModel

open System
open System.Collections.Generic



type UserDetail = {
    Username : string
    PublicKey : string
    Deleted: bool
}

type TweetDetail = {
    Username : string
    TweetID : string
    Time : DateTime
    Content : string
    Hashtag : string
    Mention : string
    RetweetFrom : string
}

type ReqDetail = {
    reqType: string
    data: string list
}

type ReplyDataWithTweet = {
    reqType: string
    state: string
    data: string list
    allTweets: TweetDetail list 
}

type ReplyData = {
    reqType: string
    state: string
    data: string list
}



// key: username, value: UserDetail
let mutable usersDict = new Dictionary<string, UserDetail>()

// key: tweetID, value: TweetDetail
let mutable tweetsDict = new Dictionary<string, TweetDetail>()

// key: username, value: [tweetID]
let mutable userTweetDict = new Dictionary<string, string list>()

// key: hashtag, value: [tweetID]
let mutable hashtagsDict = new Dictionary<string, string list>()  

// key: mentioned_username, value: [tweetID that mentioned this user]        
let mutable mentionsDict = new Dictionary<string, string list>()  

// key: username, value: [following_username]        
let mutable followingsDict = new Dictionary<string, string list>()   

// key: username, value: [followers_username]
let mutable followersDict = new Dictionary<string, string list>()   