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
    ReqType: string
    State: string
    Data: string list
    AllTweets: TweetDetail list 
}

type ReplyData = {
    ReqType: string
    State: string
    Data: string list
}

// type TweetReply = {
//     ReqType : string
//     Type : string
//     Status : int
//     TweetInfo : TweetInfo
// }

// type SubInfo = {
//     ReqType : string
//     UserID : int 
//     PublisherID : int
// }

// type SubReply = {
//     ReqType : string
//     Type : string
//     TargetUserID : int
//     Subscriber : int[]
//     Publisher : int[]
// }

// type ConnectInfo = {
//     ReqType : string
//     UserID : int
// }

// type QueryInfo = {
//     ReqType : string
//     UserID : int
//     Tag : string
// }

// type RetweetInfo = {
//     ReqType: string
//     UserID : int
//     TargetUserID : int
//     RetweetID : string
// }



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