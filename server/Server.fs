open System
open WebSocketSharp.Server
open WebSocketSharp
open Akka.Actor
open Akka.FSharp
open DBModel
open FSharp.Json

type ReqDetail = {
    reqType: string
    data: string list
}

type WsType = {
    reqType: string
    data: string
    wsSessionManager: WebSocketSessionManager
    wsSessionID: string
}


let system = ActorSystem.Create("FSharp")
let wssv = WebSocketServer("ws://localhost:9001")

let isValidUser username = 
    (usersDict.ContainsKey(username) && not usersDict.[username].Deleted) 

let serverActor = 
    spawn system "TwitterServer" 
    <| fun mailbox ->
        let mutable onlineUserSet = Set.empty
        let updateOnlineUserDB username option = 
            let mutable ret = ""
            let isConnected = onlineUserSet.Contains(username)
            if option = "connect" && not isConnected then
                if isValidUser username then
                    onlineUserSet <- onlineUserSet.Add(username)
                    ret <- "200 OK"
                else
                    ret <- "404 Not Found"
            else if option = "disconnect" && isConnected then
                onlineUserSet <- onlineUserSet.Remove(username)
                ret <- "200 OK"
            else
                ret <- "200 OK"
            ret

        let rec loop() =
            actor {
                let! message = mailbox.Receive()
                let info = Json.deserialize<WsType> message
                let req = info.reqType
                let data = Json.deserialize<ReqDetail> info.data
                let client = info.wsSessionManager
                let clientWsID = info.wsSessionID
                match req with
                | "Register" ->
                    let mutable returnMsg = "400 Bad Request"
                    let username = data.data.[0]

                    // get proper data to create user
                    let reqData: UserDetail = {
                        Username = username
                        PublicKey = data.data.[1]
                        Deleted = false
                    }

                    // do register stuff
                    if not(usersDict.ContainsKey(username)) then
                        usersDict.Add(username, reqData)
                        returnMsg <- "200 OK"

                    // res
                    let retData: ReplyData = {
                        ReqType = "Register"
                        State = returnMsg
                        Data = [username]
                    }
                    let res = Json.serialize retData
                    client.SendTo(res, clientWsID)

                | "Tweet" ->
                    let mutable returnMsg = "400 Bad Request"
                    let username = data.data.[0]
                    let tweetID = string(tweetsDict.Count + 1)
                    let tweetContent = data.data.[1]
                    let tag = data.data.[2]
                    let mentioned = data.data.[3]
                    let time = data.data.[4]

                    // get proper data to create tweet
                    let reqData: TweetDetail = {
                        Username = username
                        TweetID = tweetID
                        Time = System.DateTime.Parse time
                        Content = tweetContent
                        Hashtag = tag
                        Mention = mentioned
                        RetweetFrom = username
                    }

                    // do send tweet stuff
                    if isValidUser username then
                        tweetsDict.Add(tweetID, reqData)
                        if userTweetDict.ContainsKey(username) then
                            userTweetDict.[username] <- userTweetDict.[username] @ [tweetID]
                        else
                            userTweetDict.Add(username, [tweetID])

                        if tag <> "" && tag.[0] = '#' then
                            if not (hashtagsDict.ContainsKey(tag)) then
                                hashtagsDict.Add(tag, [tweetID])
                            else
                                hashtagsDict.[tag] <- hashtagsDict.[tag]@ [tweetID]
                        if mentioned <> "" then
                            if not (mentionsDict.ContainsKey(mentioned)) then
                                mentionsDict.Add(mentioned, [tweetID])
                            else
                                mentionsDict.[mentioned] <- mentionsDict.[mentioned] @ [tweetID]
                        returnMsg <- "200 OK"
                    else   
                        returnMsg <- "404 Not Found"

                    // res
                    let data: ReplyData = {
                        ReqType = "Tweet"
                        State = returnMsg
                        Data = [username; tweetContent]
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)

                | "Retweet" ->
                    let mutable returnMsg = "400 Bad Request"
                    let username = data.data.[0]
                    let tweetID = string(tweetsDict.Count + 1)
                    let tag = data.data.[2]
                    let mentioned = data.data.[3]
                    let retweetFrom = tweetsDict.[data.data.[5]].Username
                    let tweetContent = "Retweet from " + retweetFrom + ": " + data.data.[1]
                    

                    // get proper data to re-tweet
                    let reqData: TweetDetail = {
                        Username = username
                        TweetID = tweetID
                        Time = System.DateTime.Parse data.data.[4]
                        Content = tweetContent
                        Hashtag = tag
                        Mention = mentioned
                        RetweetFrom = retweetFrom
                    }

                    // do re-tweet stuff
                    if isValidUser username then
                        tweetsDict.Add(tweetID, reqData)

                        if userTweetDict.ContainsKey(username) then
                            userTweetDict.[username] <- userTweetDict.[username] @ [tweetID]
                        else
                            userTweetDict.Add(username, [tweetID]) 

                        if tag <> "" && tag.[0] = '#' then
                            if not (hashtagsDict.ContainsKey(tag)) then
                                hashtagsDict.Add(tag, [tweetID])
                            else
                                hashtagsDict.[tag] <- hashtagsDict.[tag] @ [tweetID]

                        if mentioned <> "" then
                            if not (mentionsDict.ContainsKey(mentioned)) then
                                mentionsDict.Add(mentioned, [tweetID])
                            else
                                mentionsDict.[mentioned] <- mentionsDict.[mentioned] @ [tweetID]
                        returnMsg <- "200 OK"

                    else
                        returnMsg <- "404 Not Found"

                    // res
                    let data: ReplyData = {
                        ReqType = "Tweet"
                        State = returnMsg
                        Data = [username; tweetContent; retweetFrom]
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)
                
                | "AddToFollowings" ->
                    let mutable returnMsg = "400 Bad Request"
                    let username = data.data.[0]
                    let other = data.data.[1]

                    // user followr other
                    if isValidUser username && isValidUser other && username <> other then
                        if not (followersDict.ContainsKey(other)) then
                            followersDict.Add(other, [username])
                        if not (List.contains username followersDict.[other]) then
                            followersDict.[other] <- followersDict.[other] @ [username]

                        if not (followingsDict.ContainsKey(username)) then
                            followingsDict.Add(username, [other])
                        if not (List.contains other followingsDict.[username]) then
                            followingsDict.[username] <- followingsDict.[username] @ [other]

                        returnMsg <- "200 OK"
                    else
                        returnMsg <- "404 Not Found"

                    // res
                    let data: ReplyData = {
                        ReqType = "Subscribe"
                        State = returnMsg
                        Data = [username; other]
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)

                | "QueryHashtag" ->
                    let mutable returnMsg = "400 Bad Request"
                    let mutable returnTweets = List.empty<TweetDetail>
                    let tag = data.data.[0]
                    if hashtagsDict.ContainsKey(tag) then
                        let tweetsList = hashtagsDict.[tag]
                        for t in tweetsList do
                            if tweetsDict.ContainsKey(t) then
                                returnTweets <- returnTweets @ [tweetsDict.[t]]
                                returnMsg <- "200 OK"
                    else
                        returnMsg <- "404 Not Found"
                    
                    // res
                    let data: ReplyDataWithTweet = {
                        ReqType = "QueryHashtag"
                        State = returnMsg
                        Data = [tag]
                        AllTweets = returnTweets
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)
                    
                | "QueryMentioned" ->
                    let mutable returnMsg = "400 Bad Request"
                    let mutable returnTweets = List.empty<TweetDetail>
                    let mentionedUser = data.data.[0]
                    if mentionsDict.ContainsKey(mentionedUser) then
                        let tweetsList = mentionsDict.[mentionedUser]
                        for t in tweetsList do
                            if tweetsDict.ContainsKey(t) then
                                returnTweets <- returnTweets @ [tweetsDict.[t]]
                                returnMsg <- "200 OK"
                    else
                        returnMsg <- "404 Not Found"
                    
                    // res
                    let data: ReplyDataWithTweet = {
                        ReqType = "QueryMentioned"
                        State = returnMsg
                        Data = [mentionedUser]
                        AllTweets = returnTweets
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)

                | "GetAllTweetsFromSubscriber" ->
                    let mutable returnMsg = "400 Bad Request"
                    let mutable returnTweets = List.empty<TweetDetail>
                    let username = data.data.[0]
                    let mutable followingList = List.empty<string>

                    if isValidUser username && followingsDict.ContainsKey(username) then
                        followingList <- followingsDict.[username]
                    let oneFollowing = followingList.[Random().Next(followingList.Length)]
                    if isValidUser oneFollowing && userTweetDict.ContainsKey(oneFollowing) then
                        for t in userTweetDict.[oneFollowing] do
                            returnTweets <- returnTweets @ [tweetsDict.[t]]
                            returnMsg <- "200 OK"
                    else
                        returnMsg <- "404 Not Found"

                    // res
                    let data: ReplyDataWithTweet = {
                        ReqType = "GetAllTweetsFromSubscriber"
                        State = returnMsg
                        Data = [username; oneFollowing]
                        AllTweets = returnTweets
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)

                | "Reconnect" ->
                    let mutable returnMsg = "400 Bad Request"
                    let mutable returnTweets = List.empty<TweetDetail>
                    let username = data.data.[0]

                    if isValidUser username then
                        returnMsg <- updateOnlineUserDB username "connect"
                        for t in userTweetDict.[username] do
                            returnTweets <- returnTweets @ [tweetsDict.[t]]

                    // res
                    let data: ReplyDataWithTweet = {
                        ReqType = "Reconnect"
                        State = returnMsg
                        Data = [username]
                        AllTweets = returnTweets
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)

                | "Disconnect" ->
                    let mutable returnMsg = "400 Bad Request"
                    let username = data.data.[0]

                    if isValidUser username then
                        returnMsg <- (updateOnlineUserDB username "disconnect")

                    // res
                    let data: ReplyData = {
                        ReqType = "Disconnect"
                        State = returnMsg
                        Data = [username]
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)

                | "Delete" ->
                    let mutable returnMsg = "400 Bad Request"
                    let username = data.data.[0]

                    // get proper data to delete user
                    let reqData: UserDetail = {
                        Username = username
                        PublicKey = data.data.[1]
                        Deleted = true
                    }

                    if isValidUser username then
                        usersDict.[username] <- reqData
                        returnMsg <- "200 OK"
                    else
                        returnMsg <- "404 Not Found"

                    // res
                    let data: ReplyData = {
                        ReqType = "Delete"
                        State = returnMsg
                        Data = [username]
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)
                
                | "GetLiveView" ->
                    let mutable returnMsg = "400 Bad Request"
                    let mutable returnTweets = List.empty<TweetDetail>
                    let username = data.data.[0]

                    if isValidUser username && userTweetDict.ContainsKey(username) then
                        for t in userTweetDict.[username] do
                            returnTweets <- returnTweets @ [tweetsDict.[t]]
                            returnMsg <- "200 OK"
                    else
                        returnMsg <- "404 Not Found"

                    // res
                    let data: ReplyDataWithTweet = {
                        ReqType = "GetLiveView"
                        State = returnMsg
                        Data = [username]
                        AllTweets = returnTweets
                    }
                    let res = Json.serialize data
                    client.SendTo(res, clientWsID)

                    | _ ->  failwith "Unknown Message"
                    return! loop()
            }
        loop()     


type Register () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "Register"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData

type Tweet () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "Tweet"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData

type Retweet () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "Retweet"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData

type AddToFollowings () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "AddToFollowings"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData
    
type QueryHashtag () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "QueryHashtag"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData

type QueryMentioned () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "QueryMentioned"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData

type GetAllTweetsFromSubscriber () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "GetAllTweetsFromSubscriber"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData

type Reconnect () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "Reconnect"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData

type Disconnect () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "Disconnect"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData

type Delete () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "Delete"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData

type GetLiveView () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        let wsJson: WsType = {
            reqType = "GetLiveView"
            data = message.Data
            wsSessionManager = x.Sessions
            wsSessionID = x.ID
        }
        let wsData = Json.serialize wsJson
        serverActor <! wsData


[<EntryPoint>]
let main argv =

    wssv.AddWebSocketService<Register> ("/Register")
    wssv.AddWebSocketService<Tweet> ("/Tweet")
    wssv.AddWebSocketService<Retweet> ("/Retweet")
    wssv.AddWebSocketService<AddToFollowings> ("/AddToFollowings")
    wssv.AddWebSocketService<QueryHashtag> ("/QueryHashtag")
    wssv.AddWebSocketService<QueryMentioned> ("/QueryMentioned")
    wssv.AddWebSocketService<GetAllTweetsFromSubscriber> ("/GetAllTweetsFromSubscriber")
    wssv.AddWebSocketService<Reconnect> ("/Reconnect")
    wssv.AddWebSocketService<Disconnect> ("/Disconnect")
    wssv.AddWebSocketService<Delete> ("/Delete")
    wssv.AddWebSocketService<GetLiveView> ("/GetLiveView")

    wssv.Start ()
    printfn "Server start"
    Console.ReadLine() |> ignore

    wssv.Stop ()

    0 // return an integer exit code