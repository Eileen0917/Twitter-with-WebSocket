﻿open System
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

type ActorMsg = 
    | ServerRegister of string * WebSocketSessionManager * string
    | ServerTweet of string * WebSocketSessionManager * string
    | ServerRetweet of string * WebSocketSessionManager * string
    | ServerAddToFollowings of string * WebSocketSessionManager * string
    | ServerQueryHashtag of string * WebSocketSessionManager * string
    | ServerQueryMentioned of string * WebSocketSessionManager * string
    | ServerReconnect  of string * WebSocketSessionManager * string
    | ServerDisconnect of string * WebSocketSessionManager * string
    | ServerDelete of string * WebSocketSessionManager * string
    | ServerGetLiveView of string * WebSocketSessionManager * string
    | ServerGetAllTweetsFromSubscriber of string * WebSocketSessionManager * string


let system = ActorSystem.Create("FSharp")
let wssv = WebSocketServer("ws://localhost:9000")

let isValidUser username = 
    (usersDict.ContainsKey(username) && not usersDict.[username].Deleted) 

let serverActorNode (serverMailbox:Actor<ActorMsg>) =

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

    let rec loop() = actor {
        let! (message: ActorMsg) = serverMailbox.Receive()

        match message with
        | ServerRegister (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
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
                reqType = "Register"
                state = returnMsg
                data = [username]
            }
            let res = Json.serialize retData
            client.SendTo(res, clientWsID)

        | ServerTweet (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
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
                reqType = "Tweet"
                state = returnMsg
                data = [username; tweetContent]
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)

        | ServerRetweet (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
            let mutable returnMsg = "400 Bad Request"
            let username = data.data.[0]
            let retweetID = data.data.[1]
            let retweetFrom = tweetsDict.[retweetID].Username
            let tweetID = string(tweetsDict.Count + 1)
            let tag = tweetsDict.[retweetID].Hashtag
            let mentioned = tweetsDict.[retweetID].Mention
            let tweetContent = "Retweet from " + retweetFrom + ": " + tweetsDict.[retweetID].Content

            // get proper data to re-tweet
            let reqData: TweetDetail = {
                Username = username
                TweetID = tweetID
                Time = System.DateTime.Parse data.data.[2]
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
                reqType = "Tweet"
                state = returnMsg
                data = [username; tweetContent; retweetFrom]
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)

        | ServerAddToFollowings (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
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
                reqType = "Subscribed"
                state = returnMsg
                data = [username; other]
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)

        | ServerQueryHashtag (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
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
                reqType = "QueriedHashtag"
                state = returnMsg
                data = [tag]
                allTweets = returnTweets
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)

        | ServerQueryMentioned (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
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
                reqType = "QueriedMentioned"
                state = returnMsg
                data = [mentionedUser]
                allTweets = returnTweets
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)
        
         | ServerGetAllTweetsFromSubscriber (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
            let mutable returnMsg = "400 Bad Request"
            let mutable returnTweets = List.empty<TweetDetail>
            let username = data.data.[0]
            let mutable followingList = List.empty<string>
            let mutable oneFollowing = username

            if isValidUser username && followingsDict.ContainsKey(username) then
                followingList <- followingsDict.[username]
                oneFollowing <- followingList.[Random().Next(followingList.Length)]
                if isValidUser oneFollowing && userTweetDict.ContainsKey(oneFollowing) then
                    for t in userTweetDict.[oneFollowing] do
                        returnTweets <- returnTweets @ [tweetsDict.[t]]
                        returnMsg <- "200 OK"
            else
                returnMsg <- "404 Not Found"

            // res
            let data: ReplyDataWithTweet = {
                reqType = "GotAllTweetsFromSubscriber"
                state = returnMsg
                data = [username; oneFollowing]
                allTweets = returnTweets
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)

        | ServerReconnect (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
            let mutable returnMsg = "400 Bad Request"
            let mutable returnTweets = List.empty<TweetDetail>
            let username = data.data.[0]

            if isValidUser username then
                returnMsg <- updateOnlineUserDB username "connect"
                if userTweetDict.ContainsKey(username) then
                    for t in userTweetDict.[username] do
                        returnTweets <- returnTweets @ [tweetsDict.[t]]

            // res
            let data: ReplyDataWithTweet = {
                reqType = "Reconnected"
                state = returnMsg
                data = [username]
                allTweets = returnTweets
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)

        | ServerDisconnect (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
            let mutable returnMsg = "400 Bad Request"
            let username = data.data.[0]

            if isValidUser username then
                returnMsg <- (updateOnlineUserDB username "disconnect")

            // res
            let data: ReplyData = {
                reqType = "Disconnect"
                state = returnMsg
                data = [username]
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)

        | ServerDelete (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
            let mutable returnMsg = "400 Bad Request"
            let username = data.data.[0]
            
            if isValidUser username then
                let pubK = usersDict.[username].PublicKey
                // get proper data to delete user
                let reqData: UserDetail = {
                    Username = username
                    PublicKey = pubK
                    Deleted = true
                }
                usersDict.[username] <- reqData
                returnMsg <- "200 OK"
            else
                returnMsg <- "404 Not Found"

            // res
            let data: ReplyData = {
                reqType = "Delete"
                state = returnMsg
                data = [username]
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)
        
        | ServerGetLiveView (x, client, clientWsID) ->
            let data = Json.deserialize<ReqDetail> x
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
                reqType = "GetLiveView"
                state = returnMsg
                data = [username]
                allTweets = returnTweets
            }
            let res = Json.serialize data
            client.SendTo(res, clientWsID)            

        return! loop()
    }
    loop()      

let serverActor = spawn system "TwitterServer" serverActorNode


type Register () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerRegister (message.Data, x.Sessions, x.ID)

type Tweet () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerTweet (message.Data, x.Sessions, x.ID)

type Retweet () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerRetweet (message.Data, x.Sessions, x.ID)

type AddToFollowings () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerAddToFollowings (message.Data, x.Sessions, x.ID)
    
type QueryHashtag () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerQueryHashtag (message.Data, x.Sessions, x.ID)

type QueryMentioned () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerQueryMentioned (message.Data, x.Sessions, x.ID)

type GetAllTweetsFromSubscriber () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerGetAllTweetsFromSubscriber (message.Data, x.Sessions, x.ID)

type Reconnect () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerReconnect (message.Data, x.Sessions, x.ID)

type Disconnect () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerDisconnect (message.Data, x.Sessions, x.ID)

type Delete () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerDelete (message.Data, x.Sessions, x.ID)

type GetLiveView () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! ServerGetLiveView (message.Data, x.Sessions, x.ID)


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