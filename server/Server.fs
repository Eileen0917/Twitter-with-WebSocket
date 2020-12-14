open System
open WebSocketSharp.Server
open WebSocketSharp
open Akka.Actor
open Akka.FSharp
open DBModel

let system = ActorSystem.Create("FSharp")
let wssv = WebSocketServer("ws://localhost:9001")

let isValidUser username = 
    (usersDict.ContainsKey(userID) && not usersDict.[username].Deleted) 

let serverActorNode (serverMailbox) =
    let nodeName = serverMailbox.Self.Path.Name

    let mutable onlineUserSet = Set.empty
    let updateOnlineUserDB username option = 
        let isConnected = onlineUserSet.Contains(username)
        if option = "connect" && not isConnected then
            if isValidUser username then
                onlineUserSet <- onlineUserSet.Add(username)
                "200 OK"
            else
                "404 Not Found"
        else if option = "disconnect" && isConnected then
            onlineUserSet <- onlineUserSet.Remove(username)
            "200 OK"
        else
            "200 OK"

    let rec loop() = actor {
        let! message = serverMailbox.Receive()

        match message with
            | Register (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let username = info.Username

                // get proper data to create user
                let reqData: UserDetail = {
                    Username = username
                    PublicKey = info.PublicKey
                    Deleted = False
                }

                // do register stuff
                if not usersDict.ContainsKey(username) then
                    usersDict.Add(username, reqData)
                    // userTweetDict.Add(username, [])
                    returnMsg <- "200 OK"

                // res
                let retData: ReplyData = {
                    ReqType = "Register"
                    State = returnMsg
                    Data = [username]
                    AllTweets = [""]
                }
                let res = Json.serialize retData
                client.SendTo(res, clientWsID)

            | Tweet (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let username = info.Username
                let tweetID = string(tweetsDict.Count + 1)
                let tag = info.Hashtag
                let mentioned = info.Mention
                let tweetContent = info.Content

                // get proper data to create tweet
                let reqData: TweetDetail = {
                    Username = username
                    TweetID = tweetID
                    Time = info.Time
                    Content = tweetContent
                    Hashtag = tag
                    Mention = mentioned
                    RetweetFrom = username
                }

                // do send tweet stuff
                if isValidUser username then
                    tweetsDict.Add(tweetID, reqData)
                    (userTweetDict.[username]).Add(tweetID) // userTweetDict
                    if tag <> "" && tag.[0] = "#" then
                        if not hashtagsDict.ContainsKey(tag) then
                            hashtagsDict.Add(tag, [])
                        (hashtagsDict.[tag]).Add(tweetID)
                    if mentioned <> "" then
                        if not mentionsDict.ContainsKey(mentioned) then
                            mentionsDict.Add(mentioned, [])
                        (mentionsDict.[mentioned]).Add(tweetID)
                    returnMsg <- "200 OK"
                else   
                    returnMsg <- "404 Not Found"

                // res
                let data: ReplyData = {
                    ReqType = "Tweet"
                    State = returnMsg
                    Data = [username; tweetContent]
                    AllTweets = [""]
                }
                let res = Json.serialize data
                client.SendTo(res, clientWsID)

            | Retweet (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let username = info.Username
                let tweetID = string(tweetsDict.Count + 1)
                let tag = info.Hashtag
                let mentioned = info.Mention
                let retweetFrom = tweetsDict.[info.RetweetFrom].Username
                let tweetContent = "Retweet from " + retweetFrom + ": " + info.Content
                

                // get proper data to re-tweet
                let reqData: TweetDetail = {
                    Username = username
                    TweetID = tweetID
                    Time = info.Time
                    Content = tweetContent
                    Hashtag = tag
                    Mention = mentioned
                    RetweetFrom = retweetFrom
                }

                // do re-tweet stuff
                if isValidUser username then
                    tweetsDict.Add(tweetID, reqData)
                    (userTweetDict.[username]).Add(tweetID)     // userTweetDict
                    if tag <> "" && tag.[0] = "#" then
                        if not hashtagsDict.ContainsKey(tag) then
                            hashtagsDict.Add(tag, [])
                        (hashtagsDict.[tag]).Add(tweetID)
                    if mentioned <> "" then
                        if not mentionsDict.ContainsKey(mentioned) then
                            mentionsDict.Add(mentioned, [])
                        (mentionsDict.[mentioned]).Add(tweetID)
                    returnMsg <- "200 OK"
                else
                    returnMsg <- "404 Not Found"

                // res
                let data: ReplyData = {
                    ReqType = "Tweet"
                    State = returnMsg
                    Data = [username; tweetContent; retweetFrom]
                    AllTweets = [""]
                }
                let res = Json.serialize data
                client.SendTo(res, clientWsID)
            
            | AddToFollowings (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let username = string(info.Username)
                let other = string(info.Following)

                // user followr other
                if isValidUser username && isValidUser other && username <> other then
                    if not followersDict.ContainsKey(other) then
                        followersDict.Add(other, [])
                    if not List.Contains username followersDict.[other] then
                        (followersDict.[other]).Add(username)

                    if not followingsDict.ContainsKey(username) then
                        followingsDict.Add(username, [])
                    if not List.Contains other followingsDict.[username] then
                        (followingsDict.[username]).Add(other)

                    returnMsg <- "200 OK"
                else
                    returnMsg <- "404 Not Found"

                // res
                let data: ReplyData = {
                    ReqType = "Subscribe"
                    State = returnMsg
                    Data = [username; other]
                    AllTweets = [""]
                }
                let res = Json.serialize data
                client.SendTo(res, clientWsID)

            | QueryHashtag (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let mutable returnTweets = List.empty<TweetDetail>
                let tag = info.Tag
                if hashtagsDict.ContainsKey(tag) then
                    tweetsList = hashtagsDict.[tag]
                    for t in tweetsList do
                        if tweetsDict.ContainsKey(t) then
                            returnTweets <- returnTweets @ [tweetsDict.[t]]
                            returnMsg <- "200 OK"
                else
                    returnMsg <- "404 Not Found"
                
                // res
                let data: ReplyData = {
                    ReqType = "QueryHashtag"
                    State = returnMsg
                    Data = [tag]
                    AllTweets = returnTweets
                }
                let res = Json.serialize data
                client.SendTo(res, clientWsID)
                
            | QueryMentioned (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let mutable returnTweets = List.empty<TweetDetail>
                let mentionedUser = info.Username
                if mentionsDict.ContainsKey(mentionedUser) then
                    tweetsList = mentionsDict.[mentionedUser]
                    for t in tweetsList do
                        if tweetsDict.ContainsKey(t) then
                            returnTweets <- returnTweets @ [tweetsDict.[t]]
                            returnMsg <- "200 OK"
                else
                    returnMsg <- "404 Not Found"
                
                // res
                let data: ReplyData = {
                    ReqType = "QueryMentioned"
                    State = returnMsg
                    Data = [mentionedUser]
                    AllTweets = returnTweets
                }
                let res = Json.serialize data
                client.SendTo(res, clientWsID)

            | GetAllTweetsFromSubscriber (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let mutable returnTweets = List.empty<TweetDetail>
                let username = info.Username
                let mutable followingList = List.empty<string>

                if isValidUser username && followingsDict.ContainsKey(username) then
                    followingList <- followingsDict.[username]
                let oneFollowing = followingList.[Random().Next(followingList.Length)]
                if isValidUser oneFollowing && userTweetDict.ContainsKey(oneFollowing) then
                    for t in userTweetDict.ContainsKey(oneFollowing)
                    returnTweets <- returnTweets @ [tweetsDict.[t]]
                    returnMsg <- "200 OK"
                else
                    returnMsg <- "404 Not Found"

                // res
                let data: ReplyData = {
                    ReqType = "GetAllTweetsFromSubscriber"
                    State = returnMsg
                    Data = [username; oneFollowing]
                    AllTweets = returnTweets
                }
                let res = Json.serialize data
                client.SendTo(res, clientWsID)

            | Reconnect (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let username = info.Username
                let returnMsg <- (updateOnlineUserDB username "connect")

                // res
                let data: ReplyData = {
                    ReqType = "Reconnect"
                    State = returnMsg
                    Data = [username]
                    AllTweets = returnTweets
                }
                let res = Json.serialize data
                client.SendTo(res, clientWsID)

            | Disconnect (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let username = info.Username
                let returnMsg <- (updateOnlineUserDB username "disconnect")

                // res
                let data: ReplyData = {
                    ReqType = "Disconnect"
                    State = returnMsg
                    Data = [username]
                    AllTweets = [""]
                }
                let res = Json.serialize data
                client.SendTo(res, clientWsID)

            | Delete (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let username = string(info.Username)

                // get proper data to delete user
                let reqData: UserDetail = {
                    Username = username
                    PublicKey = info.PublicKey
                    Deleted = True
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
                    AllTweets = [""]
                }
                let res = Json.serialize data
                client.SendTo(res, clientWsID)
            
            | GetLiveView (info, client, clientWsID) ->
                let mutable returnMsg = "400 Bad Request"
                let username = info.Username

                if isValidUser username && userTweetDict.ContainsKey(username) then
                    for t in userTweetDict.[username] do
                        returnTweets <- returnTweets @ [tweetsDict.[t]]
                        returnMsg <- "200 OK"
                else
                    returnMsg <- "404 Not Found"

                // res
                let data: ReplyData = {
                    ReqType = "GetLiveView"
                    State = returnMsg
                    Data = [username]
                    AllTweets = returnTweets
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
        serverActor <! Register (message.Data, x.Sessions, x.ID)

type Tweet () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! Tweet (message.Data, x.Sessions, x.ID)

type Retweet () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! Retweet (message.Data, x.Sessions, x.ID)

type AddToFollowings () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! AddToFollowings (message.Data, x.Sessions, x.ID)
    
type QueryHashtag () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! QueryHashtag (message.Data, x.Sessions, x.ID)

type QueryMentioned () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! QueryMentioned (message.Data, x.Sessions, x.ID)

type GetAllTweetsFromSubscriber () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! GetAllTweetsFromSubscriber (message.Data, x.Sessions, x.ID)

type Reconnect () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! Reconnect (message.Data, x.Sessions, x.ID)

type Disconnect () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! Disconnect (message.Data, x.Sessions, x.ID)

type Delete () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! Delete (message.Data, x.Sessions, x.ID)

type GetLiveView () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        serverActor <! GetLiveView (message.Data, x.Sessions, x.ID)


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