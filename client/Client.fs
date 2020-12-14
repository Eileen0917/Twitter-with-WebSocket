open System
open WebSocketSharp;
open WebSocketSharp.Server
open Akka.Actor
open Akka.FSharp

type ReqDetail = {
    reqType: string
    data: string list
}

type TweetReqDetail = {
    reqType : string
    data : string list
    tagsData : string list
    mentionedData : string list
}

let system = ActorSystem.Create("FSharp")
          
let registed = fun (arg:MessageEventArgs) ->
    printfn "registed %A" (arg.Data)

let tweeted = fun (arg:MessageEventArgs) ->
    printfn "tweeted %A" (arg.Data)

let retweeted = fun (arg:MessageEventArgs) ->
    printfn "retweeted %A" (arg.Data)

let subscribed = fun (arg:MessageEventArgs) ->
    printfn "subscribed %A" (arg.Data)

let queriedTags = fun (arg:MessageEventArgs) ->
    printfn "queriedTags %A" (arg.Data)

let queriedMentioned = fun (arg:MessageEventArgs) ->
    printfn "queriedMentioned %A" (arg.Data)

let gotAllTweetFromSub = fun (arg:MessageEventArgs) ->
    printfn "gotAllTweetFromSub %A" (arg.Data)

let reconnected = fun (arg:MessageEventArgs) ->
    printfn "reconnected %A" (arg.Data)

let disconnected = fun (arg:MessageEventArgs) ->
    printfn "retweeted %A" (arg.Data)

let deleted = fun (arg:MessageEventArgs) ->
    printfn "deleted %A" (arg.Data)

let gotLiveView = fun (arg:MessageEventArgs) ->
    printfn "gotLiveView %A" (arg.Data)


let clientActorNode (clientMailbox:Actor<_>) =
    let  wsRegister = new WebSocket("ws://localhost:9000/Register")
    let  wsTweet = new WebSocket("ws://localhost:9000/Tweet")
    let  wsRetweet = new WebSocket("ws://localhost:9000/Retweet")
    let  wsAddToFollowings = new WebSocket("ws://localhost:9000/AddToFollowings")
    let  wsQueryHashtag = new WebSocket("ws://localhost:9000/QueryHashtag")
    let  wsQueryMentioned = new WebSocket("ws://localhost:9000/QueryMentioned")
    let  wsGetAllTweetsFromSubscriber = new WebSocket("ws://localhost:9000/GetAllTweetsFromSubscriber")
    let  wsReconnect = new WebSocket("ws://localhost:9000/Reconnect")
    let  wsDisconnect = new WebSocket("ws://localhost:9000/Disconnect")
    let  wsDelete = new WebSocket("ws://localhost:9000/Delete")
    let  wsGetLiveView = new WebSocket("ws://localhost:9000/GetLiveView") 

    wsRegister.OnMessage.Add(registed)
    wsTweet.OnMessage.Add(tweeted)
    wsRetweet.OnMessage.Add(retweeted)
    wsAddToFollowings.OnMessage.Add(subscribed)
    wsQueryHashtag.OnMessage.Add(queriedTags)
    wsQueryMentioned.OnMessage.Add(queriedMentioned)
    wsGetAllTweetsFromSubscriber.OnMessage.Add(gotAllTweetFromSub)
    wsReconnect.OnMessage.Add(reconnected)
    wsDisconnect.OnMessage.Add(disconnected)
    wsGetLiveView.OnMessage.Add(gotLiveView)

    let rec loop() = actor {
        let! msg = clientMailbox.Receive()
        let info = Json.deserialize<RecordType> msg
        let pattern = info.pattern

        match pattern with
            | "Register" ->
                let username = info.data.[0]
                let publicKey = info.data.[1]

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "Register"
                    data = [username; publicKey;]
                }
                let json = Json.serialize reqData

                if not wsRegister.IsAlive then
                    wsRegister.Connect() 
                    wsRegister.Send(json)
                else
                    printfn "Connection Error"

            | "Tweet" ->
                let username = info.data.[0]
                let tweetContent = info.data.[1]
                let tags = findTag tweetContent
                let mentioneds = findMentioned tweetContent
                let time = DateTime.Now

                // get proper data send to server
                let reqData: TweetReqDetail = {
                    reqType = "Tweet"
                    data = [username; tweetContent; time]
                    tagsData = tags
                    mentionedData = mentioneds
                }
                let json = Json.serialize reqData

                if not wsTweet.IsAlive then
                    wsTweet.Connect() 
                    wsTweet.Send(json)
                else
                    printfn "Connection Error"

            | "Retweet" ->
                let username = info.data.[0]
                let tweetContent = info.data.[1]
                let reTweetID = info.data.[2]
                let tags = findTag tweetContent
                let mentioneds = findMentioned tweetContent
                let time = DateTime.Now

                // get proper data send to server
                let reqData: TweetReqDetail = {
                    reqType = "Retweet"
                    data = [username; tweetContent; time; reTweetID]
                    tagsData = tags
                    mentionedData = mentioneds
                }
                let json = Json.serialize reqData

                if not wsRetweet.IsAlive then
                    wsRetweet.Connect() 
                    wsRetweet.Send(json)
                else
                    printfn "Connection Error"

            | "AddToFollowings" ->
                let username = info.data.[0]
                let follow = info.data.[1]

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "AddToFollowings"
                    data = [username; follow]
                }
                let json = Json.serialize reqData

                if not wsAddToFollowings.IsAlive then
                    wsAddToFollowings.Connect() 
                    wsAddToFollowings.Send(json)
                else
                    printfn "Connection Error"
            
            | "QueryHashtag" ->
                let tag = info.data.[0]

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "QueryHashtag"
                    data = [tag]
                }
                let json = Json.serialize reqData

                if not wsQueryHashtag.IsAlive then
                    wsQueryHashtag.Connect() 
                    wsQueryHashtag.Send(json)
                else
                    printfn "Connection Error"

            | "QueryMentioned" ->
                let mentionedUser = info.data.[0]

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "QueryMentioned"
                    data = [mentionedUser]
                }
                let json = Json.serialize reqData

                if not wsQueryMentioned.IsAlive then
                    wsQueryMentioned.Connect() 
                    wsQueryMentioned.Send(json)
                else
                    printfn "Connection Error"
            
            | "GetAllTweetsFromSubscriber" ->
                let username = info.data.[0]

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "GetAllTweetsFromSubscriber"
                    data = [username]
                }
                let json = Json.serialize reqData

                if not wsGetAllTweetsFromSubscriber.IsAlive then
                    wsGetAllTweetsFromSubscriber.Connect() 
                    wsGetAllTweetsFromSubscriber.Send(json)
                else
                    printfn "Connection Error"

            | "Reconnect" ->
                let username = info.data.[0]

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "Reconnect"
                    data = [username]
                }
                let json = Json.serialize reqData

                if not wsReconnect.IsAlive then
                    wsReconnect.Connect() 
                    wsReconnect.Send(json)
                else
                    printfn "Connection Error"

            | "Disconnect" ->
                let username = info.data.[0]

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "Disconnect"
                    data = [username]
                }
                let json = Json.serialize reqData

                if not wsDisconnect.IsAlive then
                    wsDisconnect.Connect() 
                    wsDisconnect.Send(json)
                else
                    printfn "Connection Error"

            | "Delete" ->
                let username = info.data.[0]

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "Delete"
                    data = [username]
                }
                let json = Json.serialize reqData

                if not wsDelete.IsAlive then
                    wsDelete.Connect() 
                    wsDelete.Send(json)
                else
                    printfn "Connection Error"

            | "GetLiveView" ->
                let username = info.data.[0]

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "GetLiveView"
                    data = [username]
                }
                let json = Json.serialize reqData

                if not wsGetLiveView.IsAlive then
                    wsGetLiveView.Connect() 
                    wsGetLiveView.Send(json)
                else
                    printfn "Connection Error"

            | _ ->  failwith "Unknown Message"
            
        return! loop()
    }
    loop()        

[<EntryPoint>]
let main argv =
    
    let serverActor = spawn system "TwitterClient" serverActorNode

    serverActor <! "connect"
    serverActor <! "query"
    // serverActor <! "disconnect"
    serverActor <! "query"
    serverActor <! "connect"
    serverActor <! "query"
    //serverActor <! "query"
    
    Console.ReadLine() |> ignore

    0