open System
open WebSocketSharp;
open WebSocketSharp.Server
open Akka.Actor
open Akka.FSharp
open FSharp.Json

type ReqDetail = {
    reqType: string
    data: string list
}

type UserModeStatusCheck =
    | Success
    | Fail
    | Waiting
    | Timeout
let mutable (isUserModeLoginSuccess:UserModeStatusCheck) = Waiting

let findTag (content: string) =
    let words = content.Split [|' '|]
    let mutable returnWord = ""
    for w in words do
        if w.[0] = '#' then
            returnWord <- w
    returnWord

let findMentioned (content: string) =
    let words = content.Split [|' '|]
    let n = words.Length
    let mutable returnWord = ""
    for w in words do
        if w.[0] = '@' then
            returnWord <- w.[n..]
    returnWord

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
        let info = Json.deserialize<ReqDetail> msg
        let pattern = info.reqType

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
                let tag = findTag tweetContent
                let mentioned = findMentioned tweetContent
                let time = string(DateTime.Now)

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "Tweet"
                    data = [username; tweetContent; tag; mentioned; time]
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
                let tag = findTag tweetContent
                let mentioned = findMentioned tweetContent
                let time = string(DateTime.Now)

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "Retweet"
                    data = [username; tweetContent; tag; mentioned; time; reTweetID]
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

let getUserInput (option:string) = 
    let mutable keepPrompt = true
    let mutable userInputStr = ""
    match option with
    | "int" ->
        while keepPrompt do
            printf "Enter a number: "
            userInputStr <- Console.ReadLine()
            match (Int32.TryParse(userInputStr)) with
            | (true, _) -> (keepPrompt <- false)
            | (false, _) ->  printfn "[Error] Invalid number"
        userInputStr
    | "string" ->
        while keepPrompt do
            printf "Enter a string: "
            userInputStr <- Console.ReadLine()
            match userInputStr with
            | "" | "\n" | "\r" | "\r\n" | "\0" -> printfn "[Error] Invalid string"
            | _ -> (keepPrompt <- false)
        userInputStr
    | "YesNo" ->
        while keepPrompt do
            printf "Enter yes/no: "
            userInputStr <- Console.ReadLine()
            match userInputStr.ToLower() with
            | "yes" | "y" -> 
                (keepPrompt <- false) 
                userInputStr<-"yes"
            | "no" | "n" ->
                (keepPrompt <- false) 
                userInputStr<-"no"
            | _ -> printfn "[Error] Invalid input"
        userInputStr
    | _ ->
        userInputStr  

let printBanner (printStr:string) =
    printfn "\n----------------------------------"
    printfn "%s" printStr
    printfn "----------------------------------\n"

let showPrompt option = 
    match option with
    | "before" ->
        printfn "Log In or Sign Up\n"
        printfn "1. Sign Up\t register a Twitter account"
        printfn "2. Log In\t connect to exist account"
        printfn "3. Exit\t\t terminate this program"
    | "after" ->
        printfn "\nYou already logged in a Client Termianl\n"
        printfn "Please choose one of the commands listed below:"
        printfn "1. sendtweet\t Post a Tweet for current log in User"
        printfn "2. retweet\t Retweet a Tweet"
        printfn "3. subscribe\t Subscribe to a User"
        printfn "4. disconnect\t Disconnect/log out the current User"
        printfn "5. history\t Query a User's History Tweets"
        printfn "6. tag\t\t Query Tweets with a #Tag"
        printfn "7. mention\t Query Tweets for a mentioned User"
        printfn "8. Qsubscribe\t Query subscribe status for a User"
        printfn "9. exit\t\t terminate this program"
    | _ ->
        ()

let setTimeout _ =
    isUserModeLoginSuccess <- Timeout


let waitForServerResponse (timeout:float) =
    (* timeout: seconds *)
    let timer = new Timers.Timer(timeout*1000.0)
    isUserModeLoginSuccess <- Waiting
    timer.Elapsed.Add(setTimeout)
    timer.Start()
    printBanner "Waiting for server reply..."
    while isUserModeLoginSuccess = Waiting do ()
    timer.Close()


[<EntryPoint>]
let main argv =
    printfn "aa"
    let mutable currUsername = "temp"
    let mutable currState = 0

    (showPrompt "loginFirst")
    while true do
        printfn "qq"
        (* First State, User have to register or connect(login) first *)
        (* If successfully registered, *)
        while currState = 0 do
            let inputStr = Console.ReadLine()
            match inputStr with
                | "1" ->
                    printfn "Pleae enter your username (must be unique): "
                    let username = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "Register" ; 
                        data = [username]
                    }
                    let reqData = Json.serialize regJSON

                    let client = spawn system username clientActorNode
                    client <! reqData

                    waitForServerResponse (5.0)
                    if isUserModeLoginSuccess = Success then
                        printBanner ("Successfully registered and login as User")
                        currState <- 1
                        currUsername <- username
                        (showPrompt "afterLogin")
                    else if isUserModeLoginSuccess = Fail then
                        printBanner ("Faild to register.")
                        (showPrompt "loginFirst")
                    else
                        printBanner ("Timeout")
                        (showPrompt "loginFirst")

                | "2" ->
                    printfn "Please enter your username: "
                    let username = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "Reconnect" ; 
                        data = [username]
                    }
                    let reqData = Json.serialize regJSON

                    let client = spawn system username clientActorNode
                    client <! reqData

                    waitForServerResponse (5.0)
                    if isUserModeLoginSuccess = Success then
                        printBanner ("Successfully connected and login as User")
                        currState <- 1
                        currUsername <- username
                        (showPrompt "afterLogin")
                    else if isUserModeLoginSuccess = Fail then
                        printBanner ("Faild to connect and login.")
                        (showPrompt "loginFirst")
                    else
                        printBanner ("Timeout")
                        (showPrompt "loginFirst")

                | "3" ->
                    printfn "Bye!"
                    Environment.Exit 1

                | _ ->
                    (showPrompt "loginFirst")

        while currState = 1 do
            let inputStr = Console.ReadLine()
            match inputStr with
                | "1"| "sendtweet" ->
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    printfn "Please enter the \"Content\" of your Tweet: "
                    let content = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "Tweet" ; 
                        data = [currUsername;content]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData
                    (showPrompt "afterLogin")

                | "2"| "retweet" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    printfn "Please enter the \"TweetID\" you want to retweet: "
                    printfn "You can check \"TweetID\" with other commands."
                    let content = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "Retweet" ; 
                        data = [currUsername;content]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showPrompt "afterLogin")

                | "3"| "subscribe" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    printfn "Please enter the \"username\" you want to follow: "
                    let content = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "Tweet" ; 
                        data = [currUsername;content]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showPrompt "afterLogin")

                | "4" | "disconnect" ->
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    let regJSON: ReqDetail = { 
                        reqType = "Disconnect" ; 
                        data = [currUsername;]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    waitForServerResponse (5.0)
                    if isUserModeLoginSuccess = Success then
                        printBanner ("Successfully diconnected and logout User")
                        currUsername <- "temp"
                        currState <- 0
                        (showPrompt "loginFirst")
                    else
                        printBanner ("Faild to disconnect and logout.")
                        (showPrompt "afterLogin")

                | "5"| "tag" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    printfn "Please enter the \"tag\" you want to query (with #): "
                    let content = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "QueryTag" ; 
                        data = [currUsername;content]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showPrompt "afterLogin")

                | "6"| "mention" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    printfn "Please enter the \"username\" you want to query (without @): "
                    let content = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "QueryTag" ; 
                        data = [currUsername;content]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showPrompt "afterLogin")

                | "7"| "Qsubscribe" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    let regJSON: ReqDetail = { 
                        reqType = "QueryTag" ; 
                        data = [currUsername]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showPrompt "afterLogin")

                | "8" | "exit" ->
                    printfn "Exit the program, Bye!"
                    Environment.Exit 1

                | _ ->
                    (showPrompt "afterLogin")


    0