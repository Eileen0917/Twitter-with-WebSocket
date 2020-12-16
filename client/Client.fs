open System
open WebSocketSharp;
open WebSocketSharp.Server
open Akka.Actor
open Akka.FSharp
open FSharp.Json

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

type ResDetail = {
    reqType: string
    state: string
    data: string list
}

type ResDetailWithTweets = {
    reqType: string
    state: string
    data: string list
    allTweets: TweetDetail list 
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
    let mutable returnWord = ""
    for w in words do
        if w.[0] = '@' then
            returnWord <- w.[1..]
    returnWord

let system = ActorSystem.Create("FSharp")
          
let registed = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetail> arg.Data
    let state = info.state
    let username = info.data.[0]
    if state = "200 OK" then
        isUserModeLoginSuccess <- Success
        let regJSON: ReqDetail = { 
            reqType = "Reconnect" ; 
            data = [username]
        }
        let reqData = Json.serialize regJSON

        let client = select ("akka://FSharp/user/" + username) system
        client <! reqData
    else
        isUserModeLoginSuccess <- Fail

let tweeted = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetail> arg.Data
    let state = info.state
    let username = info.data.[0]
    let tweetContent = info.data.[1]
    if state = "200 OK" then
        printfn "%A tweeted: %A" username tweetContent
    else
        printfn "%A Not Found" username

let retweeted = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetail> arg.Data
    let state = info.state
    let username = string(info.data.[0])
    let reTweetContent = string(info.data.[1])
    let reTweetFrom = string(info.data.[2])

    if state = "200 OK" then
        printfn "\n%A retweeted: %A from %A" username reTweetContent reTweetFrom
    else
        printfn "%A Not Found" username

let subscribed = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetail> arg.Data
    let state = info.state
    let username = info.data.[0]
    let follow = info.data.[1]

    if state = "200 OK" then
        printfn "\n%A subscribed to %A " username follow
    else
        printfn "%A Not Found" username

let queriedTags = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetailWithTweets> arg.Data
    let state = info.state
    let randomHashTag = info.data.[0]
    let allTweets = info.allTweets

    if state = "200 OK" then
        printfn "\n================================================" 
        printfn "Tweets with %A" randomHashTag
        for l in allTweets do 
            printfn "\n------------------------------------"
            printfn "TweetID: %A      Time: %A" (l.TweetID) (string(l.Time))
            printfn "Author: %A" (l.Username)
            printfn "Content: %A" (l.Content)
        printfn "================================================\n"
    else
        printfn "[Client] %A Not Found" randomHashTag

let queriedMentioned = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetailWithTweets> arg.Data
    let state = info.state
    let username = info.data.[0]
    let allTweets = info.allTweets

    if state = "200 OK" then
        printfn "\n================================================" 
        printfn "Tweets mentioned %A" username
        for l in allTweets do 
            printfn "\n------------------------------------"
            printfn "TweetID: %A      Time: %A" (l.TweetID) (string(l.Time))
            printfn "Author: %A" (l.Username)
            printfn "Content: %A" (l.Content)
        printfn "================================================\n"
    else
        printfn "[Client] %A Not Found" username

let gotAllTweetFromSub = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetailWithTweets> arg.Data
    let state = info.state
    let username = info.data.[0]
    let oneFollowing = info.data.[1]
    let allTweets = info.allTweets
    if state = "200 OK" then
        printfn "\n================================================" 
        printfn "One of %A's subscriber: %A's tweets" username oneFollowing
        for l in allTweets do 
            printfn "\n------------------------------------"
            printfn "TweetID: %A      Time: %A" (l.TweetID) (string(l.Time))
            printfn "Author: %A" (l.Username)
            printfn "Content: %A" (l.Content)
        printfn "================================================\n"
    else
        printfn "%A have no following." username

let reconnected = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetailWithTweets> arg.Data
    let state = info.state
    let username = info.data.[0]
    let allTweets = info.allTweets
    if state = "200 OK" then
        isUserModeLoginSuccess <- Success
        printfn "\n================================================" 
        printfn "%A 's Tweets" username
        for l in allTweets do 
            printfn "\n------------------------------------"
            printfn "TweetID: %A      Time: %A" (l.TweetID) (string(l.Time))
            printfn "Author: %A" (l.Username)
            printfn "Content: %A" (l.Content)
        printfn "================================================\n"
    else
        isUserModeLoginSuccess <- Fail

let disconnected = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetail> arg.Data
    let state = info.state
    let username = info.data.[0]
    if state = "200 OK" then
        isUserModeLoginSuccess <- Success
    else
        isUserModeLoginSuccess <- Fail

let deleted = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetail> arg.Data
    let state = info.state
    let username = info.data.[0]
    if state = "200 OK" then
        isUserModeLoginSuccess <- Success
    else
        isUserModeLoginSuccess <- Fail

let gotLiveView = fun (arg:MessageEventArgs) ->
    let info = Json.deserialize<ResDetailWithTweets> arg.Data
    let state = info.state
    let username = info.data.[0]
    let allTweets = info.allTweets
    if state = "200 OK" then
        printfn "\n================================================" 
        printfn "%A 's All Tweets" username
        for l in allTweets do 
            printfn "\n------------------------------------"
            printfn "TweetID: %A      Time: %A" (l.TweetID) (string(l.Time))
            printfn "Author: %A" (l.Username)
            printfn "Content: %A" (l.Content)
        printfn "================================================\n"
    elif state = "User is disconnected." then
        printfn "[Client] %A is disconnected" username
    else
        printfn "%A have no tweet." username


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
    wsDelete.OnMessage.Add(deleted)

    let rec loop() = actor {
        let! msg = clientMailbox.Receive()
        let info = Json.deserialize<ReqDetail> msg
        let pattern = info.reqType

        match pattern with
            | "Register" ->
                let username = info.data.[0]
                let publicKey = "publicKey"

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
                    wsRegister.Send(json)

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
                    wsTweet.Send(json)

            | "Retweet" ->
                let username = info.data.[0]
                let reTweetID = info.data.[1]
                let time = string(DateTime.Now)

                // get proper data send to server
                let reqData: ReqDetail = {
                    reqType = "Retweet"
                    data = [username; reTweetID; time;]
                }
                let json = Json.serialize reqData

                if not wsRetweet.IsAlive then
                    wsRetweet.Connect() 
                    wsRetweet.Send(json)
                else
                    wsRetweet.Send(json)

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
                    wsAddToFollowings.Send(json)
            
            | "QueryHashtag" ->
                let content = info.data.[1]
                let tag = findTag content

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
                    wsQueryHashtag.Send(json)

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
                    wsQueryMentioned.Send(json)
            
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
                    wsGetAllTweetsFromSubscriber.Send(json)

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
                    wsReconnect.Send(json)

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
                    wsDisconnect.Send(json)

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
                    wsDelete.Send(json)

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
                    wsGetLiveView.Send(json)

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
            printf "(int)> "
            userInputStr <- Console.ReadLine()
            match (Int32.TryParse(userInputStr)) with
            | (true, _) -> (keepPrompt <- false)
            | (false, _) ->  printfn "[Error] Invalid number"
        userInputStr
    | "string" ->
        while keepPrompt do
            printf "(string)> "
            userInputStr <- Console.ReadLine()
            match userInputStr with
            | "" | "\n" | "\r" | "\r\n" | "\0" -> printfn "[Error] Invalid string"
            | _ -> (keepPrompt <- false)
        userInputStr
    | _ ->
        userInputStr  

let printBanner (printStr:string) =
    printfn "\n----------------------------------"
    printfn "%s" printStr
    printfn "----------------------------------\n"

let showMenu option = 
    match option with
    | "1" ->
        printfn "Log In or Sign Up\n"
        printfn "1. Sign Up\t register a Twitter account"
        printfn "2. Log In\t connect to exist account"
        printfn "3. Exit\t\t terminate this program"
    | "2" ->
        printfn "\nYou already logged in a Client Termianl\n"
        printfn "Please choose one of the commands listed below:"
        printfn "1. Sendtweet\t\t Post a Tweet"
        printfn "2. Retweet\t\t Retweet a Tweet with TweetID"
        printfn "3. Subscribe\t\t Subscribe to another User"
        printfn "4. Query Tag\t\t Query Tweets with a #Tag"
        printfn "5. Query Mention\t Query Tweets for a mentioned User"
        printfn "6. Query History\t Query all your Tweets"
        printfn "7. Get Other's Tweet \t Query the tweets of one of your following"
        printfn "8. Disconnect\t\t Log out"
        printfn "9. Delete\t\t Delete your account and cannot log in anymore"
        printfn "0. Exit\t\t\t terminate this program"
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
    let mutable currUsername = "temp"
    let mutable currState = 0

    (showMenu "1")
    while true do
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
                    try
                        let client = spawn system username clientActorNode
                        client <! reqData

                        waitForServerResponse (5.0)
                        if isUserModeLoginSuccess = Success then
                            printBanner ("Successfully registered and login as User")
                            currState <- 1
                            currUsername <- username
                            (showMenu "2")
                        else if isUserModeLoginSuccess = Fail then
                            printBanner ("Faild to register.")
                            (showMenu "1")
                        else
                            printBanner ("Timeout")
                            (showMenu "1")
                    with _ ->
                        printfn "Username already used. Please try another one."
                        isUserModeLoginSuccess <- Fail
                        (showMenu "1")

                    

                | "2" ->
                    printfn "Please enter your username: "
                    let username = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "Reconnect" ; 
                        data = [username]
                    }
                    let reqData = Json.serialize regJSON

                    let client = select ("akka://FSharp/user/" + username) system
                    client <! reqData

                    waitForServerResponse (5.0)
                    if isUserModeLoginSuccess = Success then
                        printBanner ("Successfully connected and login as User")
                        currState <- 1
                        currUsername <- username
                        (showMenu "2")
                    else if isUserModeLoginSuccess = Fail then
                        printBanner ("Faild to connect and login.")
                        (showMenu "1")
                    else
                        printBanner ("Timeout")
                        (showMenu "1")

                | "3" ->
                    printfn "Bye!"
                    Environment.Exit 1

                | _ ->
                    (showMenu "1")

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
                    (showMenu "2")

                | "2"| "retweet" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    printfn "Please enter the \"TweetID\" you want to retweet: "
                    printfn "You can check \"TweetID\" with other commands."
                    let tweetID = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "Retweet" ; 
                        data = [currUsername;tweetID]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showMenu "2")

                | "3"| "subscribe" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    printfn "Please enter the \"username\" you want to follow: "
                    let follow = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "AddToFollowings" ; 
                        data = [currUsername;follow]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showMenu "2")

                | "4"| "tag" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    printfn "Please enter the \"tag\" you want to query (with #): "
                    let content = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "QueryHashtag" ; 
                        data = [currUsername;content]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showMenu "2")

                | "5"| "mention" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    printfn "Please enter the \"username\" you want to query (without @): "
                    let queryUser = (getUserInput "string")
                    let regJSON: ReqDetail = { 
                        reqType = "QueryMentioned" ; 
                        data = [queryUser]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showMenu "2")

                | "6"| "GetAll" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    let regJSON: ReqDetail = { 
                        reqType = "GetLiveView" ; 
                        data = [currUsername]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showMenu "2")

                | "7"| "GetAllTweetsFromSubscriber" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    let regJSON: ReqDetail = { 
                        reqType = "GetAllTweetsFromSubscriber" ; 
                        data = [currUsername]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    (showMenu "2")

                | "8" | "disconnect" ->
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
                        (showMenu "1")
                    else
                        printBanner ("Faild to disconnect and logout.")
                        (showMenu "2")
                
                | "9"| "Delete" -> 
                    let client = select ("akka://FSharp/user/" + currUsername) system
                    let regJSON: ReqDetail = { 
                        reqType = "Delete" ; 
                        data = [currUsername]
                    }
                    let reqData = Json.serialize regJSON
                    client <! reqData

                    waitForServerResponse (5.0)
                    if isUserModeLoginSuccess = Success then
                        printBanner ("Successfully deleted and logout User")
                        currUsername <- "temp"
                        currState <- 0
                        (showMenu "1")
                    else
                        printBanner ("Faild to delete and logout.")
                        (showMenu "2")

                | "0" | "exit" ->
                    printfn "Exit the program, Bye!"
                    Environment.Exit 1

                | _ ->
                    (showMenu "2")


    0