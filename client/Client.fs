open System
open WebSocketSharp;
open WebSocketSharp.Server
open Akka.Actor
open Akka.FSharp
open FSharp.Json
open System.Security.Cryptography
open System.Text
open System.IO 

type str = Str of string
type bytes = By of byte[]

let str s = Str s
let istr (Str s) = s

let bytes b = By b
let ibytes (By b) = b

let base64 (x:bytes) : str = str (System.Convert.ToBase64String (ibytes x))
let ibase64 (x:str) : bytes = bytes (System.Convert.FromBase64String (istr x))

let utf8 (x:str) : bytes = bytes (System.Text.Encoding.UTF8.GetBytes (istr x))
let iutf8 (x:bytes) : str = str (System.Text.Encoding.UTF8.GetString (ibytes x))

//let concatfix (x:bytes) (y:bytes) : bytes = bytes (Array.append (ibytes x) (ibytes y))
//let iconcatfix (n:int)  (x:bytes) = 
//  let x = ibytes x in
//  let x1 = Array.sub x 0 n in 
//  let x2 = Array.sub x n (Array.length x - n) in 
//    (bytes x1, bytes x2) 

//let size (x:bytes) = 
//  let l = Array.length (ibytes x) in 
//  bytes ([| (byte) (l % 256) ; (byte) (l / 256) |])

//let concatvar (x:bytes) (y:bytes) : bytes = 
//  concatfix (size x) (concatfix x y)
//let iconcatvar (x:bytes) = 
//  let s,x' = iconcatfix 2 x in
//  let s = ibytes s in
//  let l = (int)(s.(0))+256* (int)(s.(1)) in
//  iconcatfix l x'

//let concat x y = concatvar x y
//let iconcat z = iconcatvar z

let equals (By x) (By y) = 
  if x.Length <> y.Length then false
  else 
    try 
      (Array.iter2 (fun x y -> if x = y then () else failwith "unequal") x y;         true)
    with _ -> false

let sha1_instance = SHA1.Create ()
let sha1 (x:bytes) : bytes = bytes (sha1_instance.ComputeHash (ibytes x))

type key = 
    SymKey of bytes
  | AsymKey of RSACryptoServiceProvider
let symkey b = SymKey b
let symkeyToBytes k = 
  match k with
    SymKey(k) -> k
  | _ -> failwith "symkeyToBytes only works with symmetric keys"

let rsaKeyFromBytes b = 
  let s = iutf8 b in
  let rsa = new RSACryptoServiceProvider() in
    rsa.FromXmlString(istr s);
    AsymKey rsa

let rsaKeyToBytes k = 
  match k with
    AsymKey(k) -> utf8 (str (k.ToXmlString(true)))
  | _ -> failwith "asymkeyToBytes only works with asymmetric keys"

let rsa_keygen () = 
  let rsa = new RSACryptoServiceProvider() in
    AsymKey rsa

let rsa_pub (k:key) = 
  match k with
      AsymKey skey ->
        let pub = skey.ExportParameters(false) in
        let rsa = new RSACryptoServiceProvider () in
          rsa.ImportParameters(pub);
          AsymKey rsa
    | _ -> failwith "Rsa_pub only defined for asymmetric private keys"

let rsa_encrypt (k:key)  (v:bytes)  = 
  match k with 
      AsymKey pkey -> bytes (pkey.Encrypt(ibytes v,true)) (* Setting OAEP=true *)
    | _ -> failwith "RSA encryption only defined for asymmetric keys"

let rsa_decrypt (k:key) (v:bytes) =
  match k with 
      AsymKey skey -> bytes (skey.Decrypt(ibytes v,true))
    | _ -> failwith "RSA encryption only defined for asymmetric keys"

let rsa_sign (k:key) (v:bytes)  = 
  match k with 
      AsymKey skey ->    
        let oid = CryptoConfig.MapNameToOID "sha1" in
          bytes (skey.SignHash (ibytes (sha1 v), oid))
    | _ -> failwith "RSA signature only defined for asymmetric keys"

let rsa_verify (k:key) (v:bytes) (x:bytes)  =
  match k with 
      AsymKey pkey ->    
        let oid = CryptoConfig.MapNameToOID "sha1" in
          if not (pkey.VerifyHash (ibytes (sha1 v), oid, ibytes x))
          then failwith "RSA_verify failed"
          else ()
    | _ -> failwith "RSA signature only defined for asymmetric keys"


let symkeys = 
  [
    ((str "A",str "B"),symkey (ibase64 (str "W2HB8TemgUwSF/GBrRhfgg==")));
    ((str "A",str "C"),symkey (ibase64 (str "hDHD7biflY8QS463AI9oog==")));
    ((str "A",str "S"),symkey (ibase64 (str "iDhpLoSqzzs0FEGO/BcF1g==")));
    ((str "A",str "E"),symkey (ibase64 (str "mwaU7dD50QTC3TnEMknLlA==")));
    ((str "B",str "A"),symkey (ibase64 (str "S/oqqIUPIPp3BQeA3fcQkw==")));
    ((str "B",str "C"),symkey (ibase64 (str "lDpajC6geRa1bniTRELi4A==")));
    ((str "B",str "S"),symkey (ibase64 (str "0263l6MI712DZi4+KUS6jg==")));
    ((str "B",str "E"),symkey (ibase64 (str "cb21AER0bvGVehq8nILwZw==")));
    ((str "C",str "A"),symkey (ibase64 (str "roKhsRba3f8XeV8VbrKmmg==")));
    ((str "C",str "B"),symkey (ibase64 (str "My4LpF+beNGj1Xg5tRwr5A==")));
    ((str "C",str "S"),symkey (ibase64 (str "5yGF7/4N5VA7897oqERW0w==")));
    ((str "C",str "E"),symkey (ibase64 (str "IcC7lZ/pryHVky85PHcsnA==")));
    ((str "S",str "A"),symkey (ibase64 (str "W7tcCX1p0OE1IDCdpVDLXQ==")));
    ((str "S",str "B"),symkey (ibase64 (str "LkVOo8hv5fqLhowwQwCg9A==")));
    ((str "S",str "C"),symkey (ibase64 (str "7UUpBicLqd6pUqeEJJNhFA==")));
    ((str "S",str "E"),symkey (ibase64 (str "9rpHZgYlWY0Z2M9ZkeTA4A==")));
    ((str "E",str "A"),symkey (ibase64 (str "opyvdpJEctriJSeeKG0yAQ==")));
    ((str "E",str "B"),symkey (ibase64 (str "/UTnV+wQi+b3eixvpldV7A==")));
    ((str "E",str "C"),symkey (ibase64 (str "2jLJblSe/EFwir9pPMERqA==")));
    ((str "E",str "S"),symkey (ibase64 (str "cP/xcXA0LPMt+haRkzFewA==")))
    ((str "A",str "S"), symkey (ibase64 (str "8sojHwf7VNQyQ2jA6LuMKg==")));
    ((str "B",str "S"), symkey (ibase64 (str "2HhYqYcSzV4fgTbcGkh4oA==")))     
  ]

let privateKeys = 
  [
    ((str "A"), rsaKeyFromBytes (utf8 (str "<RSAKeyValue><Modulus>rxX4yYfDrkhL32mmir8TibXGjzujK2v78Bl9HaPa/iPzApMhTQYUhd1iZjEKrV+Ou+i9GD68wabuSt16LDHC+uvvmmoymwRt7DODCQ/wBVnFrEkcAKqkSNslwHEek5rbXg9jjwveF2K3ZaxZmwdfXCPfj3Y8OJKlga7rk84RNhs=</Modulus><Exponent>EQ==</Exponent><P>r1pKAS1p2b7/0fOdJz/1J5QP4lJC22/A9w/IZqT+Y1kxdqPE9fYayUb1App3iUs9WaCJJZtH2dxv85OzQpjXTw==</P><Q>/5xDO1E4aHkXOrlI+4yQlH7bWUU26uXymUhASNtOZtlE+LoDpHCLZ+kgwVVYrAwU3hqP1d6huItLpx4VnMiBdQ==</Q><DP>ZyYNahq2vFI8ISXj+PhyF0gJV/QnU+diczZ14gazwfg7NrqwGDZqHAufLrU3QbPJvEBQrLWxy3KcNO14gYcVPQ==</DP><DQ>Sy31qAjUWvZwPnK7HM79WNoER261zJ38Dvch90+Ph6lQhWPi9CEZ8WKvR+vs507Y9gfP85vVNkcWQDYGW0oH9Q==</DQ><InverseQ>gsldzxZCn0lkoIZPok75TGySBs+bwc94rWAXZ5Hi/GJnkAjc7Wh5HO16uyfV3CvFW9gM2v17jf21dbqQc+krhA==</InverseQ><D>CkyWKfjtZJrXSWCRU3Smy92EJosJmSR4O0zLH9x2Sy9Kh65NQMQfUysj5+TEZI0mg4YpPalWZb6GfN/aApmD8IsdtXsotEeo0E2UICwRJZ/dVqZxNRx9F9c5RwtjqdAJ74HiONl/fyEH9aRCdVemHrOJrJ3PMdLWdRA/ooaC0Mk=</D></RSAKeyValue>")));
    ((str "B"), rsaKeyFromBytes (utf8 (str "<RSAKeyValue><Modulus>p9pb11czaMou0edru0DzQDJMFYQnlnSaCRCxtmuvvDiWciUf91wABOJhKUxoOK34HIaDp1wjH2XHtiovA39vrRIB+eOR2A80XObYk/zOZj5b0Y+DsB1VT6qghtMi0F0P2aXL0Zn2EI90Wjv6ghXrUlpIeo9iRP6VLCMs3Cne0hc=</Modulus><Exponent>EQ==</Exponent><P>rMg42uC0kfF49xlTk2qmOpPh1gctvNQc5YPk+a1waMMbPY/A2dyVPhKob86S1d8cGqX1uiRQTOSRYxAD2bg7nw==</P><Q>+LJhHX4YLixBioxm/HeghQEGXwrW8Hme/q04CrMXKrapZ/Lup6QFYZGPAOF88cfhW1jDXxBJD713f45fBZDWiQ==</Q><DP>jkqJLLkNLOT6NOeuPSqnAxBfoTMWm4GBNXutglKYzr7LI6OPwnlr2MQSPfVp3U5TYT1g86VvTmHhJGeKs0xtVQ==</DP><DQ>HUIpiv/GnAU04x+TpTtAD6XEg6bsHEqLLQVR4yQg19lBG0nBuV6XOKe2eJL/o/llzoLpzvLba0N3eGsaPOPdAQ==</DQ><InverseQ>g5vOWMFAFXfW+uhbXB+maFdHm5mU8+RtyhlcNKei9RcFamIp/b0vklYDeKcyhkMm38A57ED7scc9AeKy0nAJCw==</InverseQ><D>Oz4CTACou5KnHOhEI/jOUuSTUuNZRCknTn5c1vjUnMitkbK//PNLTQSaw0gkyLXfGSBqs4nuR1EZT1outfC+AERsA7xsXlgmGzLGpxdiZXgJ+AHNxAlsFgnM+CrbKL+eYcPdk5ApdOs6ZqIaRnX5/9gaCAuXpfzsZst9lDhxFpE=</D></RSAKeyValue>")));
    ((str "C"), rsaKeyFromBytes (utf8 (str "<RSAKeyValue><Modulus>rIS8nvAGRvRG1M/aYin4AKRBNHN8895HR1URttx7rpmWieBLxNq53UcmHqQTuMhYVGgLJXgNtOKQewtUSTHj3UyHyWFMJ0omTL0wZ1XSllTISUB1bX+nWy3UP4S3ofyKO/D2+ZPoGo/MM9TO9fMojrs9SYr5xwJBQU1qyqXvya8=</Modulus><Exponent>EQ==</Exponent><P>0oKRDL7tevjl5R3CYJDZSt8glGL9OmpEEHtg+DfUA6UAIOPD8euc5JBVueLgc9wYZAPEh0Pb68vp0qKaUBUaDQ==</P><Q>0cx5zJYNiv68tuluOBtLJfJQIOb3XJn7qo6CaMGLtgfgk1pW7vfBWdxwcep7MR9jTi26MlSKLPaLOhAs74svqw==</Q><DP>PeoqqWVU5+7aNFQL/ki4YVCvOrOz5AEjE+gNdi6JprgPGLt13b3Eu7H7GI4Fx7k0WabBVPXXRVoXmE3xJpzLbQ==</DP><DQ>JQX3YFa3GIdOeqGqCeay6JQsQgqkH2Z3tK+8qRMYp6cJg2pLk5UiHuqqbnSsU/Z67623cksncVivKF01GzauDw==</DQ><InverseQ>iCO+Gt/W6bDZ8ANCDoqNKzJhbU4XzB8x5GLHXIRVCu62S/QzWk0EFWojVT9dQYhoMdSO49G4pH9ZrPyh73Zz9A==</InverseQ><D>FEvZ9JS1ccJir5/7kxP/DyJiBiu0WOz5U6+nuyj/ffPzl8AI6fudZVOqIbjzQupkvqLUIoaYM2Xy4UyggRTtoV8VvA/+9iYjm4tuQqzXWLFoVbYFHUiLqOBUCtcHUxbs3Y6sVm9p+AmSwZtpma7YIFtqzJEag988GAedLZOQ4eE=</D></RSAKeyValue>")));
    ((str "S"), rsaKeyFromBytes (utf8 (str "<RSAKeyValue><Modulus>j80Nf4oDmJ3mf05VSQ3hM0uTY38I9L2HJ5Q6l8jLeddirJgYUk+3hQZsYYhGmDcwdPrmsxjp+sjlNGSOrXUUDU3KSYy9DfzGqWIWQDMumU5NEaERpMsH6Y0weuJajzKbHQahz0NfE8QENre9OJbg8K4I69ht32MV6x54JrQo9is=</Modulus><Exponent>EQ==</Exponent><P>ybi6dcxIGdkbY631x4xETiLPofJ1gA11MF0vDNb9HFQa7U/8fDIM0tW3P31ze7/mNs8efIGVhv0DoXZfBVKQGQ==</P><Q>tn6OKArNt8r8zXt1SiPWEcA5KYsiw1KwBih/u20uO7Iem7NXNyU0OdYg3CQWvz7XOwYGTwuzjisV9IRvNDKw4w==</Q><DP>dqjmJyzfHkN5he3b3sr7AMku17vMpa2QOpEqvEI6iSJqMT4cDNIlqTJryv6AKq0eAh97WEw51u8vT+tG9BJy4Q==</DP><DQ>lkoat5BtLfJ11mWr4rQZtEPy1usNkcugBRJLEtJiT0dkgDlW4h6jeuyTiB24YULPXci55r5XokGZnDDUDN5zjQ==</DQ><InverseQ>w98aj/4/FItHwfmOdAb4r1CL++AAoH+CVsZtnfKblVzy3sFTed5Kij1avFu7fDH0ilPyO8LGG0IWSokmCymQ0A==</InverseQ><D>bfcocJa3dLT7jocyKM5g6v2O051hM5/98Rb/oT8yL/8POLCLEcR9R5uADlkm7N7o0e0KxTErZWxzCfKLOVmHzMphWxEKNj8abwdLsgqN6Jgy97JiJ7MH0mBkffXU4vJx2yPTi6pgVdd/z+DJ36+s+gDcTOu6zUqmzWhf2j+qXWE=</D></RSAKeyValue>")));
    ((str "E"), rsaKeyFromBytes (utf8 (str "<RSAKeyValue><Modulus>o8PmgecLLftVqp1EFXyh+MJXQlcd6C9L5tr0Hicdp4iVKflsI9PrfkxOMp7a0ASok8Ps9JYWPz8iZNv3aHUPucCtmYuDxiLAqT2dxX5HRbae7P38dOv0NzfZ4BVjgf5lDGtG2exFiTCrPi+C3Ue3EAgKedA6N7dKvPE3zZorfg8=</Modulus><Exponent>EQ==</Exponent><P>0dnbcePCv6fTH4lC8MtjqOXpzoHsVpKkkzQrDU4nwC+UWcihFhoPaIekJxVO9u9jXq4HwWtG3QVtT9PBMIwMGQ==</P><Q>x8eEcznhmoh7fbVSMh9RfAZ1s0MLJk1Ff0AL4GWviEHijVvk87+juJ9kGcmXLlBEARQs0NMG8WUFZPb61LugZw==</Q><DP>JQhT5uv1MOFhfgkp7kIChzehuwfeaaFoVjZh80oHA8wpPQVJmn0Rxyb+2bh3WMDVTPGI5eXBNhAEO0N8cfqYuQ==</DP><DQ>vAcTP0WJCendo316iYbjR42b9ALdUTmq0h4pS6r/j00Rdfwix0r0cYb0zPn3sx5eHyIMD9WsLn0yQOhzmwrxUQ==</DQ><InverseQ>bemHqSRs2a2KmIquThfT1NJ1P3ugLcnFkJ/FTEMPvozMQhlaXyq3eDxscDXZJ/rJRAs8wwdM7MOrwL7K0d1JDA==</InverseQ><D>HOZV2rBNRFmHlpQ5MPfgWRM8kzyMzp7vRttYQY5upRgaUrOLjdoaf7Mc28GuJLWHKSKTOjiag6G6xoErqQWZXLuYvuEhFQVGppTFgQEQVb9bvp1zNEDHWOh7WhYP8PLuwAhCSwnWy2wmY97RpBUkIYcbz/vhKUc2o7BPe5K+yqE=</D></RSAKeyValue>")))
  ]

let publicKeys = List.map (fun (p,k) -> (p,rsa_pub k)) privateKeys

//printfn "%A" List.assoc a publicKeys

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