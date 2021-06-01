module Endpoint

open System

// Disable raw url mode for socket when running from Kestrel
let host = ""
// let host = "localhost:5000"

let rootUrl protocol =
    if host = String.Empty then host
    else sprintf "%s://%s" protocol host

// For Scala backend
let socket =
    let path = sprintf "/socket/%A" <| Guid.NewGuid ()
    rootUrl "ws" + path

// For F# backend
// let socket = rootUrl "ws" + "/socket"

let httpApi p = rootUrl "http" + "/api" + p
