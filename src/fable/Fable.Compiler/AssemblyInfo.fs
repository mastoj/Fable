﻿namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.4.3")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.4.3"
    let [<Literal>] InformationalVersion = "0.4.3"
