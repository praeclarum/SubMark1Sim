﻿namespace SubMark1Sim
open System
open AppKit

module main =
    [<EntryPoint>]
    let main args =
        NSApplication.Init ()
        NSApplication.Main (args)
        0
