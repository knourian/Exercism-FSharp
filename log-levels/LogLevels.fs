module LogLevels

let message (logLine: string): string = logLine.Substring(logLine.IndexOf(':')+1).Trim()

let logLevel(logLine: string): string =
    let fromIndex = logLine.IndexOf('[')+1
    let toIndex = logLine.IndexOf(']')
    logLine.Substring(fromIndex,toIndex-fromIndex).ToLower()

let reformat(logLine: string): string = 
    let logMessage = message logLine
    let level = logLevel logLine
    sprintf "%s (%s)" logMessage level
