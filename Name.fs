namespace Loki

open System.Diagnostics

module Name =
    // Location represents a source file location.
    // [<StructuredFormatDisplay("{FileName}:{Line}:{Column}")>]
    [<StructuredFormatDisplay("**")>]
    type Location =
        { FileName: string
          Line: int
          Column: int }

        static member FromStackFrame() =
            let frame = StackFrame(1, true)

            { FileName = frame.GetFileName()
              Line = frame.GetFileLineNumber()
              Column = frame.GetFileColumnNumber() }

    module UniqueGen =
        let mutable private unique = 0

        let Next () =
            unique <- unique + 1
            unique

    [<StructuredFormatDisplay("{Value}.{Unique}")>]
    type Name =
        { Unique: int
          Value: string }

        static member FromString value =
            { Unique = UniqueGen.Next()
              Value = value }
