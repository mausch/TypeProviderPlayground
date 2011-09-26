#r "bin\debug\TypeProviderPlayground.dll"

open TypeProviderPlayground

type _5 = TypeProviderPlayground.N<5>

printfn "%A" (typeof<_5>.GetGenericArguments())
printfn "hello!"