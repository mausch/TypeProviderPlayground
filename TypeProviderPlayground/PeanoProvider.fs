namespace TypeProviderPlayground

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open System.Runtime.CompilerServices

[<assembly: TypeProviderAssembly()>]
do()

[<Sealed>]
type Z = class end
[<Sealed>]
type 'a S = class end

open TypeHelpers

[<TypeProvider>]
type PeanoProvider(s: TypeProviderConfig) =
    let invalidate = Event<_,_>()
    interface ITypeProvider with
        member __.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            let n : int = unbox staticArguments.[0]
            let t = [1..n] |> List.fold (fun s _ -> typedefof<S<_>>.MakeGenericType [| s |]) typeof<Z>
            t |> rename typeNameWithArguments |> erase
        member x.GetNamespaces() =  
            let typeN = rename "N" typeof<Z>
            let ns = 
                let types = [| typeof<Z>; typedefof<S<_>>; typeN |] |> Array.map erase
                let namespaceName = "TypeProviderPlayground"
                { new IProvidedNamespace with
                    member x.GetNestedNamespaces() = [||]
                    member x.GetTypes() = Array.copy types
                    member x.ResolveTypeName typeName =
                        Array.find (fun ty -> ty.Name = typeName) types
                    member x.NamespaceName = namespaceName }
            [| ns |]
        member x.GetStaticParameters t =
            let p = 
                { new ParameterInfo() with
                    member z.Name = "number"
                    member z.ParameterType = typeof<int> }
            [| p |]

        [<CLIEvent>]
        member x.Invalidate = invalidate.Publish
        member x.Dispose() = ()
        member x.GetInvokerExpression(syntheticMethodBase, parameters) = null
