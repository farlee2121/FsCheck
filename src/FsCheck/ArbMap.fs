namespace rec FsCheck

open System

open FsCheck.FSharp
open FsCheck.Internals

/// Maps types to Arbitrary instances for that type.
/// Once constructed, the map is immutable.
type IArbMap =
    abstract ArbFor: Type -> Arbitrary<obj>
    abstract ArbFor<'T> : unit -> Arbitrary<'T>

type private ArbMapInit = 
| FromTypeClass of TypeClass.TypeClass<Arbitrary<obj>>
| FromDiscover of (Type * ArbMap option)

// Note: the only implementation of IArbMap. The main reason this type is
// not exposed directly, is because that gives annoying type name clashes
// between the ArbMap type, the ArbMap module (which can be addressed with ModuleSuffix),
// and the ArbMap extension type in FsCheck.Fluent.
type internal ArbMap private (init: ArbMapInit) as this=
    
    let finder = 
        match init with
        | FromTypeClass tc -> tc
        | FromDiscover (typ, existingMap) -> 
            let finder =
                match existingMap with
                | None ->
                    TypeClass.TypeClass<Arbitrary<obj>>
                        .New(injectParameters = true)
                | Some arbFinder -> arbFinder.ArbFinder
            
            finder.DiscoverAndMerge(onlyPublic = false, instancesType = typ, newInjectedConfigs = [| this |])

    member private _.ArbFinder = finder

    internal new (typ:Type, ?existingMap:ArbMap) = 
        ArbMap(FromDiscover (typ, existingMap))

    // for testing purposes
    member internal _.MemoizedInstances = finder.MemoizedInstances

    member internal _.MergeFactory(factory: Func<'a,'b>) =
        ArbMap(FromTypeClass (finder.MergeFactory(factory)) )

    interface IArbMap with
        member _.ArbFor t =
            finder.GetInstance t
            |> unbox<IArbitrary>
            |> (fun arb -> Arb.fromGenShrink (arb.GeneratorObj, arb.ShrinkerObj))

        member _.ArbFor<'TArb>() =
            finder.InstanceFor<'TArb, Arbitrary<'TArb>>()
