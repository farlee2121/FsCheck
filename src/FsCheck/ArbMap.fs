namespace FsCheck

open System

open FsCheck.FSharp
open FsCheck.Internals

/// Maps types to Arbitrary instances for that type.
/// Once constructed, the map is immutable.
type IArbMap =
    abstract ArbFor: Type -> Arbitrary<obj>
    abstract ArbFor<'T> : unit -> Arbitrary<'T>

// Note: the only implementation of IArbMap. The main reason this type is
// not exposed directly, is because that gives annoying type name clashes
// between the ArbMap type, the ArbMap module (which can be addressed with ModuleSuffix),
// and the ArbMap extension type in FsCheck.Fluent.
type internal ArbMap private  (tc: TypeClass.TypeClass<Arbitrary<obj>>) as this=
    
    let finder = tc.Merge(TypeClass.TypeClass(injectedConfigs = [| this |]))
    member private this.ArbFinder = finder

    internal new (typ:Type, ?existingMap:ArbMap) = 
        let finder =
            match existingMap with
            | None ->
                TypeClass.TypeClass<Arbitrary<obj>>
                    .New(injectParameters = true)
            | Some arbFinder -> arbFinder.ArbFinder
        
        let merged = finder.DiscoverAndMerge(onlyPublic = false, instancesType = typ)
        ArbMap(merged)

    // for testing purposes
    member internal this.MemoizedInstances = this.ArbFinder.MemoizedInstances

    member internal this.MergeFactory(factory: Func<'a,Arbitrary<'b>>) =
        ArbMap(this.ArbFinder.MergeFactory(factory)) 

    member internal this.MergeFactory(target: obj, method: System.Reflection.MethodInfo) =
        ArbMap(this.ArbFinder.MergeFactory(target,method))

    interface IArbMap with
        member this.ArbFor t =
            this.ArbFinder.GetInstance t
            |> unbox<IArbitrary>
            |> (fun arb -> Arb.fromGenShrink (arb.GeneratorObj, arb.ShrinkerObj))

        member this.ArbFor<'TArb>() =
            this.ArbFinder.InstanceFor<'TArb, Arbitrary<'TArb>>()
