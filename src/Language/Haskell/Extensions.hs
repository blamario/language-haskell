{-# Language DeriveDataTypeable, OverloadedStrings #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ awaits

module Language.Haskell.Extensions (Extension(..), ExtensionSwitch(..),
                                    on, off,
                                    allExtensions, byName, includedByDefault, implications, inverseImplications,
                                    languageVersions, partitionContradictory, switchesByName, withImplications) where

import Data.Bool (bool)
import Data.Data (Data, Typeable)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Semigroup.Union (UnionWith(..))
import Data.String (IsString)

data Extension = AllowAmbiguousTypes
               | AlternativeLayoutRule
               | AlternativeLayoutRuleTransitional
               | ApplicativeDo
               | Arrows
               | AutoDeriveTypeable
               | BangPatterns
               | BinaryLiterals
               | BlockArguments
               | CApiFFI
               | CUSKs
               | ConstrainedClassMethods
               | ConstraintKinds
               | Cpp
               | DataKinds
               | DatatypeContexts
               | DefaultSignatures
               | DeriveAnyClass
               | DeriveDataTypeable
               | DeriveFoldable
               | DeriveFunctor
               | DeriveGeneric
               | DeriveLift
               | DeriveTraversable
               | DerivingStrategies
               | DerivingVia
               | DisambiguateRecordFields
               | DoAndIfThenElse
               | DuplicateRecordFields
               | EmptyCase
               | EmptyDataDeclarations
               | EmptyDataDeriving
               | ExistentialQuantification
               | ExplicitForAll
               | ExplicitNamespaces
               | ExtendedDefaultRules
               | FlexibleContexts
               | FlexibleInstances
               | ForeignFunctionInterface
               | FunctionalDependencies
               | GADTSyntax
               | GADTs
               | GHCForeignImportPrim
               | GeneralizedNewtypeDeriving
               | Haskell2010
               | Haskell98
               | HexFloatLiterals
               | IdentifierSyntax
               | ImplicitParams
               | ImplicitPrelude
               | ImportQualifiedPost
               | ImpredicativeTypes
               | IncoherentInstances
               | InstanceSigs
               | InterruptibleFFI
               | JavaScriptFFI
               | KindSignatures
               | LambdaCase
               | LexicalNegation
               | LiberalTypeSynonyms
               | LinearTypes
               | MagicHash
               | MonadComprehensions
               | MonadFailDesugaring
               | MonoLocalBinds
               | MonoPatBinds
               | MonomorphismRestriction
               | MultiParamTypeClasses
               | MultiWayIf
               | NPlusKPatterns
               | NamedWildCards
               | NegativeLiterals
               | NondecreasingIndentation
               | NullaryTypeClasses
               | NumDecimals
               | NumericUnderscores
               | OverlappingInstances
               | OverloadedLabels
               | OverloadedLists
               | OverloadedStrings
               | PackageImports
               | ParallelArrays
               | ParallelListComp
               | ParallelListComprehensions
               | PartialTypeSignatures
               | PatternGuards
               | PatternSynonyms
               | PolyKinds
               | PostfixOperators
               | QualifiedDo
               | QuantifiedConstraints
               | QuasiQuotes
               | RankNTypes
               | RebindableSyntax
               | RecordPuns
               | RecordWildCards
               | RecursiveDo
               | RelaxedLayout
               | RelaxedPolyRec
               | RoleAnnotations
               | Safe
               | SafeImports
               | ScopedTypeVariables
               | StandaloneDeriving
               | StandaloneKindSignatures
               | StarIsType
               | StaticPointers
               | Strict
               | StrictData
               | TemplateHaskell
               | TemplateHaskellQuotes
               | TraditionalRecordSyntax
               | TransformListComp
               | Trustworthy
               | TupleSections
               | TypeApplications
               | TypeFamilies
               | TypeFamilyDependencies
               | TypeInType
               | TypeOperators
               | TypeSynonymInstances
               | UnboxedSums
               | UnboxedTuples
               | UndecidableInstances
               | UndecidableSuperClasses
               | UnicodeSyntax
               | UnliftedFFITypes
               | UnliftedNewtypes
               | Unsafe
               | ViewPatterns
               deriving (Bounded, Data, Enum, Eq, Ord, Read, Show)

newtype ExtensionSwitch = ExtensionSwitch (Extension, Bool)
                          deriving (Data, Eq, Ord, Show)

off, on :: Extension -> ExtensionSwitch
-- | The off-switch for an extension
off x = ExtensionSwitch (x, False)
-- | The on-switch for an extension
on x = ExtensionSwitch (x, True)

allExtensions :: Set Extension
allExtensions = Set.fromList [minBound .. maxBound]

includedByDefault :: Set Extension
includedByDefault = Set.fromList [DoAndIfThenElse, EmptyDataDeclarations, ForeignFunctionInterface,
                                  PatternGuards, RelaxedPolyRec]

languageVersions :: Set Extension
languageVersions = Set.fromList [Haskell98, Haskell2010]

implications :: Map Extension (Map Extension Bool)
implications = Map.fromList <$> Map.fromList [
  (AutoDeriveTypeable, [(DeriveDataTypeable, True)]),
  (DeriveTraversable, [(DeriveFoldable, True), (DeriveFunctor, True)]),
  (DerivingVia, [(DerivingStrategies, True)]),
  (ExistentialQuantification, [(ExplicitForAll, True)]),
  (FlexibleInstances, [(TypeSynonymInstances, True)]),
  (FunctionalDependencies, [(MultiParamTypeClasses, True)]),
  (GADTs, [(GADTSyntax, True), (MonoLocalBinds, True)]),
  (Haskell98, [(NPlusKPatterns, True), (NondecreasingIndentation, True),
               (DoAndIfThenElse, False), (EmptyDataDeclarations, False),
               (ForeignFunctionInterface, False), (PatternGuards, False), (RelaxedPolyRec, False)]),
  (ImpredicativeTypes, [(ExplicitForAll, True), (RankNTypes, True)]),
  (IncoherentInstances, [(OverlappingInstances, True)]),
  (LiberalTypeSynonyms, [(ExplicitForAll, True)]),
  (ParallelListComp, [(ParallelListComprehensions, True)]),
  (PolyKinds, [(KindSignatures, True)]),
  (RankNTypes, [(ExplicitForAll, True)]),
  (RebindableSyntax, [(ImplicitPrelude, False)]),
  (RecordWildCards, [(DisambiguateRecordFields, True)]),
  (ScopedTypeVariables, [(ExplicitForAll, True)]),
  (Safe, [(SafeImports, True)]),
  (Trustworthy, [(SafeImports, True)]),
  (TypeFamilies, [(ExplicitNamespaces, True), (KindSignatures, True), (MonoLocalBinds, True)]),
  (TypeFamilyDependencies, [(ExplicitNamespaces, True), (KindSignatures, True),
                            (MonoLocalBinds, True), (TypeFamilies, True)]),
  (TypeOperators, [(ExplicitNamespaces, True)]),
  (Unsafe, [(SafeImports, True)])]

inverseImplications :: Map Extension (Set Extension)
inverseImplications = getUnionWith $ Map.foldMapWithKey inverse implications 
   where inverse parent = UnionWith . Map.mapMaybe (bool Nothing $ Just $ Set.singleton parent)
      
-- | Given a set of extension switches, provides a 'Map' of extensions to their 'on'/'off' state an a 'Set' of
-- contradictory extensions.
partitionContradictory :: Set ExtensionSwitch -> (Set ExtensionSwitch, Map Extension Bool)
partitionContradictory switches = (Map.keysSet contradictions, Map.mapKeys getExtension consistents)
   where (contradictions, consistents) = Map.partitionWithKey isContradicted extensionMap
         extensionMap :: Map ExtensionSwitch Bool
         extensionMap = Map.fromSet getSwitch switches
         isContradicted (ExtensionSwitch (x, s)) _ = ExtensionSwitch (x, not s) `Set.member` switches
         getExtension (ExtensionSwitch (x, _)) = x
         getSwitch (ExtensionSwitch (_, s)) = s

-- | Adds the implied extensions to the given set of extension switches
withImplications :: Map Extension Bool -> Map Extension Bool
withImplications extensions = extensions `Map.union` Map.foldMapWithKey implied extensions
   where implied extension True = Map.findWithDefault mempty extension implications
         implied _ False = mempty

inverse :: ExtensionSwitch -> ExtensionSwitch
inverse (ExtensionSwitch (ext, s)) = ExtensionSwitch (ext, not s)

switchesByName :: (IsString t, Ord t, Semigroup t) => Map t ExtensionSwitch
switchesByName = ExtensionSwitch <$> ((flip (,) True <$> byName)
                                      <> (flip (,) False <$> Map.mapKeysMonotonic ("No" <>) byName))

byName :: (IsString t, Ord t) => Map t Extension
byName = Map.fromList [
  ("AllowAmbiguousTypes", AllowAmbiguousTypes),
  ("AlternativeLayoutRule", AlternativeLayoutRule),
  ("AlternativeLayoutRuleTransitional", AlternativeLayoutRuleTransitional),
  ("ApplicativeDo", ApplicativeDo),
  ("Arrows", Arrows),
  ("AutoDeriveTypeable", AutoDeriveTypeable),
  ("BangPatterns", BangPatterns),
  ("BinaryLiterals", BinaryLiterals),
  ("BlockArguments", BlockArguments),
  ("CApiFFI", CApiFFI),
  ("CUSKs", CUSKs),
  ("ConstrainedClassMethods", ConstrainedClassMethods),
  ("ConstraintKinds", ConstraintKinds),
  ("Cpp", Cpp),
  ("DataKinds", DataKinds),
  ("DatatypeContexts", DatatypeContexts),
  ("DefaultSignatures", DefaultSignatures),
  ("DeriveAnyClass", DeriveAnyClass),
  ("DeriveDataTypeable", DeriveDataTypeable),
  ("DeriveFoldable", DeriveFoldable),
  ("DeriveFunctor", DeriveFunctor),
  ("DeriveGeneric", DeriveGeneric),
  ("DeriveLift", DeriveLift),
  ("DeriveTraversable", DeriveTraversable),
  ("DerivingStrategies", DerivingStrategies),
  ("DerivingVia", DerivingVia),
  ("DisambiguateRecordFields", DisambiguateRecordFields),
  ("DoAndIfThenElse", DoAndIfThenElse),
  ("DuplicateRecordFields", DuplicateRecordFields),
  ("EmptyCase", EmptyCase),
  ("EmptyCase", EmptyCase),
  ("EmptyDataDeclarations", EmptyDataDeclarations),
  ("EmptyDataDecls", EmptyDataDeclarations),
  ("EmptyDataDeriving", EmptyDataDeriving),
  ("ExistentialQuantification", ExistentialQuantification),
  ("ExplicitForAll", ExplicitForAll),
  ("ExplicitNamespaces", ExplicitNamespaces),
  ("ExtendedDefaultRules", ExtendedDefaultRules),
  ("FlexibleContexts", FlexibleContexts),
  ("FlexibleInstances", FlexibleInstances),
  ("ForeignFunctionInterface", ForeignFunctionInterface),
  ("FunctionalDependencies", FunctionalDependencies),
  ("GADTSyntax", GADTSyntax),
  ("GADTs", GADTs),
  ("GHCForeignImportPrim", GHCForeignImportPrim),
  ("GeneralizedNewtypeDeriving", GeneralizedNewtypeDeriving),
  ("Haskell2010", Haskell2010),
  ("Haskell98", Haskell98),
  ("HexFloatLiterals", HexFloatLiterals),
  ("IdentifierSyntax", IdentifierSyntax),
  ("ImplicitParams", ImplicitParams),
  ("ImplicitPrelude", ImplicitPrelude),
  ("ImportQualifiedPost", ImportQualifiedPost),
  ("ImpredicativeTypes", ImpredicativeTypes),
  ("IncoherentInstances", IncoherentInstances),
  ("InstanceSigs", InstanceSigs),
  ("InterruptibleFFI", InterruptibleFFI),
  ("JavaScriptFFI", JavaScriptFFI),
  ("KindSignatures", KindSignatures),
  ("LambdaCase", LambdaCase),
  ("LexicalNegation", LexicalNegation),
  ("LiberalTypeSynonyms", LiberalTypeSynonyms),
  ("LinearTypes", LinearTypes),
  ("MagicHash", MagicHash),
  ("MonadComprehensions", MonadComprehensions),
  ("MonadFailDesugaring", MonadFailDesugaring),
  ("MonoLocalBinds", MonoLocalBinds),
  ("MonoPatBinds", MonoPatBinds),
  ("MonomorphismRestriction", MonomorphismRestriction),
  ("MultiParamTypeClasses", MultiParamTypeClasses),
  ("MultiWayIf", MultiWayIf),
  ("NPlusKPatterns", NPlusKPatterns),
  ("NamedWildCards", NamedWildCards),
  ("NegativeLiterals", NegativeLiterals),
  ("NondecreasingIndentation", NondecreasingIndentation),
  ("NullaryTypeClasses", NullaryTypeClasses),
  ("NumDecimals", NumDecimals),
  ("NumericUnderscores", NumericUnderscores),
  ("OverlappingInstances", OverlappingInstances),
  ("OverloadedLabels", OverloadedLabels),
  ("OverloadedLists", OverloadedLists),
  ("OverloadedStrings", OverloadedStrings),
  ("PackageImports", PackageImports),
  ("ParallelArrays", ParallelArrays),
  ("ParallelListComp", ParallelListComp),
  ("ParallelListComp", ParallelListComprehensions),
  ("ParallelListComprehensions", ParallelListComprehensions),
  ("PartialTypeSignatures", PartialTypeSignatures),
  ("PatternGuards", PatternGuards),
  ("PatternSynonyms", PatternSynonyms),
  ("PolyKinds", PolyKinds),
  ("PostfixOperators", PostfixOperators),
  ("QualifiedDo", QualifiedDo),
  ("QuantifiedConstraints", QuantifiedConstraints),
  ("QuasiQuotes", QuasiQuotes),
  ("RankNTypes", RankNTypes),
  ("RebindableSyntax", RebindableSyntax),
  ("RecordPuns", RecordPuns),
  ("RecordWildCards", RecordWildCards),
  ("RecursiveDo", RecursiveDo),
  ("RelaxedLayout", RelaxedLayout),
  ("RelaxedPolyRec", RelaxedPolyRec),
  ("RoleAnnotations", RoleAnnotations),
  ("Safe", Safe),
  ("ScopedTypeVariables", ScopedTypeVariables),
  ("StandaloneDeriving", StandaloneDeriving),
  ("StandaloneKindSignatures", StandaloneKindSignatures),
  ("StarIsType", StarIsType),
  ("StaticPointers", StaticPointers),
  ("Strict", Strict),
  ("StrictData", StrictData),
  ("TemplateHaskell", TemplateHaskell),
  ("TemplateHaskellQuotes", TemplateHaskellQuotes),
  ("TraditionalRecordSyntax", TraditionalRecordSyntax),
  ("TransformListComp", TransformListComp),
  ("Trustworthy", Trustworthy),
  ("TupleSections", TupleSections),
  ("TypeApplications", TypeApplications),
  ("TypeFamilies", TypeFamilies),
  ("TypeFamilyDependencies", TypeFamilyDependencies),
  ("TypeInType", TypeInType),
  ("TypeOperators", TypeOperators),
  ("TypeSynonymInstances", TypeSynonymInstances),
  ("UnboxedSums", UnboxedSums),
  ("UnboxedTuples", UnboxedTuples),
  ("UndecidableInstances", UndecidableInstances),
  ("UndecidableSuperClasses", UndecidableSuperClasses),
  ("UnicodeSyntax", UnicodeSyntax),
  ("UnliftedFFITypes", UnliftedFFITypes),
  ("UnliftedNewtypes", UnliftedNewtypes),
  ("Unsafe", Unsafe),
  ("ViewPatterns", ViewPatterns)]
