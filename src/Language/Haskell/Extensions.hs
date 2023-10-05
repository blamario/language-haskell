{-# Language DeriveDataTypeable, FlexibleInstances, OverloadedStrings, TypeFamilies, TypeOperators #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ awaits

module Language.Haskell.Extensions (Extension(..), ExtensionSwitch(..),
                                    on, off,
                                    allExtensions, byName, includedByDefault, implications, inverseImplications,
                                    languageVersions, partitionContradictory, switchesByName, withImplications) where

import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Data (Data, Typeable)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Function.Memoize (Memoizable (memoize))
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
               | DeepSubsumption
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
               | EqualityConstraints -- unnamed in GHC
               | ExistentialQuantification
               | ExplicitForAll
               | ExplicitNamespaces
               | ExtendedDefaultRules
               | FieldSelectors
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
               | IdentifierSyntax -- active but unnamed in GHC
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
               | MultiParameterConstraints -- active unnamed in GHC
               | MultiWayIf
               | NPlusKPatterns
               | NamedFieldPuns
               | NamedWildCards
               | NegativeLiterals
               | NondecreasingIndentation
               | NullaryTypeClasses
               | NumDecimals
               | NumericUnderscores
               | OverlappingInstances
               | OverloadedLabels
               | OverloadedLists
               | OverloadedRecordDot
               | OverloadedRecordUpdate
               | OverloadedStrings
               | PackageImports
               | ParallelArrays
               | ParallelListComp
               | ParallelListComprehensions
               | ParenthesizedTypeOperators     -- active but unnamed in GHC
               | GratuitouslyParenthesizedTypes -- active but unnamed in GHC
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
               | RecordWildCards
               | RecursiveDo
               | RelaxedLayout
               | RelaxedPolyRec
               | RoleAnnotations
               | Safe
               | SafeImports
               | ScopedTypeVariables
               | SpaceSensitiveOperators
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
               | TypeVariableConstraints -- active but unnamed in GHC
               | UnboxedSums
               | UnboxedTuples
               | UndecidableInstances
               | UndecidableSuperClasses
               | UnicodeSyntax
               | UnliftedDatatypes
               | UnliftedFFITypes
               | UnliftedNewtypes
               | Unsafe
               | ViewPatterns
               deriving (Bounded, Data, Enum, Eq, Ord, Read, Show)

newtype ExtensionSwitch = ExtensionSwitch (Extension, Bool)
                          deriving (Data, Eq, Ord, Show)

instance Memoizable (Set Extension) where
   memoize f s = memoize (f . setFromBits) [Set.member e s | e <- [minBound .. maxBound]]

setFromBits :: [Bool] -> Set Extension
setFromBits = Set.fromList . map fst . filter snd . zip [minBound .. maxBound]

off, on :: Extension -> ExtensionSwitch
-- | The off-switch for an extension
off x = ExtensionSwitch (x, False)
-- | The on-switch for an extension
on x = ExtensionSwitch (x, True)

allExtensions :: Set Extension
allExtensions = Set.fromList [minBound .. maxBound]

includedByDefault :: Set Extension
includedByDefault = Set.fromList [DatatypeContexts, DoAndIfThenElse, EmptyDataDeclarations, EqualityConstraints,
                                  FieldSelectors, ForeignFunctionInterface,
                                  GratuitouslyParenthesizedTypes, IdentifierSyntax,
                                  MultiParameterConstraints, ParenthesizedTypeOperators, PatternGuards,
                                  RelaxedPolyRec, SpaceSensitiveOperators, StarIsType,
                                  TraditionalRecordSyntax, TypeVariableConstraints]

languageVersions :: Set Extension
languageVersions = Set.fromList [Haskell98, Haskell2010]

implications :: Map Extension (Map Extension Bool)
implications = transitiveClosure directImplications directImplications
  where transitiveClosure margin c
           | all Map.null margin' = c
           | otherwise = transitiveClosure margin' (Map.unionWith (<>) c margin')
           where margin' = Map.mapWithKey (Map.foldMapWithKey . marginOf) margin
                 marginOf k1 k2 True = Map.findWithDefault mempty k2 c Map.\\ Map.findWithDefault mempty k1 c
                 marginOf _ _ False = mempty

directImplications :: Map Extension (Map Extension Bool)
directImplications = Map.fromList <$> Map.fromList [
  (AutoDeriveTypeable, [(DeriveDataTypeable, True)]),
  (DeriveTraversable, [(DeriveFoldable, True), (DeriveFunctor, True)]),
  (DerivingVia, [(DerivingStrategies, True)]),
  (DuplicateRecordFields, [(DisambiguateRecordFields, True)]),
  (ExistentialQuantification, [(ExplicitForAll, True)]),
  (FlexibleInstances, [(TypeSynonymInstances, True)]),
  (FunctionalDependencies, [(MultiParamTypeClasses, True)]),
  (GADTs, [(EqualityConstraints, True), (ExistentialQuantification, True), (GADTSyntax, True), (MonoLocalBinds, True)]),
  (Haskell98, [(NPlusKPatterns, True), (NondecreasingIndentation, True),
               (DoAndIfThenElse, False), (EmptyDataDeclarations, False),
               (ForeignFunctionInterface, False), (PatternGuards, False), (RelaxedPolyRec, False)]),
  (ImpredicativeTypes, [(ExplicitForAll, True), (RankNTypes, True)]),
  (IncoherentInstances, [(OverlappingInstances, True)]),
  (KindSignatures, [(GratuitouslyParenthesizedTypes, True)]),
  (LiberalTypeSynonyms, [(ExplicitForAll, True)]),
  (LinearTypes, [(SpaceSensitiveOperators, True)]),
  (MultiParamTypeClasses, [(ConstrainedClassMethods, True), (MultiParameterConstraints, True)]),
  (ParallelListComp, [(ParallelListComprehensions, True)]),
  (PolyKinds, [(KindSignatures, True)]),
  (RankNTypes, [(ExplicitForAll, True)]),
  (RebindableSyntax, [(ImplicitPrelude, False)]),
  (RecordWildCards, [(DisambiguateRecordFields, True)]),
  (Safe, [(SafeImports, True)]),
  (ScopedTypeVariables, [(ExplicitForAll, True)]),
  (Trustworthy, [(SafeImports, True)]),
  (TypeFamilies, [(EqualityConstraints, True), (ExplicitNamespaces, True),
                  (KindSignatures, True), (MonoLocalBinds, True)]),
  (TypeFamilyDependencies, [(TypeFamilies, True)]),
  (TypeInType, [(PolyKinds, True), (DataKinds, True), (KindSignatures, True)]),
  (TypeOperators, [(ExplicitNamespaces, True), (ParenthesizedTypeOperators, True)]),
  (UnliftedDatatypes, [(DataKinds, True), (StandaloneKindSignatures, True)]),
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
withImplications extensions = extensions <> Map.unions (implications `Map.intersection` Map.filter id extensions)

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
  ("DeepSubsumption", DeepSubsumption),
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
  ("EqualityConstraints", EqualityConstraints), 
  ("ExistentialQuantification", ExistentialQuantification),
  ("ExplicitForAll", ExplicitForAll),
  ("ExplicitNamespaces", ExplicitNamespaces),
  ("ExtendedDefaultRules", ExtendedDefaultRules),
  ("FieldSelectors", FieldSelectors),
  ("FlexibleContexts", FlexibleContexts),
  ("FlexibleInstances", FlexibleInstances),
  ("ForeignFunctionInterface", ForeignFunctionInterface),
  ("FunctionalDependencies", FunctionalDependencies),
  ("GADTSyntax", GADTSyntax),
  ("GADTs", GADTs),
  ("GHCForeignImportPrim", GHCForeignImportPrim),
  ("GeneralisedNewtypeDeriving", GeneralizedNewtypeDeriving),
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
  ("MultiParameterConstraints", MultiParameterConstraints),
  ("MultiParamTypeClasses", MultiParamTypeClasses),
  ("MultiWayIf", MultiWayIf),
  ("NPlusKPatterns", NPlusKPatterns),
  ("NamedFieldPuns", NamedFieldPuns),
  ("NamedWildCards", NamedWildCards),
  ("NegativeLiterals", NegativeLiterals),
  ("NondecreasingIndentation", NondecreasingIndentation),
  ("NullaryTypeClasses", NullaryTypeClasses),
  ("NumDecimals", NumDecimals),
  ("NumericUnderscores", NumericUnderscores),
  ("OverlappingInstances", OverlappingInstances),
  ("OverloadedLabels", OverloadedLabels),
  ("OverloadedLists", OverloadedLists),
  ("OverloadedRecordDot", OverloadedRecordDot),
  ("OverloadedRecordUpdate", OverloadedRecordUpdate),
  ("OverloadedStrings", OverloadedStrings),
  ("PackageImports", PackageImports),
  ("ParallelArrays", ParallelArrays),
  ("ParallelListComp", ParallelListComp),
  ("ParallelListComp", ParallelListComprehensions),
  ("ParallelListComprehensions", ParallelListComprehensions),
  ("GratuitouslyParenthesizedTypes", GratuitouslyParenthesizedTypes),
  ("ParenthesizedTypeOperators", ParenthesizedTypeOperators),
  ("PartialTypeSignatures", PartialTypeSignatures),
  ("PatternGuards", PatternGuards),
  ("PatternSynonyms", PatternSynonyms),
  ("PolyKinds", PolyKinds),
  ("PostfixOperators", PostfixOperators),
  ("QualifiedDo", QualifiedDo),
  ("QuantifiedConstraints", QuantifiedConstraints),
  ("QuasiQuotes", QuasiQuotes),
  ("Rank2Types", RankNTypes),
  ("RankNTypes", RankNTypes),
  ("RebindableSyntax", RebindableSyntax),
  ("RecordWildCards", RecordWildCards),
  ("RecursiveDo", RecursiveDo),
  ("RelaxedLayout", RelaxedLayout),
  ("RelaxedPolyRec", RelaxedPolyRec),
  ("RoleAnnotations", RoleAnnotations),
  ("Safe", Safe),
  ("ScopedTypeVariables", ScopedTypeVariables),
  ("SpaceSensitiveOperators", SpaceSensitiveOperators),
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
  ("UnliftedDatatypes", UnliftedDatatypes),
  ("UnliftedFFITypes", UnliftedFFITypes),
  ("UnliftedNewtypes", UnliftedNewtypes),
  ("Unsafe", Unsafe),
  ("ViewPatterns", ViewPatterns)]
