{-# Language DataKinds, DeriveDataTypeable, FlexibleInstances, OverloadedStrings, TypeFamilies, TypeOperators #-}

-- | The module exports the set of recognized language extensions, mostly corresponding to GHC extensions
module Language.Haskell.Extensions (Extension(..), ExtensionSwitch(..),
                                    On, Off, on, off,
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

-- | The enumeration of all language extensions
data Extension = AllowAmbiguousTypes
               | AlternativeLayoutRule
               | AlternativeLayoutRuleTransitional
               | ApplicativeDo
               | Arrows
               | AutoDeriveTypeable
               | BangDataFields -- ^ active but nameless in GHC and Report
               | BangPatterns
               | BinaryLiterals
               | BlockArguments
               | CApiFFI
               | CUSKs
               | ConstrainedClassMethods
               | ConstraintsAreTypes -- ^ active but nameless in GHC
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
               | EqualityConstraints -- ^ nameless in GHC
               | ExistentialQuantification
               | ExplicitForAll
               | ExplicitNamespaces
               | ExtendedDefaultRules
               | ExtendedLiterals
               | FieldSelectors
               | FlexibleContexts
               | FlexibleInstances
               | ForeignFunctionInterface
               | FunctionalDependencies
               | GADTSyntax
               | GADTs
               | GHC2021
               | GHC2024
               | GHCForeignImportPrim
               | GeneralizedNewtypeDeriving
               | Haskell2010
               | Haskell98
               | HexFloatLiterals
               | IdentifierSyntax -- ^ active but nameless in GHC
               | ImplicitParameters
               | ImplicitPrelude
               | ImportQualifiedPost
               | ImpredicativeTypes
               | IncoherentInstances
               | InferredTypeVariables -- ^ nameless in GHC
               | InstanceSigs
               | InterruptibleFFI
               | JavaScriptFFI
               | KindSignatures
               | LambdaCase
               | LexicalNegation
               | LiberalTypeSynonyms
               | LinearTypes
               | ListTuplePuns
               | MagicHash
               | MonadComprehensions
               | MonadFailDesugaring
               | MonoLocalBinds
               | MonoPatBinds
               | MonomorphismRestriction
               | MultilineStrings
               | MultiParamTypeClasses
               | MultiParameterConstraints -- ^ active, nameless in GHC
               | MultiWayIf
               | NPlusKPatterns
               | NamedDefaults
               | NamedFieldPuns
               | NamedWildCards
               | NegativeLiterals
               | NondecreasingIndentation
               | NullaryTypeClasses
               | NumDecimals
               | NumericUnderscores
               | OrPatterns
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
               | ParenthesizedTypeOperators     -- ^ active but nameless in GHC
               | GratuitouslyParenthesizedTypes -- ^ active but nameless in GHC
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
               | RequiredTypeArguments
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
               | TypeAbstractions
               | TypeAbstractionsOrApplicationsInConstructorPatterns -- ^ nameless
               | TypeApplications
               | TypeData
               | TypeFamilies
               | TypeFamilyDependencies
               | TypeInType
               | TypeOperators
               | TypeSynonymInstances
               | TypeVariableConstraints -- ^ active but nameless in GHC
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
               | VisibleDependentQuantification -- ^ nameless
               deriving (Bounded, Data, Enum, Eq, Ord, Read, Show)

-- | An extension together with an on/off boolean
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

type On (e :: Extension) = 'ExtensionSwitch '( e, 'True) :: ExtensionSwitch
type Off (e :: Extension) = 'ExtensionSwitch '( e, 'False) :: ExtensionSwitch

-- | Set of all extensions
allExtensions :: Set Extension
allExtensions = Set.fromList [minBound .. maxBound]

-- | Set of extensions that are on by default
includedByDefault :: Set Extension
includedByDefault = Set.fromList [BangDataFields, ConstraintsAreTypes,
                                  DatatypeContexts, DoAndIfThenElse, EmptyDataDeclarations, EqualityConstraints,
                                  FieldSelectors, ForeignFunctionInterface,
                                  GratuitouslyParenthesizedTypes, IdentifierSyntax, ListTuplePuns,
                                  MultiParameterConstraints, ParenthesizedTypeOperators, PatternGuards,
                                  RelaxedPolyRec, SpaceSensitiveOperators, StarIsType,
                                  TraditionalRecordSyntax, TypeVariableConstraints]

-- | Set of language version extensions, such as 'Haskell2010'
languageVersions :: Set Extension
languageVersions = Set.fromList [Haskell98, Haskell2010]

-- | Map of all extension implications, including 'directImplications' but adding transitive implications
implications :: Map Extension (Map Extension Bool)
implications = transitiveClosure directImplications directImplications
  where transitiveClosure margin c
           | all Map.null margin' = c
           | otherwise = transitiveClosure margin' (Map.unionWith (<>) c margin')
           where margin' = Map.mapWithKey (Map.foldMapWithKey . marginOf) margin
                 marginOf k1 k2 True = Map.findWithDefault mempty k2 c Map.\\ Map.findWithDefault mempty k1 c
                 marginOf _ _ False = mempty

-- | Map of direct extension implications
directImplications :: Map Extension (Map Extension Bool)
directImplications = Map.fromList <$> Map.fromList [
  (AutoDeriveTypeable, [(DeriveDataTypeable, True)]),
  (DeriveTraversable, [(DeriveFoldable, True), (DeriveFunctor, True)]),
  (DerivingVia, [(DerivingStrategies, True)]),
  (DuplicateRecordFields, [(DisambiguateRecordFields, True)]),
  (ExistentialQuantification, [(ExplicitForAll, True)]),
  (ExplicitForAll, [(InferredTypeVariables, True)]),
  (FlexibleInstances, [(TypeSynonymInstances, True)]),
  (FunctionalDependencies, [(MultiParamTypeClasses, True)]),
  (GADTs, [(EqualityConstraints, True), (ExistentialQuantification, True), (GADTSyntax, True), (MonoLocalBinds, True)]),
  (GHC2021, [(BangPatterns, True), (BinaryLiterals, True),
             (ConstrainedClassMethods, True), (ConstraintKinds, True),
             (DeriveDataTypeable, True), (DeriveFoldable, True), (DeriveFunctor, True),
             (DeriveGeneric, True), (DeriveLift, True), (DeriveTraversable, True), (DoAndIfThenElse, True),
             (EmptyCase, True), (EmptyDataDeclarations, True), (EmptyDataDeriving, True),
             (ExistentialQuantification, True), (ExplicitForAll, True),
             (FieldSelectors, True), (FlexibleContexts, True),
             (FlexibleInstances, True), (ForeignFunctionInterface, True),
             (GADTSyntax, True), (GeneralizedNewtypeDeriving, True),
             (HexFloatLiterals, True),
             (ImplicitPrelude, True), (ImportQualifiedPost, True), (InstanceSigs, True),
             (KindSignatures, True),
             (MonomorphismRestriction, True), (MultiParamTypeClasses, True),
             (NamedFieldPuns, True), (NamedWildCards, True), (NumericUnderscores, True),
             (PatternGuards, True), (PolyKinds, True), (PostfixOperators, True),
             (RankNTypes, True), (RelaxedPolyRec, True),
             (ScopedTypeVariables, True), (StandaloneDeriving, True),
             (StandaloneKindSignatures, True), (StarIsType, True),
             (TraditionalRecordSyntax, True), (TupleSections, True),
             (TypeApplications, True), (TypeOperators, True), (TypeSynonymInstances, True),
             (ExplicitNamespaces, False)]),
  (GHC2024, [(GHC2021, True), (DataKinds, True), (DerivingStrategies, True), (DisambiguateRecordFields, True),
             (ExplicitNamespaces, True), (GADTs, True), (LambdaCase, True),
             (MonoLocalBinds, True), (RoleAnnotations, True)]),
  (Haskell98, [(NPlusKPatterns, True), (NondecreasingIndentation, True),
               (DoAndIfThenElse, False), (EmptyDataDeclarations, False),
               (ForeignFunctionInterface, False), (PatternGuards, False), (RelaxedPolyRec, False)]),
  (ImpredicativeTypes, [(ExplicitForAll, True), (RankNTypes, True)]),
  (IncoherentInstances, [(OverlappingInstances, True)]),
  (JavaScriptFFI, [(InterruptibleFFI, True)]),
  (KindSignatures, [(GratuitouslyParenthesizedTypes, True)]),
  (LiberalTypeSynonyms, [(ExplicitForAll, True)]),
  (LinearTypes, [(SpaceSensitiveOperators, True)]),
  (MultiParamTypeClasses, [(ConstrainedClassMethods, True), (MultiParameterConstraints, True)]),
  (ParallelArrays, [(ParallelListComprehensions, True)]),
  (ParallelListComp, [(ParallelListComprehensions, True)]),
  (PolyKinds, [(KindSignatures, True), (VisibleDependentQuantification, True)]),
  (QuantifiedConstraints, [(ExplicitForAll, True)]),
  (RankNTypes, [(ExplicitForAll, True)]),
  (RebindableSyntax, [(ImplicitPrelude, False)]),
  (RecordWildCards, [(DisambiguateRecordFields, True)]),
  (RequiredTypeArguments, [(VisibleDependentQuantification, True)]),
  (Safe, [(SafeImports, True)]),
  (ScopedTypeVariables, [(ExplicitForAll, True)]),
  (StandaloneKindSignatures, [(CUSKs, False)]),
  (Strict, [(StrictData, True)]),
  (TemplateHaskell, [(TemplateHaskellQuotes, True)]),
  (Trustworthy, [(SafeImports, True)]),
  (TypeAbstractions, [(TypeAbstractionsOrApplicationsInConstructorPatterns, True)]),
  (TypeApplications, [(InferredTypeVariables, True), (TypeAbstractionsOrApplicationsInConstructorPatterns, True)]),
  (TypeFamilies, [(EqualityConstraints, True), (ExplicitNamespaces, True),
                  (KindSignatures, True), (MonoLocalBinds, True)]),
  (TypeFamilyDependencies, [(TypeFamilies, True)]),
  (TypeInType, [(PolyKinds, True), (DataKinds, True), (KindSignatures, True)]),
  (TypeOperators, [(ExplicitNamespaces, True), (ParenthesizedTypeOperators, True)]),
  (UnboxedTuples, [(UnboxedSums, True)]),
  (UnliftedDatatypes, [(DataKinds, True), (StandaloneKindSignatures, True)]),
  (Unsafe, [(SafeImports, True)])]

-- | Inverse of the 'implications' map
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

-- | Map from valid extension switch strings (such as "EmptyCase" or "NoArrows") to the corresponding
-- extension switches
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
  ("BangDataFields", BangDataFields),
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
  ("EmptyDataDeclarations", EmptyDataDeclarations),
  ("EmptyDataDecls", EmptyDataDeclarations),
  ("EmptyDataDeriving", EmptyDataDeriving),
  ("EqualityConstraints", EqualityConstraints), 
  ("ExistentialQuantification", ExistentialQuantification),
  ("ExplicitForAll", ExplicitForAll),
  ("ExplicitNamespaces", ExplicitNamespaces),
  ("ExtendedDefaultRules", ExtendedDefaultRules),
  ("ExtendedLiterals", ExtendedLiterals),
  ("FieldSelectors", FieldSelectors),
  ("FlexibleContexts", FlexibleContexts),
  ("FlexibleInstances", FlexibleInstances),
  ("ForeignFunctionInterface", ForeignFunctionInterface),
  ("FunctionalDependencies", FunctionalDependencies),
  ("GADTSyntax", GADTSyntax),
  ("GADTs", GADTs),
  ("GHC2021", GHC2021),
  ("GHC2024", GHC2024),
  ("GHCForeignImportPrim", GHCForeignImportPrim),
  ("GeneralisedNewtypeDeriving", GeneralizedNewtypeDeriving),
  ("GeneralizedNewtypeDeriving", GeneralizedNewtypeDeriving),
  ("Haskell2010", Haskell2010),
  ("Haskell98", Haskell98),
  ("HexFloatLiterals", HexFloatLiterals),
  ("IdentifierSyntax", IdentifierSyntax),
  ("ImplicitParameters", ImplicitParameters),
  ("ImplicitParams", ImplicitParameters),
  ("ImplicitPrelude", ImplicitPrelude),
  ("ImportQualifiedPost", ImportQualifiedPost),
  ("ImpredicativeTypes", ImpredicativeTypes),
  ("IncoherentInstances", IncoherentInstances),
  ("InferredTypeVariables", InferredTypeVariables),
  ("InstanceSigs", InstanceSigs),
  ("InterruptibleFFI", InterruptibleFFI),
  ("JavaScriptFFI", JavaScriptFFI),
  ("KindSignatures", KindSignatures),
  ("LambdaCase", LambdaCase),
  ("LexicalNegation", LexicalNegation),
  ("LiberalTypeSynonyms", LiberalTypeSynonyms),
  ("LinearTypes", LinearTypes),
  ("ListTuplePuns", ListTuplePuns),
  ("MagicHash", MagicHash),
  ("MonadComprehensions", MonadComprehensions),
  ("MonadFailDesugaring", MonadFailDesugaring),
  ("MonoLocalBinds", MonoLocalBinds),
  ("MonoPatBinds", MonoPatBinds),
  ("MonomorphismRestriction", MonomorphismRestriction),
  ("MultilineStrings", MultilineStrings),
  ("MultiParameterConstraints", MultiParameterConstraints),
  ("MultiParamTypeClasses", MultiParamTypeClasses),
  ("MultiWayIf", MultiWayIf),
  ("NPlusKPatterns", NPlusKPatterns),
  ("NamedDefaults", NamedDefaults),
  ("NamedFieldPuns", NamedFieldPuns),
  ("NamedWildCards", NamedWildCards),
  ("NegativeLiterals", NegativeLiterals),
  ("NondecreasingIndentation", NondecreasingIndentation),
  ("NullaryTypeClasses", NullaryTypeClasses),
  ("NumDecimals", NumDecimals),
  ("NumericUnderscores", NumericUnderscores),
  ("OrPatterns", OrPatterns),
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
  ("RequiredTypeArguments", RequiredTypeArguments),
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
  ("TypeAbstractions", TypeAbstractions),
  ("TypeApplications", TypeApplications),
  ("TypeData", TypeData),
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
