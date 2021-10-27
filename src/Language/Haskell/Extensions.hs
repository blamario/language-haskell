{-# Language DeriveDataTypeable, OverloadedStrings #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ awaits

module Language.Haskell.Extensions (Extension(..), ExtensionSwitch(..),
                                    allExtensions, byName, switchesByName, implications) where

import Data.Data (Data, Typeable)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
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

data ExtensionSwitch = Yes Extension
                     | No Extension
                     deriving (Data, Eq, Ord, Show)

allExtensions :: Set Extension
allExtensions = Set.fromList [minBound .. maxBound]

implications :: Map Extension (Set ExtensionSwitch)
implications = Set.fromList <$> Map.fromList [
  (AutoDeriveTypeable, [Yes DeriveDataTypeable]),
  (DeriveTraversable, [Yes DeriveFoldable, Yes DeriveFunctor]),
  (DerivingVia, [Yes DerivingStrategies]),
  (ExistentialQuantification, [Yes ExplicitForAll]),
  (FlexibleInstances, [Yes TypeSynonymInstances]),
  (FunctionalDependencies, [Yes MultiParamTypeClasses]),
  (GADTs, [Yes GADTSyntax, Yes MonoLocalBinds]),
  (Haskell98, [Yes NPlusKPatterns, Yes NondecreasingIndentation,
               No DoAndIfThenElse, No EmptyDataDeclarations,
               No ForeignFunctionInterface, No PatternGuards, No RelaxedPolyRec]),
  (ImpredicativeTypes, [Yes ExplicitForAll, Yes RankNTypes]),
  (IncoherentInstances, [Yes OverlappingInstances]),
  (LiberalTypeSynonyms, [Yes ExplicitForAll]),
  (ParallelListComp, [Yes ParallelListComprehensions]),
  (PolyKinds, [Yes KindSignatures]),
  (RankNTypes, [Yes ExplicitForAll]),
  (RebindableSyntax, [No ImplicitPrelude]),
  (RecordWildCards, [Yes DisambiguateRecordFields]),
  (ScopedTypeVariables, [Yes ExplicitForAll]),
  (Safe, [Yes SafeImports]),
  (Trustworthy, [Yes SafeImports]),
  (TypeFamilies, [Yes ExplicitNamespaces, Yes KindSignatures, Yes MonoLocalBinds]),
  (TypeFamilyDependencies, [Yes ExplicitNamespaces, Yes KindSignatures, Yes MonoLocalBinds, Yes TypeFamilies]),
  (TypeOperators, [Yes ExplicitNamespaces]),
  (Unsafe, [Yes SafeImports])]

switchesByName :: (IsString t, Ord t, Semigroup t) => Map t ExtensionSwitch
switchesByName = (Yes <$> byName) <> (No <$> Map.mapKeysMonotonic ("No" <>) byName)

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
