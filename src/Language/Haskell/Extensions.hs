{-# Language DeriveDataTypeable, OverloadedStrings #-}

-- | Missing syntax extensions:
-- * @QualifiedDo@ requires TemplateHaskell 2.17
-- * @TransformListComp@ is not supported by TemplateHaskell
-- * @Arrows@ is not supported by TemplateHaskell
-- * @LexicalNegation@ awaits

module Language.Haskell.Extensions (Extension(..), allExtensions, byName, implications) where

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
               | EmptyDataDecls
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
               | NoImplicitPrelude
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
               | ViewPatterns
               deriving (Data, Enum, Eq, Ord, Read, Show)

allExtensions :: Set Extension
allExtensions = Set.fromList [AllowAmbiguousTypes ..]

implications :: Map Extension (Set Extension)
implications = Set.fromList <$> Map.fromList [
  (AutoDeriveTypeable, [DeriveDataTypeable]),
  (DeriveTraversable, [DeriveFoldable, DeriveFunctor]),
  (DerivingVia, [DerivingStrategies]),
  (ExistentialQuantification, [ExplicitForAll]),
  (FlexibleInstances, [TypeSynonymInstances]),
  (FunctionalDependencies, [MultiParamTypeClasses]),
  (GADTs, [GADTSyntax, MonoLocalBinds]),
  (Haskell98, [NPlusKPatterns]),
  (ImpredicativeTypes, [ExplicitForAll, RankNTypes]),
  (IncoherentInstances, [OverlappingInstances]),
  (LexicalNegation, [NegativeLiterals]),
  (LiberalTypeSynonyms, [ExplicitForAll]),
  (PolyKinds, [KindSignatures]),
  (RankNTypes, [ExplicitForAll]),
  (RebindableSyntax, [NoImplicitPrelude]),
  (RecordWildCards, [DisambiguateRecordFields]),
  (ScopedTypeVariables, [ExplicitForAll]),
  (TypeFamilies, [ExplicitNamespaces, KindSignatures, MonoLocalBinds]),
  (TypeFamilyDependencies, [ExplicitNamespaces, KindSignatures, MonoLocalBinds, TypeFamilies]),
  (TypeOperators, [ExplicitNamespaces])]

byName :: (IsString t, Ord t) => Map t Extension
byName = Map.fromList [
  ("AlternativeLayoutRule", AlternativeLayoutRule),
  ("BlockArguments", BlockArguments),
  ("EmptyCase", EmptyCase),
  ("EmptyDataDecls", EmptyDataDeclarations),
  ("IdentifierSyntax", IdentifierSyntax),
  ("LambdaCase", LambdaCase),
  ("Haskell2010", Haskell2010),
  ("MagicHash", MagicHash),
  ("MonadComprehensions", MonadComprehensions),
  ("MonadFailDesugaring", MonadFailDesugaring),
  ("NoImplicitPrelude", NoImplicitPrelude),
  ("OverloadedLists", OverloadedLists),
  ("MultiWayIf", MultiWayIf),
  ("ParallelListComp", ParallelListComprehensions),
  ("RebindableSyntax", RebindableSyntax),
  ("RecursiveDo", RecursiveDo),
  ("TupleSections", TupleSections),
  ("UnicodeSyntax", UnicodeSyntax),
  ("Cpp", Cpp),
  ("OverlappingInstances", OverlappingInstances),
  ("UndecidableInstances", UndecidableInstances),
  ("IncoherentInstances", IncoherentInstances),
  ("UndecidableSuperClasses", UndecidableSuperClasses),
  ("MonomorphismRestriction", MonomorphismRestriction),
  ("MonoPatBinds", MonoPatBinds),
  ("MonoLocalBinds", MonoLocalBinds),
  ("RelaxedPolyRec", RelaxedPolyRec),
  ("ExtendedDefaultRules", ExtendedDefaultRules),
  ("ForeignFunctionInterface", ForeignFunctionInterface),
  ("UnliftedFFITypes", UnliftedFFITypes),
  ("InterruptibleFFI", InterruptibleFFI),
  ("CApiFFI", CApiFFI),
  ("GHCForeignImportPrim", GHCForeignImportPrim),
  ("JavaScriptFFI", JavaScriptFFI),
  ("ParallelArrays", ParallelArrays),
  ("Arrows", Arrows),
  ("TemplateHaskell", TemplateHaskell),
  ("TemplateHaskellQuotes", TemplateHaskellQuotes),
  ("QualifiedDo", QualifiedDo),
  ("QuasiQuotes", QuasiQuotes),
  ("ImplicitParams", ImplicitParams),
  ("ImplicitPrelude", ImplicitPrelude),
  ("ScopedTypeVariables", ScopedTypeVariables),
  ("AllowAmbiguousTypes", AllowAmbiguousTypes),
  ("UnboxedTuples", UnboxedTuples),
  ("UnboxedSums", UnboxedSums),
  ("UnliftedNewtypes", UnliftedNewtypes),
  ("BangPatterns", BangPatterns),
  ("TypeFamilies", TypeFamilies),
  ("TypeFamilyDependencies", TypeFamilyDependencies),
  ("TypeInType", TypeInType),
  ("OverloadedStrings", OverloadedStrings),
  ("NumDecimals", NumDecimals),
  ("DisambiguateRecordFields", DisambiguateRecordFields),
  ("RecordWildCards", RecordWildCards),
  ("RecordPuns", RecordPuns),
  ("ViewPatterns", ViewPatterns),
  ("GADTs", GADTs),
  ("GADTSyntax", GADTSyntax),
  ("NPlusKPatterns", NPlusKPatterns),
  ("DoAndIfThenElse", DoAndIfThenElse),
  ("ConstraintKinds", ConstraintKinds),
  ("PolyKinds", PolyKinds),
  ("DataKinds", DataKinds),
  ("InstanceSigs", InstanceSigs),
  ("ApplicativeDo", ApplicativeDo),
  ("LinearTypes", LinearTypes),
  ("StandaloneDeriving", StandaloneDeriving),
  ("DeriveDataTypeable", DeriveDataTypeable),
  ("AutoDeriveTypeable", AutoDeriveTypeable),
  ("DeriveFunctor", DeriveFunctor),
  ("DeriveTraversable", DeriveTraversable),
  ("DeriveFoldable", DeriveFoldable),
  ("DeriveGeneric", DeriveGeneric),
  ("DefaultSignatures", DefaultSignatures),
  ("DeriveAnyClass", DeriveAnyClass),
  ("DeriveLift", DeriveLift),
  ("DerivingStrategies", DerivingStrategies),
  ("DerivingVia", DerivingVia),
  ("TypeSynonymInstances", TypeSynonymInstances),
  ("FlexibleContexts", FlexibleContexts),
  ("FlexibleInstances", FlexibleInstances),
  ("ConstrainedClassMethods", ConstrainedClassMethods),
  ("MultiParamTypeClasses", MultiParamTypeClasses),
  ("NullaryTypeClasses", NullaryTypeClasses),
  ("FunctionalDependencies", FunctionalDependencies),
  ("ExistentialQuantification", ExistentialQuantification),
  ("EmptyDataDecls", EmptyDataDecls),
  ("KindSignatures", KindSignatures),
  ("RoleAnnotations", RoleAnnotations),
  ("ParallelListComp", ParallelListComp),
  ("TransformListComp", TransformListComp),
  ("GeneralizedNewtypeDeriving", GeneralizedNewtypeDeriving),
  ("PostfixOperators", PostfixOperators),
  ("PatternGuards", PatternGuards),
  ("LiberalTypeSynonyms", LiberalTypeSynonyms),
  ("RankNTypes", RankNTypes),
  ("ImpredicativeTypes", ImpredicativeTypes),
  ("TypeOperators", TypeOperators),
  ("ExplicitNamespaces", ExplicitNamespaces),
  ("PackageImports", PackageImports),
  ("ExplicitForAll", ExplicitForAll),
  ("AlternativeLayoutRuleTransitional", AlternativeLayoutRuleTransitional),
  ("DatatypeContexts", DatatypeContexts),
  ("NondecreasingIndentation", NondecreasingIndentation),
  ("RelaxedLayout", RelaxedLayout),
  ("TraditionalRecordSyntax", TraditionalRecordSyntax),
  ("BinaryLiterals", BinaryLiterals),
  ("NegativeLiterals", NegativeLiterals),
  ("HexFloatLiterals", HexFloatLiterals),
  ("DuplicateRecordFields", DuplicateRecordFields),
  ("OverloadedLabels", OverloadedLabels),
  ("EmptyCase", EmptyCase),
  ("PatternSynonyms", PatternSynonyms),
  ("PartialTypeSignatures", PartialTypeSignatures),
  ("NamedWildCards", NamedWildCards),
  ("StaticPointers", StaticPointers),
  ("TypeApplications", TypeApplications),
  ("Strict", Strict),
  ("StrictData", StrictData),
  ("EmptyDataDeriving", EmptyDataDeriving),
  ("NumericUnderscores", NumericUnderscores),
  ("QuantifiedConstraints", QuantifiedConstraints),
  ("StarIsType", StarIsType),
  ("ImportQualifiedPost", ImportQualifiedPost),
  ("CUSKs", CUSKs),
  ("StandaloneKindSignatures", StandaloneKindSignatures),
  ("LexicalNegation", LexicalNegation)]
