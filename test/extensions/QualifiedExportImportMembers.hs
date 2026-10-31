{-# LANGUAGE QualifiedExportImportMembers #-}

module QualifiedExportImportMembers (M.Monoid(M.mempty), M.Last(L.getLast)) where

import qualified Data.Monoid as M
import qualified Data.Monoid as M (Monoid(M.mempty), Last(L.getLast))
