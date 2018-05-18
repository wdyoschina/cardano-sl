{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Helpers
       (
       -- * From/to
         showReadTest
       , canonicalJsonTest
       ) where

import           Universum

import           Data.Functor.Identity (Identity (..))
import           Data.Typeable (typeRep)
import           Prelude (read)
import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary, Property, (.&&.), (===))
import qualified Text.JSON.Canonical as CanonicalJSON

import           Pos.Core.Genesis (SchemaError)

----------------------------------------------------------------------------
-- From/to tests
----------------------------------------------------------------------------

showReadId :: (Show a, Eq a, Read a) => a -> Property
showReadId a = read (show a) === a

type IdTestingRequiredClassesAlmost a = (Eq a, Show a, Arbitrary a, Typeable a)

type IdTestingRequiredClasses f a = (Eq a, Show a, Arbitrary a, Typeable a, f a)

identityTest :: forall a. (IdTestingRequiredClassesAlmost a) => (a -> Property) -> Spec
identityTest fun = prop (typeName @a) fun
  where
    typeName :: forall x. Typeable x => String
    typeName = show $ typeRep (Proxy @a)

showReadTest :: forall a. IdTestingRequiredClasses Read a => Spec
showReadTest = identityTest @a showReadId


type ToAndFromCanonicalJson a
     = ( CanonicalJSON.ToJSON Identity a
       , CanonicalJSON.FromJSON (Either SchemaError) a
       )

canonicalJsonTest ::
       forall a. (IdTestingRequiredClassesAlmost a, ToAndFromCanonicalJson a)
    => Spec
canonicalJsonTest =
    identityTest @a $ \x ->
        canonicalJsonRenderAndDecode x .&&. canonicalJsonPrettyAndDecode x
  where
    canonicalJsonRenderAndDecode x =
        let encodedX =
                CanonicalJSON.renderCanonicalJSON $
                runIdentity $ CanonicalJSON.toJSON x
        in canonicalJsonDecodeAndCompare x encodedX
    canonicalJsonPrettyAndDecode x =
        let encodedX =
                encodeUtf8 $
                CanonicalJSON.prettyCanonicalJSON $
                runIdentity $ CanonicalJSON.toJSON x
        in canonicalJsonDecodeAndCompare x encodedX
    canonicalJsonDecodeAndCompare ::
           CanonicalJSON.FromJSON (Either SchemaError) a
        => a
        -> LByteString
        -> Property
    canonicalJsonDecodeAndCompare x encodedX =
        let decodedValue =
                either (error . toText) identity $
                CanonicalJSON.parseCanonicalJSON encodedX
            decodedX =
                either (error . pretty @SchemaError) identity $
                CanonicalJSON.fromJSON decodedValue
        in decodedX === x
