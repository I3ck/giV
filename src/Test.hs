module Main where

import Types
import Instances ()
import Process   (tryReadVersionTag)
import Version   (applyChange, version)
import Utils     (maybeRead, replace, ignoreFirstIncrement, tryReadVersion)
import Parse

import Test.Hspec

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "Utils" $ do
    it "maybeRead" $ do
      (maybeRead "a" :: Maybe Int)
        `shouldBe`
        Nothing

      (maybeRead "a12" :: Maybe Int)
        `shouldBe`
        Nothing

      (maybeRead "12a" :: Maybe Int)
        `shouldBe`
        Nothing

      (maybeRead "12a12" :: Maybe Int)
        `shouldBe`
        Nothing

      (maybeRead "12" :: Maybe Int)
        `shouldBe`
        Just 12

      (maybeRead "1212" :: Maybe Int)
        `shouldBe`
        Just 1212

      (maybeRead "1212.4" :: Maybe Double)
        `shouldBe`
        Just 1212.4

      (maybeRead "1212" :: Maybe Double)
        `shouldBe`
        Just 1212.0


    it "replace" $ do
      replace "aa" "bb" "cataab"
        `shouldBe`
        "catbbb"

      replace "aa" "bb" "cataabaa"
        `shouldBe`
        "catbbbbb"

      replace "aa" "bb" ""
        `shouldBe`
        ""


    it "tryReadVersion" $ do
      tryReadVersion "a"
        `shouldBe`
        Nothing

      tryReadVersion ""
        `shouldBe`
        Nothing

      tryReadVersion "1.2.3"
        `shouldBe`
        (Just $ Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 0})

      tryReadVersion "v1.2"
        `shouldBe`
        Nothing

      tryReadVersion "v1.2.3.4"
        `shouldBe`
        Nothing

      tryReadVersion "av1.2.3"
        `shouldBe`
        Nothing

      tryReadVersion "v1.2.3a"
        `shouldBe`
        Nothing

      tryReadVersion "vv1.2.3"
        `shouldBe`
        Nothing

      tryReadVersion "v1.2.3"
        `shouldBe`
        Nothing


    it "ignoreFirstIncrement" $ do
      ignoreFirstIncrement []
        `shouldBe`
        []

      ignoreFirstIncrement [Fix, Fix]
        `shouldBe`
        [Fix]

      ignoreFirstIncrement [Fix, Breaking]
        `shouldBe`
        [Breaking]

      ignoreFirstIncrement [Fix]
        `shouldBe`
        []

      ignoreFirstIncrement [Fix, Feature, SetTo mempty]
        `shouldBe`
        [Feature, SetTo mempty]

      ignoreFirstIncrement [SetTo mempty, Fix, Feature]
        `shouldBe`
        [SetTo mempty, Fix, Feature]

  describe "Process" $ do
    it "tryReadVersionTag" $ do
      tryReadVersionTag (Tag "a")
        `shouldBe`
        Nothing

      tryReadVersionTag (Tag "")
        `shouldBe`
        Nothing

      tryReadVersionTag (Tag "1.2.3")
        `shouldBe`
        Nothing

      tryReadVersionTag (Tag "v1.2")
        `shouldBe`
        Nothing

      tryReadVersionTag (Tag "v1.2.3.4")
        `shouldBe`
        Nothing

      tryReadVersionTag (Tag "av1.2.3")
        `shouldBe`
        Nothing

      tryReadVersionTag (Tag "v1.2.3a")
        `shouldBe`
        Nothing

      tryReadVersionTag (Tag "vv1.2.3")
        `shouldBe`
        Nothing

      tryReadVersionTag (Tag "v1.2.3")
        `shouldBe`
        (Just $ Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 0})

  describe "Version" $ do
    it "Default" $ do
      mempty `shouldBe` Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}
    it "Ordering" $ do
      Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}
        < Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}
        `shouldBe` False

      Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}
        < Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 1}
        `shouldBe` True

      Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 50}
        < Version{vmajor = 0, vminor = 0, vpatch = 1, vcount = 0}
        `shouldBe` True

      Version{vmajor = 0, vminor = 0, vpatch = 13, vcount = 0}
        < Version{vmajor = 0, vminor = 1, vpatch = 0,  vcount = 0}
        `shouldBe` True

      Version{vmajor = 0, vminor = 12, vpatch = 0, vcount = 0}
        < Version{vmajor = 1, vminor = 0,  vpatch = 0,  vcount = 0}
        `shouldBe` True

      Version{vmajor = 1, vminor = 0, vpatch = 0, vcount = 0}
        < Version{vmajor = 1, vminor = 0, vpatch = 0, vcount = 1}
        `shouldBe` True

      Version{vmajor = 1, vminor = 0, vpatch = 0, vcount = 0}
        < Version{vmajor = 2, vminor = 0, vpatch = 0, vcount = 0}
        `shouldBe` True

    it "Merging" $ do
      Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}
        <> Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}
        `shouldBe`
           Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}

      Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 1}
        <> Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}
        `shouldBe`
           Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 1}

      Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 1}
        <> Version{vmajor = 0, vminor = 1, vpatch = 0, vcount = 0}
        `shouldBe`
           Version{vmajor = 0, vminor = 1, vpatch = 0, vcount = 0}

      Version{vmajor = 2, vminor = 0, vpatch = 0, vcount = 1}
        <> Version{vmajor = 3, vminor = 1, vpatch = 0, vcount = 0}
        `shouldBe`
           Version{vmajor = 3, vminor = 1, vpatch = 0, vcount = 0}


    it "applychange" $ do
       applyChange NoChange Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}
         `shouldBe`
         Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 5}

       applyChange Fix Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}
         `shouldBe`
         Version{vmajor = 1, vminor = 2, vpatch = 4, vcount = 0}

       applyChange Feature Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}
         `shouldBe`
         Version{vmajor = 1, vminor = 3, vpatch = 0, vcount = 0}

       applyChange Breaking Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}
         `shouldBe`
         Version{vmajor = 2, vminor = 0, vpatch = 0, vcount = 0}



    it "version" $ do
      version Nothing BranchMaster{branch = [], master = []}
        `shouldBe`
        Version{vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [], master = []}
        `shouldBe`
        Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [], master = [Breaking]}
        `shouldBe`
        Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [], master = [Breaking, Fix]}
        `shouldBe`
        Version{vmajor = 1, vminor = 2, vpatch = 4, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [], master = [Breaking, Fix, NoChange]}
        `shouldBe`
        Version{vmajor = 1, vminor = 2, vpatch = 4, vcount = 1}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Fix], master = [Breaking, Fix, NoChange]}
        `shouldBe`
        Version{vmajor = 1, vminor = 2, vpatch = 5, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Fix, Feature], master = [Breaking, Fix, NoChange]}
        `shouldBe`
        Version{vmajor = 1, vminor = 3, vpatch = 0, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Fix, Feature, Feature], master = [Breaking, Fix, NoChange]}
        `shouldBe`
        Version{vmajor = 1, vminor = 4, vpatch = 0, vcount = 0}


      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Breaking], master = []}
        `shouldBe`
        Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Breaking, Feature], master = []}
        `shouldBe`
        Version{vmajor = 1, vminor = 3, vpatch = 0, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Breaking, Feature, Feature], master = []}
        `shouldBe`
        Version{vmajor = 1, vminor = 4, vpatch = 0, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Breaking, Feature, Feature, Fix], master = []}
        `shouldBe`
        Version{vmajor = 1, vminor = 4, vpatch = 1, vcount = 0}


      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [], master = [SetTo $ Version{vmajor = 2, vminor = 3, vpatch = 4, vcount = 5}]}
        `shouldBe`
        Version{vmajor = 2, vminor = 3, vpatch = 4, vcount = 5}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [], master = [SetTo $ Version{vmajor = 2, vminor = 3, vpatch = 4, vcount = 5}, Feature]}
        `shouldBe`
        Version{vmajor = 2, vminor = 4, vpatch = 0, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Fix], master = [SetTo $ Version{vmajor = 2, vminor = 3, vpatch = 4, vcount = 5}, Feature]}
        `shouldBe`
        Version{vmajor = 2, vminor = 4, vpatch = 1, vcount = 0}

  describe "Parse" $ do
    it "parseCommitString" $ do
      parseCommitString (CommitString "*hello world")
        `shouldBe`
        (pure [CommitRaw{refs = Nothing, rmessage = Message "hello world"}])

      parseCommitString (CommitString "tag: abc*hello world")
        `shouldBe`
        (pure [CommitRaw{refs = Just [Ref "tag: abc"], rmessage = Message "hello world"}])

      parseCommitString (CommitString "tag: abc, HEAD -> foo*hello world")
        `shouldBe`
        (pure [CommitRaw{refs = Just [Ref "tag: abc", Ref "HEAD -> foo"], rmessage = Message "hello world"}])

