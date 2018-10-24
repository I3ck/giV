module Main where

import Types
import Instances ()
import Process (tryReadVersionTag)
import Version (applyChange, version)

import Test.Hspec

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
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
        Version{vmajor = 2, vminor = 0, vpatch = 0, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [], master = [Breaking, Fix]}
        `shouldBe`
        Version{vmajor = 2, vminor = 0, vpatch = 1, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [], master = [Breaking, Fix, NoChange]}
        `shouldBe`
        Version{vmajor = 2, vminor = 0, vpatch = 1, vcount = 1}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Fix], master = [Breaking, Fix, NoChange]}
        `shouldBe`
        Version{vmajor = 2, vminor = 0, vpatch = 2, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Fix, Feature], master = [Breaking, Fix, NoChange]}
        `shouldBe`
        Version{vmajor = 2, vminor = 1, vpatch = 0, vcount = 0}

      version (Just Version{vmajor = 1, vminor = 2, vpatch = 3, vcount = 4}) BranchMaster{branch = [Fix, Feature, Feature], master = [Breaking, Fix, NoChange]}
        `shouldBe`
        Version{vmajor = 2, vminor = 2, vpatch = 0, vcount = 0}

