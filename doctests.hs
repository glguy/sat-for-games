import Test.DocTest
main = doctest ["--fast", "-isrc", "src/TotalMap.hs", "src/Count.hs",
                "src/Select.hs", "src/Choice.hs", "src/LightsOut.hs",
                "src/Zhed.hs", "src/Ersatz/Prelude.hs"]
