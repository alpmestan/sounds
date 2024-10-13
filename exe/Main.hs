{-# LANGUAGE DataKinds #-}

import Data.List (unfoldr)
import LambdaSound
import System.Random

import qualified Deque.Strict as DQ

karplusStrong
  :: (StdGen -> Maybe (Pulse, StdGen)) -- ^ how to initialize each entry of the initial buffer
  -> (StdGen -> Pulse -> Pulse -> (Pulse, StdGen)) -- ^ slide step
  -> Int -- ^ sample rate
  -> (Hz -> Sound 'I Pulse) -- ^ instrument
karplusStrong genPulse f sampleRate freq =
  let -- we generate s[0], ..., s[T-1]
      waveTable = take waveTableLen $ unfoldr genPulse gen
      -- construct our deque with those elements
      deque0 = DQ.fromConsAndSnocLists waveTable []
      -- sliding function
      slide (dq, g) = case DQ.uncons dq of
        Just (a, as)
          | Just a' <- DQ.head as ->
              let (new_a, g') = f g a a' in
                (a, (DQ.snoc new_a as, g'))
        _ -> error "welp"
  in unfoldrSoundPulse slide (deque0, gen)

  where waveTableLen = floor (fromIntegral sampleRate / freq) :: Int
        gen = mkStdGen 142

main :: IO ()
main = do
  let sampleRate = 44100

      attenuate a b = 0.995*(a+b)/2
      centeredPulse g = case randomR (-0.5, 0.5) g of
          (a, g') -> Just (Pulse a, g')

      guitar = karplusStrong centeredPulse (\_gen a a' -> (attenuate a a', _gen)) sampleRate
      snare = karplusStrong (\g -> Just (0.5, g))
        (\gen a a' -> case random gen of
            (b, gen') ->
              let v = attenuate a a'
              in (if b then v else negate v, gen')
        ) sampleRate

      -- guitar sounds
      sound1 = setDuration 2 $ asNote guitar a3
      sound2 = simpleReverb 0.01 sound1
      sound3 = setDuration 2 $ parallel [ asNote guitar x | x <- [c3, e3, g3] ]
      sound4 = simpleReverb 0.01 sound3

      -- drum sound
      sound5 = setDuration 0.3 $ asNote snare a3

      -- demo piece
      sound6 =
        let gtrLoop =
              [ ([c3, g3], [c4, d4+1])
              , ([a2+1, g3], [c4, d4+1])
              , ([g2+1, f3], [g4, f4])
              , ([f2, d3+1], [f4, d4+1])
              ]
            gtr = repeatSound 2 $ sequentially
              [ repeatSound 4 $ parallel
                -- we play the notes from 'l' in parallel, followed by the
                -- notes from 'r' in sequence
                [ setDuration 0.9 (parallel [ asNote guitar x | x <- l ])
                , setDuration 0.3 silence >>> sequentially (map (setDuration 0.3 . asNote guitar) r)
                ]
              | (l, r) <- gtrLoop
              ]
            dr1 = repeatSound 48 (setDuration 0.6 sound5)
            dr2 = repeatSound 96 (reduce 0.8 sound5)
            dr3 = repeatSound 32 (amplify 1.2 $ setDuration 0.75 silence >>> setDuration 0.15 sound5)
        in setDuration 2 silence >>> parallel [gtr, dr1, dr2, dr3]

      -- all the sounds from this post
      sounds = [sound1, sound2, sound3, sound4, sound5, sound6]

  mapM_ (play sampleRate 1) sounds
  sequence_ [ saveWav ("karplus_strong_" ++ show n ++ ".wav") (fromIntegral sampleRate) s | (n, s) <- zip [(1 :: Int)..] sounds ]
