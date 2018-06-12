package language_R

import language_Bacht.BachTStore

import scala.collection.mutable

/**
  * We don't extend from BachtStore cause we don't have the same behavior at all
  * We need to keep track of timestamps, which can't be easily done with the BachtStore architecture
  */
class LanguageRStore extends BachTStore
{

    /**
      * We keep track of the current time
      */
    var simulationTime: Int = 0

    /**
      * Our store does now contain a timestamp in the key
      */
    var store: mutable.Map[(String, Int), Int] = mutable.Map[(String, Int), Int]()


    def tell(token: String, time: Int): Boolean =
    {
        val timedToken = (token, makeTimestamp(time))

        if ( store.contains(timedToken) )
        {
            store(timedToken) += 1
        }
        else
        {
            store = store ++ Map(timedToken -> 1)
        }
        true
    }

    private
    def makeTimestamp(time: Int) =
    {
        time + simulationTime
    }


    def ask(token: String, time: Int): Boolean =
    {
        val validTokens = store.filter
        { case ((name, timestamp), value) => (name equals token) && isStillValid(timestamp) && (value > 0)
        }

        tick()
        validTokens.nonEmpty
    }

    private
    def isStillValid(timestamp: Int): Boolean =
    {
        timestamp - simulationTime >= 0
    }

    def get(token: String, time: Int): Boolean =
    {
        val validTokens = store.filter
        { case ((name, timestamp), value) => (name equals token) && isStillValid(timestamp) && (value > 0)
        }

        if ( validTokens.nonEmpty && validTokens.contains((token, time)) )
        {
            store(token, time) -= 1
            tick()
            return true
        }

        tick()
        false

    }


    def nask(token: String, time: Int): Boolean =
    {
        val validTokens = store.filter
        { case ((name, timestamp), value) => (name equals token) && isStillValid(timestamp) && (value > 0)
        }

        tick()
        validTokens.nonEmpty || !validTokens.contains((token, time))

    }

    def tick(): Unit =
    {
        simulationTime += 1
        store = store.filter
        { case ((_, timestamp), _) => timestamp >= simulationTime }
    }

    override
    def printStore()
    {
        println("Simultation time : " + simulationTime)
        print("{ ")
        for ( (t, d) <- store ) print(t + "(" + store(t) + ")")
        println(" }")
    }

    override
    def clear_store(): Unit =
    {
        store = mutable.Map[(String, Int), Int]()
    }


}

object rStore extends LanguageRStore
{

    def reset()
    {
        clear_store()
    }

}
