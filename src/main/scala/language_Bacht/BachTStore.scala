package language_Bacht

import libs.Store

import scala.collection.mutable

class BachTStore extends Store
{
    var store: mutable.Map[String, Int] = mutable.Map[String, Int]()

    def tell(token: String): Boolean =
    {
        if ( store.contains(token) )
        {
            store(token) += 1
        }
        else
        {
            store = store ++ mutable.Map(token -> 1)
        }
        true
    }


    def ask(token: String): Boolean =
    {
        if ( store.contains(token) )
        {
            if ( store(token) >= 1 )
            {
                true
            }
            else
            {
                false
            }
        }
        else
        {
            false
        }
    }


    def get(token: String): Boolean =
    {
        if ( store.contains(token) )
        {
            if ( store(token) >= 1 )
            {
                store(token) = store(token) - 1
                true
            }
            else
            {
                false
            }
        }
        else
        {
            false
        }
    }


    def nask(token: String): Boolean =
    {
        if ( store.contains(token) )
        {
            if ( store(token) >= 1 )
            {
                false
            }
            else
            {
                true
            }
        }
        else
        {
            true
        }
    }

    def printStore()
    {
        print("{ ")
        for ( (t, d) <- store ) print(t + "(" + store(t) + ")")
        println(" }")
    }

    def clear_store()
    {
        store = mutable.Map[String, Int]()
    }

}

